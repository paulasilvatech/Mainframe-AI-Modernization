"""
Base Agent Framework for Mainframe Modernization
Production-ready implementation with error handling, metrics, and state management
"""

import asyncio
import logging
import time
import uuid
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Dict, List, Optional, Callable
from contextlib import asynccontextmanager

import aioredis
from prometheus_client import Counter, Histogram, Gauge, Info

# Metrics
agent_tasks_total = Counter('agent_tasks_total', 'Total agent tasks', ['agent_type', 'task_type', 'status'])
agent_task_duration = Histogram('agent_task_duration_seconds', 'Agent task duration', ['agent_type', 'task_type'])
agent_active_tasks = Gauge('agent_active_tasks', 'Currently active agent tasks', ['agent_type'])
agent_errors_total = Counter('agent_errors_total', 'Total agent errors', ['agent_type', 'error_type'])
agent_info = Info('agent', 'Agent information')


class AgentStatus(Enum):
    """Agent status enumeration"""
    INITIALIZING = "initializing"
    READY = "ready"
    BUSY = "busy"
    ERROR = "error"
    SHUTTING_DOWN = "shutting_down"
    STOPPED = "stopped"


class TaskStatus(Enum):
    """Task status enumeration"""
    PENDING = "pending"
    RUNNING = "running"
    COMPLETED = "completed"
    FAILED = "failed"
    CANCELLED = "cancelled"


@dataclass
class AgentState:
    """Agent state management"""
    status: AgentStatus = AgentStatus.INITIALIZING
    current_task: Optional[str] = None
    task_count: int = 0
    error_count: int = 0
    last_activity: datetime = field(default_factory=datetime.utcnow)
    metadata: Dict[str, Any] = field(default_factory=dict)


@dataclass
class TaskContext:
    """Context for task execution"""
    task_id: str
    task_type: str
    params: Dict[str, Any]
    created_at: datetime = field(default_factory=datetime.utcnow)
    started_at: Optional[datetime] = None
    completed_at: Optional[datetime] = None
    status: TaskStatus = TaskStatus.PENDING
    result: Optional[Any] = None
    error: Optional[str] = None
    metadata: Dict[str, Any] = field(default_factory=dict)


class BaseAgent(ABC):
    """
    Base class for all modernization agents
    Provides common functionality for error handling, metrics, and state management
    """
    
    def __init__(self, 
                 agent_id: Optional[str] = None,
                 agent_type: str = "base",
                 capabilities: List[str] = None,
                 config: Dict[str, Any] = None):
        """
        Initialize base agent
        
        Args:
            agent_id: Unique agent identifier
            agent_type: Type of agent (e.g., 'discovery', 'analysis', 'transformation')
            capabilities: List of agent capabilities
            config: Agent configuration
        """
        self.agent_id = agent_id or str(uuid.uuid4())
        self.agent_type = agent_type
        self.capabilities = capabilities or []
        self.config = config or {}
        
        # Logging
        self.logger = logging.getLogger(f"{self.__class__.__name__}[{self.agent_id}]")
        
        # State management
        self.state = AgentState()
        self._state_lock = asyncio.Lock()
        
        # Task management
        self._active_tasks: Dict[str, TaskContext] = {}
        self._task_history: List[TaskContext] = []
        self._max_history_size = self.config.get('max_history_size', 1000)
        
        # Redis connection for distributed state
        self._redis_client: Optional[aioredis.Redis] = None
        self._redis_prefix = f"agent:{self.agent_type}:{self.agent_id}"
        
        # Callbacks
        self._on_error_callbacks: List[Callable] = []
        self._on_complete_callbacks: List[Callable] = []
        
        # Update agent info metric
        agent_info.info({
            'agent_id': self.agent_id,
            'agent_type': self.agent_type,
            'capabilities': ','.join(self.capabilities)
        })
    
    async def initialize(self) -> None:
        """Initialize the agent"""
        try:
            self.logger.info(f"Initializing {self.agent_type} agent {self.agent_id}")
            
            # Connect to Redis if configured
            if self.config.get('redis_url'):
                await self._connect_redis()
            
            # Perform agent-specific initialization
            await self._initialize()
            
            # Update state
            await self._update_state(AgentStatus.READY)
            
            self.logger.info(f"Agent {self.agent_id} initialized successfully")
            
        except Exception as e:
            self.logger.error(f"Failed to initialize agent: {e}")
            await self._update_state(AgentStatus.ERROR)
            raise
    
    async def shutdown(self) -> None:
        """Shutdown the agent gracefully"""
        try:
            self.logger.info(f"Shutting down agent {self.agent_id}")
            
            await self._update_state(AgentStatus.SHUTTING_DOWN)
            
            # Cancel active tasks
            for task_id in list(self._active_tasks.keys()):
                await self.cancel_task(task_id)
            
            # Perform agent-specific shutdown
            await self._shutdown()
            
            # Disconnect from Redis
            if self._redis_client:
                await self._redis_client.close()
            
            await self._update_state(AgentStatus.STOPPED)
            
            self.logger.info(f"Agent {self.agent_id} shutdown complete")
            
        except Exception as e:
            self.logger.error(f"Error during shutdown: {e}")
            raise
    
    async def execute_task(self, task_type: str, params: Dict[str, Any]) -> Any:
        """
        Execute a task with full error handling and metrics
        
        Args:
            task_type: Type of task to execute
            params: Task parameters
            
        Returns:
            Task result
        """
        task_id = str(uuid.uuid4())
        task_context = TaskContext(
            task_id=task_id,
            task_type=task_type,
            params=params
        )
        
        # Track active task
        self._active_tasks[task_id] = task_context
        agent_active_tasks.labels(agent_type=self.agent_type).inc()
        
        try:
            # Update state
            await self._update_state(AgentStatus.BUSY, current_task=task_id)
            
            # Start timing
            start_time = time.time()
            task_context.started_at = datetime.utcnow()
            task_context.status = TaskStatus.RUNNING
            
            self.logger.info(f"Executing task {task_id} of type {task_type}")
            
            # Execute the task
            result = await self._execute_task(task_type, params, task_context)
            
            # Update task context
            task_context.completed_at = datetime.utcnow()
            task_context.status = TaskStatus.COMPLETED
            task_context.result = result
            
            # Record metrics
            duration = time.time() - start_time
            agent_task_duration.labels(
                agent_type=self.agent_type,
                task_type=task_type
            ).observe(duration)
            
            agent_tasks_total.labels(
                agent_type=self.agent_type,
                task_type=task_type,
                status='success'
            ).inc()
            
            self.logger.info(f"Task {task_id} completed successfully in {duration:.2f}s")
            
            # Execute callbacks
            await self._execute_callbacks(self._on_complete_callbacks, task_context)
            
            return result
            
        except asyncio.CancelledError:
            task_context.status = TaskStatus.CANCELLED
            agent_tasks_total.labels(
                agent_type=self.agent_type,
                task_type=task_type,
                status='cancelled'
            ).inc()
            raise
            
        except Exception as e:
            # Handle errors
            task_context.status = TaskStatus.FAILED
            task_context.error = str(e)
            
            agent_errors_total.labels(
                agent_type=self.agent_type,
                error_type=type(e).__name__
            ).inc()
            
            agent_tasks_total.labels(
                agent_type=self.agent_type,
                task_type=task_type,
                status='failed'
            ).inc()
            
            self.logger.error(f"Task {task_id} failed: {e}", exc_info=True)
            
            # Execute error callbacks
            await self._execute_callbacks(self._on_error_callbacks, task_context, e)
            
            # Update error count
            async with self._state_lock:
                self.state.error_count += 1
            
            raise
            
        finally:
            # Clean up
            self._active_tasks.pop(task_id, None)
            agent_active_tasks.labels(agent_type=self.agent_type).dec()
            
            # Add to history
            self._add_to_history(task_context)
            
            # Update state if no other active tasks
            if not self._active_tasks:
                await self._update_state(AgentStatus.READY, current_task=None)
    
    async def cancel_task(self, task_id: str) -> bool:
        """
        Cancel a running task
        
        Args:
            task_id: Task ID to cancel
            
        Returns:
            True if cancelled, False if not found
        """
        task_context = self._active_tasks.get(task_id)
        if not task_context:
            return False
        
        task_context.status = TaskStatus.CANCELLED
        self.logger.info(f"Cancelled task {task_id}")
        
        return True
    
    async def get_state(self) -> Dict[str, Any]:
        """Get current agent state"""
        async with self._state_lock:
            return {
                'agent_id': self.agent_id,
                'agent_type': self.agent_type,
                'status': self.state.status.value,
                'current_task': self.state.current_task,
                'task_count': self.state.task_count,
                'error_count': self.state.error_count,
                'last_activity': self.state.last_activity.isoformat(),
                'active_tasks': len(self._active_tasks),
                'capabilities': self.capabilities,
                'metadata': self.state.metadata
            }
    
    async def get_metrics(self) -> Dict[str, Any]:
        """Get agent metrics"""
        return {
            'agent_id': self.agent_id,
            'agent_type': self.agent_type,
            'total_tasks': self.state.task_count,
            'error_count': self.state.error_count,
            'active_tasks': len(self._active_tasks),
            'success_rate': (
                (self.state.task_count - self.state.error_count) / self.state.task_count
                if self.state.task_count > 0 else 0
            ),
            'average_task_duration': self._calculate_average_duration(),
            'task_history': [
                {
                    'task_id': t.task_id,
                    'task_type': t.task_type,
                    'status': t.status.value,
                    'duration': (
                        (t.completed_at - t.started_at).total_seconds()
                        if t.completed_at and t.started_at else None
                    )
                }
                for t in self._task_history[-10:]  # Last 10 tasks
            ]
        }
    
    def add_error_callback(self, callback: Callable) -> None:
        """Add error callback"""
        self._on_error_callbacks.append(callback)
    
    def add_complete_callback(self, callback: Callable) -> None:
        """Add completion callback"""
        self._on_complete_callbacks.append(callback)
    
    # Abstract methods to be implemented by subclasses
    
    @abstractmethod
    async def _initialize(self) -> None:
        """Agent-specific initialization"""
        pass
    
    @abstractmethod
    async def _shutdown(self) -> None:
        """Agent-specific shutdown"""
        pass
    
    @abstractmethod
    async def _execute_task(self, task_type: str, params: Dict[str, Any], 
                           context: TaskContext) -> Any:
        """
        Execute agent-specific task
        
        Args:
            task_type: Type of task
            params: Task parameters
            context: Task context
            
        Returns:
            Task result
        """
        pass
    
    # Protected helper methods
    
    async def _update_state(self, status: AgentStatus, **kwargs) -> None:
        """Update agent state"""
        async with self._state_lock:
            self.state.status = status
            self.state.last_activity = datetime.utcnow()
            
            for key, value in kwargs.items():
                if hasattr(self.state, key):
                    setattr(self.state, key, value)
            
            # Persist to Redis if available
            if self._redis_client:
                await self._persist_state()
    
    async def _connect_redis(self) -> None:
        """Connect to Redis for distributed state"""
        try:
            self._redis_client = await aioredis.create_redis_pool(
                self.config['redis_url'],
                encoding='utf-8'
            )
            self.logger.info("Connected to Redis for state management")
        except Exception as e:
            self.logger.warning(f"Failed to connect to Redis: {e}")
            # Continue without Redis (local state only)
    
    async def _persist_state(self) -> None:
        """Persist state to Redis"""
        if not self._redis_client:
            return
        
        try:
            state_data = {
                'status': self.state.status.value,
                'current_task': self.state.current_task,
                'task_count': str(self.state.task_count),
                'error_count': str(self.state.error_count),
                'last_activity': self.state.last_activity.isoformat()
            }
            
            await self._redis_client.hmset_dict(
                f"{self._redis_prefix}:state",
                state_data
            )
            
            # Set expiration
            await self._redis_client.expire(
                f"{self._redis_prefix}:state",
                3600  # 1 hour
            )
            
        except Exception as e:
            self.logger.error(f"Failed to persist state: {e}")
    
    def _add_to_history(self, task_context: TaskContext) -> None:
        """Add task to history with size limit"""
        self._task_history.append(task_context)
        
        # Maintain history size limit
        if len(self._task_history) > self._max_history_size:
            self._task_history = self._task_history[-self._max_history_size:]
        
        # Update task count
        self.state.task_count += 1
    
    def _calculate_average_duration(self) -> float:
        """Calculate average task duration from history"""
        durations = []
        
        for task in self._task_history:
            if task.started_at and task.completed_at:
                duration = (task.completed_at - task.started_at).total_seconds()
                durations.append(duration)
        
        return sum(durations) / len(durations) if durations else 0
    
    async def _execute_callbacks(self, callbacks: List[Callable], 
                                context: TaskContext, 
                                error: Optional[Exception] = None) -> None:
        """Execute callbacks safely"""
        for callback in callbacks:
            try:
                if asyncio.iscoroutinefunction(callback):
                    await callback(self, context, error)
                else:
                    callback(self, context, error)
            except Exception as e:
                self.logger.error(f"Error in callback: {e}")
    
    @asynccontextmanager
    async def _task_timer(self, task_type: str):
        """Context manager for timing tasks"""
        start_time = time.time()
        try:
            yield
        finally:
            duration = time.time() - start_time
            agent_task_duration.labels(
                agent_type=self.agent_type,
                task_type=task_type
            ).observe(duration)


class AgentException(Exception):
    """Base exception for agent errors"""
    pass


class TaskExecutionError(AgentException):
    """Task execution error"""
    pass


class AgentNotReadyError(AgentException):
    """Agent not ready error"""
    pass


class ResourceExhaustedError(AgentException):
    """Resource exhausted error"""
    pass 