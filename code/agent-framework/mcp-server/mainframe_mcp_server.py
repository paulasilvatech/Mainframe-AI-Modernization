"""
Mainframe MCP Server
Production-ready Model Context Protocol server for mainframe modernization
Provides context management, tool registration, and agent coordination
"""

import asyncio
import json
import uuid
import logging
from datetime import datetime
from typing import Dict, Any, List, Optional, Callable, Set
from dataclasses import dataclass, field
from collections import defaultdict
from contextlib import asynccontextmanager

from fastapi import FastAPI, WebSocket, WebSocketDisconnect, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel, Field
import uvicorn


# Request/Response Models

class AgentRegistration(BaseModel):
    """Agent registration request"""
    agent_id: Optional[str] = None
    agent_type: str
    capabilities: List[str]
    metadata: Dict[str, Any] = Field(default_factory=dict)


class ToolRegistration(BaseModel):
    """Tool registration request"""
    tool_name: str
    description: str
    parameters: Dict[str, Any]
    required_parameters: List[str] = Field(default_factory=list)
    agent_id: Optional[str] = None


class ToolExecutionRequest(BaseModel):
    """Tool execution request"""
    tool_name: str
    parameters: Dict[str, Any]
    context_id: Optional[str] = None
    timeout: Optional[int] = 300  # seconds


class ContextRequest(BaseModel):
    """Context management request"""
    context_type: str  # 'cobol', 'natural', 'pl1', etc.
    scope: Optional[str] = 'session'  # 'session', 'global', 'agent'
    metadata: Dict[str, Any] = Field(default_factory=dict)


# Internal Data Models

@dataclass
class Agent:
    """Registered agent information"""
    agent_id: str
    agent_type: str
    capabilities: List[str]
    metadata: Dict[str, Any]
    websocket: Optional[WebSocket] = None
    registered_at: datetime = field(default_factory=datetime.utcnow)
    last_heartbeat: datetime = field(default_factory=datetime.utcnow)
    status: str = "active"


@dataclass
class Tool:
    """Registered tool information"""
    tool_name: str
    description: str
    parameters: Dict[str, Any]
    required_parameters: List[str]
    agent_id: Optional[str]
    handler: Optional[Callable] = None
    registered_at: datetime = field(default_factory=datetime.utcnow)


@dataclass
class Context:
    """Context information"""
    context_id: str
    context_type: str
    scope: str
    data: Dict[str, Any] = field(default_factory=dict)
    created_at: datetime = field(default_factory=datetime.utcnow)
    updated_at: datetime = field(default_factory=datetime.utcnow)
    accessed_by: Set[str] = field(default_factory=set)


@dataclass
class TaskExecution:
    """Task execution tracking"""
    task_id: str
    tool_name: str
    parameters: Dict[str, Any]
    agent_id: Optional[str]
    status: str = "pending"  # pending, running, completed, failed
    result: Optional[Any] = None
    error: Optional[str] = None
    started_at: Optional[datetime] = None
    completed_at: Optional[datetime] = None


class MainframeMCPServer:
    """
    Production MCP Server for Mainframe Modernization
    Manages agents, tools, contexts, and orchestration
    """
    
    def __init__(self, config: Dict[str, Any] = None):
        self.config = config or {}
        self.logger = logging.getLogger(__name__)
        
        # Storage
        self.agents: Dict[str, Agent] = {}
        self.tools: Dict[str, Tool] = {}
        self.contexts: Dict[str, Context] = {}
        self.executions: Dict[str, TaskExecution] = {}
        
        # WebSocket connections
        self.websockets: Dict[str, WebSocket] = {}
        
        # Mainframe-specific contexts
        self._init_mainframe_contexts()
        
        # Create FastAPI app
        self.app = self._create_app()
        
    def _init_mainframe_contexts(self):
        """Initialize mainframe-specific contexts"""
        self.language_contexts = {
            'cobol': {
                'patterns': {
                    'divisions': ['IDENTIFICATION', 'ENVIRONMENT', 'DATA', 'PROCEDURE'],
                    'data_types': ['PIC', 'COMP', 'COMP-3', 'DISPLAY'],
                    'verbs': ['MOVE', 'COMPUTE', 'PERFORM', 'IF', 'EVALUATE']
                },
                'conventions': {
                    'naming': 'UPPERCASE-WITH-HYPHENS',
                    'indentation': 'column-based',
                    'max_line_length': 72
                }
            },
            'natural': {
                'patterns': {
                    'statements': ['DEFINE DATA', 'READ', 'FIND', 'STORE', 'UPDATE'],
                    'data_types': ['A', 'N', 'P', 'I', 'L'],
                    'constructs': ['FOR', 'REPEAT', 'DECIDE', 'ESCAPE']
                },
                'conventions': {
                    'naming': 'MIXED-CASE',
                    'indentation': 'structured',
                    'modules': ['programs', 'subprograms', 'maps', 'copycodes']
                }
            },
            'pl1': {
                'patterns': {
                    'declarations': ['DECLARE', 'DCL'],
                    'data_types': ['FIXED', 'FLOAT', 'CHAR', 'BIT'],
                    'control': ['DO', 'IF', 'SELECT', 'CALL']
                },
                'conventions': {
                    'naming': 'mixed_case_underscore',
                    'structure': 'block-structured'
                }
            }
        }
        
        # Initialize global contexts
        for lang, config in self.language_contexts.items():
            context = Context(
                context_id=f"global_{lang}_context",
                context_type=lang,
                scope='global',
                data=config
            )
            self.contexts[context.context_id] = context
    
    def _create_app(self) -> FastAPI:
        """Create FastAPI application"""
        app = FastAPI(
            title="Mainframe MCP Server",
            description="Model Context Protocol server for mainframe modernization",
            version="1.0.0"
        )
        
        # Add CORS middleware
        app.add_middleware(
            CORSMiddleware,
            allow_origins=self.config.get('cors_origins', ["*"]),
            allow_credentials=True,
            allow_methods=["*"],
            allow_headers=["*"],
        )
        
        # Register routes
        self._register_routes(app)
        
        return app
    
    def _register_routes(self, app: FastAPI):
        """Register API routes"""
        
        @app.get("/health")
        async def health_check():
            return {
                "status": "healthy",
                "timestamp": datetime.utcnow().isoformat(),
                "agents": len(self.agents),
                "tools": len(self.tools),
                "active_executions": sum(1 for e in self.executions.values() if e.status == "running")
            }
        
        @app.post("/agents/register")
        async def register_agent(registration: AgentRegistration):
            """Register a new agent"""
            agent_id = registration.agent_id or str(uuid.uuid4())
            
            agent = Agent(
                agent_id=agent_id,
                agent_type=registration.agent_type,
                capabilities=registration.capabilities,
                metadata=registration.metadata
            )
            
            self.agents[agent_id] = agent
            self.logger.info(f"Registered agent {agent_id} of type {registration.agent_type}")
            
            return {
                "agent_id": agent_id,
                "status": "registered",
                "timestamp": agent.registered_at.isoformat()
            }
        
        @app.get("/agents")
        async def list_agents():
            """List all registered agents"""
            return {
                "agents": [
                    {
                        "agent_id": agent.agent_id,
                        "agent_type": agent.agent_type,
                        "capabilities": agent.capabilities,
                        "status": agent.status,
                        "last_heartbeat": agent.last_heartbeat.isoformat()
                    }
                    for agent in self.agents.values()
                ]
            }
        
        @app.get("/agents/{agent_id}")
        async def get_agent(agent_id: str):
            """Get specific agent details"""
            agent = self.agents.get(agent_id)
            if not agent:
                raise HTTPException(status_code=404, detail="Agent not found")
            
            return {
                "agent_id": agent.agent_id,
                "agent_type": agent.agent_type,
                "capabilities": agent.capabilities,
                "metadata": agent.metadata,
                "status": agent.status,
                "registered_at": agent.registered_at.isoformat(),
                "last_heartbeat": agent.last_heartbeat.isoformat()
            }
        
        @app.post("/tools/register")
        async def register_tool(registration: ToolRegistration):
            """Register a new tool"""
            tool = Tool(
                tool_name=registration.tool_name,
                description=registration.description,
                parameters=registration.parameters,
                required_parameters=registration.required_parameters,
                agent_id=registration.agent_id
            )
            
            self.tools[registration.tool_name] = tool
            self.logger.info(f"Registered tool {registration.tool_name}")
            
            return {
                "tool_name": registration.tool_name,
                "status": "registered",
                "timestamp": tool.registered_at.isoformat()
            }
        
        @app.get("/tools")
        async def list_tools():
            """List all registered tools"""
            return {
                "tools": [
                    {
                        "tool_name": tool.tool_name,
                        "description": tool.description,
                        "parameters": tool.parameters,
                        "agent_id": tool.agent_id
                    }
                    for tool in self.tools.values()
                ]
            }
        
        @app.post("/tools/execute")
        async def execute_tool(request: ToolExecutionRequest):
            """Execute a tool"""
            tool = self.tools.get(request.tool_name)
            if not tool:
                raise HTTPException(status_code=404, detail="Tool not found")
            
            # Validate required parameters
            missing_params = []
            for param in tool.required_parameters:
                if param not in request.parameters:
                    missing_params.append(param)
            
            if missing_params:
                raise HTTPException(
                    status_code=400, 
                    detail=f"Missing required parameters: {missing_params}"
                )
            
            # Create execution record
            task_id = str(uuid.uuid4())
            execution = TaskExecution(
                task_id=task_id,
                tool_name=request.tool_name,
                parameters=request.parameters,
                agent_id=tool.agent_id
            )
            
            self.executions[task_id] = execution
            
            # Execute tool asynchronously
            asyncio.create_task(self._execute_tool(execution, request.context_id))
            
            return {
                "task_id": task_id,
                "status": "started",
                "tool_name": request.tool_name
            }
        
        @app.get("/tasks/{task_id}")
        async def get_task_status(task_id: str):
            """Get task execution status"""
            execution = self.executions.get(task_id)
            if not execution:
                raise HTTPException(status_code=404, detail="Task not found")
            
            response = {
                "task_id": execution.task_id,
                "tool_name": execution.tool_name,
                "status": execution.status,
                "started_at": execution.started_at.isoformat() if execution.started_at else None,
                "completed_at": execution.completed_at.isoformat() if execution.completed_at else None
            }
            
            if execution.status == "completed":
                response["result"] = execution.result
            elif execution.status == "failed":
                response["error"] = execution.error
            
            return response
        
        @app.post("/contexts/create")
        async def create_context(request: ContextRequest):
            """Create a new context"""
            context_id = str(uuid.uuid4())
            
            # Initialize with language-specific defaults
            initial_data = {}
            if request.context_type in self.language_contexts:
                initial_data = self.language_contexts[request.context_type].copy()
            
            initial_data.update(request.metadata)
            
            context = Context(
                context_id=context_id,
                context_type=request.context_type,
                scope=request.scope,
                data=initial_data
            )
            
            self.contexts[context_id] = context
            
            return {
                "context_id": context_id,
                "context_type": request.context_type,
                "scope": request.scope,
                "created_at": context.created_at.isoformat()
            }
        
        @app.get("/contexts/{context_id}")
        async def get_context(context_id: str):
            """Get context data"""
            context = self.contexts.get(context_id)
            if not context:
                raise HTTPException(status_code=404, detail="Context not found")
            
            return {
                "context_id": context.context_id,
                "context_type": context.context_type,
                "scope": context.scope,
                "data": context.data,
                "created_at": context.created_at.isoformat(),
                "updated_at": context.updated_at.isoformat(),
                "accessed_by": list(context.accessed_by)
            }
        
        @app.put("/contexts/{context_id}")
        async def update_context(context_id: str, data: Dict[str, Any]):
            """Update context data"""
            context = self.contexts.get(context_id)
            if not context:
                raise HTTPException(status_code=404, detail="Context not found")
            
            context.data.update(data)
            context.updated_at = datetime.utcnow()
            
            return {
                "context_id": context_id,
                "status": "updated",
                "updated_at": context.updated_at.isoformat()
            }
        
        @app.websocket("/ws/{agent_id}")
        async def websocket_endpoint(websocket: WebSocket, agent_id: str):
            """WebSocket endpoint for real-time communication"""
            await websocket.accept()
            
            # Store WebSocket connection
            self.websockets[agent_id] = websocket
            
            # Update agent if exists
            if agent_id in self.agents:
                self.agents[agent_id].websocket = websocket
                self.agents[agent_id].status = "connected"
            
            try:
                while True:
                    # Receive messages
                    data = await websocket.receive_json()
                    
                    # Handle different message types
                    if data.get("type") == "heartbeat":
                        await self._handle_heartbeat(agent_id)
                    elif data.get("type") == "tool_result":
                        await self._handle_tool_result(data)
                    elif data.get("type") == "context_update":
                        await self._handle_context_update(data)
                    else:
                        await websocket.send_json({
                            "type": "error",
                            "message": f"Unknown message type: {data.get('type')}"
                        })
                        
            except WebSocketDisconnect:
                # Clean up on disconnect
                self.websockets.pop(agent_id, None)
                if agent_id in self.agents:
                    self.agents[agent_id].websocket = None
                    self.agents[agent_id].status = "disconnected"
                self.logger.info(f"Agent {agent_id} disconnected")
    
    async def _execute_tool(self, execution: TaskExecution, context_id: Optional[str]):
        """Execute a tool asynchronously"""
        try:
            execution.status = "running"
            execution.started_at = datetime.utcnow()
            
            tool = self.tools[execution.tool_name]
            
            # Get context if specified
            context = None
            if context_id:
                context = self.contexts.get(context_id)
                if context:
                    context.accessed_by.add(execution.agent_id or "system")
            
            # If tool has a registered handler, use it
            if tool.handler:
                result = await tool.handler(execution.parameters, context)
            # If tool is associated with an agent, send via WebSocket
            elif tool.agent_id and tool.agent_id in self.websockets:
                result = await self._execute_via_agent(
                    tool.agent_id,
                    execution.tool_name,
                    execution.parameters,
                    context
                )
            else:
                # Built-in tool execution
                result = await self._execute_builtin_tool(
                    execution.tool_name,
                    execution.parameters,
                    context
                )
            
            execution.result = result
            execution.status = "completed"
            execution.completed_at = datetime.utcnow()
            
        except Exception as e:
            execution.error = str(e)
            execution.status = "failed"
            execution.completed_at = datetime.utcnow()
            self.logger.error(f"Tool execution failed: {e}")
    
    async def _execute_via_agent(self, agent_id: str, tool_name: str, 
                                parameters: Dict[str, Any], context: Optional[Context]) -> Any:
        """Execute tool via agent WebSocket"""
        websocket = self.websockets.get(agent_id)
        if not websocket:
            raise Exception(f"Agent {agent_id} not connected")
        
        # Send execution request
        request_id = str(uuid.uuid4())
        await websocket.send_json({
            "type": "tool_execution",
            "request_id": request_id,
            "tool_name": tool_name,
            "parameters": parameters,
            "context": context.data if context else None
        })
        
        # Wait for response (simplified - production would use proper async handling)
        # In production, this would use a response queue or similar mechanism
        await asyncio.sleep(0.1)  # Placeholder
        
        return {"status": "executed_via_agent", "agent_id": agent_id}
    
    async def _execute_builtin_tool(self, tool_name: str, parameters: Dict[str, Any], 
                                   context: Optional[Context]) -> Any:
        """Execute built-in MCP tools"""
        
        # Mainframe-specific built-in tools
        if tool_name == "analyze_cobol_structure":
            return await self._analyze_cobol_structure(parameters, context)
        elif tool_name == "extract_business_logic":
            return await self._extract_business_logic(parameters, context)
        elif tool_name == "generate_test_cases":
            return await self._generate_test_cases(parameters, context)
        elif tool_name == "assess_complexity":
            return await self._assess_complexity(parameters, context)
        elif tool_name == "map_dependencies":
            return await self._map_dependencies(parameters, context)
        else:
            raise Exception(f"Unknown built-in tool: {tool_name}")
    
    async def _analyze_cobol_structure(self, parameters: Dict[str, Any], 
                                     context: Optional[Context]) -> Dict[str, Any]:
        """Built-in COBOL structure analysis"""
        code = parameters.get('code', '')
        
        # Simple structure analysis (production would use proper parser)
        structure = {
            'divisions': [],
            'sections': [],
            'paragraphs': [],
            'data_items': 0,
            'procedures': 0
        }
        
        # Extract divisions
        import re
        division_pattern = re.compile(r'(\w+)\s+DIVISION', re.IGNORECASE)
        for match in division_pattern.finditer(code):
            structure['divisions'].append({
                'name': match.group(1),
                'position': match.start()
            })
        
        return {
            'structure': structure,
            'analysis_timestamp': datetime.utcnow().isoformat()
        }
    
    async def _extract_business_logic(self, parameters: Dict[str, Any], 
                                    context: Optional[Context]) -> Dict[str, Any]:
        """Extract business logic from code"""
        code = parameters.get('code', '')
        language = parameters.get('language', 'cobol')
        
        # Simplified business logic extraction
        rules = []
        
        if language.lower() == 'cobol':
            # Look for COMPUTE statements
            import re
            compute_pattern = re.compile(r'COMPUTE\s+(.*?)(?:END-COMPUTE|\.)', re.IGNORECASE | re.DOTALL)
            for i, match in enumerate(compute_pattern.finditer(code)):
                rules.append({
                    'id': f'BR-{i+1:03d}',
                    'type': 'calculation',
                    'expression': match.group(1).strip(),
                    'location': match.start()
                })
        
        return {
            'rules': rules,
            'total_rules': len(rules),
            'extraction_timestamp': datetime.utcnow().isoformat()
        }
    
    async def _generate_test_cases(self, parameters: Dict[str, Any], 
                                 context: Optional[Context]) -> Dict[str, Any]:
        """Generate test cases based on code analysis"""
        business_rules = parameters.get('business_rules', [])
        coverage_target = parameters.get('coverage_target', 80)
        
        test_cases = []
        
        for rule in business_rules:
            # Generate test cases for each business rule
            test_cases.append({
                'test_id': f"TC-{rule['id']}",
                'description': f"Test for {rule['type']} rule {rule['id']}",
                'input': self._generate_test_input(rule),
                'expected_output': self._generate_expected_output(rule),
                'test_type': 'unit'
            })
        
        return {
            'test_cases': test_cases,
            'total_tests': len(test_cases),
            'estimated_coverage': min(len(test_cases) * 10, coverage_target),
            'generation_timestamp': datetime.utcnow().isoformat()
        }
    
    def _generate_test_input(self, rule: Dict[str, Any]) -> Dict[str, Any]:
        """Generate test input for a business rule"""
        # Simplified test input generation
        return {
            'type': 'generated',
            'values': {}
        }
    
    def _generate_expected_output(self, rule: Dict[str, Any]) -> Any:
        """Generate expected output for test"""
        # Simplified expected output
        return {
            'type': 'calculated',
            'value': None
        }
    
    async def _assess_complexity(self, parameters: Dict[str, Any], 
                               context: Optional[Context]) -> Dict[str, Any]:
        """Assess code complexity"""
        code = parameters.get('code', '')
        
        # Simple complexity metrics
        lines = code.split('\n')
        
        return {
            'metrics': {
                'lines_of_code': len(lines),
                'cyclomatic_complexity': self._calculate_cyclomatic_complexity(code),
                'nesting_depth': self._calculate_nesting_depth(code),
                'comment_ratio': self._calculate_comment_ratio(lines)
            },
            'complexity_rating': 'medium',  # Simplified
            'assessment_timestamp': datetime.utcnow().isoformat()
        }
    
    def _calculate_cyclomatic_complexity(self, code: str) -> int:
        """Calculate cyclomatic complexity"""
        # Simplified - count decision points
        complexity = 1
        decision_keywords = ['IF', 'WHEN', 'UNTIL', 'WHILE']
        
        for keyword in decision_keywords:
            complexity += code.upper().count(keyword)
        
        return complexity
    
    def _calculate_nesting_depth(self, code: str) -> int:
        """Calculate maximum nesting depth"""
        # Simplified nesting calculation
        max_depth = 0
        current_depth = 0
        
        for line in code.split('\n'):
            if 'IF' in line.upper() or 'PERFORM' in line.upper():
                current_depth += 1
                max_depth = max(max_depth, current_depth)
            elif 'END-IF' in line.upper() or 'END-PERFORM' in line.upper():
                current_depth = max(0, current_depth - 1)
        
        return max_depth
    
    def _calculate_comment_ratio(self, lines: List[str]) -> float:
        """Calculate comment to code ratio"""
        comment_lines = sum(1 for line in lines if line.strip().startswith('*'))
        return comment_lines / max(len(lines), 1)
    
    async def _map_dependencies(self, parameters: Dict[str, Any], 
                               context: Optional[Context]) -> Dict[str, Any]:
        """Map code dependencies"""
        code = parameters.get('code', '')
        
        dependencies = {
            'programs': [],
            'copybooks': [],
            'files': [],
            'tables': []
        }
        
        # Extract dependencies (simplified)
        import re
        
        # CALL statements
        call_pattern = re.compile(r'CALL\s+["\'](\w+)["\']', re.IGNORECASE)
        for match in call_pattern.finditer(code):
            dependencies['programs'].append(match.group(1))
        
        # COPY statements
        copy_pattern = re.compile(r'COPY\s+(\w+)', re.IGNORECASE)
        for match in copy_pattern.finditer(code):
            dependencies['copybooks'].append(match.group(1))
        
        return {
            'dependencies': dependencies,
            'total_dependencies': sum(len(deps) for deps in dependencies.values()),
            'mapping_timestamp': datetime.utcnow().isoformat()
        }
    
    async def _handle_heartbeat(self, agent_id: str):
        """Handle agent heartbeat"""
        if agent_id in self.agents:
            self.agents[agent_id].last_heartbeat = datetime.utcnow()
            
            # Send acknowledgment
            websocket = self.websockets.get(agent_id)
            if websocket:
                await websocket.send_json({
                    "type": "heartbeat_ack",
                    "timestamp": datetime.utcnow().isoformat()
                })
    
    async def _handle_tool_result(self, data: Dict[str, Any]):
        """Handle tool execution result from agent"""
        request_id = data.get('request_id')
        result = data.get('result')
        error = data.get('error')
        
        # Update execution record
        # In production, this would map request_id to task_id
        # For now, this is a placeholder
        self.logger.info(f"Received tool result for request {request_id}")
    
    async def _handle_context_update(self, data: Dict[str, Any]):
        """Handle context update from agent"""
        context_id = data.get('context_id')
        updates = data.get('updates', {})
        
        if context_id in self.contexts:
            context = self.contexts[context_id]
            context.data.update(updates)
            context.updated_at = datetime.utcnow()
            
            self.logger.info(f"Updated context {context_id}")
    
    @asynccontextmanager
    async def lifespan(self):
        """Server lifecycle management"""
        # Startup
        self.logger.info("Starting Mainframe MCP Server")
        
        # Initialize built-in tools
        await self._register_builtin_tools()
        
        # Start background tasks
        self._background_tasks = [
            asyncio.create_task(self._monitor_agents()),
            asyncio.create_task(self._cleanup_executions())
        ]
        
        yield
        
        # Shutdown
        self.logger.info("Shutting down Mainframe MCP Server")
        
        # Cancel background tasks
        for task in self._background_tasks:
            task.cancel()
        
        # Close WebSocket connections
        for ws in self.websockets.values():
            await ws.close()
    
    async def _register_builtin_tools(self):
        """Register built-in MCP tools"""
        builtin_tools = [
            {
                'tool_name': 'analyze_cobol_structure',
                'description': 'Analyze COBOL program structure',
                'parameters': {
                    'code': {'type': 'string', 'description': 'COBOL source code'}
                },
                'required_parameters': ['code']
            },
            {
                'tool_name': 'extract_business_logic',
                'description': 'Extract business logic from mainframe code',
                'parameters': {
                    'code': {'type': 'string', 'description': 'Source code'},
                    'language': {'type': 'string', 'description': 'Programming language'}
                },
                'required_parameters': ['code']
            },
            {
                'tool_name': 'generate_test_cases',
                'description': 'Generate test cases from business rules',
                'parameters': {
                    'business_rules': {'type': 'array', 'description': 'List of business rules'},
                    'coverage_target': {'type': 'integer', 'description': 'Target coverage percentage'}
                },
                'required_parameters': ['business_rules']
            },
            {
                'tool_name': 'assess_complexity',
                'description': 'Assess code complexity metrics',
                'parameters': {
                    'code': {'type': 'string', 'description': 'Source code'}
                },
                'required_parameters': ['code']
            },
            {
                'tool_name': 'map_dependencies',
                'description': 'Map code dependencies',
                'parameters': {
                    'code': {'type': 'string', 'description': 'Source code'}
                },
                'required_parameters': ['code']
            }
        ]
        
        for tool_config in builtin_tools:
            tool = Tool(
                tool_name=tool_config['tool_name'],
                description=tool_config['description'],
                parameters=tool_config['parameters'],
                required_parameters=tool_config['required_parameters'],
                agent_id=None  # Built-in tools have no agent
            )
            self.tools[tool.tool_name] = tool
            
        self.logger.info(f"Registered {len(builtin_tools)} built-in tools")
    
    async def _monitor_agents(self):
        """Monitor agent health"""
        while True:
            try:
                current_time = datetime.utcnow()
                
                for agent in self.agents.values():
                    # Check heartbeat timeout
                    time_since_heartbeat = (current_time - agent.last_heartbeat).total_seconds()
                    
                    if time_since_heartbeat > 60 and agent.status == "active":
                        agent.status = "unresponsive"
                        self.logger.warning(f"Agent {agent.agent_id} is unresponsive")
                    elif time_since_heartbeat > 300:
                        agent.status = "dead"
                        self.logger.error(f"Agent {agent.agent_id} is dead")
                
                await asyncio.sleep(30)  # Check every 30 seconds
                
            except asyncio.CancelledError:
                break
            except Exception as e:
                self.logger.error(f"Error in agent monitoring: {e}")
    
    async def _cleanup_executions(self):
        """Clean up old execution records"""
        while True:
            try:
                current_time = datetime.utcnow()
                
                # Remove completed executions older than 1 hour
                to_remove = []
                for task_id, execution in self.executions.items():
                    if execution.status in ["completed", "failed"]:
                        if execution.completed_at:
                            age = (current_time - execution.completed_at).total_seconds()
                            if age > 3600:  # 1 hour
                                to_remove.append(task_id)
                
                for task_id in to_remove:
                    del self.executions[task_id]
                
                if to_remove:
                    self.logger.info(f"Cleaned up {len(to_remove)} old executions")
                
                await asyncio.sleep(300)  # Clean every 5 minutes
                
            except asyncio.CancelledError:
                break
            except Exception as e:
                self.logger.error(f"Error in execution cleanup: {e}")
    
    def run(self, host: str = "0.0.0.0", port: int = 8080):
        """Run the MCP server"""
        uvicorn.run(
            self.app,
            host=host,
            port=port,
            lifespan="on",
            log_level="info"
        )


# Create and run server
if __name__ == "__main__":
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    )
    
    server = MainframeMCPServer({
        'cors_origins': ["*"],
        'max_agents': 100,
        'max_executions': 1000
    })
    
    server.run() 