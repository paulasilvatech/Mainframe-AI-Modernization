# agent_framework.py
# Azure AI Foundry Mainframe Modernization - Agent Framework
# This implementation provides a foundation for building agent-based mainframe modernization systems

import os
import json
import logging
import uuid
import datetime
from typing import Dict, List, Optional, Any
from azure.identity import DefaultAzureCredential
from azure.ai.openai import AzureOpenAI
from azure.cosmos import CosmosClient

# Configure logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(name)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

class Agent:
    """Base class for specialized agents in the modernization framework"""
    
    def __init__(self, name: str, role: str, deployment_name: str):
        """
        Initialize an agent with name, role and associated Azure OpenAI deployment
        
        Args:
            name: Unique identifier for the agent
            role: The agent's specialized role (e.g., "COBOL Expert", "Domain Expert")
            deployment_name: The Azure OpenAI deployment to use for this agent
        """
        self.name = name
        self.role = role
        self.deployment_name = deployment_name
        self.agent_id = str(uuid.uuid4())
        self.system_prompt = self._get_default_system_prompt()
        
        # Initialize Azure OpenAI client
        try:
            credential = DefaultAzureCredential()
            self.client = AzureOpenAI(
                azure_endpoint=os.environ["AZURE_OPENAI_ENDPOINT"],
                credential=credential,
                api_version="2023-12-01-preview"
            )
            logger.info(f"Agent {self.name} initialized with role {self.role}")
        except Exception as e:
            logger.error(f"Failed to initialize OpenAI client: {str(e)}")
            raise
    
    def _get_default_system_prompt(self) -> str:
        """Return the default system prompt for this agent role"""
        return f"""
        You are an AI assistant specialized as a {self.role} in the context of mainframe modernization.
        Your task is to provide expertise in your domain and collaborate with other specialists.
        Always format your responses in a clear, structured manner.
        """
    
    def set_system_prompt(self, prompt: str) -> None:
        """Set a custom system prompt for this agent"""
        self.system_prompt = prompt
        logger.info(f"Custom system prompt set for agent {self.name}")
    
    async def process(self, context: Dict[str, Any], user_input: str) -> Dict[str, Any]:
        """
        Process the input in the current context and return results
        
        Args:
            context: The current conversation context
            user_input: The specific input for this agent to process
            
        Returns:
            Updated context with agent's findings
        """
        try:
            # Call Azure OpenAI with the agent's system prompt
            response = self.client.chat.completions.create(
                model=self.deployment_name,
                messages=[
                    {"role": "system", "content": self.system_prompt},
                    {"role": "user", "content": user_input}
                ],
                temperature=0.1,
                max_tokens=4000
            )
            
            # Extract the agent's findings
            findings = response.choices[0].message.content
            
            # Update the context with this agent's findings
            if "agent_findings" not in context:
                context["agent_findings"] = {}
            
            context["agent_findings"][self.name] = findings
            logger.info(f"Agent {self.name} successfully processed input")
            
            return context
        except Exception as e:
            logger.error(f"Error during processing for agent {self.name}: {str(e)}")
            context["agent_findings"][self.name] = {"error": str(e)}
            return context


class COBOLExpertAgent(Agent):
    """Specialized agent for analyzing COBOL code"""
    
    def __init__(self, name: str = "COBOL Expert", deployment_name: str = "cobol-expert"):
        super().__init__(name, "COBOL Expert", deployment_name)
    
    def _get_default_system_prompt(self) -> str:
        return """
        You are an expert COBOL developer with 30+ years of experience in mainframe systems.
        Your task is to analyze COBOL code, understanding its structure, business logic, and dependencies.
        
        Provide a detailed analysis including:
        1. Program structure and organization
        2. Key business functions and their implementation
        3. Data structures and definitions
        4. External dependencies and interactions
        5. Performance considerations
        6. Potential challenges for modernization
        
        Format your response as a structured JSON object with the following schema:
        {
            "programName": "string",
            "programType": "batch|online|subroutine",
            "divisions": [{"name": "string", "purpose": "string"}],
            "businessFunctions": [{"name": "string", "description": "string", "location": "string"}],
            "dataStructures": [{"name": "string", "type": "string", "usage": "string"}],
            "externalSystems": [{"type": "string", "purpose": "string"}],
            "errorHandling": [{"scenario": "string", "mechanism": "string"}],
            "performanceCritical": [{"section": "string", "reason": "string"}],
            "modernizationChallenges": [{"challenge": "string", "severity": "high|medium|low"}]
        }
        """


class DomainExpertAgent(Agent):
    """Specialized agent for extracting business rules and domain knowledge"""
    
    def __init__(self, name: str = "Domain Expert", deployment_name: str = "domain-expert"):
        super().__init__(name, "Domain Expert", deployment_name)
    
    def _get_default_system_prompt(self) -> str:
        return """
        You are a Domain Expert with deep understanding of business processes implemented in mainframe systems.
        Your task is to analyze code and documentation to identify business rules, requirements, and domain-specific knowledge.
        
        Focus on:
        1. Business rules embedded in the code
        2. Domain terminology and concepts
        3. Regulatory and compliance requirements
        4. Business process flows
        5. Critical business logic that must be preserved
        
        Format your response as a structured JSON object with the following schema:
        {
            "domainName": "string",
            "businessRules": [{"name": "string", "description": "string", "criticality": "high|medium|low"}],
            "terminologyDictionary": [{"term": "string", "definition": "string"}],
            "businessProcesses": [{"name": "string", "steps": [{"step": "string", "description": "string"}]}],
            "regulatoryRequirements": [{"requirement": "string", "regulation": "string"}],
            "recommendations": [{"area": "string", "recommendation": "string"}]
        }
        """


class JavaExpertAgent(Agent):
    """Specialized agent for converting COBOL to Java"""
    
    def __init__(self, name: str = "Java Expert", deployment_name: str = "java-expert"):
        super().__init__(name, "Java Expert", deployment_name)
    
    def _get_default_system_prompt(self) -> str:
        return """
        You are a Java Expert specializing in converting COBOL applications to modern Java.
        Your task is to translate COBOL constructs into idiomatic, maintainable Java code.
        
        Follow these guidelines:
        1. Use modern Java features and best practices
        2. Implement appropriate design patterns for the target architecture
        3. Create clean, maintainable code with proper error handling
        4. Preserve all business logic and functionality
        5. Optimize for performance and readability
        
        For each converted component, provide:
        1. The Java implementation
        2. Explanation of design decisions
        3. Mapping between original COBOL and new Java structures
        4. Notes on any special handling or edge cases
        """


class TestExpertAgent(Agent):
    """Specialized agent for generating test cases and validation strategies"""
    
    def __init__(self, name: str = "Test Expert", deployment_name: str = "test-expert"):
        super().__init__(name, "Test Expert", deployment_name)
    
    def _get_default_system_prompt(self) -> str:
        return """
        You are a Test Engineering Expert specializing in validating mainframe modernization projects.
        Your task is to design comprehensive test strategies and test cases to ensure functional equivalence.
        
        Provide the following:
        1. Overall test strategy
        2. Unit test cases with inputs and expected outputs
        3. Integration test scenarios
        4. Performance test strategies
        5. Validation approaches for data integrity
        
        Format test cases as executable code where possible, and provide clear documentation
        for test setup, execution, and validation criteria.
        """


class WorkflowManagerAgent(Agent):
    """Specialized agent for orchestrating the modernization process"""
    
    def __init__(self, name: str = "Workflow Manager", deployment_name: str = "workflow-manager"):
        super().__init__(name, "Workflow Manager", deployment_name)
    
    def _get_default_system_prompt(self) -> str:
        return """
        You are the Workflow Manager responsible for orchestrating the modernization of mainframe applications.
        Your responsibilities include:
        1. Coordinating the activities of specialized expert agents
        2. Ensuring all agents have the necessary context for their tasks
        3. Identifying knowledge gaps that require human intervention
        4. Synthesizing outputs from multiple agents into cohesive deliverables
        5. Managing the overall modernization process from analysis to deployment
        
        You must maintain a high-level view of the process while ensuring each agent receives specific,
        relevant information. Always ensure that the modernization process follows established patterns
        and meets quality standards.
        """
    
    async def orchestrate(self, context: Dict[str, Any], specialists: List[Agent]) -> Dict[str, Any]:
        """
        Orchestrate the workflow among specialist agents
        
        Args:
            context: The current modernization context
            specialists: List of specialist agents to involve
            
        Returns:
            Updated context with all agent findings and synthesized results
        """
        try:
            # 1. Initial assessment
            input_for_manager = f"""
            Initial assessment request:
            Program ID: {context.get('programId', 'Unknown')}
            Program size: {len(context.get('programContent', ''))} characters
            
            Please provide an initial assessment and workflow plan for modernizing this program.
            """
            
            context = await self.process(context, input_for_manager)
            workflow_plan = context["agent_findings"].get(self.name, "")
            
            # 2. Run specialist agents in sequence
            for agent in specialists:
                # Prepare context for this specialist
                agent_prompt = f"""
                Program ID: {context.get('programId', 'Unknown')}
                
                Program content:
                {context.get('programContent', '')}
                
                Workflow Manager guidance:
                {workflow_plan}
                
                Based on your expertise as a {agent.role}, please analyze this program.
                """
                
                # Process with the specialist agent
                context = await agent.process(context, agent_prompt)
            
            # 3. Synthesize results
            synthesis_prompt = "Synthesize the findings from all specialist agents into a cohesive modernization plan."
            for agent_name, findings in context.get("agent_findings", {}).items():
                if agent_name != self.name:
                    synthesis_prompt += f"\n\nFindings from {agent_name}:\n{findings}"
            
            # Final synthesis by workflow manager
            context = await self.process(context, synthesis_prompt)
            
            # Mark as completed
            context["orchestration_complete"] = True
            
            return context
        except Exception as e:
            logger.error(f"Error during orchestration: {str(e)}")
            context["orchestration_error"] = str(e)
            return context


class AgentFramework:
    """Framework for managing agents and their interactions"""
    
    def __init__(self, cosmos_url: Optional[str] = None, cosmos_key: Optional[str] = None):
        """
        Initialize the agent framework
        
        Args:
            cosmos_url: Azure Cosmos DB account URL for knowledge repository
            cosmos_key: Azure Cosmos DB account key
        """
        self.agents = {}
        self.workflow_manager = None
        
        # Initialize knowledge repository if Cosmos DB credentials provided
        self.knowledge_repository = None
        if cosmos_url and cosmos_key:
            try:
                cosmos_client = CosmosClient(cosmos_url, credential=cosmos_key)
                database = cosmos_client.get_database_client("AgentKnowledge")
                self.knowledge_repository = database.get_container_client("Findings")
                logger.info("Knowledge repository initialized with Cosmos DB")
            except Exception as e:
                logger.error(f"Failed to initialize knowledge repository: {str(e)}")
    
    def register_agent(self, agent: Agent) -> None:
        """Register an agent with the framework"""
        self.agents[agent.name] = agent
        logger.info(f"Agent {agent.name} registered with framework")
    
    def set_workflow_manager(self, manager: WorkflowManagerAgent) -> None:
        """Set the workflow manager agent"""
        self.workflow_manager = manager
        logger.info(f"Workflow manager set to {manager.name}")
    
    async def process_program(self, program_id: str, program_content: str) -> Dict[str, Any]:
        """
        Process a mainframe program through the agent framework
        
        Args:
            program_id: Identifier for the program
            program_content: The source code content
            
        Returns:
            Processing results from all agents
        """
        if not self.workflow_manager:
            raise ValueError("Workflow manager must be set before processing programs")
        
        # Initialize context
        context = {
            "programId": program_id,
            "programContent": program_content,
            "timestamp": datetime.datetime.utcnow().isoformat(),
            "agent_findings": {}
        }
        
        # Get specialist agents
        specialists = list(self.agents.values())
        
        # Orchestrate the workflow
        result = await self.workflow_manager.orchestrate(context, specialists)
        
        # Store in knowledge repository if available
        if self.knowledge_repository:
            try:
                self.knowledge_repository.create_item(body=result)
                logger.info(f"Stored processing results for program {program_id} in knowledge repository")
            except Exception as e:
                logger.error(f"Failed to store results in knowledge repository: {str(e)}")
        
        return result


# Example usage
async def main():
    # Initialize the framework
    framework = AgentFramework()
    
    # Create and register agents
    workflow_manager = WorkflowManagerAgent()
    cobol_expert = COBOLExpertAgent()
    domain_expert = DomainExpertAgent()
    java_expert = JavaExpertAgent()
    test_expert = TestExpertAgent()
    
    framework.register_agent(cobol_expert)
    framework.register_agent(domain_expert)
    framework.register_agent(java_expert)
    framework.register_agent(test_expert)
    framework.set_workflow_manager(workflow_manager)
    
    # Process a program
    program_id = "CUSTMNT"
    with open("CUSTMNT.cbl", "r") as f:
        program_content = f.read()
    
    result = await framework.process_program(program_id, program_content)
    
    # Save the result
    with open(f"{program_id}_modernization_result.json", "w") as f:
        json.dump(result, f, indent=2)


if __name__ == "__main__":
    import asyncio
    asyncio.run(main()) 