# 🤖 AI-Powered Code Analysis

This chapter provides a detailed technical guide for conducting AI-powered analysis of IBM z/OS mainframe code as part of your modernization journey.

## 📋 Overview

Code analysis is a critical step in mainframe modernization, enabling teams to understand complex legacy applications, identify potential modernization issues, and make informed transformation decisions. Azure AI Foundry provides advanced AI capabilities specifically designed for mainframe code analysis, helping you uncover insights that would be difficult or time-consuming to discover manually.

## 🎯 Objectives

| Objective | Description |
|-----------|-------------|
| Code Analysis | Implement automated analysis of mainframe code (COBOL, JCL, Assembler, and more) |
| Business Rules | Extract business rules and logic from legacy applications |
| Quality Assessment | Identify code complexity, quality issues, and modernization challenges |
| Documentation | Generate comprehensive documentation from legacy code |
| Data Mapping | Map data structures and relationships |

## 💡 Analysis Capabilities

Azure AI Foundry provides the following code analysis capabilities:

### 1. Syntax and Structure Analysis

| Capability | Description |
|------------|-------------|
| Parsing | Parsing and tokenization of mainframe languages |
| AST Generation | Abstract Syntax Tree (AST) generation |
| Control Flow | Control flow analysis |
| Data Flow | Data flow tracking |
| Dead Code | Dead code identification |

### 2. Business Logic Extraction

| Capability | Description |
|------------|-------------|
| Business Rules | Business rule identification and extraction |
| Pattern Recognition | Domain-specific language pattern recognition |
| Decision Logic | Decision logic mapping |
| Algorithms | Calculation and algorithm identification |
| Process Modeling | Business process modeling |

### 3. Data Structure Analysis

| Capability | Description |
|------------|-------------|
| Data Definitions | Data definition extraction |
| Schema Mapping | File and database schema mapping |
| Layout Analysis | Record layout analysis |
| Data Lineage | Data lineage tracking |
| Dependencies | Data dependency identification |

### 4. Dependency Mapping

| Capability | Description |
|------------|-------------|
| Program Dependencies | Program-to-program dependencies |
| Data Dependencies | Program-to-data dependencies |
| JCL Dependencies | JCL job dependencies |
| External Interfaces | External system interface identification |
| Visualization | Component relationship visualization |

### 5. Quality Assessment

| Capability | Description |
|------------|-------------|
| Complexity | Complexity measurement |
| Maintainability | Maintainability analysis |
| Code Duplication | Duplicated code detection |
| Error Handling | Error handling examination |
| Performance | Performance hotspot identification |

## 🛠️ Setup and Configuration

### Prerequisites

Before using the AI-powered code analysis tools, ensure you have:

| Prerequisite | Description |
|--------------|-------------|
| Source Code | Access to mainframe source code (COBOL, JCL, Copybooks, etc.) |
| AI Foundry | Azure AI Foundry environment configured |
| Development Environment | Development environment setup (see [💻 Development Environment Configuration](../04-development-environment/README.md)) |

### Configuration Steps

1. **Prepare Source Code Repository**:

   ```bash
   # Create a directory for your mainframe code
   mkdir -p mainframe-source/{cobol,jcl,copybooks,proc}
   
   # Copy or clone your source code
   cp -R /path/to/source/* mainframe-source/
   ```

2. **Configure Analysis Settings**:

   Create a configuration file for your analysis:

   ```json
   {
     "analysis": {
       "languages": ["cobol", "jcl", "assembler", "pl1"],
       "includePaths": ["copybooks/", "proc/"],
       "excludePatterns": ["test/", "*.temp.*"],
       "features": {
         "businessRules": true,
         "dataAnalysis": true,
         "qualityMetrics": true,
         "dependencyMapping": true,
         "documentation": true
       }
     },
     "output": {
       "format": ["html", "json", "markdown"],
       "reportPath": "./analysis-results"
     }
   }
   ```

3. **Initialize AI Foundry Analysis**:

   ```bash
   az ai-foundry code-analysis init --config code-analysis-config.json
   ```

## 🔍 Running Code Analysis

### Basic Analysis

To run a basic analysis of your mainframe code:

```bash
az ai-foundry code-analysis run --source-dir ./mainframe-source --output-dir ./analysis-results
```

### Advanced Analysis Options

For more targeted analysis:

```bash
az ai-foundry code-analysis run \
  --source-dir ./mainframe-source \
  --output-dir ./analysis-results \
  --language cobol \
  --focus business-rules \
  --generate-documentation \
  --include-metrics \
  --dependency-depth 3
```

### Integration with CI/CD

Integrate code analysis into GitHub Actions:

```yaml
- name: Run AI-Powered Code Analysis
  uses: azure/ai-foundry-code-analysis@v1
  with:
    source-directory: ${{ github.workspace }}/src
    languages: cobol,jcl
    output-directory: ${{ github.workspace }}/analysis
    upload-results: true
```

Integrate code analysis into Azure DevOps:

```yaml
- task: AIFoundryCodeAnalysis@1
  displayName: 'Analyze Mainframe Code'
  inputs:
    sourceDirectory: '$(Build.SourcesDirectory)/src'
    languages: 'cobol,jcl'
    outputDirectory: '$(Build.ArtifactStagingDirectory)/analysis'
    uploadResults: true
```

## 📊 Analyzing Results

### Key Analysis Reports

The code analysis generates several reports:

| Report | Description |
|--------|-------------|
| Executive Summary | High-level overview of the codebase |
| Business Rules Catalog | Extracted business rules and logic |
| Data Dictionary | Compilation of data structures and relationships |
| Quality Metrics Dashboard | Code quality and complexity metrics |
| Dependency Network | Visualization of component relationships |
| Modernization Recommendations | Suggested approaches for transformation |

### Business Rules Extraction

The business rules extraction capability identifies and documents business logic:

```json
{
  "rule_id": "BR-ACCT-001",
  "name": "Account Interest Calculation",
  "description": "Calculates monthly interest for savings accounts",
  "type": "Calculation",
  "complexity": "Medium",
  "source_locations": [
    {
      "program": "ACCTPROC.cbl",
      "start_line": 245,
      "end_line": 278
    }
  ],
  "variables": [
    {"name": "BALANCE", "type": "PIC 9(9)V99"},
    {"name": "INTEREST-RATE", "type": "PIC 9V999"},
    {"name": "ACCRUED-INTEREST", "type": "PIC 9(7)V99"}
  ],
  "logic": "IF ACCOUNT-TYPE = 'SAVINGS' AND BALANCE > 0\n  COMPUTE ACCRUED-INTEREST = (BALANCE * INTEREST-RATE) / 12\nEND-IF",
  "notes": "Interest calculated monthly on positive balances for savings accounts only"
}
```

### Quality Metrics

Code quality metrics help identify areas for improvement:

```
Program: CUSTMGMT.cbl
- Complexity: 87 (High)
- Maintainability Index: 42 (Low)
- Lines of Code: 2,453
- Comment Density: 8%
- Dead Code: 15%
- Duplicated Code: 22%
- Error Handling Coverage: 63%
```

### Dependency Visualization

Dependency visualization helps understand component relationships:

```
CUSTMGMT.cbl
├── Calls: ACCTPROC.cbl, VALADDR.cbl, PRNTDOC.cbl
├── Uses Data: CUSTOMER.cpy, ACCOUNT.cpy
├── Reads: CUSTMAST.vsam, ACCTMAST.vsam
└── Writes: CUSTREPT.output

Executed By: CUSTJOB.jcl (STEP010)
```

## 💡 Practical Examples

### Example 1: Analyzing a Customer Management System

This example demonstrates analyzing a customer management COBOL program:

1. **Run Targeted Analysis**:

   ```bash
   az ai-foundry code-analysis run \
     --source ./mainframe-source/cobol/CUSTMGMT.cbl \
     --include-copybooks ./mainframe-source/copybooks \
     --focus business-rules,data-structures \
     --output ./analysis-results/custmgmt
   ```

2. **Review Extracted Business Rules**:

   ```
   Business Rule: BR-CUST-001 (Customer Eligibility Verification)
   Location: CUSTMGMT.cbl (Lines 320-345)
   Summary: Determines customer eligibility for premium services based on
            account age, balance, and transaction history.
   Logic:
     IF ACCOUNT-AGE > 2 AND
        AVERAGE-BALANCE > 25000 AND
        TRANSACTION-COUNT > 5
        SET PREMIUM-ELIGIBLE TO TRUE
     ELSE
        SET PREMIUM-ELIGIBLE TO FALSE
   ```

3. **Review Data Structures**:

   ```
   Data Structure: CUSTOMER-RECORD
   Defined in: CUSTOMER.cpy
   Used in: CUSTMGMT.cbl, ACCTPROC.cbl, CUSTINQ.cbl
   Fields:
     - CUST-ID          PIC X(10)
     - CUST-NAME        PIC X(30)
     - CUST-ADDRESS     PIC X(50)
     - CUST-PHONE       PIC X(15)
     - CUST-TYPE        PIC X(1)
     - CUST-STATUS      PIC X(1)
     - CUST-BALANCE     PIC 9(9)V99
     - CUST-OPEN-DATE   PIC 9(8)
   ```

### Example 2: Analyzing a Financial Transaction Processing System

This example demonstrates analyzing a more complex transaction processing system:

1. **Run Comprehensive Analysis**:

   ```bash
   az ai-foundry code-analysis run \
     --source ./mainframe-source/cobol/TRANSACT.cbl \
     --include-dependencies \
     --dependency-depth 2 \
     --focus all \
     --output ./analysis-results/transaction-system
   ```

2. **Review Dependency Map**:

   The analysis generates a visual dependency map showing relationships between components:

   ```
   TRANSACT.cbl (Transaction Processing)
   ├── Calls: ACCTVAL.cbl (Account Validation)
   │   ├── Calls: CUSTVAL.cbl (Customer Validation)
   │   └── Uses: ACCOUNT.cpy, VALIDATION.cpy
   ├── Calls: TRANLOG.cbl (Transaction Logging)
   │   ├── Writes: TRANLOG.vsam
   │   └── Uses: LOGDATA.cpy
   ├── Calls: BALUPDT.cbl (Balance Update)
   │   ├── Reads/Writes: ACCTMAST.vsam
   │   └── Uses: ACCOUNT.cpy
   └── Uses: TRANTYPE.cpy, ACCOUNT.cpy, ERRORS.cpy
   
   Executed By: TRANJOB.jcl (STEP020)
   ```

3. **Review Quality Assessment**:

   ```
   Component: TRANSACT.cbl
   
   Critical Issues:
   - High Cyclomatic Complexity (142) in PROCESS-TRANSACTION paragraph
   - Duplicated code in error handling routines
   - Inadequate input validation for transaction amounts
   
   Modernization Recommendations:
   - Refactor PROCESS-TRANSACTION into smaller, focused routines
   - Implement centralized error handling
   - Enhance input validation for all transaction types
   - Consider extracting fee calculation logic as separate component
   ```

## 🔗 Integration with Dependency Mapping

Combine code analysis with dependency mapping for comprehensive insights:

```bash
# First run code analysis
az ai-foundry code-analysis run --source-dir ./mainframe-source --output-dir ./analysis-results

# Then run dependency mapping on the same codebase
python ../dependency-mapping/map_dependencies.py --source-dir ./mainframe-source --output ./analysis-results/dependency-map.json

# Generate integrated report
az ai-foundry generate-report --code-analysis ./analysis-results --dependency-map ./analysis-results/dependency-map.json --output ./final-report
```

The integrated analysis provides a comprehensive view of the application, combining business logic insights with structural dependencies.

## ❓ Troubleshooting

| Issue | Resolution |
|-------|------------|
| Parsing errors in COBOL code | Verify that the code follows standard COBOL syntax or specify compiler dialect with `--dialect` option |
| Missing copybook references | Ensure all copybooks are available and specify include directories with `--include-copybooks` |
| Incomplete business rule extraction | Adjust sensitivity with `--rule-sensitivity` parameter or annotate complex business rules in code comments |
| High resource usage during analysis | Analyze code in smaller batches or adjust `--memory-optimization` setting |
| Dependency mapping timeout | Increase timeout with `--timeout` option or reduce scope with `--dependency-depth` |

## ✅ Best Practices

| Practice | Description |
|----------|-------------|
| Focused Analysis | Start by analyzing core business-critical programs |
| Rule Validation | Have domain experts review extracted business rules |
| Metric-Based Prioritization | Use quality metrics to prioritize modernization efforts |
| Documentation | Maintain documentation of insights gained from analysis |
| Regular Analysis | Run analysis regularly to track modernization progress |
| Test Integration | Use extracted business rules to generate test cases |

## 🤖 Agent-Based Code Analysis

### Integration with MCP

The AI-powered code analysis can be enhanced using MCP-enabled agents for more sophisticated analysis:

```python
# agents/code_analysis_orchestrator.py
class AICodeAnalysisOrchestrator:
    def __init__(self, mcp_client, ai_foundry_client):
        self.mcp_client = mcp_client
        self.ai_foundry_client = ai_foundry_client
        self.agents = {
            'structure': StructureAnalysisAgent(),
            'complexity': ComplexityAnalysisAgent(),
            'security': SecurityAnalysisAgent(),
            'patterns': PatternDetectionAgent(),
            'business_logic': BusinessLogicExtractionAgent()
        }
    
    async def analyze_codebase(self, repository_path):
        """Orchestrate parallel analysis using multiple specialized agents"""
        # Discovery phase
        files = await self.agents['structure'].discover_files(repository_path)
        
        # Parallel analysis phase
        analysis_tasks = []
        for file in files:
            if file['language'] in ['COBOL', 'Natural', 'PL/I']:
                # Create analysis tasks for each agent
                analysis_tasks.extend([
                    self.agents['structure'].analyze_structure(file),
                    self.agents['complexity'].calculate_metrics(file),
                    self.agents['security'].scan_vulnerabilities(file),
                    self.agents['patterns'].detect_patterns(file),
                    self.agents['business_logic'].extract_rules(file)
                ])
        
        # Execute all analyses in parallel
        results = await asyncio.gather(*analysis_tasks)
        
        # Aggregate and correlate results
        return await self._aggregate_results(results)
    
    async def _aggregate_results(self, results):
        """Aggregate results from multiple agents"""
        aggregated = {
            'summary': {},
            'detailed_findings': {},
            'recommendations': [],
            'risk_assessment': {}
        }
        
        # Use AI to synthesize findings
        synthesis_prompt = self._build_synthesis_prompt(results)
        ai_synthesis = await self.ai_foundry_client.analyze(synthesis_prompt)
        
        aggregated['summary'] = ai_synthesis
        return aggregated
```

### Multi-Agent Analysis Example

Here's how to implement multi-agent analysis for comprehensive code understanding:

```python
# agents/specialized_agents.py
class COBOLAnalyzerAgent:
    """Specialized agent for COBOL code analysis"""
    
    def __init__(self, mcp_context):
        self.mcp_context = mcp_context
        self.analyzer = COBOLParser()
        
    async def analyze(self, code_file):
        """Deep COBOL-specific analysis"""
        # Parse COBOL structure
        ast = self.analyzer.parse(code_file)
        
        # Extract COBOL-specific patterns
        patterns = {
            'divisions': self._extract_divisions(ast),
            'sections': self._extract_sections(ast),
            'paragraphs': self._extract_paragraphs(ast),
            'data_structures': self._extract_data_structures(ast),
            'file_operations': self._extract_file_operations(ast),
            'database_calls': self._extract_database_calls(ast),
            'business_logic': self._extract_business_logic(ast)
        }
        
        # Use MCP context for enhanced analysis
        enhanced_analysis = await self.mcp_context.enhance_analysis(patterns)
        
        return {
            'language': 'COBOL',
            'file': code_file,
            'patterns': patterns,
            'enhanced_analysis': enhanced_analysis,
            'complexity_score': self._calculate_complexity(ast),
            'modernization_candidates': self._identify_modernization_opportunities(patterns)
        }

class NaturalAnalyzerAgent:
    """Specialized agent for Natural/Adabas analysis"""
    
    async def analyze(self, code_file):
        """Natural-specific analysis with Adabas considerations"""
        # Parse Natural code
        parsed = self.parser.parse(code_file)
        
        # Extract Natural-specific elements
        analysis = {
            'maps': self._extract_maps(parsed),
            'ddms': self._extract_ddms(parsed),
            'subprograms': self._extract_subprograms(parsed),
            'database_access': self._analyze_adabas_access(parsed),
            'mu_pe_fields': self._analyze_mu_pe_fields(parsed)
        }
        
        # Identify migration complexities
        analysis['migration_complexity'] = self._assess_migration_complexity(analysis)
        
        return analysis
```

### Production Agent Examples

#### DiscoveryAgent with MCP

```python
class DiscoveryAgent:
    """Enhanced discovery agent with MCP integration"""
    
    def __init__(self, mcp_client):
        self.mcp_client = mcp_client
        self.discovery_tools = ['file_scanner', 'dependency_mapper', 'catalog_reader']
        
    async def discover_mainframe_assets(self, source_path):
        """Discover all mainframe assets with intelligent categorization"""
        # Register with MCP server
        await self.mcp_client.register_agent({
            'name': 'discovery_agent',
            'capabilities': ['file_discovery', 'dependency_mapping', 'catalog_analysis']
        })
        
        # Execute discovery tools via MCP
        discoveries = await self.mcp_client.execute_tool(
            'comprehensive_discovery',
            {'path': source_path, 'depth': 'full'}
        )
        
        # Categorize and prioritize findings
        categorized = await self._categorize_assets(discoveries)
        
        return {
            'total_assets': len(discoveries),
            'by_type': categorized,
            'critical_programs': await self._identify_critical_programs(discoveries),
            'recommended_order': await self._suggest_analysis_order(categorized)
        }
```

#### TransformationAgent with AI Foundry

```python
class TransformationAgent:
    """Intelligent transformation agent with AI Foundry integration"""
    
    def __init__(self, ai_foundry_client, mcp_client):
        self.ai_foundry = ai_foundry_client
        self.mcp = mcp_client
        
    async def transform_code(self, source_code, source_lang, target_lang):
        """Transform code with AI assistance and validation"""
        # Pre-transformation analysis
        analysis = await self.mcp.execute_tool(
            'pre_transform_analysis',
            {'code': source_code, 'language': source_lang}
        )
        
        # AI-powered transformation
        transformation_prompt = self._build_transformation_prompt(
            source_code, source_lang, target_lang, analysis
        )
        
        transformed_code = await self.ai_foundry.transform(transformation_prompt)
        
        # Post-transformation validation
        validation = await self._validate_transformation(
            source_code, transformed_code, analysis['business_rules']
        )
        
        return {
            'success': validation['passed'],
            'transformed_code': transformed_code,
            'validation_results': validation,
            'preserved_logic': validation['business_logic_preserved'],
            'optimization_suggestions': await self._suggest_optimizations(transformed_code)
        }
```

#### TestGeneratorAgent with Intelligent Test Selection

```python
class TestGeneratorAgent:
    """AI-powered test generation agent"""
    
    async def generate_tests(self, code_analysis, transformation_result):
        """Generate comprehensive test suites based on analysis"""
        # Identify test scenarios from business rules
        test_scenarios = await self._extract_test_scenarios(
            code_analysis['business_rules']
        )
        
        # Generate test cases for each scenario
        test_suites = {}
        for scenario in test_scenarios:
            test_suite = await self._generate_test_suite(
                scenario,
                transformation_result['transformed_code']
            )
            test_suites[scenario['name']] = test_suite
        
        # Generate edge cases and negative tests
        edge_cases = await self._generate_edge_cases(code_analysis)
        
        # Create test documentation
        documentation = await self._document_tests(test_suites, edge_cases)
        
        return {
            'test_suites': test_suites,
            'edge_cases': edge_cases,
            'coverage_estimate': self._estimate_coverage(test_suites, code_analysis),
            'documentation': documentation,
            'execution_order': self._optimize_execution_order(test_suites)
        }
```

#### DeploymentAgent with Self-Healing

```python
class DeploymentAgent:
    """Self-healing deployment agent"""
    
    async def deploy_with_intelligence(self, artifact, environment, strategy='canary'):
        """Deploy with self-healing capabilities"""
        # Assess deployment risk
        risk_assessment = await self._assess_deployment_risk(artifact, environment)
        
        # Select optimal deployment strategy
        deployment_strategy = await self._select_strategy(
            risk_assessment, strategy, environment
        )
        
        # Execute deployment with monitoring
        deployment_id = await self._initiate_deployment(
            artifact, environment, deployment_strategy
        )
        
        # Monitor and heal if necessary
        monitoring_task = asyncio.create_task(
            self._monitor_and_heal(deployment_id)
        )
        
        # Wait for deployment completion
        result = await self._wait_for_completion(deployment_id, monitoring_task)
        
        return {
            'deployment_id': deployment_id,
            'status': result['status'],
            'healing_actions': result.get('healing_actions', []),
            'metrics': result['metrics'],
            'recommendations': await self._generate_recommendations(result)
        }
```

### Agent Orchestration Patterns

#### Workflow Definitions

```yaml
# workflows/code_analysis_workflow.yaml
name: Comprehensive Code Analysis Workflow
version: 1.0

agents:
  - name: discovery_agent
    type: DiscoveryAgent
    config:
      scan_depth: full
      include_patterns: ["*.cbl", "*.nat", "*.pli", "*.jcl"]
      
  - name: structure_agent
    type: StructureAnalysisAgent
    config:
      analysis_level: detailed
      
  - name: complexity_agent
    type: ComplexityAnalysisAgent
    config:
      metrics: ["cyclomatic", "halstead", "maintainability"]
      
  - name: security_agent
    type: SecurityAnalysisAgent
    config:
      scan_type: comprehensive
      include_owasp: true

workflow:
  - stage: discovery
    agent: discovery_agent
    outputs: [file_list, dependency_map]
    
  - stage: parallel_analysis
    parallel: true
    agents:
      - structure_agent
      - complexity_agent
      - security_agent
    inputs:
      files: discovery.file_list
      
  - stage: synthesis
    agent: synthesis_agent
    inputs:
      structure: parallel_analysis.structure_agent
      complexity: parallel_analysis.complexity_agent
      security: parallel_analysis.security_agent
```

#### Python Orchestrator Implementation

```python
class AgentOrchestrator:
    """Orchestrate multi-agent workflows"""
    
    def __init__(self, workflow_definition, mcp_client):
        self.workflow = self._load_workflow(workflow_definition)
        self.mcp = mcp_client
        self.agents = {}
        self.results = {}
        
    async def execute_workflow(self, context):
        """Execute the defined workflow"""
        # Initialize agents
        await self._initialize_agents()
        
        # Execute workflow stages
        for stage in self.workflow['stages']:
            if stage.get('parallel'):
                await self._execute_parallel_stage(stage, context)
            else:
                await self._execute_sequential_stage(stage, context)
        
        # Generate final report
        return await self._generate_report()
    
    async def _execute_parallel_stage(self, stage, context):
        """Execute multiple agents in parallel"""
        tasks = []
        for agent_name in stage['agents']:
            agent = self.agents[agent_name]
            inputs = self._prepare_inputs(stage, agent_name)
            tasks.append(agent.execute(inputs, context))
        
        results = await asyncio.gather(*tasks)
        
        # Store results
        for i, agent_name in enumerate(stage['agents']):
            self.results[f"{stage['name']}.{agent_name}"] = results[i]
```

#### Error Handling and Recovery

```python
class ResilientOrchestrator(AgentOrchestrator):
    """Orchestrator with error handling and recovery"""
    
    async def _execute_with_recovery(self, agent, task, max_retries=3):
        """Execute agent task with automatic recovery"""
        for attempt in range(max_retries):
            try:
                result = await agent.execute(task)
                return result
            except AgentException as e:
                self.logger.warning(f"Agent error on attempt {attempt + 1}: {e}")
                
                # Try recovery strategy
                recovery_strategy = self._select_recovery_strategy(e)
                if recovery_strategy:
                    await self._apply_recovery(recovery_strategy, agent, task)
                    continue
                    
                if attempt == max_retries - 1:
                    # Final attempt failed
                    return await self._handle_failure(agent, task, e)
    
    def _select_recovery_strategy(self, error):
        """Select appropriate recovery strategy based on error"""
        if isinstance(error, ResourceExhaustedError):
            return {'type': 'scale_resources', 'factor': 2}
        elif isinstance(error, TimeoutError):
            return {'type': 'increase_timeout', 'factor': 1.5}
        elif isinstance(error, DependencyError):
            return {'type': 'retry_with_fallback', 'fallback': 'cached_data'}
        return None
```

### Performance Optimization

```python
class OptimizedAnalysisOrchestrator:
    """Performance-optimized orchestrator for large codebases"""
    
    def __init__(self, mcp_client, config):
        self.mcp = mcp_client
        self.config = config
        self.cache = AnalysisCache()
        self.thread_pool = ThreadPoolExecutor(max_workers=config['max_workers'])
        
    async def analyze_large_codebase(self, codebase_path):
        """Analyze large codebase with optimization"""
        # Chunk the codebase
        chunks = await self._chunk_codebase(codebase_path)
        
        # Process chunks in parallel with caching
        chunk_results = []
        for chunk in chunks:
            # Check cache first
            cached_result = await self.cache.get(chunk['hash'])
            if cached_result:
                chunk_results.append(cached_result)
                continue
            
            # Process chunk
            result = await self._process_chunk(chunk)
            
            # Cache result
            await self.cache.set(chunk['hash'], result)
            chunk_results.append(result)
        
        # Merge results
        return await self._merge_results(chunk_results)
    
    async def _process_chunk(self, chunk):
        """Process a chunk of code with optimized resource usage"""
        # Distribute across available agents
        available_agents = await self.mcp.get_available_agents('analysis')
        
        # Load balance across agents
        agent = self._select_optimal_agent(available_agents, chunk)
        
        # Execute with resource monitoring
        with ResourceMonitor() as monitor:
            result = await agent.analyze(chunk)
            
        # Adjust resources if needed
        if monitor.high_resource_usage():
            await self._adjust_resources(agent)
        
        return result
```

## ➡️ Next Steps

After completing code analysis:

1. Develop a [🗺️ Modernization Strategy](../03-foundation/modernization-strategy.md) based on analysis insights
2. Set up [🐙 GitHub Integration](../06-github-integration/README.md) or [🔄 Azure DevOps Integration](../07-azure-devops-integration/README.md)
3. Begin [🧠 AI-Powered Transformation](../08-ai-transformation/README.md) of selected components
4. Implement [📦 CI/CD Pipelines](../09-cicd-implementation/README.md) for automated build and deployment
5. Explore [🔌 MCP-Enabled Agent Architecture](../15-mcp-enabled-agent-architecture/README.md) for advanced orchestration
6. Implement [🔧 Agentic DevOps](../16-agentic-devops/README.md) for self-healing pipelines

## 📚 References

- [🔗 Dependency Mapping Guide](../02-discovery/dependency-mapping.md)
- [⚠️ Risk Assessment Guide](../10-risk-management/README.md)
- [🔄 Hybrid Operations Management](../11-hybrid-operations/README.md)
- [🤖 Agent-Based Mainframe Modernization](../12-agent-based-modernization/README.md)
- [🔌 MCP-Enabled Agent Architecture](../15-mcp-enabled-agent-architecture/README.md) 