# Appendix A: Workshop - COBOL Modernization Journey with AI Agents

## Workshop Overview

This hands-on workshop guides you through a complete COBOL modernization journey using AI agents, Azure AI Platform, and GitHub integration. You'll work with a realistic banking system to experience the full modernization lifecycle.

### Duration
- **Total Time**: 2 days (16 hours)
- **Day 1**: Foundation and Analysis (8 hours)
- **Day 2**: Transformation and Deployment (8 hours)

### Prerequisites
- Basic understanding of COBOL programming
- Azure subscription with AI services enabled
- GitHub account with Actions enabled
- Docker Desktop installed
- Python 3.8+ installed
- VS Code with COBOL and Python extensions

### Learning Objectives
By the end of this workshop, you will be able to:
- Set up an agent-based modernization environment
- Analyze COBOL applications using AI agents
- Extract and document business rules
- Transform COBOL code to modern languages
- Generate comprehensive test suites
- Deploy modernized applications with CI/CD

### Required Tools and Setup
```bash
# Clone workshop repository
git clone https://github.com/mainframe-modernization/cobol-workshop.git
cd cobol-workshop

# Install dependencies
pip install -r requirements.txt

# Set up environment variables
cp .env.example .env
# Edit .env with your Azure and GitHub credentials
```

---

## Day 1: Foundation and Analysis

### Module 1: Environment Setup (2 hours)

#### 1.1 Docker Environment Setup

Create a Docker compose configuration for the workshop:

```yaml
# docker-compose.yml
version: '3.8'

services:
  mcp-server:
    image: mainframe-modernization/mcp-server:latest
    ports:
      - "8080:8080"
    environment:
      - AZURE_AI_ENDPOINT=${AZURE_AI_ENDPOINT}
      - AZURE_AI_KEY=${AZURE_AI_KEY}
    volumes:
      - ./config:/app/config
      - ./workspace:/workspace

  agent-runtime:
    image: mainframe-modernization/agent-runtime:latest
    depends_on:
      - mcp-server
    environment:
      - MCP_SERVER_URL=http://mcp-server:8080
      - GITHUB_TOKEN=${GITHUB_TOKEN}
    volumes:
      - ./agents:/app/agents
      - ./workspace:/workspace

  postgres:
    image: postgres:14
    environment:
      - POSTGRES_DB=modernization
      - POSTGRES_USER=workshop
      - POSTGRES_PASSWORD=workshop123
    volumes:
      - postgres_data:/var/lib/postgresql/data

  redis:
    image: redis:7
    ports:
      - "6379:6379"

volumes:
  postgres_data:
```

Start the environment:
```bash
docker-compose up -d
```

#### 1.2 Sample COBOL Application Setup

The workshop includes a sample banking system with the following components:

```
banking-system/
├── cobol/
│   ├── ACCTMGMT.cbl    # Account management
│   ├── CUSTPROC.cbl    # Customer processing
│   ├── TRANSACT.cbl    # Transaction processing
│   └── RPRTGEN.cbl     # Report generation
├── copybooks/
│   ├── ACCOUNT.cpy     # Account data structure
│   ├── CUSTOMER.cpy    # Customer data structure
│   └── TRANS.cpy       # Transaction data structure
├── jcl/
│   ├── DAILYBATCH.jcl  # Daily batch processing
│   └── MONTHEND.jcl    # Month-end processing
└── data/
    └── test-data.txt   # Sample test data
```

Example COBOL program (ACCTMGMT.cbl):
```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCTMGMT.
       AUTHOR. WORKSHOP-TEAM.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNT-FILE ASSIGN TO 'ACCTMAST'
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS ACCT-NUMBER.
           
       DATA DIVISION.
       FILE SECTION.
       FD  ACCOUNT-FILE.
       01  ACCOUNT-RECORD.
           COPY ACCOUNT.
           
       WORKING-STORAGE SECTION.
       01  WS-FLAGS.
           05  WS-EOF              PIC X VALUE 'N'.
           05  WS-VALID-ACCT       PIC X VALUE 'N'.
           
       01  WS-CALCULATIONS.
           05  WS-INTEREST-RATE    PIC 9V999 VALUE 0.025.
           05  WS-MONTHLY-RATE     PIC 9V9999.
           05  WS-INTEREST-AMT     PIC 9(7)V99.
           
       PROCEDURE DIVISION.
       MAIN-PROCESS.
           PERFORM OPEN-FILES
           PERFORM PROCESS-ACCOUNTS UNTIL WS-EOF = 'Y'
           PERFORM CLOSE-FILES
           STOP RUN.
           
       PROCESS-ACCOUNTS.
           READ ACCOUNT-FILE NEXT
               AT END MOVE 'Y' TO WS-EOF
               NOT AT END PERFORM CALCULATE-INTEREST
           END-READ.
           
       CALCULATE-INTEREST.
           IF ACCT-TYPE = 'SAV' AND ACCT-BALANCE > 0
               COMPUTE WS-MONTHLY-RATE = WS-INTEREST-RATE / 12
               COMPUTE WS-INTEREST-AMT = 
                   ACCT-BALANCE * WS-MONTHLY-RATE
               ADD WS-INTEREST-AMT TO ACCT-BALANCE
               PERFORM UPDATE-ACCOUNT
           END-IF.
```

#### 1.3 Initialize Agent Framework

Exercise 1: Initialize the agent framework

```python
# exercises/01_initialize_agents.py
import asyncio
from agents.mcp_client import MCPClient
from agents.orchestrator import AgentOrchestrator

async def initialize_workshop_agents():
    """Initialize agents for the workshop"""
    
    # Connect to MCP server
    mcp_client = MCPClient("http://localhost:8080")
    
    # Initialize orchestrator
    orchestrator = AgentOrchestrator(mcp_client)
    
    # Register workshop agents
    agents = [
        {
            'name': 'cobol_discovery',
            'type': 'DiscoveryAgent',
            'capabilities': ['file_scan', 'dependency_map']
        },
        {
            'name': 'cobol_analyzer',
            'type': 'COBOLAnalyzerAgent',
            'capabilities': ['syntax_analysis', 'business_logic_extraction']
        },
        {
            'name': 'modernization_planner',
            'type': 'PlanningAgent',
            'capabilities': ['strategy_selection', 'risk_assessment']
        }
    ]
    
    for agent_config in agents:
        await orchestrator.register_agent(agent_config)
        print(f"✅ Registered {agent_config['name']}")
    
    # Verify agents are ready
    status = await orchestrator.get_system_status()
    print(f"\nSystem Status: {status}")
    
    return orchestrator

# Run the initialization
if __name__ == "__main__":
    orchestrator = asyncio.run(initialize_workshop_agents())
```

### Module 2: AI-Powered Analysis (3 hours)

#### Exercise 1: Deep Code Analysis

```python
# exercises/02_deep_analysis.py
async def analyze_cobol_program(orchestrator, program_path):
    """Perform deep analysis of COBOL program"""
    
    # Read COBOL source
    with open(program_path, 'r') as f:
        cobol_source = f.read()
    
    # Execute discovery phase
    print("🔍 Phase 1: Discovery")
    discovery_result = await orchestrator.execute_agent(
        'cobol_discovery',
        {
            'action': 'scan',
            'path': program_path,
            'include_dependencies': True
        }
    )
    
    print(f"Found {len(discovery_result['dependencies'])} dependencies")
    
    # Execute analysis phase
    print("\n📊 Phase 2: Deep Analysis")
    analysis_result = await orchestrator.execute_agent(
        'cobol_analyzer',
        {
            'action': 'analyze',
            'source': cobol_source,
            'focus': ['structure', 'complexity', 'business_logic']
        }
    )
    
    # Display results
    print("\n📋 Analysis Results:")
    print(f"- Program Structure: {analysis_result['structure']['type']}")
    print(f"- Complexity Score: {analysis_result['complexity']['score']}/100")
    print(f"- Business Rules Found: {len(analysis_result['business_rules'])}")
    
    # Generate analysis report
    report = await orchestrator.generate_report(
        'analysis',
        {
            'discovery': discovery_result,
            'analysis': analysis_result
        }
    )
    
    with open(f"{program_path}_analysis.json", 'w') as f:
        json.dump(report, f, indent=2)
    
    return report

# Analyze the account management program
analysis = asyncio.run(
    analyze_cobol_program(orchestrator, "banking-system/cobol/ACCTMGMT.cbl")
)
```

#### Exercise 2: Dependency Mapping

```python
# exercises/03_dependency_mapping.py
async def map_system_dependencies(orchestrator, system_path):
    """Map dependencies across the entire system"""
    
    print("🗺️ Mapping System Dependencies")
    
    # Discover all components
    components = await orchestrator.execute_agent(
        'cobol_discovery',
        {
            'action': 'discover_portfolio',
            'path': system_path,
            'recursive': True
        }
    )
    
    # Build dependency graph
    dependency_graph = await orchestrator.execute_agent(
        'dependency_mapper',
        {
            'action': 'build_graph',
            'components': components,
            'include_data_flows': True
        }
    )
    
    # Visualize dependencies
    visualization = await orchestrator.execute_agent(
        'visualization_agent',
        {
            'action': 'create_diagram',
            'graph': dependency_graph,
            'format': 'mermaid'
        }
    )
    
    print("\n📊 Dependency Visualization:")
    print(visualization['diagram'])
    
    # Identify critical paths
    critical_paths = await orchestrator.execute_agent(
        'dependency_analyzer',
        {
            'action': 'find_critical_paths',
            'graph': dependency_graph
        }
    )
    
    print("\n⚠️ Critical Dependencies:")
    for path in critical_paths['paths']:
        print(f"- {' -> '.join(path['components'])}")
        print(f"  Risk Level: {path['risk_level']}")
    
    return dependency_graph
```

#### Exercise 3: Business Logic Extraction

```python
# exercises/04_business_logic.py
async def extract_business_logic(orchestrator, program_path):
    """Extract and document business logic"""
    
    print("💼 Extracting Business Logic")
    
    # Extract business rules
    extraction_result = await orchestrator.execute_agent(
        'business_logic_extractor',
        {
            'action': 'extract',
            'program': program_path,
            'include_calculations': True,
            'include_validations': True,
            'include_workflows': True
        }
    )
    
    print(f"\n📑 Found {len(extraction_result['rules'])} Business Rules:")
    
    for idx, rule in enumerate(extraction_result['rules'], 1):
        print(f"\n{idx}. {rule['name']}")
        print(f"   Type: {rule['type']}")
        print(f"   Description: {rule['description']}")
        print(f"   Location: Lines {rule['start_line']}-{rule['end_line']}")
        
        if rule['type'] == 'calculation':
            print(f"   Formula: {rule['formula']}")
        elif rule['type'] == 'validation':
            print(f"   Condition: {rule['condition']}")
    
    # Generate business documentation
    documentation = await orchestrator.execute_agent(
        'documentation_agent',
        {
            'action': 'generate_business_docs',
            'rules': extraction_result['rules'],
            'format': 'markdown'
        }
    )
    
    with open("business_logic_documentation.md", 'w') as f:
        f.write(documentation['content'])
    
    return extraction_result
```

**Solution for Exercise 3:**
```python
# Expected output for ACCTMGMT.cbl:
{
    "rules": [
        {
            "id": "BR-001",
            "name": "Monthly Interest Calculation",
            "type": "calculation",
            "description": "Calculate monthly interest for savings accounts",
            "formula": "INTEREST = BALANCE * (ANNUAL_RATE / 12)",
            "variables": {
                "ANNUAL_RATE": 0.025,
                "applies_to": "SAVINGS_ACCOUNTS_ONLY"
            },
            "start_line": 45,
            "end_line": 52
        },
        {
            "id": "BR-002",
            "name": "Interest Eligibility",
            "type": "validation",
            "description": "Only savings accounts with positive balance earn interest",
            "condition": "ACCT-TYPE = 'SAV' AND ACCT-BALANCE > 0",
            "start_line": 43,
            "end_line": 44
        }
    ]
}
```

### Module 3: Transformation Planning (3 hours)

#### Exercise 4: Strategy Selection

```python
# exercises/05_strategy_selection.py
async def select_modernization_strategy(orchestrator, analysis_results):
    """Select optimal modernization strategy"""
    
    print("🎯 Selecting Modernization Strategy")
    
    # Assess modernization options
    strategy_assessment = await orchestrator.execute_agent(
        'strategy_advisor',
        {
            'action': 'assess_options',
            'analysis': analysis_results,
            'target_platforms': ['java', 'python', 'csharp'],
            'constraints': {
                'timeline': '6_months',
                'risk_tolerance': 'medium',
                'preserve_logic': True
            }
        }
    )
    
    print("\n📊 Strategy Assessment:")
    for option in strategy_assessment['options']:
        print(f"\n{option['target_language']}:")
        print(f"  - Feasibility: {option['feasibility_score']}/10")
        print(f"  - Risk Level: {option['risk_level']}")
        print(f"  - Estimated Effort: {option['effort_days']} days")
        print(f"  - Advantages: {', '.join(option['advantages'])}")
        print(f"  - Challenges: {', '.join(option['challenges'])}")
    
    # Select recommended strategy
    recommended = strategy_assessment['recommended']
    print(f"\n✅ Recommended Strategy: {recommended['target_language']}")
    print(f"Rationale: {recommended['rationale']}")
    
    return recommended
```

#### Exercise 5: Risk Assessment

```python
# exercises/06_risk_assessment.py
async def assess_modernization_risks(orchestrator, program_analysis, strategy):
    """Comprehensive risk assessment"""
    
    print("⚠️ Assessing Modernization Risks")
    
    # Perform risk analysis
    risk_assessment = await orchestrator.execute_agent(
        'risk_analyzer',
        {
            'action': 'analyze_risks',
            'program_complexity': program_analysis['complexity'],
            'dependencies': program_analysis['dependencies'],
            'strategy': strategy,
            'include_mitigations': True
        }
    )
    
    print("\n📊 Risk Assessment Results:")
    
    # Display risks by category
    for category, risks in risk_assessment['risks_by_category'].items():
        print(f"\n{category.upper()} Risks:")
        for risk in risks:
            print(f"  - {risk['name']}")
            print(f"    Probability: {risk['probability']}")
            print(f"    Impact: {risk['impact']}")
            print(f"    Score: {risk['score']}/100")
            
            if risk['mitigations']:
                print(f"    Mitigations:")
                for mitigation in risk['mitigations']:
                    print(f"      • {mitigation}")
    
    # Overall risk score
    print(f"\n📈 Overall Risk Score: {risk_assessment['overall_score']}/100")
    print(f"Risk Level: {risk_assessment['risk_level']}")
    
    return risk_assessment
```

#### Exercise 6: Resource Planning

```python
# exercises/07_resource_planning.py
async def plan_modernization_resources(orchestrator, strategy, risk_assessment):
    """Plan resources for modernization"""
    
    print("📅 Planning Modernization Resources")
    
    # Generate resource plan
    resource_plan = await orchestrator.execute_agent(
        'resource_planner',
        {
            'action': 'create_plan',
            'strategy': strategy,
            'risks': risk_assessment,
            'team_size': 5,
            'include_timeline': True,
            'include_skills': True
        }
    )
    
    print("\n👥 Team Composition:")
    for role in resource_plan['team_roles']:
        print(f"  - {role['title']}: {role['count']} person(s)")
        print(f"    Skills: {', '.join(role['required_skills'])}")
    
    print("\n📅 Timeline:")
    for phase in resource_plan['timeline']:
        print(f"\n{phase['name']} ({phase['duration']} weeks):")
        for task in phase['tasks']:
            print(f"  - {task['name']}: {task['effort_days']} days")
            print(f"    Assigned to: {task['assigned_role']}")
    
    print(f"\n💰 Estimated Total Effort: {resource_plan['total_effort_days']} person-days")
    
    return resource_plan
```

---

## Day 2: Transformation and Deployment

### Module 4: Code Transformation (4 hours)

#### Exercise 7: COBOL to Java Transformation

```python
# exercises/08_cobol_to_java.py
async def transform_cobol_to_java(orchestrator, cobol_file, business_rules):
    """Transform COBOL program to Java"""
    
    print("🔄 Transforming COBOL to Java")
    
    # Read COBOL source
    with open(cobol_file, 'r') as f:
        cobol_source = f.read()
    
    # Execute transformation
    transformation_result = await orchestrator.execute_agent(
        'code_transformer',
        {
            'action': 'transform',
            'source_code': cobol_source,
            'source_language': 'cobol',
            'target_language': 'java',
            'business_rules': business_rules,
            'options': {
                'framework': 'spring-boot',
                'preserve_comments': True,
                'generate_tests': True,
                'optimize_performance': True
            }
        }
    )
    
    print("\n✅ Transformation Complete!")
    print(f"Lines of Code: {transformation_result['metrics']['loc']}")
    print(f"Classes Generated: {transformation_result['metrics']['classes']}")
    print(f"Test Coverage: {transformation_result['metrics']['test_coverage']}%")
    
    # Save transformed code
    output_dir = "transformed/java"
    os.makedirs(output_dir, exist_ok=True)
    
    for file_name, content in transformation_result['files'].items():
        file_path = os.path.join(output_dir, file_name)
        os.makedirs(os.path.dirname(file_path), exist_ok=True)
        with open(file_path, 'w') as f:
            f.write(content)
        print(f"📄 Generated: {file_path}")
    
    return transformation_result
```

**Expected Java Output (AccountManager.java):**
```java
package com.workshop.banking.account;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * Account Management Service
 * Modernized from COBOL program ACCTMGMT
 * Original Author: WORKSHOP-TEAM
 */
@Service
public class AccountManager {
    
    private static final BigDecimal ANNUAL_INTEREST_RATE = new BigDecimal("0.025");
    private static final int MONTHS_IN_YEAR = 12;
    
    private final AccountRepository accountRepository;
    
    public AccountManager(AccountRepository accountRepository) {
        this.accountRepository = accountRepository;
    }
    
    /**
     * Process all accounts and calculate interest for eligible accounts
     * Business Rule BR-001: Monthly Interest Calculation
     */
    @Transactional
    public void processAccounts() {
        accountRepository.findAll().forEach(this::processAccount);
    }
    
    /**
     * Calculate and apply interest for a single account
     * Business Rule BR-002: Interest Eligibility
     */
    private void processAccount(Account account) {
        if (isEligibleForInterest(account)) {
            BigDecimal interestAmount = calculateMonthlyInterest(account.getBalance());
            account.setBalance(account.getBalance().add(interestAmount));
            accountRepository.save(account);
        }
    }
    
    /**
     * Check if account is eligible for interest
     * Only savings accounts with positive balance earn interest
     */
    private boolean isEligibleForInterest(Account account) {
        return "SAV".equals(account.getType()) 
            && account.getBalance().compareTo(BigDecimal.ZERO) > 0;
    }
    
    /**
     * Calculate monthly interest amount
     * Formula: INTEREST = BALANCE * (ANNUAL_RATE / 12)
     */
    private BigDecimal calculateMonthlyInterest(BigDecimal balance) {
        BigDecimal monthlyRate = ANNUAL_INTEREST_RATE.divide(
            new BigDecimal(MONTHS_IN_YEAR), 
            6, 
            RoundingMode.HALF_UP
        );
        return balance.multiply(monthlyRate).setScale(2, RoundingMode.HALF_UP);
    }
}
```

#### Exercise 8: Pattern Application

```python
# exercises/09_pattern_application.py
async def apply_modernization_patterns(orchestrator, transformed_code):
    """Apply design patterns to modernized code"""
    
    print("🎨 Applying Modernization Patterns")
    
    # Identify applicable patterns
    pattern_analysis = await orchestrator.execute_agent(
        'pattern_analyzer',
        {
            'action': 'identify_patterns',
            'code': transformed_code,
            'pattern_types': [
                'repository',
                'service',
                'factory',
                'strategy',
                'observer'
            ]
        }
    )
    
    print("\n📋 Applicable Patterns:")
    for pattern in pattern_analysis['applicable_patterns']:
        print(f"  - {pattern['name']}: {pattern['rationale']}")
    
    # Apply selected patterns
    enhanced_code = await orchestrator.execute_agent(
        'pattern_applicator',
        {
            'action': 'apply_patterns',
            'code': transformed_code,
            'patterns': pattern_analysis['applicable_patterns'],
            'refactor_level': 'comprehensive'
        }
    )
    
    print("\n✅ Patterns Applied Successfully!")
    
    # Generate pattern documentation
    pattern_docs = await orchestrator.execute_agent(
        'documentation_agent',
        {
            'action': 'document_patterns',
            'patterns_applied': enhanced_code['patterns_applied'],
            'format': 'markdown'
        }
    )
    
    return enhanced_code
```

#### Exercise 9: Parallel Transformation

```python
# exercises/10_parallel_transformation.py
async def transform_system_parallel(orchestrator, system_components):
    """Transform multiple components in parallel"""
    
    print("⚡ Parallel System Transformation")
    
    # Group components by dependency level
    dependency_levels = await orchestrator.execute_agent(
        'dependency_analyzer',
        {
            'action': 'calculate_levels',
            'components': system_components
        }
    )
    
    transformed_components = {}
    
    # Transform each level in parallel
    for level, components in dependency_levels.items():
        print(f"\n🔄 Transforming Level {level} ({len(components)} components)")
        
        # Create transformation tasks
        tasks = []
        for component in components:
            task = orchestrator.execute_agent(
                'code_transformer',
                {
                    'action': 'transform',
                    'component': component,
                    'context': transformed_components  # Previously transformed
                }
            )
            tasks.append(task)
        
        # Execute in parallel
        results = await asyncio.gather(*tasks)
        
        # Store results
        for component, result in zip(components, results):
            transformed_components[component['name']] = result
            print(f"  ✅ {component['name']} transformed")
    
    # Validate system integrity
    validation = await orchestrator.execute_agent(
        'system_validator',
        {
            'action': 'validate_integrity',
            'components': transformed_components
        }
    )
    
    print(f"\n📊 System Validation: {'PASSED' if validation['passed'] else 'FAILED'}")
    
    return transformed_components
```

### Module 5: Testing and Validation (2 hours)

#### Exercise 10: AI-Generated Tests

```python
# exercises/11_test_generation.py
async def generate_comprehensive_tests(orchestrator, java_code, business_rules):
    """Generate comprehensive test suite"""
    
    print("🧪 Generating Test Suite")
    
    # Generate different types of tests
    test_suite = await orchestrator.execute_agent(
        'test_generator',
        {
            'action': 'generate_suite',
            'code': java_code,
            'business_rules': business_rules,
            'test_types': [
                'unit',
                'integration',
                'edge_cases',
                'negative',
                'performance'
            ],
            'framework': 'junit5',
            'coverage_target': 90
        }
    )
    
    print(f"\n📊 Test Suite Generated:")
    print(f"  - Unit Tests: {len(test_suite['unit_tests'])}")
    print(f"  - Integration Tests: {len(test_suite['integration_tests'])}")
    print(f"  - Edge Case Tests: {len(test_suite['edge_cases'])}")
    print(f"  - Negative Tests: {len(test_suite['negative_tests'])}")
    print(f"  - Performance Tests: {len(test_suite['performance_tests'])}")
    
    # Save test files
    for test_type, tests in test_suite.items():
        for test in tests:
            file_path = f"transformed/java/src/test/java/{test['file_name']}"
            os.makedirs(os.path.dirname(file_path), exist_ok=True)
            with open(file_path, 'w') as f:
                f.write(test['content'])
    
    return test_suite
```

**Example Generated Test (AccountManagerTest.java):**
```java
package com.workshop.banking.account;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import java.math.BigDecimal;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class AccountManagerTest {
    
    @Mock
    private AccountRepository accountRepository;
    
    private AccountManager accountManager;
    
    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        accountManager = new AccountManager(accountRepository);
    }
    
    @Test
    void testInterestCalculationForSavingsAccount() {
        // Given - Business Rule BR-001
        Account savingsAccount = Account.builder()
            .accountNumber("12345")
            .type("SAV")
            .balance(new BigDecimal("1000.00"))
            .build();
            
        when(accountRepository.findAll()).thenReturn(List.of(savingsAccount));
        
        // When
        accountManager.processAccounts();
        
        // Then - Monthly interest at 2.5% annual = 0.2083% monthly
        BigDecimal expectedBalance = new BigDecimal("1002.08");
        assertEquals(expectedBalance, savingsAccount.getBalance());
        verify(accountRepository).save(savingsAccount);
    }
    
    @ParameterizedTest
    @CsvSource({
        "CHK, 1000.00, 1000.00",  // Checking account - no interest
        "SAV, 0.00, 0.00",        // Zero balance - no interest
        "SAV, -100.00, -100.00"   // Negative balance - no interest
    })
    void testInterestEligibility(String accountType, String balance, String expectedBalance) {
        // Business Rule BR-002 validation
        Account account = Account.builder()
            .accountNumber("12345")
            .type(accountType)
            .balance(new BigDecimal(balance))
            .build();
            
        when(accountRepository.findAll()).thenReturn(List.of(account));
        
        accountManager.processAccounts();
        
        assertEquals(new BigDecimal(expectedBalance), account.getBalance());
    }
}
```

#### Exercise 11: Regression Testing

```python
# exercises/12_regression_testing.py
async def run_regression_tests(orchestrator, original_code, transformed_code, test_data):
    """Run regression tests comparing original and transformed behavior"""
    
    print("🔄 Running Regression Tests")
    
    # Create test scenarios from test data
    test_scenarios = await orchestrator.execute_agent(
        'test_scenario_generator',
        {
            'action': 'generate_scenarios',
            'test_data': test_data,
            'coverage_type': 'comprehensive'
        }
    )
    
    # Run tests on both versions
    regression_results = await orchestrator.execute_agent(
        'regression_tester',
        {
            'action': 'compare_behaviors',
            'original_code': original_code,
            'transformed_code': transformed_code,
            'scenarios': test_scenarios,
            'comparison_criteria': [
                'output_equivalence',
                'performance',
                'error_handling'
            ]
        }
    )
    
    print("\n📊 Regression Test Results:")
    print(f"  - Total Scenarios: {regression_results['total_scenarios']}")
    print(f"  - Passed: {regression_results['passed']}")
    print(f"  - Failed: {regression_results['failed']}")
    print(f"  - Behavior Match: {regression_results['behavior_match_percentage']}%")
    
    if regression_results['failed'] > 0:
        print("\n❌ Failed Scenarios:")
        for failure in regression_results['failures']:
            print(f"  - {failure['scenario_name']}")
            print(f"    Expected: {failure['expected']}")
            print(f"    Actual: {failure['actual']}")
            print(f"    Difference: {failure['difference']}")
    
    return regression_results
```

#### Exercise 12: Performance Validation

```python
# exercises/13_performance_validation.py
async def validate_performance(orchestrator, transformed_system):
    """Validate performance of transformed system"""
    
    print("⚡ Performance Validation")
    
    # Define performance benchmarks
    benchmarks = {
        'response_time_p95': 200,  # milliseconds
        'throughput': 1000,        # transactions per second
        'memory_usage': 512,       # MB
        'cpu_usage': 70           # percentage
    }
    
    # Run performance tests
    performance_results = await orchestrator.execute_agent(
        'performance_tester',
        {
            'action': 'run_benchmarks',
            'system': transformed_system,
            'benchmarks': benchmarks,
            'duration': 300,  # 5 minutes
            'load_pattern': 'stepped'
        }
    )
    
    print("\n📊 Performance Results:")
    for metric, result in performance_results['metrics'].items():
        benchmark = benchmarks.get(metric, 'N/A')
        status = "✅" if result['value'] <= benchmark else "❌"
        print(f"  {status} {metric}: {result['value']} (benchmark: {benchmark})")
    
    # Generate performance report
    report = await orchestrator.generate_report(
        'performance',
        performance_results
    )
    
    return performance_results
```

### Module 6: Production Deployment (2 hours)

#### Exercise 13: Pipeline Creation

```python
# exercises/14_pipeline_creation.py
async def create_cicd_pipeline(orchestrator, project_config):
    """Create CI/CD pipeline for modernized application"""
    
    print("🚀 Creating CI/CD Pipeline")
    
    # Generate GitHub Actions workflow
    pipeline_config = await orchestrator.execute_agent(
        'pipeline_generator',
        {
            'action': 'generate_pipeline',
            'platform': 'github_actions',
            'project': project_config,
            'stages': [
                'build',
                'test',
                'security_scan',
                'deploy'
            ],
            'environments': ['dev', 'staging', 'production']
        }
    )
    
    # Save workflow file
    workflow_path = ".github/workflows/modernized-app-pipeline.yml"
    os.makedirs(os.path.dirname(workflow_path), exist_ok=True)
    with open(workflow_path, 'w') as f:
        f.write(pipeline_config['workflow'])
    
    print(f"✅ Pipeline created: {workflow_path}")
    
    # Create deployment configurations
    for env in ['dev', 'staging', 'production']:
        deploy_config = await orchestrator.execute_agent(
            'deployment_configurator',
            {
                'action': 'create_config',
                'environment': env,
                'application': project_config['name'],
                'platform': 'kubernetes'
            }
        )
        
        config_path = f"deployments/{env}/deployment.yaml"
        os.makedirs(os.path.dirname(config_path), exist_ok=True)
        with open(config_path, 'w') as f:
            f.write(deploy_config['manifest'])
    
    return pipeline_config
```

**Generated GitHub Actions Workflow:**
```yaml
name: Modernized Banking System CI/CD

on:
  push:
    branches: [main, develop]
  pull_request:
    branches: [main]

env:
  JAVA_VERSION: '17'
  MAVEN_VERSION: '3.8.6'

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v3
    
    - name: Set up JDK
      uses: actions/setup-java@v3
      with:
        java-version: ${{ env.JAVA_VERSION }}
        distribution: 'temurin'
    
    - name: Cache Maven dependencies
      uses: actions/cache@v3
      with:
        path: ~/.m2
        key: ${{ runner.os }}-m2-${{ hashFiles('**/pom.xml') }}
    
    - name: Build application
      run: mvn clean compile
    
    - name: Upload artifacts
      uses: actions/upload-artifact@v3
      with:
        name: build-artifacts
        path: target/

  test:
    needs: build
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v3
    
    - name: Set up JDK
      uses: actions/setup-java@v3
      with:
        java-version: ${{ env.JAVA_VERSION }}
    
    - name: Run unit tests
      run: mvn test
    
    - name: Run integration tests
      run: mvn integration-test
    
    - name: Generate test report
      uses: dorny/test-reporter@v1
      with:
        name: Test Results
        path: target/surefire-reports/*.xml
        reporter: java-junit

  security-scan:
    needs: build
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v3
    
    - name: Run security scan
      uses: github/codeql-action/analyze@v2
    
    - name: Dependency check
      run: mvn dependency-check:check

  deploy:
    needs: [test, security-scan]
    runs-on: ubuntu-latest
    if: github.ref == 'refs/heads/main'
    steps:
    - name: Deploy to Kubernetes
      uses: azure/k8s-deploy@v4
      with:
        manifests: |
          deployments/production/deployment.yaml
          deployments/production/service.yaml
        images: |
          ${{ secrets.REGISTRY }}/banking-system:${{ github.sha }}
```

#### Exercise 14: Canary Deployment

```python
# exercises/15_canary_deployment.py
async def execute_canary_deployment(orchestrator, application, environment):
    """Execute canary deployment with monitoring"""
    
    print("🐤 Executing Canary Deployment")
    
    # Create canary deployment plan
    canary_plan = await orchestrator.execute_agent(
        'deployment_planner',
        {
            'action': 'create_canary_plan',
            'application': application,
            'environment': environment,
            'strategy': {
                'initial_percentage': 10,
                'increment': 20,
                'analysis_duration': 300,  # 5 minutes per phase
                'success_criteria': {
                    'error_rate': 0.01,
                    'latency_p95': 200,
                    'cpu_usage': 80
                }
            }
        }
    )
    
    # Execute deployment
    deployment_result = await orchestrator.execute_agent(
        'canary_deployer',
        {
            'action': 'deploy',
            'plan': canary_plan,
            'monitoring_enabled': True,
            'auto_rollback': True
        }
    )
    
    print("\n📊 Canary Deployment Progress:")
    for phase in deployment_result['phases']:
        print(f"\nPhase {phase['number']}: {phase['percentage']}% traffic")
        print(f"  Duration: {phase['duration']}s")
        print(f"  Error Rate: {phase['metrics']['error_rate']}")
        print(f"  Latency P95: {phase['metrics']['latency_p95']}ms")
        print(f"  Decision: {phase['decision']}")
    
    print(f"\n{'✅' if deployment_result['success'] else '❌'} Deployment {'Completed' if deployment_result['success'] else 'Rolled Back'}")
    
    return deployment_result
```

#### Exercise 15: Monitoring Setup

```python
# exercises/16_monitoring_setup.py
async def setup_production_monitoring(orchestrator, application):
    """Set up comprehensive monitoring"""
    
    print("📊 Setting Up Production Monitoring")
    
    # Configure monitoring
    monitoring_config = await orchestrator.execute_agent(
        'monitoring_configurator',
        {
            'action': 'setup_monitoring',
            'application': application,
            'metrics': [
                'application_metrics',
                'infrastructure_metrics',
                'business_metrics',
                'security_metrics'
            ],
            'alerting': {
                'channels': ['email', 'slack', 'pagerduty'],
                'severity_levels': ['warning', 'error', 'critical']
            },
            'dashboards': [
                'application_health',
                'business_kpis',
                'infrastructure',
                'security'
            ]
        }
    )
    
    # Deploy monitoring stack
    deployment_result = await orchestrator.execute_agent(
        'infrastructure_deployer',
        {
            'action': 'deploy_monitoring',
            'config': monitoring_config,
            'components': [
                'prometheus',
                'grafana',
                'alertmanager',
                'loki'
            ]
        }
    )
    
    print("\n✅ Monitoring Stack Deployed:")
    print(f"  - Grafana URL: {deployment_result['urls']['grafana']}")
    print(f"  - Prometheus URL: {deployment_result['urls']['prometheus']}")
    print(f"  - Alert Manager URL: {deployment_result['urls']['alertmanager']}")
    
    # Create alerts
    alerts_created = await orchestrator.execute_agent(
        'alert_configurator',
        {
            'action': 'create_alerts',
            'rules': [
                {
                    'name': 'high_error_rate',
                    'condition': 'error_rate > 0.05',
                    'severity': 'critical'
                },
                {
                    'name': 'high_latency',
                    'condition': 'latency_p95 > 500',
                    'severity': 'warning'
                },
                {
                    'name': 'low_availability',
                    'condition': 'availability < 0.99',
                    'severity': 'error'
                }
            ]
        }
    )
    
    print(f"\n📢 Created {len(alerts_created['alerts'])} monitoring alerts")
    
    return monitoring_config
```

## Workshop Summary

### Key Takeaways

1. **Agent-Based Architecture**: Successfully implemented a multi-agent system for COBOL modernization
2. **AI-Powered Analysis**: Used AI agents to extract business logic and analyze code complexity
3. **Automated Transformation**: Transformed COBOL to Java while preserving business logic
4. **Comprehensive Testing**: Generated and executed comprehensive test suites
5. **Production Deployment**: Implemented CI/CD pipeline with canary deployment

### Skills Acquired

- Setting up MCP-enabled agent environments
- Orchestrating multiple AI agents for complex tasks
- Analyzing and transforming legacy COBOL code
- Implementing modern CI/CD practices
- Deploying and monitoring modernized applications

### Next Steps

1. Apply these techniques to your own mainframe applications
2. Explore advanced agent patterns in Chapter 15
3. Implement self-healing pipelines from Chapter 16
4. Join the community forum to share experiences

### Additional Resources

- [Sample Code Repository](https://github.com/mainframe-modernization/workshop-samples)
- [Agent Development Guide](../15-mcp-enabled-agent-architecture/README.md)
- [Best Practices Document](../best-practices/README.md)
- [Community Forum](https://community.mainframe-modernization.org)

## Appendix: Troubleshooting Guide

### Common Issues and Solutions

| Issue | Solution |
|-------|----------|
| Agent connection timeout | Check MCP server is running: `docker-compose ps` |
| Transformation errors | Verify COBOL syntax and ensure all copybooks are available |
| Test generation failures | Check business rules are properly extracted |
| Deployment issues | Verify Kubernetes cluster connectivity and credentials |
| Performance problems | Scale agent resources: `docker-compose scale agent-runtime=3` |

### Debug Commands

```bash
# Check agent logs
docker-compose logs -f agent-runtime

# Monitor MCP server
curl http://localhost:8080/health

# View agent status
python -m agents.cli status --all

# Reset environment
docker-compose down -v && docker-compose up -d
``` 