# 🏫 Workshop: Natural/Adabas Modernization Journey with AI Agents

## Workshop Overview

This hands-on workshop guides participants through modernizing Natural/Adabas systems using AI agents, focusing on data model transformation and application migration.

### Duration
- **Full Workshop**: 2 days (16 hours)
- **Express Version**: 1 day (8 hours)

### Prerequisites
- Basic understanding of Natural programming language
- Familiarity with Adabas database concepts
- Azure account with AI Foundry access
- GitHub account with Copilot access

### Learning Objectives
1. Analyze Natural programs and Adabas DDMs using AI agents
2. Transform Adabas data models to modern databases
3. Migrate Natural applications to Java/Spring Boot
4. Implement AI-powered testing for data integrity
5. Deploy modernized applications with intelligent monitoring

## Module 1: Natural/Adabas Environment Setup (2 hours)

### 1.1 Workshop Environment Configuration

```bash
# Setup Natural/Adabas workshop environment
git clone https://github.com/your-org/natural-adabas-workshop
cd natural-adabas-workshop

# Initialize Python environment
python -m venv workshop-env
source workshop-env/bin/activate

# Install workshop dependencies
pip install -r requirements.txt

# Start Adabas emulator (for testing)
docker-compose -f emulator/adabas-docker-compose.yml up -d

# Initialize MCP with Natural/Adabas contexts
python scripts/init_natural_mcp.py
```

### 1.2 Sample Natural/Adabas Application

We'll work with an insurance system that includes:
- Policy management (POLICY.NSP)
- Claims processing (CLAIMS.NSP)
- Customer management (CUSTOMER.NSP)
- Premium calculations (PREMIUM.NSP)

```natural
* POLICY.NSP - Policy Management Program
DEFINE DATA
LOCAL
01 POLICY-VIEW VIEW OF POLICY-FILE
  02 POLICY-NUMBER      (A10)
  02 CUSTOMER-ID        (A8)
  02 POLICY-TYPE        (A2)
  02 COVERAGE-AMOUNT    (P11.2)
  02 PREMIUM-AMOUNT     (P9.2)
  02 EFFECTIVE-DATE     (N8)
  02 EXPIRY-DATE        (N8)
  02 STATUS             (A1)
  02 REDEFINE STATUS
    03 STATUS-VALUES    (A1/1:3)

01 CUSTOMER-VIEW VIEW OF CUSTOMER-FILE
  02 CUSTOMER-ID        (A8)
  02 CUSTOMER-NAME      (A30)
  02 ADDRESS-LINES      (A30/1:3)  /* MU field
  02 PHONE-NUMBERS      (A15/1:5)  /* MU field
  02 EMAIL              (A50)

01 #WORK-AREAS
  02 #POLICY-COUNT      (N5)
  02 #TOTAL-PREMIUM     (P13.2)
  02 #ERROR-MESSAGE     (A80)
  02 #RETURN-CODE       (N2)

01 #SCREEN-FIELDS
  02 #FUNCTION          (A1)
  02 #POLICY-NUMBER     (A10)
  02 #MSG               (A79)
END-DEFINE

* Main processing logic
REPEAT
  INPUT USING MAP 'POLMAP'
    #FUNCTION #POLICY-NUMBER
  
  DECIDE FOR FIRST CONDITION
    WHEN #FUNCTION = 'A'
      PERFORM ADD-POLICY
    WHEN #FUNCTION = 'U'
      PERFORM UPDATE-POLICY
    WHEN #FUNCTION = 'D'
      PERFORM DELETE-POLICY
    WHEN #FUNCTION = 'I'
      PERFORM INQUIRY
    WHEN #FUNCTION = 'X'
      ESCAPE BOTTOM
    WHEN NONE
      REINPUT 'Invalid function'
  END-DECIDE
END-REPEAT

* Subroutines
DEFINE SUBROUTINE ADD-POLICY
  /* Policy creation logic
  READ POLICY-VIEW WITH POLICY-NUMBER = #POLICY-NUMBER
    IF NO RECORDS FOUND
      /* Create new policy
      POLICY-VIEW.POLICY-NUMBER := #POLICY-NUMBER
      STORE POLICY-VIEW
    END-NOREC
  END-READ
END-SUBROUTINE
```

### 1.3 Adabas DDM (Data Definition Module)

```
* POLICY-FILE DDM
01 AA POLICY-FILE
  02 AB POLICY-NUMBER      A   10 D
  02 AC CUSTOMER-ID        A    8 N
  02 AD POLICY-TYPE        A    2 F
  02 AE COVERAGE-AMOUNT    P  5.2 F
  02 AF PREMIUM-AMOUNT     P  4.2 F
  02 AG EFFECTIVE-DATE     N    8 F
  02 AH EXPIRY-DATE        N    8 F
  02 AI STATUS             A    1 F
  02 AJ AGENT-ID           A    6 N
  02 AK COVERAGE-DETAILS              PE
    03 AL COVERAGE-TYPE    A    3
    03 AM COVERAGE-LIMIT   P  5.2
    03 AN DEDUCTIBLE       P  4.2
  02 AO BENEFICIARIES      A   30 MU
  02 AP PAYMENT-HISTORY               PE
    03 AQ PAYMENT-DATE     N    8
    03 AR PAYMENT-AMOUNT   P  4.2
    03 AS PAYMENT-METHOD   A    1
```

### 1.4 Initialize Natural/Adabas Agents

```python
# workshop/init_natural_agents.py
import asyncio
from agentic_framework import AgentOrchestrator, MCPServer
from agents.natural import (
    NaturalAnalyzerAgent,
    AdabasSchemaAnalyzer,
    DataMigrationAgent,
    NaturalTransformationAgent,
    AdabasToSQLAgent
)

async def initialize_natural_workshop():
    """Initialize agents for Natural/Adabas workshop"""
    
    # Configure MCP with Natural/Adabas contexts
    mcp_server = MCPServer(
        name="natural-adabas-mcp",
        contexts={
            "natural": NaturalProgrammingContext(),
            "adabas": AdabasDatabaseContext(),
            "insurance": InsuranceDomainContext()
        }
    )
    await mcp_server.start()
    
    # Initialize orchestrator
    orchestrator = AgentOrchestrator(
        mcp_server=mcp_server,
        azure_endpoint=os.getenv("AZURE_AI_FOUNDRY_ENDPOINT")
    )
    
    # Register specialized agents
    agents = {
        "natural_analyzer": NaturalAnalyzerAgent(orchestrator),
        "adabas_analyzer": AdabasSchemaAnalyzer(orchestrator),
        "data_migrator": DataMigrationAgent(orchestrator),
        "natural_transformer": NaturalTransformationAgent(orchestrator),
        "sql_converter": AdabasToSQLAgent(orchestrator)
    }
    
    for name, agent in agents.items():
        await orchestrator.register_agent(name, agent)
    
    print("✅ Natural/Adabas agents initialized!")
    return orchestrator
```

## Module 2: Natural Program Analysis (3 hours)

### 2.1 Natural Code Analysis Exercise

```python
# workshop/exercises/analyze_natural.py
async def analyze_natural_program(orchestrator, natural_file):
    """Analyze Natural program structure and complexity"""
    
    print(f"\n🔍 Analyzing Natural Program: {natural_file}")
    
    with open(natural_file, 'r') as f:
        natural_code = f.read()
    
    analyzer = orchestrator.get_agent("natural_analyzer")
    
    # Comprehensive Natural analysis
    analysis = await analyzer.analyze({
        "code": natural_code,
        "analysis_types": [
            "structure",
            "data_access",
            "business_logic",
            "maps",
            "subprograms",
            "complexity"
        ],
        "include_modernization_hints": True
    })
    
    # Display analysis results
    print("\n📊 Natural Program Analysis:")
    print(f"  Program Type: {analysis['program_type']}")
    print(f"  Lines of Code: {analysis['metrics']['loc']}")
    print(f"  Complexity Score: {analysis['complexity']['score']}/100")
    
    print("\n🗄️ Data Access Patterns:")
    for pattern in analysis['data_access_patterns']:
        print(f"  - {pattern['operation']}: {pattern['file']} ({pattern['count']} occurrences)")
    
    print("\n🖥️ Maps Used:")
    for map_name in analysis['maps']:
        print(f"  - {map_name}")
    
    print("\n📞 Subprogram Calls:")
    for subprogram in analysis['subprograms']:
        print(f"  - {subprogram['name']} ({subprogram['type']})")
    
    print("\n💡 Modernization Recommendations:")
    for rec in analysis['modernization_recommendations']:
        print(f"  - {rec['recommendation']}")
        print(f"    Complexity: {rec['complexity']}")
    
    return analysis

# Exercise: Analyze all Natural programs
async def analyze_insurance_system():
    orchestrator = await initialize_natural_workshop()
    
    natural_files = [
        "sample-apps/insurance/POLICY.NSP",
        "sample-apps/insurance/CLAIMS.NSP",
        "sample-apps/insurance/CUSTOMER.NSP",
        "sample-apps/insurance/PREMIUM.NSP"
    ]
    
    analyses = {}
    for file in natural_files:
        analyses[file] = await analyze_natural_program(orchestrator, file)
    
    # Generate system-wide analysis
    await generate_system_analysis(analyses)
```

### 2.2 Adabas DDM Analysis

```python
# workshop/exercises/analyze_adabas_ddm.py
async def analyze_adabas_schema(orchestrator, ddm_files):
    """Analyze Adabas DDMs and suggest modern data models"""
    
    print("\n🗄️ Analyzing Adabas Schema...")
    
    adabas_analyzer = orchestrator.get_agent("adabas_analyzer")
    
    schema_analysis = {}
    
    for ddm_file in ddm_files:
        with open(ddm_file, 'r') as f:
            ddm_content = f.read()
        
        # Analyze DDM
        analysis = await adabas_analyzer.analyze_ddm({
            "ddm_content": ddm_content,
            "analyze_mu_fields": True,  # Multiple value fields
            "analyze_pe_groups": True,  # Periodic groups
            "suggest_normalization": True,
            "target_database": "postgresql"
        })
        
        schema_analysis[ddm_file] = analysis
        
        # Display DDM analysis
        print(f"\n📋 DDM: {ddm_file}")
        print(f"  Fields: {len(analysis['fields'])}")
        print(f"  MU Fields: {len(analysis['mu_fields'])}")
        print(f"  PE Groups: {len(analysis['pe_groups'])}")
        
        print("\n  🔄 Suggested Transformations:")
        for transform in analysis['transformations']:
            print(f"    - {transform['field']}: {transform['suggestion']}")
    
    # Generate modern schema
    modern_schema = await generate_modern_schema(adabas_analyzer, schema_analysis)
    
    return modern_schema

async def generate_modern_schema(analyzer, schema_analysis):
    """Generate modern relational schema from Adabas"""
    
    print("\n🏗️ Generating Modern Schema...")
    
    modern_schema = await analyzer.generate_modern_schema({
        "adabas_schemas": schema_analysis,
        "target_database": "postgresql",
        "normalization_level": "3nf",
        "handle_mu_fields": "separate_tables",
        "handle_pe_groups": "json_columns",
        "generate_orm": True,
        "orm_framework": "jpa"
    })
    
    # Save generated schema
    with open("output/modern_schema.sql", 'w') as f:
        f.write(modern_schema['sql_ddl'])
    
    with open("output/jpa_entities.java", 'w') as f:
        f.write(modern_schema['jpa_entities'])
    
    print("  ✓ Generated PostgreSQL schema")
    print("  ✓ Generated JPA entities")
    print(f"  ✓ Tables created: {len(modern_schema['tables'])}")
    
    return modern_schema
```

### 2.3 Business Logic Extraction

```python
# workshop/exercises/extract_natural_logic.py
async def extract_business_logic_from_natural(orchestrator, natural_program):
    """Extract business rules from Natural programs"""
    
    print(f"\n🧠 Extracting Business Logic from {natural_program}")
    
    analyzer = orchestrator.get_agent("natural_analyzer")
    
    # Extract business logic with AI
    business_logic = await analyzer.extract_business_logic({
        "program_file": natural_program,
        "include_calculations": True,
        "include_validations": True,
        "include_decision_trees": True,
        "output_format": "structured",
        "generate_documentation": True
    })
    
    print("\n📋 Extracted Business Rules:")
    
    # Display validations
    print("\n✅ Validation Rules:")
    for validation in business_logic['validations']:
        print(f"  - {validation['name']}")
        print(f"    Field: {validation['field']}")
        print(f"    Rule: {validation['rule']}")
        print(f"    Error: {validation['error_message']}")
    
    # Display calculations
    print("\n🧮 Business Calculations:")
    for calc in business_logic['calculations']:
        print(f"  - {calc['name']}")
        print(f"    Formula: {calc['formula']}")
        print(f"    Variables: {', '.join(calc['variables'])}")
    
    # Display decision logic
    print("\n🌳 Decision Trees:")
    for decision in business_logic['decision_trees']:
        print(f"  - {decision['name']}")
        visualize_decision_tree(decision['tree'])
    
    # Generate business rule documentation
    doc = generate_business_documentation(business_logic)
    with open(f"docs/{Path(natural_program).stem}_rules.md", 'w') as f:
        f.write(doc)
    
    return business_logic

def visualize_decision_tree(tree):
    """Simple visualization of decision tree"""
    def print_node(node, indent=4):
        print(" " * indent + f"IF {node['condition']}")
        print(" " * (indent + 2) + f"THEN {node['then_action']}")
        if 'else_action' in node:
            print(" " * (indent + 2) + f"ELSE {node['else_action']}")
        for child in node.get('children', []):
            print_node(child, indent + 4)
    
    print_node(tree['root'])
```

## Module 3: Data Migration (4 hours)

### 3.1 Data Migration Planning

```python
# workshop/exercises/plan_data_migration.py
async def plan_data_migration(orchestrator, adabas_schemas, modern_schemas):
    """Plan comprehensive data migration strategy"""
    
    print("\n📊 Planning Data Migration...")
    
    data_migrator = orchestrator.get_agent("data_migrator")
    
    # Create migration plan
    migration_plan = await data_migrator.create_migration_plan({
        "source_schemas": adabas_schemas,
        "target_schemas": modern_schemas,
        "migration_approach": "phased",
        "include_data_validation": True,
        "handle_mu_fields": {
            "strategy": "normalize",
            "create_junction_tables": True
        },
        "handle_pe_groups": {
            "strategy": "json_or_normalize",
            "threshold": 5  # Normalize if > 5 occurrences
        },
        "data_transformations": {
            "date_format": "convert_natural_to_iso",
            "packed_decimal": "convert_to_numeric",
            "redefines": "handle_with_views"
        }
    })
    
    print("\n📋 Migration Plan Summary:")
    print(f"  Total Tables: {len(migration_plan['table_mappings'])}")
    print(f"  Total Fields: {migration_plan['total_fields']}")
    print(f"  Complex Transformations: {len(migration_plan['complex_transformations'])}")
    print(f"  Estimated Duration: {migration_plan['estimated_hours']} hours")
    
    print("\n🔄 Table Mappings:")
    for mapping in migration_plan['table_mappings']:
        print(f"  {mapping['source']} → {mapping['target']}")
        print(f"    Fields: {len(mapping['field_mappings'])}")
        if mapping['requires_transformation']:
            print(f"    ⚠️  Requires transformation")
    
    return migration_plan

# Exercise: Execute data migration
async def execute_data_migration(orchestrator, migration_plan):
    """Execute the data migration plan"""
    
    print("\n🚀 Executing Data Migration...")
    
    data_migrator = orchestrator.get_agent("data_migrator")
    
    # Configure migration execution
    execution_config = {
        "batch_size": 10000,
        "parallel_threads": 4,
        "validation_mode": "continuous",
        "rollback_enabled": True,
        "progress_tracking": True
    }
    
    # Start migration
    migration_job = await data_migrator.start_migration(
        plan=migration_plan,
        config=execution_config
    )
    
    # Monitor progress
    async for progress in migration_job.monitor():
        print(f"\r⏳ Progress: {progress['percentage']:.1f}% "
              f"({progress['records_migrated']:,}/{progress['total_records']:,}) "
              f"ETA: {progress['eta']}", end='')
        
        if progress['errors']:
            print(f"\n⚠️  Errors detected: {len(progress['errors'])}")
            for error in progress['errors'][:5]:
                print(f"  - {error['table']}: {error['message']}")
    
    print("\n\n✅ Migration Complete!")
    
    # Get final report
    report = await migration_job.get_report()
    print("\n📊 Migration Report:")
    print(f"  Duration: {report['duration']}")
    print(f"  Records Migrated: {report['total_records_migrated']:,}")
    print(f"  Success Rate: {report['success_rate']:.1%}")
    
    return report
```

### 3.2 Data Validation Exercise

```python
# workshop/exercises/validate_migration.py
async def validate_migrated_data(orchestrator, source_db, target_db):
    """Comprehensive validation of migrated data"""
    
    print("\n🔍 Validating Migrated Data...")
    
    validator = orchestrator.get_agent("data_validator")
    
    # Define validation rules
    validation_config = {
        "validation_types": [
            "record_count",
            "data_integrity",
            "referential_integrity",
            "business_rules",
            "calculated_fields"
        ],
        "sampling_strategy": {
            "method": "stratified",
            "sample_size": 0.1,  # 10% sample
            "include_edge_cases": True
        },
        "comparison_tolerance": {
            "numeric_fields": 0.01,  # 1% tolerance
            "date_fields": 0  # Exact match
        }
    }
    
    # Execute validation
    validation_results = await validator.validate_migration({
        "source": source_db,
        "target": target_db,
        "config": validation_config
    })
    
    # Display results
    print("\n✅ Validation Results:")
    for check in validation_results['checks']:
        status = "✓" if check['passed'] else "✗"
        print(f"  {status} {check['name']}: {check['message']}")
    
    if validation_results['data_discrepancies']:
        print("\n⚠️  Data Discrepancies Found:")
        for disc in validation_results['data_discrepancies'][:10]:
            print(f"  Table: {disc['table']}, Field: {disc['field']}")
            print(f"    Source: {disc['source_value']}")
            print(f"    Target: {disc['target_value']}")
    
    # Generate detailed report
    await generate_validation_report(validation_results)
    
    return validation_results
```

## Module 4: Natural to Java Transformation (4 hours)

### 4.1 Natural Program Transformation

```python
# workshop/exercises/transform_natural_to_java.py
async def transform_natural_to_java(orchestrator, natural_program, modern_schema):
    """Transform Natural program to Java/Spring Boot"""
    
    print(f"\n🔄 Transforming {natural_program} to Java...")
    
    transformer = orchestrator.get_agent("natural_transformer")
    
    # Read Natural code
    with open(natural_program, 'r') as f:
        natural_code = f.read()
    
    # Configure transformation
    transformation_config = {
        "source_code": natural_code,
        "target_framework": "spring_boot",
        "architecture_pattern": "clean_architecture",
        "include_components": [
            "rest_controllers",
            "services",
            "repositories",
            "entities",
            "dtos",
            "mappers"
        ],
        "database_schema": modern_schema,
        "use_lombok": True,
        "generate_tests": True,
        "test_coverage_target": 85
    }
    
    # Execute transformation
    transformation_result = await transformer.transform(transformation_config)
    
    print("\n📦 Generated Components:")
    for component in transformation_result['components']:
        print(f"  - {component['type']}: {component['name']}")
        
        # Save component
        file_path = f"output/java/{component['package_path']}/{component['name']}.java"
        Path(file_path).parent.mkdir(parents=True, exist_ok=True)
        with open(file_path, 'w') as f:
            f.write(component['content'])
    
    # Display sample controller
    controller = next(c for c in transformation_result['components'] 
                     if c['type'] == 'controller')
    print(f"\n📄 Sample Controller ({controller['name']}):")
    print(controller['content'][:800] + "...")
    
    return transformation_result

# Example transformation result
async def transform_policy_management():
    orchestrator = await initialize_natural_workshop()
    
    # First analyze the Natural program
    analysis = await analyze_natural_program(
        orchestrator,
        "sample-apps/insurance/POLICY.NSP"
    )
    
    # Get modern schema
    modern_schema = await get_modern_schema(orchestrator)
    
    # Transform to Java
    result = await transform_natural_to_java(
        orchestrator,
        "sample-apps/insurance/POLICY.NSP",
        modern_schema
    )
    
    print("\n✅ Transformation Complete!")
    print(f"  Files Generated: {len(result['components'])}")
    print(f"  Test Coverage: {result['test_coverage']}%")
```

### 4.2 Map Transformation

```python
# workshop/exercises/transform_natural_maps.py
async def transform_natural_maps_to_ui(orchestrator, map_files):
    """Transform Natural maps to modern UI components"""
    
    print("\n🖼️ Transforming Natural Maps to Modern UI...")
    
    ui_transformer = orchestrator.get_agent("ui_transformer")
    
    for map_file in map_files:
        with open(map_file, 'r') as f:
            map_content = f.read()
        
        # Analyze map structure
        map_analysis = await ui_transformer.analyze_natural_map({
            "map_content": map_content,
            "extract_fields": True,
            "extract_layout": True
        })
        
        # Transform to modern UI
        ui_result = await ui_transformer.transform_to_ui({
            "map_analysis": map_analysis,
            "target_framework": "react",
            "ui_library": "material_ui",
            "responsive": True,
            "generate_api_integration": True,
            "include_validation": True
        })
        
        print(f"\n🎨 Transformed: {map_file}")
        print(f"  Component Type: {ui_result['component_type']}")
        print(f"  Form Fields: {len(ui_result['form_fields'])}")
        
        # Save React component
        component_path = f"output/ui/{ui_result['component_name']}.jsx"
        with open(component_path, 'w') as f:
            f.write(ui_result['component_code'])
        
        # Save CSS
        css_path = f"output/ui/{ui_result['component_name']}.css"
        with open(css_path, 'w') as f:
            f.write(ui_result['styles'])
        
        print(f"  ✓ Generated: {component_path}")
        print(f"  ✓ Generated: {css_path}")
    
    return ui_results
```

## Module 5: Testing and Validation (3 hours)

### 5.1 Comprehensive Test Generation

```python
# workshop/exercises/generate_comprehensive_tests.py
async def generate_natural_migration_tests(orchestrator, 
                                         original_natural, 
                                         transformed_java):
    """Generate comprehensive tests for Natural to Java migration"""
    
    print("\n🧪 Generating Comprehensive Test Suite...")
    
    test_generator = orchestrator.get_agent("test_generator")
    
    # Generate different test types
    test_config = {
        "original_system": {
            "type": "natural",
            "files": original_natural,
            "database": "adabas"
        },
        "new_system": {
            "type": "java",
            "files": transformed_java,
            "database": "postgresql"
        },
        "test_categories": {
            "unit_tests": {
                "coverage_target": 90,
                "include_edge_cases": True
            },
            "integration_tests": {
                "test_database_operations": True,
                "test_api_endpoints": True
            },
            "regression_tests": {
                "compare_outputs": True,
                "data_scenarios": "comprehensive"
            },
            "performance_tests": {
                "baseline": "natural_performance_metrics",
                "acceptable_degradation": 10
            }
        }
    }
    
    # Generate tests
    test_suite = await test_generator.generate_migration_tests(test_config)
    
    print("\n📋 Generated Tests:")
    for category, tests in test_suite.items():
        print(f"\n{category.upper()}:")
        for test in tests['test_classes']:
            print(f"  - {test['name']}: {test['test_count']} tests")
    
    # Save test files
    for test_file in test_suite['files']:
        save_test_file(test_file)
    
    return test_suite

# Execute regression testing
async def execute_regression_tests(orchestrator, test_suite):
    """Execute regression tests between Natural and Java systems"""
    
    print("\n🔄 Executing Regression Tests...")
    
    test_executor = orchestrator.get_agent("test_executor")
    
    # Configure parallel execution
    execution_config = {
        "parallel_execution": True,
        "max_parallel": 4,
        "timeout_per_test": 300,
        "capture_metrics": True,
        "generate_report": True
    }
    
    # Run tests
    results = await test_executor.execute_tests(
        test_suite=test_suite,
        config=execution_config
    )
    
    # Display results
    print("\n📊 Test Results:")
    print(f"  Total Tests: {results['total']}")
    print(f"  Passed: {results['passed']} ✓")
    print(f"  Failed: {results['failed']} ✗")
    print(f"  Skipped: {results['skipped']} ⚠️")
    print(f"  Duration: {results['duration']}")
    
    if results['failed'] > 0:
        print("\n❌ Failed Tests:")
        for failure in results['failures'][:5]:
            print(f"  - {failure['test']}: {failure['reason']}")
    
    return results
```

### 5.2 Data Integrity Testing

```python
# workshop/exercises/test_data_integrity.py
async def test_data_integrity(orchestrator, source_data, migrated_data):
    """Test data integrity after migration"""
    
    print("\n🔍 Testing Data Integrity...")
    
    integrity_tester = orchestrator.get_agent("data_integrity_tester")
    
    # Define integrity checks
    integrity_tests = await integrity_tester.create_integrity_tests({
        "test_types": [
            "primary_key_uniqueness",
            "foreign_key_relationships",
            "data_type_consistency",
            "business_rule_compliance",
            "calculated_field_accuracy",
            "mu_field_migration",
            "pe_group_transformation"
        ],
        "custom_rules": [
            {
                "name": "premium_calculation",
                "rule": "premium_amount = coverage_amount * rate_factor",
                "tolerance": 0.01
            },
            {
                "name": "policy_status_validity",
                "rule": "status IN ('A', 'D', 'C')",
                "strict": True
            }
        ]
    })
    
    # Execute tests
    test_results = await integrity_tester.execute_tests(
        source=source_data,
        target=migrated_data,
        tests=integrity_tests
    )
    
    # Display results
    print("\n✅ Integrity Test Results:")
    for test in test_results['tests']:
        status = "✓" if test['passed'] else "✗"
        print(f"  {status} {test['name']}")
        if not test['passed']:
            print(f"    Issues: {test['issue_count']}")
            print(f"    Sample: {test['sample_issue']}")
    
    # Generate detailed report
    report_path = "output/data_integrity_report.html"
    await integrity_tester.generate_report(test_results, report_path)
    print(f"\n📄 Detailed report: {report_path}")
    
    return test_results
```

## Module 6: CI/CD Pipeline Implementation (2 hours)

### 6.1 Natural/Adabas CI/CD Pipeline

```yaml
# workshop/pipelines/natural-adabas-cicd.yml
name: Natural/Adabas Modernization Pipeline

on:
  push:
    branches: [main, develop]
  workflow_dispatch:
    inputs:
      migration_phase:
        description: 'Migration Phase'
        required: true
        type: choice
        options:
          - analysis
          - schema_migration
          - data_migration
          - code_transformation
          - testing
          - deployment

jobs:
  agent-setup:
    name: Initialize Agents
    runs-on: ubuntu-latest
    outputs:
      session_id: ${{ steps.init.outputs.session_id }}
    
    steps:
      - uses: actions/checkout@v4
      
      - name: Setup Natural/Adabas Agents
        id: init
        run: |
          python workshop/init_natural_agents.py
          echo "session_id=${{ env.SESSION_ID }}" >> $GITHUB_OUTPUT

  schema-analysis:
    name: Analyze Adabas Schema
    needs: agent-setup
    runs-on: ubuntu-latest
    if: contains(fromJson('["analysis", "schema_migration"]'), github.event.inputs.migration_phase)
    
    steps:
      - uses: actions/checkout@v4
      
      - name: Analyze DDMs
        run: |
          python -m agents.adabas_analyzer \
            --ddm-path ddms/ \
            --output-format json \
            --include-relationships true \
            --suggest-normalizations true
      
      - name: Generate Modern Schema
        run: |
          python -m agents.schema_generator \
            --analysis-file adabas-analysis.json \
            --target-db postgresql \
            --normalization 3nf \
            --generate-migrations true

  data-migration:
    name: Migrate Adabas Data
    needs: [agent-setup, schema-analysis]
    runs-on: ubuntu-latest
    if: github.event.inputs.migration_phase == 'data_migration'
    
    steps:
      - uses: actions/checkout@v4
      
      - name: Create Migration Plan
        run: |
          python -m agents.data_migrator \
            --create-plan \
            --source adabas \
            --target postgresql \
            --include-validation true
      
      - name: Execute Migration
        run: |
          python -m agents.data_migrator \
            --execute \
            --plan migration-plan.json \
            --batch-size 10000 \
            --parallel 4
      
      - name: Validate Migration
        run: |
          python -m agents.data_validator \
            --source adabas \
            --target postgresql \
            --validation-level comprehensive

  code-transformation:
    name: Transform Natural Programs
    needs: [agent-setup, schema-analysis]
    runs-on: ubuntu-latest
    if: github.event.inputs.migration_phase == 'code_transformation'
    strategy:
      matrix:
        program: [policy, claims, customer, premium]
    
    steps:
      - uses: actions/checkout@v4
      
      - name: Transform Natural to Java
        run: |
          python -m agents.natural_transformer \
            --program natural/${{ matrix.program }}.nsp \
            --target java-spring-boot \
            --architecture clean \
            --generate-tests true
      
      - name: Generate API Documentation
        run: |
          python -m agents.doc_generator \
            --source transformed/${{ matrix.program }} \
            --format openapi \
            --include-examples true

  comprehensive-testing:
    name: Comprehensive Testing
    needs: code-transformation
    runs-on: ubuntu-latest
    if: github.event.inputs.migration_phase == 'testing'
    
    steps:
      - uses: actions/checkout@v4
      
      - name: Setup Test Environment
        run: |
          docker-compose -f test-env/docker-compose.yml up -d
          ./scripts/wait-for-services.sh
      
      - name: Run Test Suite
        run: |
          python -m agents.test_orchestrator \
            --test-categories "unit,integration,regression,performance" \
            --parallel true \
            --generate-missing-tests true
      
      - name: Compare with Natural System
        run: |
          python -m agents.regression_tester \
            --original natural-system \
            --modernized java-system \
            --scenarios comprehensive

  intelligent-deployment:
    name: Deploy Modernized System
    needs: comprehensive-testing
    runs-on: ubuntu-latest
    environment: production
    if: github.event.inputs.migration_phase == 'deployment'
    
    steps:
      - uses: actions/checkout@v4
      
      - name: Deployment Strategy Decision
        id: strategy
        run: |
          python -m agents.deployment_manager \
            --analyze-system-complexity \
            --determine-strategy auto \
            --risk-assessment true
      
      - name: Execute Deployment
        run: |
          python -m agents.deployment_manager \
            --execute \
            --strategy ${{ steps.strategy.outputs.strategy }} \
            --monitoring-enabled true
```

### 6.2 Monitoring and Observability

```python
# workshop/exercises/setup_monitoring.py
async def setup_modernized_system_monitoring(orchestrator, deployed_services):
    """Setup comprehensive monitoring for modernized Natural/Adabas system"""
    
    print("\n📡 Setting up Monitoring...")
    
    monitor = orchestrator.get_agent("system_monitor")
    
    # Configure monitoring
    monitoring_config = {
        "services": deployed_services,
        "metrics": {
            "application": [
                "response_time",
                "throughput",
                "error_rate",
                "active_users"
            ],
            "database": [
                "query_performance",
                "connection_pool",
                "deadlocks",
                "slow_queries"
            ],
            "business": [
                "policies_created",
                "claims_processed",
                "premium_calculations",
                "daily_transactions"
            ]
        },
        "comparison_baseline": {
            "source": "natural_adabas_metrics",
            "alert_on_degradation": True,
            "threshold": 10  # 10% degradation
        },
        "alerting": {
            "channels": ["slack", "email"],
            "escalation_policy": "standard"
        }
    }
    
    # Setup monitoring
    monitoring_setup = await monitor.configure(monitoring_config)
    
    # Create dashboards
    dashboards = await monitor.create_dashboards({
        "overview": {
            "widgets": [
                "system_health",
                "performance_comparison",
                "error_trends",
                "business_metrics"
            ]
        },
        "detailed": {
            "widgets": [
                "api_performance",
                "database_metrics",
                "user_activity",
                "resource_utilization"
            ]
        }
    })
    
    print("\n✅ Monitoring Setup Complete!")
    print(f"  Metrics: {len(monitoring_setup['metrics'])}")
    print(f"  Alerts: {len(monitoring_setup['alert_rules'])}")
    print(f"  Dashboards: {len(dashboards)}")
    
    return monitoring_setup
```

## Workshop Conclusion

### Final Project: Complete Natural/Adabas Modernization

```python
# workshop/final_project.py
async def complete_natural_adabas_modernization():
    """Complete end-to-end Natural/Adabas modernization"""
    
    print("\n🚀 Natural/Adabas Modernization Journey")
    print("=" * 50)
    
    # Initialize
    orchestrator = await initialize_natural_workshop()
    
    # Phase 1: Analysis
    print("\n📊 Phase 1: System Analysis")
    natural_analysis = await analyze_natural_programs(orchestrator)
    adabas_analysis = await analyze_adabas_schemas(orchestrator)
    
    # Phase 2: Schema Modernization
    print("\n🗄️ Phase 2: Schema Modernization")
    modern_schema = await modernize_database_schema(
        orchestrator,
        adabas_analysis
    )
    
    # Phase 3: Data Migration
    print("\n💾 Phase 3: Data Migration")
    migration_results = await migrate_adabas_data(
        orchestrator,
        adabas_analysis,
        modern_schema
    )
    
    # Phase 4: Code Transformation
    print("\n🔄 Phase 4: Code Transformation")
    transformation_results = await transform_natural_programs(
        orchestrator,
        natural_analysis,
        modern_schema
    )
    
    # Phase 5: Testing
    print("\n🧪 Phase 5: Comprehensive Testing")
    test_results = await execute_all_tests(
        orchestrator,
        transformation_results,
        migration_results
    )
    
    # Phase 6: Deployment
    print("\n🚀 Phase 6: Production Deployment")
    deployment_results = await deploy_modernized_system(
        orchestrator,
        transformation_results,
        test_results
    )
    
    # Generate final report
    final_report = await generate_modernization_report({
        "analysis": {
            "natural": natural_analysis,
            "adabas": adabas_analysis
        },
        "migration": migration_results,
        "transformation": transformation_results,
        "testing": test_results,
        "deployment": deployment_results
    })
    
    print("\n✅ Modernization Complete!")
    print_summary(final_report)
    
    return final_report

def print_summary(report):
    """Print modernization summary"""
    print("\n📊 Modernization Summary:")
    print(f"  Duration: {report['total_duration']}")
    print(f"  Natural Programs: {report['programs_transformed']}")
    print(f"  DDMs Migrated: {report['ddms_migrated']}")
    print(f"  Data Records: {report['records_migrated']:,}")
    print(f"  Test Coverage: {report['test_coverage']}%")
    print(f"  Performance Delta: {report['performance_change']:+.1f}%")
```

## Key Takeaways

1. **AI-Powered Analysis** accelerates Natural/Adabas understanding
2. **Intelligent Schema Migration** handles complex Adabas structures
3. **Automated Data Migration** ensures data integrity
4. **Smart Code Transformation** preserves business logic
5. **Comprehensive Testing** validates functionality
6. **Agentic Deployment** ensures smooth production rollout

## Resources

- Workshop Repository: `github.com/your-org/natural-adabas-workshop`
- Natural/Adabas Modernization Guide: `docs.natural-modernization.io`
- Community Support: `community.mainframe-modernization.io`

Would you like me to continue with the CI/CD Modernization Journey document?