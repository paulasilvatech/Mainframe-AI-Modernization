# Appendix B: Workshop - Natural/Adabas Modernization Journey

## Workshop Overview

This hands-on workshop guides you through modernizing Natural/Adabas applications, with special focus on data migration challenges, MU/PE field handling, and insurance system use cases. You'll work with a realistic insurance policy management system.

### Duration
- **Total Time**: 2 days (16 hours)
- **Day 1**: Natural and Adabas Analysis (8 hours)
- **Day 2**: Migration Execution (8 hours)

### Prerequisites
- Understanding of Natural programming
- Basic knowledge of Adabas database concepts
- Azure subscription with AI services
- Docker Desktop installed
- Java/Spring Boot development environment
- Python 3.8+ installed

### Learning Objectives
By the end of this workshop, you will be able to:
- Analyze Natural programs and Adabas DDMs
- Handle complex MU (Multiple Value) and PE (Periodic Group) fields
- Transform Natural code to modern Java/Spring Boot
- Migrate Adabas data to relational databases
- Implement comprehensive testing strategies
- Deploy modernized insurance applications

### Insurance System Case Study

We'll work with a policy management system containing:
- Policy holder information
- Coverage details with multiple values
- Claims history (periodic groups)
- Premium calculations
- Document management

---

## Day 1: Natural and Adabas Analysis

### Module 1: Natural Program Analysis (3 hours)

#### 1.1 Environment Setup

```yaml
# docker-compose.yml
version: '3.8'

services:
  natural-analyzer:
    image: mainframe-modernization/natural-analyzer:latest
    ports:
      - "8081:8080"
    environment:
      - AZURE_AI_ENDPOINT=${AZURE_AI_ENDPOINT}
      - AZURE_AI_KEY=${AZURE_AI_KEY}
    volumes:
      - ./natural-code:/workspace/natural
      - ./ddms:/workspace/ddms
      
  adabas-emulator:
    image: mainframe-modernization/adabas-emulator:latest
    ports:
      - "30000:30000"
    volumes:
      - ./test-data:/data
      
  transformation-engine:
    image: mainframe-modernization/transformation-engine:latest
    depends_on:
      - natural-analyzer
    environment:
      - TARGET_PLATFORM=spring-boot
      - DATABASE_TARGET=postgresql

  postgres:
    image: postgres:14
    environment:
      - POSTGRES_DB=insurance
      - POSTGRES_USER=workshop
      - POSTGRES_PASSWORD=workshop123
    ports:
      - "5432:5432"
```

#### 1.2 Sample Natural Application

Insurance policy management system structure:

```
insurance-system/
├── natural/
│   ├── POLMAINT.NSP     # Policy maintenance
│   ├── CLMPROC.NSP      # Claims processing
│   ├── PREMCALC.NSP     # Premium calculation
│   └── RPTGEN.NSP       # Report generation
├── maps/
│   ├── POLMAP01.NSM     # Policy entry map
│   └── CLMMAP01.NSM     # Claims entry map
├── ddms/
│   ├── POLICY.NSD       # Policy master DDM
│   ├── POLICYHOLDER.NSD # Policy holder DDM
│   ├── COVERAGE.NSD     # Coverage DDM
│   └── CLAIMS.NSD       # Claims DDM
└── copycode/
    └── CALCPREM.NSC     # Premium calculation routines
```

Example Natural Program (POLMAINT.NSP):
```natural
DEFINE DATA
LOCAL USING POLICY
LOCAL USING POLICYHOLDER
LOCAL
1 #POLICY-NUMBER (A10)
1 #ACTION (A1)
1 #I (I2)
1 #J (I2)
1 #TOTAL-PREMIUM (P9.2)
1 #MSG (A60)
END-DEFINE
*
SET KEY PF3
*
REPEAT
  INPUT USING MAP 'POLMAP01'
  *
  DECIDE ON FIRST #ACTION
    VALUE 'A'  /* Add new policy
      PERFORM ADD-POLICY
    VALUE 'U'  /* Update policy
      PERFORM UPDATE-POLICY
    VALUE 'D'  /* Delete policy
      PERFORM DELETE-POLICY
    VALUE 'I'  /* Inquiry
      PERFORM INQUIRY-POLICY
    NONE
      REINPUT 'Invalid action. Enter A/U/D/I'
  END-DECIDE
  *
  IF *PF-KEY = 'PF3'
    ESCAPE BOTTOM
  END-IF
END-REPEAT
*
DEFINE SUBROUTINE ADD-POLICY
  RESET POLICY
  MOVE #POLICY-NUMBER TO POLICY-NUMBER
  *
  /* Handle multiple coverages (MU field)
  FOR #I = 1 TO 5
    IF COVERAGE-TYPE (#I) NE ' '
      ADD 1 TO C*COVERAGE-TYPE
      COMPUTE COVERAGE-PREMIUM (#I) = 
        COVERAGE-AMOUNT (#I) * 0.001
      ADD COVERAGE-PREMIUM (#I) TO #TOTAL-PREMIUM
    END-IF
  END-FOR
  *
  /* Handle claims history (PE group)
  FOR #J = 1 TO 10
    IF CLAIM-DATE (#J) NE 0
      ADD 1 TO C*CLAIM-DATE
    END-IF
  END-FOR
  *
  MOVE #TOTAL-PREMIUM TO ANNUAL-PREMIUM
  STORE POLICY
  MOVE 'Policy added successfully' TO #MSG
END-SUBROUTINE
*
DEFINE SUBROUTINE UPDATE-POLICY
  FIND POLICY WITH POLICY-NUMBER = #POLICY-NUMBER
    /* Update logic here
    UPDATE
  END-FIND
END-SUBROUTINE
*
END
```

#### 1.3 Natural Code Analysis Exercises

**Exercise 1: Analyze Natural Program Structure**

```python
# exercises/01_natural_analysis.py
import asyncio
from agents.natural_analyzer import NaturalAnalyzerAgent

async def analyze_natural_program(file_path):
    """Analyze Natural program structure and patterns"""
    
    analyzer = NaturalAnalyzerAgent()
    
    # Read Natural source
    with open(file_path, 'r') as f:
        natural_code = f.read()
    
    # Perform analysis
    analysis = await analyzer.analyze(natural_code)
    
    print("📊 Natural Program Analysis:")
    print(f"Program Type: {analysis['program_type']}")
    print(f"Data Areas: {len(analysis['data_areas'])}")
    print(f"Subroutines: {len(analysis['subroutines'])}")
    print(f"External Calls: {len(analysis['external_calls'])}")
    
    # Analyze MU/PE usage
    print("\n📋 MU/PE Field Analysis:")
    for field in analysis['mu_pe_fields']:
        print(f"  - {field['name']}: {field['type']} ({field['occurrences']})")
        print(f"    Usage: {field['usage_pattern']}")
    
    # Business logic extraction
    print("\n💼 Business Logic:")
    for rule in analysis['business_rules']:
        print(f"  - {rule['name']}: {rule['description']}")
        print(f"    Location: {rule['location']}")
    
    return analysis

# Run analysis
analysis = asyncio.run(
    analyze_natural_program("insurance-system/natural/POLMAINT.NSP")
)
```

**Exercise 2: Map Natural Maps**

```python
# exercises/02_map_analysis.py
async def analyze_natural_maps(map_file):
    """Analyze Natural maps for UI modernization"""
    
    map_analyzer = MapAnalyzerAgent()
    
    # Analyze map structure
    map_analysis = await map_analyzer.analyze(map_file)
    
    print("🖼️ Map Analysis:")
    print(f"Map Name: {map_analysis['name']}")
    print(f"Screen Size: {map_analysis['rows']} x {map_analysis['columns']}")
    print(f"Fields: {len(map_analysis['fields'])}")
    
    # Generate modern UI equivalent
    modern_ui = await map_analyzer.generate_modern_ui(map_analysis)
    
    print("\n🎨 Modern UI Generation:")
    print(f"Framework: {modern_ui['framework']}")
    print(f"Components: {len(modern_ui['components'])}")
    
    # Save generated UI code
    with open(f"{map_analysis['name']}_modern.tsx", 'w') as f:
        f.write(modern_ui['code'])
    
    return modern_ui
```

### Module 2: Data Model Transformation (3 hours)

#### 2.1 DDM Analysis and Schema Design

**Exercise 3: DDM to Modern Schema Conversion**

```python
# exercises/03_ddm_conversion.py
async def convert_ddm_to_schema(ddm_file):
    """Convert Adabas DDM to modern database schema"""
    
    ddm_converter = DDMConverterAgent()
    
    # Parse DDM
    ddm_structure = await ddm_converter.parse_ddm(ddm_file)
    
    print("📊 DDM Structure:")
    print(f"File: {ddm_structure['file_name']}")
    print(f"Fields: {len(ddm_structure['fields'])}")
    print(f"MU Fields: {len([f for f in ddm_structure['fields'] if f['is_mu']])}")
    print(f"PE Groups: {len(ddm_structure['pe_groups'])}")
    
    # Convert to relational schema
    schema = await ddm_converter.convert_to_relational(ddm_structure)
    
    print("\n🗄️ Relational Schema Design:")
    for table in schema['tables']:
        print(f"\nTable: {table['name']}")
        print("Columns:")
        for col in table['columns']:
            print(f"  - {col['name']} {col['type']} {col.get('constraints', '')}")
        
        if table.get('foreign_keys'):
            print("Foreign Keys:")
            for fk in table['foreign_keys']:
                print(f"  - {fk['column']} -> {fk['references']}")
    
    # Generate DDL
    ddl = await ddm_converter.generate_ddl(schema, dialect='postgresql')
    
    with open(f"{ddm_structure['file_name']}_schema.sql", 'w') as f:
        f.write(ddl)
    
    return schema
```

Example DDM (POLICY.NSD):
```
* POLICY - Policy Master File
*
T L DB Name                             F Leng  S D Remark
* -- -- -------------------------------- - ----  - - ------------------------
  1  AA POLICY-NUMBER                   A   10  D
  1  AB POLICY-HOLDER-ID                A    8    D
  1  AC POLICY-TYPE                     A    3
  1  AD EFFECTIVE-DATE                  P    7
  1  AE EXPIRY-DATE                     P    7
  1  AF ANNUAL-PREMIUM                  P    9.2
*
* Multiple-value field for coverages
M 1  BA COVERAGE-TYPE                   A    3
M 1  BB COVERAGE-AMOUNT                 P    9.2
M 1  BC COVERAGE-PREMIUM                P    7.2
*
* Periodic group for claims history
P 1  CA CLAIMS-GROUP
  2  CB CLAIM-DATE                     P    7
  2  CC CLAIM-AMOUNT                   P    9.2
  2  CD CLAIM-STATUS                   A    1
  2  CE CLAIM-DESCRIPTION              A   50
```

Generated PostgreSQL Schema:
```sql
-- Main policy table
CREATE TABLE policy (
    policy_id SERIAL PRIMARY KEY,
    policy_number VARCHAR(10) UNIQUE NOT NULL,
    policy_holder_id VARCHAR(8) NOT NULL,
    policy_type VARCHAR(3) NOT NULL,
    effective_date DATE NOT NULL,
    expiry_date DATE,
    annual_premium DECIMAL(11,2),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Coverage table (from MU fields)
CREATE TABLE policy_coverage (
    coverage_id SERIAL PRIMARY KEY,
    policy_id INTEGER NOT NULL,
    coverage_type VARCHAR(3) NOT NULL,
    coverage_amount DECIMAL(11,2),
    coverage_premium DECIMAL(9,2),
    sequence_number INTEGER NOT NULL,
    FOREIGN KEY (policy_id) REFERENCES policy(policy_id),
    UNIQUE(policy_id, sequence_number)
);

-- Claims table (from PE group)
CREATE TABLE policy_claims (
    claim_id SERIAL PRIMARY KEY,
    policy_id INTEGER NOT NULL,
    claim_date DATE NOT NULL,
    claim_amount DECIMAL(11,2),
    claim_status CHAR(1),
    claim_description VARCHAR(50),
    pe_occurrence INTEGER NOT NULL,
    FOREIGN KEY (policy_id) REFERENCES policy(policy_id),
    INDEX idx_claim_date (claim_date)
);
```

#### 2.2 MU/PE Field Handling Strategies

**Exercise 4: Normalization Strategies**

```python
# exercises/04_mu_pe_normalization.py
async def design_mu_pe_normalization(ddm_analysis):
    """Design normalization strategy for MU/PE fields"""
    
    normalizer = DataNormalizationAgent()
    
    # Analyze MU/PE patterns
    patterns = await normalizer.analyze_patterns(ddm_analysis)
    
    print("🔍 MU/PE Pattern Analysis:")
    for pattern in patterns:
        print(f"\nField: {pattern['field_name']}")
        print(f"Type: {pattern['type']} ({pattern['max_occurrences']} occurrences)")
        print(f"Usage Pattern: {pattern['usage_pattern']}")
        print(f"Recommended Strategy: {pattern['strategy']}")
    
    # Generate normalization plan
    normalization_plan = await normalizer.create_normalization_plan(patterns)
    
    print("\n📋 Normalization Plan:")
    for step in normalization_plan['steps']:
        print(f"\n{step['order']}. {step['description']}")
        print(f"   Source: {step['source_structure']}")
        print(f"   Target: {step['target_structure']}")
        print(f"   Transformation: {step['transformation_logic']}")
    
    return normalization_plan
```

**Exercise 5: Data Migration Planning**

```python
# exercises/05_migration_planning.py
async def plan_data_migration(schema, source_ddms):
    """Create comprehensive data migration plan"""
    
    migration_planner = MigrationPlannerAgent()
    
    # Analyze data volumes
    volumes = await migration_planner.estimate_volumes(source_ddms)
    
    print("📊 Data Volume Estimates:")
    for entity in volumes:
        print(f"  - {entity['name']}: {entity['record_count']:,} records")
        print(f"    Size: {entity['estimated_size_mb']:,} MB")
    
    # Create migration strategy
    strategy = await migration_planner.create_strategy({
        'schema': schema,
        'volumes': volumes,
        'constraints': {
            'downtime_window': '4 hours',
            'parallel_streams': 4,
            'validation_level': 'comprehensive'
        }
    })
    
    print("\n🎯 Migration Strategy:")
    print(f"Approach: {strategy['approach']}")
    print(f"Phases: {len(strategy['phases'])}")
    print(f"Estimated Duration: {strategy['estimated_duration']}")
    
    # Generate migration scripts
    scripts = await migration_planner.generate_scripts(strategy)
    
    for script_name, content in scripts.items():
        with open(f"migration/{script_name}", 'w') as f:
            f.write(content)
    
    return strategy
```

### Module 3: Migration Preparation (2 hours)

#### 3.1 Tool Setup and Configuration

**Exercise 6: Migration Tool Setup**

```python
# exercises/06_tool_setup.py
async def setup_migration_tools():
    """Set up and configure migration tools"""
    
    tool_configurator = ToolConfiguratorAgent()
    
    # Configure extraction tools
    extraction_config = await tool_configurator.configure_extraction({
        'source_type': 'adabas',
        'connection': {
            'host': 'localhost',
            'port': 30000,
            'database': 1,
            'file_numbers': [10, 11, 12, 13]  # POLICY, POLICYHOLDER, COVERAGE, CLAIMS
        },
        'extraction_mode': 'incremental',
        'format': 'csv'
    })
    
    # Configure transformation tools
    transformation_config = await tool_configurator.configure_transformation({
        'mappings': 'schema_mappings.json',
        'validation_rules': 'validation_rules.yaml',
        'error_handling': 'quarantine',
        'batch_size': 10000
    })
    
    # Configure loading tools
    loading_config = await tool_configurator.configure_loading({
        'target_type': 'postgresql',
        'connection': {
            'host': 'localhost',
            'port': 5432,
            'database': 'insurance',
            'schema': 'modernized'
        },
        'load_strategy': 'bulk',
        'commit_interval': 5000
    })
    
    print("✅ Migration tools configured successfully")
    
    return {
        'extraction': extraction_config,
        'transformation': transformation_config,
        'loading': loading_config
    }
```

#### 3.2 Migration Scripts Generation

**Exercise 7: Generate Migration Scripts**

```python
# exercises/07_migration_scripts.py
async def generate_migration_scripts(migration_config):
    """Generate all migration scripts"""
    
    script_generator = MigrationScriptGenerator()
    
    # Generate extraction scripts
    extraction_scripts = await script_generator.generate_extraction_scripts({
        'source_files': ['POLICY', 'COVERAGE', 'CLAIMS'],
        'selection_criteria': {
            'POLICY': "POLICY-TYPE IN ('AUTO', 'HOME', 'LIFE')",
            'active_only': True
        },
        'mu_pe_handling': {
            'strategy': 'flatten_with_occurrence',
            'null_handling': 'preserve'
        }
    })
    
    # Generate transformation scripts
    transformation_scripts = await script_generator.generate_transformation_scripts({
        'mappings': {
            'POLICY': {
                'target_table': 'policy',
                'field_mappings': {
                    'POLICY-NUMBER': 'policy_number',
                    'EFFECTIVE-DATE': {
                        'target': 'effective_date',
                        'transformation': 'packed_to_date'
                    }
                }
            },
            'COVERAGE': {
                'target_table': 'policy_coverage',
                'type': 'multiple_value',
                'parent_key': 'POLICY-NUMBER',
                'occurrence_handling': 'separate_rows'
            }
        }
    })
    
    # Generate validation scripts
    validation_scripts = await script_generator.generate_validation_scripts({
        'validation_types': [
            'record_counts',
            'sum_totals',
            'key_integrity',
            'business_rules'
        ],
        'sampling_rate': 0.1  # Validate 10% sample
    })
    
    # Save all scripts
    script_dir = "migration/scripts"
    os.makedirs(script_dir, exist_ok=True)
    
    all_scripts = {
        **extraction_scripts,
        **transformation_scripts,
        **validation_scripts
    }
    
    for name, content in all_scripts.items():
        with open(f"{script_dir}/{name}", 'w') as f:
            f.write(content)
        print(f"📄 Generated: {name}")
    
    return all_scripts
```

#### 3.3 Test Data Generation

**Exercise 8: Generate Test Data**

```python
# exercises/08_test_data_generation.py
async def generate_test_data(schema, sample_size=1000):
    """Generate test data for validation"""
    
    test_data_generator = TestDataGeneratorAgent()
    
    # Generate test policies
    test_policies = await test_data_generator.generate_policies({
        'count': sample_size,
        'policy_types': ['AUTO', 'HOME', 'LIFE'],
        'coverage_distribution': {
            'min': 1,
            'max': 5,
            'average': 2.5
        },
        'claims_distribution': {
            'percentage_with_claims': 0.3,
            'min_claims': 1,
            'max_claims': 3
        }
    })
    
    # Generate Natural format test data
    natural_format = await test_data_generator.convert_to_natural_format(
        test_policies,
        ddm_structure
    )
    
    # Generate SQL insert statements
    sql_inserts = await test_data_generator.generate_sql_inserts(
        test_policies,
        schema
    )
    
    # Save test data
    with open("test_data/policies_natural.dat", 'w') as f:
        f.write(natural_format)
    
    with open("test_data/policies_sql.sql", 'w') as f:
        f.write(sql_inserts)
    
    print(f"✅ Generated {sample_size} test records")
    
    return test_policies
```

---

## Day 2: Migration Execution

### Module 4: Natural to Java Transformation (4 hours)

#### 4.1 Natural Program Transformation

**Exercise 9: Map Natural Programs**

```python
# exercises/09_natural_mapping.py
async def map_natural_to_java(natural_program):
    """Map Natural program structure to Java"""
    
    natural_mapper = NaturalToJavaMapper()
    
    # Analyze Natural program
    analysis = await natural_mapper.analyze_natural(natural_program)
    
    # Create Java class structure
    java_structure = await natural_mapper.create_java_structure({
        'package': 'com.insurance.policy',
        'framework': 'spring-boot',
        'data_access': 'jpa',
        'natural_analysis': analysis
    })
    
    print("📦 Java Package Structure:")
    for component in java_structure['components']:
        print(f"  - {component['type']}: {component['class_name']}")
        print(f"    Package: {component['package']}")
        print(f"    Dependencies: {', '.join(component['dependencies'])}")
    
    return java_structure
```

**Exercise 10: Transform Business Logic**

```python
# exercises/10_transform_logic.py
async def transform_natural_to_java(natural_code, java_structure):
    """Transform Natural business logic to Java"""
    
    transformer = NaturalToJavaTransformer()
    
    # Transform each subroutine
    transformations = []
    for subroutine in natural_code['subroutines']:
        java_method = await transformer.transform_subroutine({
            'natural_code': subroutine,
            'target_class': java_structure['service_class'],
            'preserve_logic': True,
            'add_logging': True,
            'add_validation': True
        })
        transformations.append(java_method)
    
    # Generate complete Java classes
    java_classes = await transformer.generate_classes({
        'structure': java_structure,
        'methods': transformations,
        'annotations': ['@Service', '@Transactional'],
        'error_handling': 'spring_exception_handler'
    })
    
    # Save Java files
    for class_info in java_classes:
        file_path = f"src/main/java/{class_info['package_path']}/{class_info['class_name']}.java"
        os.makedirs(os.path.dirname(file_path), exist_ok=True)
        with open(file_path, 'w') as f:
            f.write(class_info['content'])
        print(f"✅ Generated: {class_info['class_name']}.java")
    
    return java_classes
```

**Generated Java Example (PolicyService.java):**
```java
package com.insurance.policy.service;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import java.math.BigDecimal;
import java.util.List;

/**
 * Policy Management Service
 * Modernized from Natural program POLMAINT
 */
@Service
@Transactional
@RequiredArgsConstructor
@Slf4j
public class PolicyService {
    
    private final PolicyRepository policyRepository;
    private final CoverageRepository coverageRepository;
    private final PremiumCalculator premiumCalculator;
    
    /**
     * Add new policy with coverages
     * Modernized from ADD-POLICY subroutine
     */
    public PolicyDto addPolicy(PolicyCreateRequest request) {
        log.info("Adding new policy: {}", request.getPolicyNumber());
        
        // Create policy entity
        Policy policy = Policy.builder()
            .policyNumber(request.getPolicyNumber())
            .policyHolderId(request.getPolicyHolderId())
            .policyType(request.getPolicyType())
            .effectiveDate(request.getEffectiveDate())
            .build();
        
        // Handle multiple coverages (from MU field)
        BigDecimal totalPremium = BigDecimal.ZERO;
        List<Coverage> coverages = request.getCoverages().stream()
            .filter(cov -> cov.getCoverageType() != null)
            .map(cov -> {
                // Calculate coverage premium
                BigDecimal premium = premiumCalculator.calculate(
                    cov.getCoverageType(),
                    cov.getCoverageAmount()
                );
                
                return Coverage.builder()
                    .policy(policy)
                    .coverageType(cov.getCoverageType())
                    .coverageAmount(cov.getCoverageAmount())
                    .coveragePremium(premium)
                    .build();
            })
            .collect(Collectors.toList());
        
        // Calculate total premium
        totalPremium = coverages.stream()
            .map(Coverage::getCoveragePremium)
            .reduce(BigDecimal.ZERO, BigDecimal::add);
        
        policy.setAnnualPremium(totalPremium);
        policy.setCoverages(coverages);
        
        // Save policy with coverages
        Policy savedPolicy = policyRepository.save(policy);
        
        log.info("Policy {} added successfully with {} coverages", 
                 savedPolicy.getPolicyNumber(), 
                 coverages.size());
        
        return mapToDto(savedPolicy);
    }
    
    /**
     * Update existing policy
     * Modernized from UPDATE-POLICY subroutine
     */
    public PolicyDto updatePolicy(String policyNumber, PolicyUpdateRequest request) {
        Policy policy = policyRepository.findByPolicyNumber(policyNumber)
            .orElseThrow(() -> new PolicyNotFoundException(policyNumber));
        
        // Update logic here
        // ...
        
        return mapToDto(policyRepository.save(policy));
    }
}
```

**Exercise 11: Generate Spring Boot Application**

```python
# exercises/11_generate_springboot.py
async def generate_spring_boot_app(java_classes):
    """Generate complete Spring Boot application"""
    
    spring_generator = SpringBootGenerator()
    
    # Generate application structure
    app_structure = await spring_generator.create_application({
        'group_id': 'com.insurance',
        'artifact_id': 'policy-management',
        'java_version': '17',
        'spring_boot_version': '3.1.0',
        'dependencies': [
            'spring-boot-starter-web',
            'spring-boot-starter-data-jpa',
            'spring-boot-starter-validation',
            'postgresql',
            'lombok',
            'mapstruct'
        ]
    })
    
    # Generate entities
    entities = await spring_generator.generate_entities(schema)
    
    # Generate repositories
    repositories = await spring_generator.generate_repositories(entities)
    
    # Generate REST controllers
    controllers = await spring_generator.generate_controllers(java_classes)
    
    # Generate configuration
    config_files = await spring_generator.generate_configuration({
        'database': {
            'type': 'postgresql',
            'connection': database_config
        },
        'logging': {
            'level': 'INFO',
            'pattern': 'modernized'
        },
        'metrics': {
            'enabled': True,
            'export': 'prometheus'
        }
    })
    
    # Generate tests
    test_classes = await spring_generator.generate_tests({
        'services': java_classes,
        'coverage_target': 80,
        'test_types': ['unit', 'integration']
    })
    
    print("✅ Spring Boot application generated successfully")
    
    return app_structure
```

### Module 5: Data Migration Execution (2 hours)

#### 5.1 Execute Migration

**Exercise 12: Run Data Migration**

```python
# exercises/12_execute_migration.py
async def execute_data_migration(migration_plan):
    """Execute the data migration"""
    
    migration_executor = DataMigrationExecutor()
    
    print("🚀 Starting Data Migration")
    
    # Phase 1: Extract data from Adabas
    extraction_result = await migration_executor.extract_data({
        'plan': migration_plan['extraction'],
        'parallel_streams': 4,
        'checkpoint_interval': 10000
    })
    
    print(f"✅ Extracted {extraction_result['total_records']:,} records")
    
    # Phase 2: Transform data
    transformation_result = await migration_executor.transform_data({
        'source_data': extraction_result['output_files'],
        'mappings': migration_plan['transformation'],
        'validation': 'strict',
        'error_threshold': 0.01  # 1% error tolerance
    })
    
    print(f"✅ Transformed {transformation_result['successful_records']:,} records")
    print(f"⚠️  Errors: {transformation_result['error_count']}")
    
    # Phase 3: Load data
    load_result = await migration_executor.load_data({
        'transformed_data': transformation_result['output_files'],
        'target_config': migration_plan['loading'],
        'load_strategy': 'bulk',
        'validation': 'post_load'
    })
    
    print(f"✅ Loaded {load_result['loaded_records']:,} records")
    
    # Phase 4: Validate migration
    validation_result = await migration_executor.validate_migration({
        'source_counts': extraction_result['counts'],
        'target_counts': load_result['counts'],
        'validation_queries': migration_plan['validation'],
        'sample_size': 0.05  # Validate 5% sample
    })
    
    print("\n📊 Migration Validation:")
    for check in validation_result['checks']:
        status = "✅" if check['passed'] else "❌"
        print(f"  {status} {check['name']}: {check['result']}")
    
    return {
        'extraction': extraction_result,
        'transformation': transformation_result,
        'loading': load_result,
        'validation': validation_result
    }
```

#### 5.2 Migration Monitoring

**Exercise 13: Monitor Migration Progress**

```python
# exercises/13_migration_monitoring.py
async def monitor_migration_progress(migration_job):
    """Monitor migration progress in real-time"""
    
    monitor = MigrationMonitor()
    
    # Set up monitoring dashboard
    dashboard = await monitor.create_dashboard({
        'job_id': migration_job['id'],
        'metrics': [
            'records_processed',
            'throughput',
            'error_rate',
            'eta'
        ],
        'refresh_interval': 5  # seconds
    })
    
    print(f"📊 Monitoring Dashboard: {dashboard['url']}")
    
    # Monitor until completion
    while not migration_job['completed']:
        status = await monitor.get_status(migration_job['id'])
        
        print(f"\r⏳ Progress: {status['percentage']:.1f}% "
              f"({status['records_processed']:,}/{status['total_records']:,}) "
              f"Throughput: {status['records_per_second']:,} rec/s "
              f"ETA: {status['eta']}", end='')
        
        if status['errors'] > 0:
            print(f"\n⚠️  Errors detected: {status['errors']}")
            
        await asyncio.sleep(5)
    
    print("\n✅ Migration completed!")
    
    # Generate final report
    report = await monitor.generate_report(migration_job['id'])
    
    with open("migration_report.html", 'w') as f:
        f.write(report['html'])
    
    return report
```

### Module 6: Testing and Deployment (2 hours)

#### 6.1 Integration Testing

**Exercise 14: Integration Testing**

```python
# exercises/14_integration_testing.py
async def run_integration_tests(modernized_app):
    """Run comprehensive integration tests"""
    
    test_runner = IntegrationTestRunner()
    
    # Test data integrity
    data_tests = await test_runner.test_data_integrity({
        'original_system': 'adabas://localhost:30000',
        'modernized_system': 'postgresql://localhost:5432/insurance',
        'test_scenarios': [
            'policy_creation',
            'coverage_modification',
            'premium_calculation',
            'claims_processing'
        ]
    })
    
    print("🧪 Data Integrity Tests:")
    for test in data_tests['results']:
        status = "✅" if test['passed'] else "❌"
        print(f"  {status} {test['name']}: {test['message']}")
    
    # Test business logic
    logic_tests = await test_runner.test_business_logic({
        'test_cases': load_business_test_cases(),
        'compare_with_original': True,
        'tolerance': 0.001  # For floating point comparisons
    })
    
    print("\n🧪 Business Logic Tests:")
    for test in logic_tests['results']:
        status = "✅" if test['passed'] else "❌"
        print(f"  {status} {test['name']}")
        if not test['passed']:
            print(f"     Expected: {test['expected']}")
            print(f"     Actual: {test['actual']}")
    
    # Performance testing
    perf_tests = await test_runner.test_performance({
        'scenarios': [
            {
                'name': 'policy_search',
                'load': 100,  # requests per second
                'duration': 60  # seconds
            },
            {
                'name': 'premium_calculation',
                'load': 50,
                'duration': 60
            }
        ],
        'sla': {
            'response_time_p95': 200,  # milliseconds
            'error_rate': 0.01  # 1%
        }
    })
    
    print("\n⚡ Performance Test Results:")
    for scenario in perf_tests['scenarios']:
        print(f"\n{scenario['name']}:")
        print(f"  Response Time (P95): {scenario['p95_response_time']}ms")
        print(f"  Throughput: {scenario['throughput']} req/s")
        print(f"  Error Rate: {scenario['error_rate']:.2%}")
    
    return {
        'data_integrity': data_tests,
        'business_logic': logic_tests,
        'performance': perf_tests
    }
```

#### 6.2 Performance Validation

**Exercise 15: Performance Comparison**

```python
# exercises/15_performance_validation.py
async def validate_performance(original_system, modernized_system):
    """Compare performance between original and modernized systems"""
    
    performance_validator = PerformanceValidator()
    
    # Define test workload
    workload = {
        'scenarios': [
            {
                'name': 'policy_inquiry',
                'operations': ['search', 'retrieve', 'display'],
                'data_volume': 1000
            },
            {
                'name': 'batch_premium_calculation',
                'operations': ['calculate', 'update', 'report'],
                'data_volume': 10000
            }
        ]
    }
    
    # Run on original system
    print("📊 Testing Original System...")
    original_results = await performance_validator.benchmark(
        original_system,
        workload
    )
    
    # Run on modernized system
    print("📊 Testing Modernized System...")
    modern_results = await performance_validator.benchmark(
        modernized_system,
        workload
    )
    
    # Compare results
    comparison = await performance_validator.compare(
        original_results,
        modern_results
    )
    
    print("\n📈 Performance Comparison:")
    for scenario in comparison['scenarios']:
        print(f"\n{scenario['name']}:")
        print(f"  Original: {scenario['original_time']:.2f}s")
        print(f"  Modern: {scenario['modern_time']:.2f}s")
        print(f"  Improvement: {scenario['improvement']:.1f}%")
        
        if scenario['improvement'] < 0:
            print(f"  ⚠️  Performance degradation detected!")
    
    return comparison
```

#### 6.3 Phased Deployment

**Exercise 16: Execute Phased Deployment**

```python
# exercises/16_phased_deployment.py
async def execute_phased_deployment(deployment_plan):
    """Execute phased deployment strategy"""
    
    deployment_executor = PhasedDeploymentExecutor()
    
    print("🚀 Starting Phased Deployment")
    
    for phase in deployment_plan['phases']:
        print(f"\n📋 Phase {phase['number']}: {phase['name']}")
        print(f"   Scope: {phase['scope']}")
        print(f"   Duration: {phase['duration']}")
        
        # Deploy phase
        phase_result = await deployment_executor.deploy_phase({
            'phase': phase,
            'validation': 'comprehensive',
            'rollback_enabled': True,
            'monitoring': {
                'alerts': ['error_rate', 'response_time'],
                'threshold': phase['success_criteria']
            }
        })
        
        # Validate phase
        validation = await deployment_executor.validate_phase(phase_result)
        
        if validation['passed']:
            print(f"   ✅ Phase {phase['number']} completed successfully")
        else:
            print(f"   ❌ Phase {phase['number']} failed validation")
            
            # Execute rollback if needed
            if phase['rollback_on_failure']:
                rollback = await deployment_executor.rollback_phase(phase)
                print(f"   🔄 Rollback {'successful' if rollback['success'] else 'failed'}")
                break
        
        # Wait before next phase
        if phase.get('stabilization_period'):
            print(f"   ⏳ Stabilization period: {phase['stabilization_period']} hours")
            await asyncio.sleep(phase['stabilization_period'] * 3600)
    
    return deployment_executor.get_deployment_summary()
```

## Workshop Summary

### Key Achievements

1. **Natural Analysis**: Successfully analyzed Natural programs with MU/PE fields
2. **DDM Transformation**: Converted Adabas DDMs to modern relational schemas
3. **Code Modernization**: Transformed Natural to Spring Boot applications
4. **Data Migration**: Migrated complex hierarchical data to PostgreSQL
5. **Testing Strategy**: Implemented comprehensive testing approach
6. **Phased Deployment**: Successfully deployed using risk-managed approach

### Special Considerations for Natural/Adabas

1. **MU/PE Field Handling**:
   - Normalize to separate tables for relational databases
   - Maintain occurrence tracking for data integrity
   - Consider JSON/JSONB for variable structures

2. **Performance Optimization**:
   - Index strategies for normalized data
   - Query optimization for joined tables
   - Caching strategies for frequently accessed data

3. **Data Integrity**:
   - Maintain referential integrity across normalized tables
   - Implement validation for business rules
   - Ensure transaction consistency

### Next Steps

1. Apply these patterns to your Natural/Adabas applications
2. Explore advanced MCP patterns in Chapter 15
3. Implement monitoring strategies from Chapter 16
4. Share your migration experiences with the community

### Resources

- [Natural/Adabas Migration Toolkit](https://github.com/mainframe-modernization/natural-toolkit)
- [DDM Converter Library](https://github.com/mainframe-modernization/ddm-converter)
- [Spring Boot Templates](https://github.com/mainframe-modernization/spring-templates)
- [Migration Best Practices](../best-practices/natural-adabas.md)

## Troubleshooting Guide

### Common Natural/Adabas Issues

| Issue | Solution |
|-------|----------|
| MU field overflow | Implement dynamic table allocation in target schema |
| PE group complexity | Use JSON columns for complex nested structures |
| Character encoding | Configure proper EBCDIC to UTF-8 conversion |
| Packed decimal precision | Use DECIMAL type with sufficient precision |
| Date format conversion | Implement custom date converters for Natural formats |
| Performance degradation | Analyze and optimize join strategies |

### Debug Tips

```bash
# Monitor Adabas emulator
docker logs -f adabas-emulator

# Check Natural analyzer output
curl http://localhost:8081/api/analysis/status

# Validate DDM conversion
python -m validators.ddm_validator check --file POLICY.NSD

# Test data migration
psql -U workshop -d insurance -c "SELECT COUNT(*) FROM policy;"
``` 