# AI-Powered Code Analysis

This chapter provides a detailed technical guide for conducting AI-powered analysis of IBM z/OS mainframe code as part of your modernization journey.

## Overview

Code analysis is a critical step in mainframe modernization, enabling teams to understand complex legacy applications, identify potential modernization issues, and make informed transformation decisions. Azure AI Foundry provides advanced AI capabilities specifically designed for mainframe code analysis, helping you uncover insights that would be difficult or time-consuming to discover manually.

## Objectives

- Implement automated analysis of mainframe code (COBOL, JCL, Assembler, and more)
- Extract business rules and logic from legacy applications
- Identify code complexity, quality issues, and modernization challenges
- Generate comprehensive documentation from legacy code
- Map data structures and relationships

## Analysis Capabilities

Azure AI Foundry provides the following code analysis capabilities:

### 1. Syntax and Structure Analysis

- Parsing and tokenization of mainframe languages
- Abstract Syntax Tree (AST) generation
- Control flow analysis
- Data flow tracking
- Dead code identification

### 2. Business Logic Extraction

- Business rule identification and extraction
- Domain-specific language pattern recognition
- Decision logic mapping
- Calculation and algorithm identification
- Business process modeling

### 3. Data Structure Analysis

- Data definition extraction
- File and database schema mapping
- Record layout analysis
- Data lineage tracking
- Data dependency identification

### 4. Dependency Mapping

- Program-to-program dependencies
- Program-to-data dependencies
- JCL job dependencies
- External system interface identification
- Component relationship visualization

### 5. Quality Assessment

- Complexity measurement
- Maintainability analysis
- Duplicated code detection
- Error handling examination
- Performance hotspot identification

## Setup and Configuration

### Prerequisites

Before using the AI-powered code analysis tools, ensure you have:

- Access to mainframe source code (COBOL, JCL, Copybooks, etc.)
- Azure AI Foundry environment configured
- Development environment setup (see [Development Environment Configuration](../04-development-environment/README.md))

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

## Running Code Analysis

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

## Analyzing Results

### Key Analysis Reports

The code analysis generates several reports:

1. **Executive Summary**: High-level overview of the codebase
2. **Business Rules Catalog**: Extracted business rules and logic
3. **Data Dictionary**: Compilation of data structures and relationships
4. **Quality Metrics Dashboard**: Code quality and complexity metrics
5. **Dependency Network**: Visualization of component relationships
6. **Modernization Recommendations**: Suggested approaches for transformation

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

## Practical Examples

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

## Integration with Dependency Mapping

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

## Troubleshooting

| Issue | Resolution |
|-------|------------|
| Parsing errors in COBOL code | Verify that the code follows standard COBOL syntax or specify compiler dialect with `--dialect` option |
| Missing copybook references | Ensure all copybooks are available and specify include directories with `--include-copybooks` |
| Incomplete business rule extraction | Adjust sensitivity with `--rule-sensitivity` parameter or annotate complex business rules in code comments |
| High resource usage during analysis | Analyze code in smaller batches or adjust `--memory-optimization` setting |
| Dependency mapping timeout | Increase timeout with `--timeout` option or reduce scope with `--dependency-depth` |

## Best Practices

1. **Start with Focused Analysis**: Begin by analyzing core business-critical programs
2. **Validate Extracted Rules**: Have domain experts review extracted business rules
3. **Prioritize Based on Metrics**: Use quality metrics to prioritize modernization efforts
4. **Document Insights**: Maintain documentation of insights gained from analysis
5. **Regular Analysis**: Run analysis regularly to track modernization progress
6. **Combine with Testing**: Use extracted business rules to generate test cases

## Next Steps

After completing code analysis:

1. Develop a [Modernization Strategy](../03-foundation/modernization-strategy.md) based on analysis insights
2. Set up [GitHub Integration](../06-github-integration/README.md) or [Azure DevOps Integration](../07-azure-devops-integration/README.md)
3. Begin [AI-Powered Transformation](../08-ai-transformation/README.md) of selected components
4. Implement [CI/CD Pipelines](../09-cicd-implementation/README.md) for automated build and deployment

## References

- [Dependency Mapping Guide](../02-discovery/dependency-mapping.md)
- [Risk Assessment Guide](../10-risk-management/README.md)
- [Hybrid Operations Management](../11-hybrid-operations/README.md) 