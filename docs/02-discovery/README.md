# Discovery and Assessment Phase

## Overview

This chapter guides you through the discovery and assessment phase of your mainframe modernization journey using Azure AI Foundry. It covers the process of analyzing your mainframe environment, understanding application complexities, and developing a data-driven modernization strategy.

## Prerequisites

- Completed [Introduction to Azure AI Foundry](../01-introduction/README.md)
- Azure subscription with Azure AI Foundry enabled
- Access to mainframe system documentation and code repositories
- Appropriate permissions to analyze mainframe applications

## Step 1: Setting Up Azure AI Foundry for Mainframe Analysis

Before you can begin analyzing your mainframe environment, you need to set up Azure AI Foundry and configure it for mainframe code analysis.

### Creating an Azure AI Foundry Resource

1. Log in to the Azure Portal and navigate to the Azure AI Foundry service
2. Create a new Azure AI Foundry resource:

```bash
# Create an Azure AI Foundry resource using Azure CLI
az group create --name mainframe-modernization-rg --location eastus2

az aifoundry create \
  --name mainframe-foundry \
  --resource-group mainframe-modernization-rg \
  --location eastus2 \
  --sku standard
```

3. Configure the Azure AI Foundry resource for COBOL analysis:

```bash
# Configure Azure AI Foundry for COBOL analysis
az aifoundry cobol-config create \
  --name mainframe-foundry \
  --resource-group mainframe-modernization-rg \
  --dialect "IBM-Enterprise-COBOL" \
  --copybook-path "code/mainframe/copybooks" \
  --enable-business-rule-extraction true
```

### Key Considerations

- Choose the appropriate COBOL dialect based on your mainframe environment
- Ensure that Azure AI Foundry has access to your copybooks, which may require additional configuration
- Consider the scale and complexity of your mainframe applications when selecting the service tier

## Step 2: Inventory Collection and Initial Analysis

The next step is to collect a comprehensive inventory of your mainframe applications and perform an initial analysis to understand their structure and complexity.

### Collecting Mainframe Application Inventory

1. Identify all mainframe applications in scope for modernization
2. Document key information for each application:

| Application Attribute | Description | Example |
|----------------------|-------------|---------|
| Application Name | Business name of the application | Customer Management System |
| Application Code | System code or identifier | CUSTMGMT |
| Business Function | Primary business function | Customer account management |
| Criticality | Business criticality level | High/Medium/Low |
| Mainframe Components | Program types involved | COBOL programs, JCL, Copybooks |
| Database Technologies | Database systems used | DB2, VSAM, IMS |
| Transaction Processing | TP monitors used | CICS, IMS-TM |
| Integration Points | Interfaces with other systems | Batch file transfers, MQ, Web Services |

3. Upload the application inventory to Azure AI Foundry:

```bash
# Upload application inventory to Azure AI Foundry
az aifoundry inventory upload \
  --name mainframe-foundry \
  --resource-group mainframe-modernization-rg \
  --inventory-file application-inventory.csv \
  --format csv
```

### Uploading Source Code for Analysis

1. Prepare mainframe code for analysis by organizing it into appropriate directories:

```bash
# Example directory structure
mkdir -p code/mainframe/cobol code/mainframe/jcl code/mainframe/copybooks
```

2. Upload the source code to Azure AI Foundry for analysis:

```bash
# Upload source code to Azure AI Foundry
az aifoundry code upload \
  --name mainframe-foundry \
  --resource-group mainframe-modernization-rg \
  --source-dir code/mainframe \
  --recursive true
```

### Key Considerations

- Ensure all code components are included, especially copybooks and include files
- Maintain the original directory structure to preserve relationships between components
- Consider data sensitivity and apply appropriate data governance controls

## Step 3: AI-Powered Code Analysis

Azure AI Foundry uses advanced AI models to analyze your mainframe code and provide insights into its structure, complexity, and business rules.

### Running the Analysis

1. Initiate the code analysis process:

```bash
# Start the code analysis process
az aifoundry analysis start \
  --name mainframe-foundry \
  --resource-group mainframe-modernization-rg \
  --analysis-name "Initial-Analysis" \
  --analysis-type "comprehensive" \
  --include-business-rules true \
  --include-dependencies true
```

2. Monitor the analysis progress:

```bash
# Check the status of the analysis
az aifoundry analysis status \
  --name mainframe-foundry \
  --resource-group mainframe-modernization-rg \
  --analysis-name "Initial-Analysis"
```

### Understanding Analysis Results

Once the analysis is complete, Azure AI Foundry provides several insights:

1. **Code Structure Analysis**: Breakdown of programs, procedures, and components
2. **Complexity Metrics**: Cyclomatic complexity, lines of code, and maintainability index
3. **Business Rule Extraction**: Identification of business rules embedded in the code
4. **Dependency Mapping**: Visualization of relationships between components
5. **Data Flow Analysis**: Tracking of data transformations through the system

```bash
# View the analysis results
az aifoundry analysis results \
  --name mainframe-foundry \
  --resource-group mainframe-modernization-rg \
  --analysis-name "Initial-Analysis" \
  --output-file analysis-results.json
```

### Key Considerations

- The initial analysis may take several hours for large codebases
- Results may not capture 100% of business rules or dependencies
- Manual validation and refinement of the results may be necessary

## Step 4: Business Rule Extraction

Azure AI Foundry's business rule extraction capability identifies and documents business logic embedded in your mainframe code.

### Extracting Business Rules

1. Review the business rules identified during the code analysis:

```bash
# Extract business rules to a separate file
az aifoundry rules extract \
  --name mainframe-foundry \
  --resource-group mainframe-modernization-rg \
  --analysis-name "Initial-Analysis" \
  --output-format markdown \
  --output-file business-rules.md
```

2. Business rules are categorized by type and complexity:

| Rule Category | Description | Example |
|---------------|-------------|---------|
| Calculation Rules | Mathematical or financial calculations | Interest calculation for different account types |
| Validation Rules | Data validation and verification | Customer ID format validation |
| Process Flow Rules | Business process control logic | Order processing workflow |
| Decision Rules | Business decision criteria | Credit approval criteria |

3. Validate and refine the extracted business rules:

```bash
# Update business rule documentation
az aifoundry rules update \
  --name mainframe-foundry \
  --resource-group mainframe-modernization-rg \
  --rules-file updated-business-rules.md
```

### Key Considerations

- Business rule extraction is not 100% accurate and requires human validation
- Some complex or implicit rules may require additional analysis
- Consider involving business domain experts in the validation process

## Step 5: Dependency Mapping

Understanding dependencies between different components of your mainframe environment is crucial for planning your modernization approach.

### Analyzing Dependencies

1. Generate a dependency map from the analysis results:

```bash
# Generate dependency map
az aifoundry dependency-map generate \
  --name mainframe-foundry \
  --resource-group mainframe-modernization-rg \
  --analysis-name "Initial-Analysis" \
  --output-format graphml \
  --output-file dependency-map.graphml
```

2. The dependency map includes various types of dependencies:

| Dependency Type | Description |
|-----------------|-------------|
| Program Call | Direct call between programs |
| Copybook Include | Program including a copybook |
| Data Access | Program accessing a database or file |
| Job Flow | Sequential relationship between jobs |
| Integration | Interface with external systems |

3. Visualize the dependency map using the Azure AI Foundry portal or export it to external tools.

### Identifying Critical Paths

1. Use the dependency map to identify critical paths in your mainframe applications:

```bash
# Identify critical paths
az aifoundry critical-path analyze \
  --name mainframe-foundry \
  --resource-group mainframe-modernization-rg \
  --dependency-map dependency-map.graphml \
  --output-file critical-paths.json
```

2. Review the critical paths to understand key dependencies and potential modernization risks.

### Key Considerations

- Focus on high-level dependencies first, then drill down into detailed component-level dependencies
- Consider both technical and business dependencies
- Use dependency information to inform your modernization strategy

## Step 6: Modernization Readiness Assessment

Based on the analysis results, Azure AI Foundry can help you assess the readiness of your mainframe applications for modernization.

### Generating Readiness Scores

1. Run the modernization readiness assessment:

```bash
# Generate modernization readiness assessment
az aifoundry readiness assess \
  --name mainframe-foundry \
  --resource-group mainframe-modernization-rg \
  --analysis-name "Initial-Analysis" \
  --output-file readiness-assessment.json
```

2. The readiness assessment includes scores for various aspects:

| Readiness Aspect | Description | Scoring |
|------------------|-------------|---------|
| Technical Complexity | Complexity of the code and architecture | 1-5 (1=simple, 5=complex) |
| Documentation Quality | Quality and completeness of documentation | 1-5 (1=poor, 5=excellent) |
| Testing Coverage | Availability and quality of test cases | 1-5 (1=none, 5=comprehensive) |
| Business Criticality | Importance to business operations | 1-5 (1=low, 5=high) |
| Modernization Benefit | Potential benefit from modernization | 1-5 (1=low, 5=high) |

3. Use the readiness scores to prioritize applications for modernization:

```bash
# Generate prioritization recommendations
az aifoundry prioritize \
  --name mainframe-foundry \
  --resource-group mainframe-modernization-rg \
  --readiness-assessment readiness-assessment.json \
  --output-file modernization-priorities.json
```

### Key Considerations

- Balance technical considerations with business priorities
- Consider application interdependencies when planning the modernization sequence
- Plan for a phased approach that delivers incremental value

## Step 7: Developing a Modernization Strategy

Based on the assessment results, develop a comprehensive modernization strategy tailored to your organization's needs.

### Modernization Approaches

Azure AI Foundry can recommend appropriate modernization approaches for each application:

| Approach | Description | Best For |
|----------|-------------|----------|
| Rehost | Migrate as-is to cloud infrastructure | Low complexity, low business criticality |
| Refactor | Restructure and optimize code without changing functionality | Medium complexity, medium business value |
| Rearchitect | Significantly alter the application architecture | High complexity, high business value |
| Rebuild | Completely rewrite the application | High complexity, high modernization benefit |
| Replace | Replace with commercial off-the-shelf solutions | Low unique business value |

### Generating a Modernization Roadmap

1. Use Azure AI Foundry to generate a modernization roadmap:

```bash
# Generate modernization roadmap
az aifoundry roadmap generate \
  --name mainframe-foundry \
  --resource-group mainframe-modernization-rg \
  --prioritization modernization-priorities.json \
  --timeline "18 months" \
  --output-file modernization-roadmap.json
```

2. Review and refine the roadmap based on business priorities and constraints.

### Key Considerations

- Align the modernization strategy with overall business objectives
- Consider resource constraints and technical dependencies
- Plan for a phased approach with clear milestones and success criteria

## Validation

To verify that the discovery and assessment phase has been completed successfully:

1. Check that all analysis tasks have completed:

```bash
# Verify all analysis tasks are complete
az aifoundry analysis list \
  --name mainframe-foundry \
  --resource-group mainframe-modernization-rg \
  --query "[?status=='Completed'].name" -o tsv
```

2. Ensure that key deliverables are available:
   - Comprehensive application inventory
   - Code analysis results
   - Business rule documentation
   - Dependency maps
   - Modernization readiness assessment
   - Prioritized modernization roadmap

## Troubleshooting

| Issue | Cause | Solution |
|-------|-------|----------|
| Analysis fails with timeout | Large codebase exceeding time limits | Split the analysis into smaller batches by application or component |
| Incomplete business rule extraction | Complex or uncommon code patterns | Use the Azure AI Foundry portal to manually refine business rule extraction |
| Missing dependencies | Incomplete code upload or external dependencies | Ensure all code components are uploaded and document external dependencies manually |
| Low confidence in analysis results | Unusual coding patterns or custom macros | Provide additional context information and conduct manual validation |

## Next Steps

Now that you've completed the discovery and assessment phase, you can proceed to:

1. [Foundation Setup](../03-foundation/README.md): Set up the infrastructure for your modernization journey
2. [GitHub Integration for Mainframe Code](../04-github-integration/README.md): Configure GitHub for mainframe code management

## Additional Resources

- [Azure AI Foundry Analysis Documentation](https://learn.microsoft.com/en-us/azure/ai-foundry/analysis/)
- [Business Rule Extraction Guide](https://learn.microsoft.com/en-us/azure/ai-foundry/business-rules/)
- [Dependency Mapping Tools](https://learn.microsoft.com/en-us/azure/ai-foundry/dependency-mapping/)
- [Sample Analysis Reports](../../code/ai-foundry/code-analysis/sample-reports/) 