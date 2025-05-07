# Discovery and Assessment Phase

This chapter covers the discovery and assessment phase for IBM z/OS mainframe modernization using Azure AI Foundry.

## Overview

The discovery and assessment phase is the crucial first step in your mainframe modernization journey. Using Azure AI Foundry's intelligence capabilities, this phase establishes a comprehensive understanding of your existing IBM z/OS environment, identifies dependencies, and creates an actionable modernization strategy.

## Objectives

- Create a detailed inventory of all mainframe applications and components
- Map dependencies between applications, programs, and data assets
- Assess application complexity and identify modernization candidates
- Extract business rules and application knowledge
- Develop a comprehensive modernization strategy

## Technical Steps

1. [Mainframe Inventory Process](01-inventory.md) - Systematic inventory of your z/OS environment
2. [Dependency Mapping with AI Foundry](02-dependency-mapping.md) - AI-assisted mapping of application dependencies
3. [AI-Powered Assessment Criteria](03-assessment-criteria.md) - Using AI to assess modernization complexity and priority

## Technical Prerequisites

Before beginning this phase, ensure you have:

| Requirement | Details |
|-------------|---------|
| z/OS Access | Access to the IBM z/OS environment with appropriate permissions |
| System Catalogs | Access to system catalogs for program and dataset information |
| SMF Records | Access to System Management Facility (SMF) records for usage analysis |
| Source Code Access | Access to COBOL, PL/I, JCL, and Assembler code repositories |
| Database Schemas | Access to DB2, IMS, and VSAM file structures |
| Transaction Logs | Access to CICS and IMS transaction logs (if applicable) |

## Implementation Steps

1. **Deploy Discovery Tools**
   - Install Azure AI Foundry discovery agents on z/OS
   - Configure secure connectivity between z/OS and Azure
   - Validate access permissions and connectivity

2. **Execute Application Inventory**
   - Scan system catalogs for program information
   - Analyze JCL procedures for job relationships
   - Extract COBOL, PL/I, and Assembler program information
   - Document database schemas and file structures

3. **Analyze Application Usage**
   - Process SMF records to identify usage patterns
   - Map transaction volumes and frequencies
   - Identify peak processing periods
   - Document batch window requirements

4. **Map Dependencies**
   - Use AI-powered analysis to identify program calls
   - Map data access patterns and dependencies
   - Document external interfaces and integration points
   - Create application dependency graphs

5. **Assess Modernization Potential**
   - Apply AI analysis to determine modernization complexity
   - Categorize applications by modernization approach
   - Identify technical debt and risks
   - Prioritize applications for modernization

## Key Deliverables

- **Mainframe Application Inventory** - Comprehensive catalog of all applications and components
- **Dependency Map** - Visual and data representation of application interdependencies
- **Complexity Assessment** - Analysis of application complexity and modernization effort
- **Business Rule Documentation** - Extracted business rules and application logic
- **Modernization Strategy** - Recommended approach for each application component
- **Implementation Plan** - Prioritized roadmap for modernization implementation

## Tools and Techniques

| Tool/Technique | Purpose |
|----------------|---------|
| AI Code Analyzer | Deep analysis of source code to extract patterns and dependencies |
| Catalog Explorer | Extraction of program information from system catalogs |
| SMF Analyzer | Processing of performance and usage data |
| Dependency Grapher | Visual representation of application dependencies |
| Business Rule Extractor | Identification of business rules in source code |
| Complexity Calculator | Assessment of application complexity for modernization |

## Practical Example

See the [Code Examples](../../code/ai-foundry/analysis/) directory for practical examples of:
- Catalog exploration scripts
- SMF record processing
- Code analysis implementation
- Dependency mapping visualization
- Assessment report generation

## Next Steps

After completing this phase, continue to the [Foundation Setup](../03-foundation/README.md) phase to establish the core infrastructure for your modernization initiative.

## Additional Resources

- [Azure AI Foundry Analysis Documentation](https://learn.microsoft.com/en-us/azure/ai-foundry/analysis/)
- [Business Rule Extraction Guide](https://learn.microsoft.com/en-us/azure/ai-foundry/business-rules/)
- [Dependency Mapping Tools](https://learn.microsoft.com/en-us/azure/ai-foundry/dependency-mapping/)
- [Sample Analysis Reports](../../code/ai-foundry/code-analysis/sample-reports/) 