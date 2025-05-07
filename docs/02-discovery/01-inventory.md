# Mainframe Inventory Process

This document outlines the technical process for creating a comprehensive inventory of IBM z/OS mainframe applications and components using Azure AI Foundry.

## Inventory Objectives

The inventory process has these technical objectives:
- Document all production and development mainframe applications
- Catalog all COBOL, PL/I, Assembler, and JCL components
- Identify and document all datasets and databases
- Map transaction processing components (CICS, IMS)
- Document batch processing schedules and dependencies
- Identify interfaces and integration points

## Technical Implementation

### Deployment Architecture

![Inventory Architecture](../../images/inventory-architecture.png)

The inventory process uses a secure agent-based architecture:

1. **Azure AI Foundry Agents**
   - Lightweight z/OS agents deployed on LPAR
   - Secure, read-only access to system resources
   - Data compression and encryption for transfer

2. **Azure Inventory Service**
   - Secure ingestion pipeline for mainframe metadata
   - AI-powered analysis and classification
   - Version-controlled inventory repository
   - RESTful API for integration with other tools

### Data Collection Process

The inventory process follows this technical workflow:

```
┌────────────────┐     ┌─────────────────┐     ┌──────────────────┐
│ System Catalog │────▶│ AI Foundry Agent│────▶│ Discovery Service│
│   Extraction   │     │   Processing    │     │     Analysis     │
└────────────────┘     └─────────────────┘     └──────────────────┘
        │                                               │
        ▼                                               ▼
┌────────────────┐     ┌─────────────────┐     ┌──────────────────┐
│   Source Code  │────▶│  Source Analysis│────▶│  Knowledge Base  │
│   Repository   │     │    Processing   │     │    Generation    │
└────────────────┘     └─────────────────┘     └──────────────────┘
```

#### System Catalog Analysis

The system catalog extraction includes:

| Catalog Type | Information Extracted |
|--------------|----------------------|
| Master Catalog | Dataset definitions, aliases, connections |
| Program Libraries | Load module information, link dates |
| Procedure Libraries | JCL procedures, included components |
| CICS Resources | Program definitions, transaction mappings |
| DB2 Catalog | Table definitions, stored procedures |
| IMS Catalog | Database definitions, PSBs, DBDs |

#### Source Code Analysis

Source code extraction includes:

| Source Type | Analysis Performed |
|-------------|-------------------|
| COBOL Programs | Program structure, copybooks, file definitions |
| PL/I Programs | Program structure, includes, file definitions |
| Assembler Programs | Macro usage, system interfaces, memory usage |
| JCL Procedures | Job steps, program execution, dataset usage |
| CICS Components | Screen definitions, transaction flow |
| IMS Components | Message processing programs, screen formats |

### Implementation Steps

1. **Prepare the Environment**
   ```bash
   # Install AI Foundry Agents
   EXEC 'SYS2.AIFOUNDRY.CNTL(INSTALL)'
   
   # Configure Agent Security
   EXEC 'SYS2.AIFOUNDRY.CNTL(SECURITY)'
   
   # Validate Agent Installation
   EXEC 'SYS2.AIFOUNDRY.CNTL(VERIFY)'
   ```

2. **Execute System Catalog Extraction**
   ```bash
   # Run Catalog Extraction Job
   EXEC 'SYS2.AIFOUNDRY.CNTL(CATALOG)'
   
   # Extract Program Information
   EXEC 'SYS2.AIFOUNDRY.CNTL(PROGRAMS)'
   
   # Extract Database Definitions
   EXEC 'SYS2.AIFOUNDRY.CNTL(DBEXTRACT)'
   ```

3. **Execute Source Code Extraction**
   ```bash
   # Extract COBOL Source Code
   EXEC 'SYS2.AIFOUNDRY.CNTL(COBOLSRC)'
   
   # Extract PL/I Source Code
   EXEC 'SYS2.AIFOUNDRY.CNTL(PLISRC)'
   
   # Extract JCL Procedures
   EXEC 'SYS2.AIFOUNDRY.CNTL(JCLSRC)'
   ```

4. **Process Extracted Data**
   ```bash
   # Azure CLI Commands
   az ai-foundry inventory process --agent-output /path/to/extracted/data
   az ai-foundry inventory validate --validation-level comprehensive
   az ai-foundry inventory generate-report --format html
   ```

## Inventory Deliverables

The inventory process produces these technical deliverables:

1. **Application Catalog**
   - Hierarchical representation of applications
   - Component relationships and dependencies
   - Technical specifications and metadata

2. **Component Repository**
   - Source code repository in version control
   - Metadata mapping for components
   - Cross-reference indexes

3. **Dataset Inventory**
   - Dataset specifications and layouts
   - Usage patterns and access methods
   - Retention specifications

4. **System Interface Map**
   - External system integration points
   - Communication protocols and formats
   - Interface specifications

## Verification Process

To verify inventory completeness:

```bash
# Run Verification Process
az ai-foundry inventory verify --verification-type coverage
az ai-foundry inventory verify --verification-type accuracy
az ai-foundry inventory verify --verification-type completeness
```

The verification process produces a detailed report highlighting:
- Missing components or metadata
- Inconsistencies in collected data
- Recommended remediation steps

## Next Steps

After completing the inventory process:
- Proceed to [Dependency Mapping](02-dependency-mapping.md) to analyze relationships
- Analyze complexity using [Assessment Criteria](03-assessment-criteria.md)
- Begin planning your [Modernization Strategy](../03-foundation/modernization-strategy.md) 