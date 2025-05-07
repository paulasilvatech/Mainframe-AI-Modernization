# New Directory Structure for Azure AI Foundry Mainframe Modernization Playbook

Based on the README, here is the planned directory structure:

```
docs/
├── 01-introduction/
│   ├── README.md                        # Introduction to Azure AI Foundry for Mainframe
│   ├── 01-overview.md                   # Technical Overview
│   ├── 02-architecture.md               # Technical Architecture
│   ├── 03-prerequisites.md              # Installation Prerequisites
│   └── 04-zos-integration.md            # IBM z/OS Integration Setup
├── 02-discovery/
│   ├── README.md                        # Discovery and Assessment Phase
│   ├── 01-inventory.md                  # Mainframe Inventory Process
│   ├── 02-dependency-mapping.md         # Dependency Mapping
│   └── 03-assessment-criteria.md        # Assessment Criteria
├── 03-foundation/
│   ├── README.md                        # Foundation Setup
│   ├── 01-azure-setup.md                # Azure Environment Setup
│   ├── 02-ai-foundry-installation.md    # AI Foundry Installation
│   └── 03-security-configuration.md     # Security Configuration
├── 04-development-environment/
│   ├── README.md                        # Development Environment Configuration
│   ├── 01-local-setup.md                # Developer Workstation Setup
│   ├── 02-mainframe-connectivity.md     # Mainframe Connectivity
│   └── 03-hybrid-tools.md               # Hybrid Development Tools
├── 05-code-analysis/
│   ├── README.md                        # AI-Powered Code Analysis
│   ├── 01-cobol-analysis.md             # COBOL Analysis
│   ├── 02-pl1-analysis.md               # PL/I Analysis
│   ├── 03-jcl-analysis.md               # JCL Analysis
│   └── 04-business-rules.md             # Business Rules Extraction
├── 06-github-integration/
│   ├── README.md                        # GitHub Integration
│   ├── 01-repository-setup.md           # Repository Setup
│   ├── 02-workflow-configuration.md     # GitHub Actions Workflows
│   ├── 03-security-settings.md          # Security Settings
│   └── 04-developer-workflow.md         # Developer Workflow
├── 07-azure-devops-integration/
│   ├── README.md                        # Azure DevOps Integration
│   ├── 01-project-setup.md              # Project Setup
│   ├── 02-pipeline-configuration.md     # Azure Pipelines
│   ├── 03-security-settings.md          # Security Settings
│   └── 04-developer-workflow.md         # Developer Workflow
├── 08-cicd-implementation/
│   ├── README.md                        # CI/CD Implementation
│   ├── 01-build-pipelines.md            # Build Pipeline Implementation
│   ├── 02-test-automation.md            # Test Automation
│   ├── 03-deployment-pipelines.md       # Deployment Pipeline Implementation
│   └── 04-quality-gates.md              # Quality Gates
├── 09-risk-assessment/
│   ├── README.md                        # Risk Assessment Implementation
│   ├── 01-risk-modeling.md              # Risk Modeling
│   ├── 02-impact-analysis.md            # Impact Analysis
│   └── 03-mitigation-strategies.md      # Mitigation Strategies
├── 10-deployment-automation/
│   ├── README.md                        # Deployment Automation
│   ├── 01-deployment-patterns.md        # Deployment Patterns
│   ├── 02-automated-rollback.md         # Automated Rollback
│   └── 03-validation-steps.md           # Validation Steps
├── 11-hybrid-operations/
│   ├── README.md                        # Hybrid Operations
│   ├── 01-monitoring.md                 # Cross-Platform Monitoring
│   ├── 02-alerting.md                   # Alerting Configuration
│   └── 03-incident-management.md        # Incident Management
└── 12-case-studies/
    ├── README.md                        # Case Studies and Examples
    ├── 01-financial-services.md         # Financial Services Example
    ├── 02-insurance.md                  # Insurance Example
    └── 03-government.md                 # Government Example

code/
├── github/                              # GitHub Integration Examples
├── azure-devops/                        # Azure DevOps Examples  
├── ai-foundry/                          # AI Foundry Examples
├── mainframe/                           # IBM z/OS code with integration points
└── hybrid/                              # Hybrid implementations and patterns

templates/
├── gitattributes/                       # Git Configuration Templates
├── workflows/                           # GitHub Actions workflow templates
├── pipelines/                           # Azure Pipelines templates
└── deployment/                          # Deployment automation templates
```

This structure aligns with the chapter organization in the README file and provides equal coverage for GitHub and Azure DevOps integration paths. 