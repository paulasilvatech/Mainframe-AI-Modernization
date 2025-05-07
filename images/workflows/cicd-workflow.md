# CI/CD Workflow for IBM z/OS with Azure AI Foundry

This document provides CI/CD workflow diagrams for IBM z/OS integration with Azure AI Foundry, showing both GitHub and Azure DevOps implementation paths.

## Dual Path CI/CD Workflow

The following ASCII diagram illustrates the comprehensive CI/CD workflow with parallel paths for GitHub and Azure DevOps:

```
┌────────────────────────────────────────────────────────────────────────────────────────────────────────┐
│                                      IBM z/OS ENVIRONMENT                                              │
├────────────────────┬─────────────────────────────────────────────────────────────┬────────────────────┤
│                    │                                                             │                    │
│  ┌──────────────┐  │  ┌──────────────┐    ┌──────────────┐    ┌──────────────┐  │  ┌──────────────┐  │
│  │              │  │  │              │    │              │    │              │  │  │              │  │
│  │   COBOL      │  │  │   PL/I       │    │    JCL       │    │   Assembler  │  │  │   CICS/IMS   │  │
│  │   Programs   │  │  │   Programs   │    │  Procedures  │    │   Programs   │  │  │ Transactions │  │
│  │              │  │  │              │    │              │    │              │  │  │              │  │
│  └──────┬───────┘  │  └──────┬───────┘    └──────┬───────┘    └──────┬───────┘  │  └──────┬───────┘  │
│         │          │         │                   │                   │          │         │          │
└─────────┼──────────┼─────────┼───────────────────┼───────────────────┼──────────┼─────────┼──────────┘
          │          │         │                   │                   │          │         │           
          │          │         │                   │                   │          │         │           
┌─────────┼──────────┼─────────┼───────────────────┼───────────────────┼──────────┼─────────┼──────────┐
│         │          │         │    AZURE AI FOUNDRY INTEGRATION LAYER │          │         │          │
│  ┌──────▼──────┐   │  ┌──────▼──────┐       ┌──────▼──────┐   ┌──────▼──────┐   │  ┌──────▼──────┐   │
│  │             │   │  │             │       │             │   │             │   │  │             │   │
│  │  Source     │   │  │  Source     │       │  Source     │   │  Source     │   │  │  Source     │   │
│  │  Extraction │   │  │  Extraction │       │  Extraction │   │  Extraction │   │  │  Extraction │   │
│  │             │   │  │             │       │             │   │             │   │  │             │   │
│  └──────┬──────┘   │  └──────┬──────┘       └──────┬──────┘   └──────┬──────┘   │  └──────┬──────┘   │
│         │          │         │                     │                 │          │         │          │
│         │          │         │                     │                 │          │         │          │
│  ┌──────▼──────┐   │  ┌──────▼──────┐       ┌──────▼──────┐   ┌──────▼──────┐   │  ┌──────▼──────┐   │
│  │             │   │  │             │       │             │   │             │   │  │             │   │
│  │ AI-Powered  │   │  │ AI-Powered  │       │ AI-Powered  │   │ AI-Powered  │   │  │ AI-Powered  │   │
│  │ Analysis    │   │  │ Analysis    │       │ Analysis    │   │ Analysis    │   │  │ Analysis    │   │
│  │             │   │  │             │       │             │   │             │   │  │             │   │
│  └──────┬──────┘   │  └──────┬──────┘       └──────┬──────┘   └──────┬──────┘   │  └──────┬──────┘   │
│         │          │         │                     │                 │          │         │          │
└─────────┼──────────┼─────────┼─────────────────────┼─────────────────┼──────────┼─────────┼──────────┘
          │          │         │                     │                 │          │         │           
          │          │         │                     │                 │          │         │           
┌─────────┼──────────┼─────────┼─────────────────────┼─────────────────┼──────────┼─────────┼──────────┐
│         │          │         │                     │                 │          │         │          │
│         ▼          │         ▼                     ▼                 ▼          │         ▼          │
│  ┌─────────────────────────────────────────────────────────────────────────────────────────────┐     │
│  │                                                                                             │     │
│  │                             VERSION CONTROL REPOSITORY                                      │     │
│  │                                                                                             │     │
│  └───────────┬─────────────────────────────────────────────────────────────────┬───────────────┘     │
│              │                                                                 │                     │
│              │                                                                 │                     │
│       ┌──────▼───────┐                                                  ┌──────▼───────┐             │
│       │              │                                                  │              │             │
│       │   GitHub     │                                                  │ Azure DevOps │             │
│       │ Repository   │                                                  │ Repository   │             │
│       │              │                                                  │              │             │
│       └──────┬───────┘                                                  └──────┬───────┘             │
│              │                                                                 │                     │
└──────────────┼─────────────────────────────────────────────────────────────────┼─────────────────────┘
               │                                                                 │                      
         ┌─────┴─────┐                                                     ┌─────┴─────┐                
         │           │                                                     │           │                
┌────────▼───────────▼──────────┐                               ┌──────────▼───────────▼────────┐      
│     GITHUB CI/CD PIPELINE     │                               │     AZURE DEVOPS PIPELINE     │      
├────────────────────────────────┤                              ├────────────────────────────────┤      
│                               │                               │                               │      
│  ┌─────────────────────────┐  │                               │  ┌─────────────────────────┐  │      
│  │                         │  │                               │  │                         │  │      
│  │   GitHub Actions:       │  │                               │  │   Azure Pipelines:      │  │      
│  │   Build Workflow        │  │                               │  │   Build Pipeline        │  │      
│  │                         │  │                               │  │                         │  │      
│  └────────────┬────────────┘  │                               │  └────────────┬────────────┘  │      
│               │               │                               │               │               │      
│  ┌────────────▼────────────┐  │                               │  ┌────────────▼────────────┐  │      
│  │                         │  │                               │  │                         │  │      
│  │   GitHub Actions:       │  │                               │  │   Azure Pipelines:      │  │      
│  │   Test Workflow         │  │                               │  │   Test Pipeline         │  │      
│  │                         │  │                               │  │                         │  │      
│  └────────────┬────────────┘  │                               │  └────────────┬────────────┘  │      
│               │               │                               │               │               │      
│  ┌────────────▼────────────┐  │                               │  ┌────────────▼────────────┐  │      
│  │                         │  │                               │  │                         │  │      
│  │   GitHub Actions:       │  │                               │  │   Azure Pipelines:      │  │      
│  │   Security Scan         │  │                               │  │   Security Scan         │  │      
│  │                         │  │                               │  │                         │  │      
│  └────────────┬────────────┘  │                               │  └────────────┬────────────┘  │      
│               │               │                               │               │               │      
│  ┌────────────▼────────────┐  │                               │  ┌────────────▼────────────┐  │      
│  │                         │  │                               │  │                         │  │      
│  │   GitHub Actions:       │  │                               │  │   Azure Pipelines:      │  │      
│  │   Package & Release     │  │                               │  │   Package & Release     │  │      
│  │                         │  │                               │  │                         │  │      
│  └────────────┬────────────┘  │                               │  └────────────┬────────────┘  │      
│               │               │                               │               │               │      
│  ┌────────────▼────────────┐  │                               │  ┌────────────▼────────────┐  │      
│  │                         │  │                               │  │                         │  │      
│  │   GitHub Actions:       │  │                               │  │   Azure Pipelines:      │  │      
│  │   Deploy Workflow       │  │                               │  │   Deploy Pipeline       │  │      
│  │                         │  │                               │  │                         │  │      
│  └────────────┬────────────┘  │                               │  └────────────┬────────────┘  │      
│               │               │                               │               │               │      
└───────────────┼───────────────┘                               └───────────────┼───────────────┘      
                │                                                               │                       
                │                                                               │                       
                ▼                                                               ▼                       
┌───────────────────────────────┐                               ┌───────────────────────────────┐       
│                               │                               │                               │       
│   DEPLOYMENT ENVIRONMENTS     │                               │   DEPLOYMENT ENVIRONMENTS     │       
│                               │                               │                               │       
├───────────────────────────────┤                               ├───────────────────────────────┤       
│                               │                               │                               │       
│  ┌─────────────────────────┐  │                               │  ┌─────────────────────────┐  │       
│  │                         │  │                               │  │                         │  │       
│  │      Development        │  │                               │  │      Development        │  │       
│  │                         │  │                               │  │                         │  │       
│  └────────────┬────────────┘  │                               │  └────────────┬────────────┘  │       
│               │               │                               │               │               │       
│  ┌────────────▼────────────┐  │                               │  ┌────────────▼────────────┐  │       
│  │                         │  │                               │  │                         │  │       
│  │        Test             │  │                               │  │        Test             │  │       
│  │                         │  │                               │  │                         │  │       
│  └────────────┬────────────┘  │                               │  └────────────┬────────────┘  │       
│               │               │                               │               │               │       
│  ┌────────────▼────────────┐  │                               │  ┌────────────▼────────────┐  │       
│  │                         │  │                               │  │                         │  │       
│  │       Staging           │  │                               │  │       Staging           │  │       
│  │                         │  │                               │  │                         │  │       
│  └────────────┬────────────┘  │                               │  └────────────┬────────────┘  │       
│               │               │                               │               │               │       
│  ┌────────────▼────────────┐  │                               │  ┌────────────▼────────────┐  │       
│  │                         │  │                               │  │                         │  │       
│  │      Production         │  │                               │  │      Production         │  │       
│  │                         │  │                               │  │                         │  │       
│  └─────────────────────────┘  │                               │  └─────────────────────────┘  │       
│                               │                               │                               │       
└───────────────────────────────┘                               └───────────────────────────────┘       
```

## GitHub CI/CD Implementation Path

The GitHub path provides a workflow using GitHub Actions for building, testing, and deploying IBM z/OS applications:

1. **Source Control**
   - IBM z/OS source code committed to GitHub repository
   - Copybooks, COBOL, PL/I, JCL, and other assets stored with proper `.gitattributes`

2. **CI/CD Pipeline Components**
   - **Build Workflow**
     - Compiles COBOL/PL/I programs
     - Validates JCL procedures
     - Checks syntax and dependencies
     - Uses custom GitHub Actions for mainframe compilation

   - **Test Workflow**
     - Runs unit tests for compiled programs
     - Executes integration tests against test environment
     - Performs batch process validation

   - **Security Scan**
     - Scans code for security vulnerabilities
     - Validates against security policies
     - Checks for sensitive data exposure

   - **Package & Release**
     - Creates deployable artifacts
     - Generates deployment packages
     - Creates GitHub Release with artifacts

   - **Deploy Workflow**
     - Deploys to target environments
     - Manages environment-specific configurations
     - Handles rollback if deployment fails

3. **Deployment Targets**
   - Progressive deployment through Development, Test, Staging, and Production
   - Environment-specific configurations managed through GitHub Environments
   - Approval gates between environments

## Azure DevOps CI/CD Implementation Path

The Azure DevOps path provides a parallel implementation using Azure Pipelines:

1. **Source Control**
   - IBM z/OS source code stored in Azure Repos
   - Branch policies enforce code review and quality gates

2. **CI/CD Pipeline Components**
   - **Build Pipeline**
     - Compiles mainframe programs
     - Uses Azure DevOps extensions for mainframe builds
     - Creates build artifacts

   - **Test Pipeline**
     - Executes automated tests
     - Validates functional requirements
     - Reports test coverage

   - **Security Scan**
     - Static code analysis
     - Security vulnerability scanning
     - Compliance validation

   - **Package & Release**
     - Creates deployment packages
     - Generates release artifacts
     - Prepares deployment manifest

   - **Deploy Pipeline**
     - Handles deployment to target environments
     - Uses deployment groups for environment management
     - Implements blue/green or canary deployment patterns

3. **Release Management**
   - Release definitions control the deployment flow
   - Approval gates between environments
   - Environment-specific variables and configurations

## Azure AI Foundry Integration

Azure AI Foundry enhances both CI/CD paths with:

1. **Pre-Commit Analysis**
   - AI-powered code analysis before commit
   - Business rule validation
   - Impact assessment

2. **Build-Time Intelligence**
   - Intelligent test selection
   - Risk-based quality gates
   - Performance optimization suggestions

3. **Deployment Intelligence**
   - AI-driven deployment strategy recommendations
   - Risk assessment for each deployment
   - Rollback prediction and mitigation

## Implementation Guidelines

To implement either CI/CD path:

1. **Initialize Version Control**
   - Set up proper `.gitattributes` or Azure Repos settings for mainframe code
   - Configure branch policies and protection rules

2. **Configure Build Agents**
   - Install necessary mainframe compilation tools on build agents
   - Set up connectivity to mainframe environment for testing

3. **Create Pipeline Definitions**
   - Define workflow files (GitHub) or pipeline YAML (Azure DevOps)
   - Configure environment-specific variables and secrets

4. **Set Up Deployment Targets**
   - Configure development, test, staging, and production environments
   - Establish connectivity and access control for each environment

5. **Integrate Azure AI Foundry**
   - Configure AI services for code analysis
   - Set up intelligence-driven quality gates
   - Implement risk assessment for deployments

## Practical Example

For detailed implementation examples, see:
- [GitHub CI/CD Implementation](../../code/github/workflows/)
- [Azure DevOps CI/CD Implementation](../../code/azure-devops/pipelines/) 