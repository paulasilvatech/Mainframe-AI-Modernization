# GitHub & Azure DevOps Integration for Mainframe Modernization

This document provides detailed implementation guidance for setting up GitHub or Azure DevOps for mainframe modernization with Azure AI Foundry.

## Integration Overview

The following diagram illustrates the DevOps integration architecture for mainframe modernization:

```
                    ┌───────────────────────────────────────────────────────────────┐
                    │                GITHUB OR AZURE DEVOPS                         │
┌─────────────┐     │                                                               │     ┌──────────────┐
│             │     │  ┌─────────────┐    ┌─────────────┐    ┌─────────────┐       │     │              │
│  IBM z/OS   │     │  │ Source      │    │ CI/CD       │    │ Automated   │       │     │  Azure AI    │
│ Environment ├─────┼─►│ Repository  ├───►│ Pipelines   ├───►│ Testing     ├──────►├────►│  Foundry     │
│             │     │  │             │    │             │    │             │       │     │              │
└─────────────┘     │  └─────────────┘    └─────────────┘    └─────────────┘       │     └──────────────┘
                    │                                                               │
                    │  ┌─────────────┐    ┌─────────────┐    ┌─────────────┐       │
                    │  │ Work Item   │    │ Code Review │    │ Artifact    │       │
                    │  │ Management  │    │ System      │    │ Registry    │       │
                    │  │             │    │             │    │             │       │
                    │  └─────────────┘    └─────────────┘    └─────────────┘       │
                    │                                                               │
                    └───────────────────────────────────────────────────────────────┘
```

## GitHub Integration Setup

### 1. Repository Structure

Set up your GitHub repository with the following structure for mainframe applications:

```
mainframe-modernization/
├── .github/
│   ├── workflows/            # GitHub Actions workflow definitions
│   ├── actions/              # Custom actions for mainframe operations
│   └── templates/            # Issue and PR templates
├── src/
│   ├── cobol/                # COBOL source code
│   ├── jcl/                  # JCL procedures
│   ├── copybook/             # COBOL copybooks
│   ├── bms/                  # BMS screen definitions
│   └── data/                 # Test data definitions
├── modern/
│   ├── java/                 # Modernized Java code
│   ├── csharp/               # Modernized C# code
│   ├── api/                  # API specifications
│   └── web/                  # Web front-end components
├── scripts/
│   ├── build/                # Build scripts
│   ├── deploy/               # Deployment scripts
│   └── test/                 # Test automation scripts
├── docs/
│   ├── architecture/         # Architecture documentation
│   ├── operations/           # Operational procedures
│   └── apis/                 # API documentation
└── config/
    ├── build/                # Build configuration
    ├── deploy/               # Deployment configuration
    └── analysis/             # Analysis configuration
```

### 2. Git Configuration for Mainframe Files

Create a `.gitattributes` file to handle mainframe file formats:

```
# COBOL files
*.cbl   text eol=lf working-tree-encoding=IBM-1047 git-encoding=utf-8
*.cob   text eol=lf working-tree-encoding=IBM-1047 git-encoding=utf-8
*.copy  text eol=lf working-tree-encoding=IBM-1047 git-encoding=utf-8
*.cpy   text eol=lf working-tree-encoding=IBM-1047 git-encoding=utf-8

# JCL files
*.jcl   text eol=lf working-tree-encoding=IBM-1047 git-encoding=utf-8

# PL/I files
*.pli   text eol=lf working-tree-encoding=IBM-1047 git-encoding=utf-8

# Assembler files
*.asm   text eol=lf working-tree-encoding=IBM-1047 git-encoding=utf-8

# BMS Maps
*.bms   text eol=lf working-tree-encoding=IBM-1047 git-encoding=utf-8

# Binary data files
*.vsam  binary
*.db2   binary
```

### 3. GitHub Actions for Mainframe CI/CD

Create a GitHub Actions workflow for mainframe CI/CD integration:

```yaml
# .github/workflows/mainframe-cicd.yml
name: Mainframe CI/CD

on:
  push:
    branches: [ main, dev ]
    paths:
      - 'src/**'
      - 'config/**'
  pull_request:
    branches: [ main ]
    paths:
      - 'src/**'
      - 'config/**'
  workflow_dispatch:
    inputs:
      environment:
        description: 'Deployment environment'
        required: true
        default: 'dev'

jobs:
  analyze:
    name: Analyze Mainframe Code
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      - name: Set up Azure CLI
        uses: azure/login@v1
        with:
          creds: ${{ secrets.AZURE_CREDENTIALS }}
          
      - name: Invoke AI Foundry Analysis
        uses: azure/cli@v1
        with:
          inlineScript: |
            az extension add --name ai-foundry
            az ai-foundry analysis start \
              --resource-group ${{ secrets.RESOURCE_GROUP }} \
              --workspace ${{ secrets.WORKSPACE_NAME }} \
              --source-path ./src \
              --output-path ./analysis \
              --config-file ./config/analysis/ai-config.json
      
      - name: Upload Analysis Results
        uses: actions/upload-artifact@v3
        with:
          name: code-analysis
          path: ./analysis
  
  build:
    name: Build Mainframe Components
    runs-on: ubuntu-latest
    needs: analyze
    steps:
      - uses: actions/checkout@v3
      
      - name: Download Analysis Results
        uses: actions/download-artifact@v3
        with:
          name: code-analysis
          path: ./analysis
      
      - name: Build COBOL Programs
        uses: ./.github/actions/build-cobol
        with:
          source-dir: ./src/cobol
          output-dir: ./build
          options: ${{ secrets.COBOL_BUILD_OPTIONS }}
      
      - name: Package Artifacts
        run: |
          mkdir -p artifacts
          cp -r build/* artifacts/
          cp -r src/jcl artifacts/
          cp -r config/deploy artifacts/
      
      - name: Upload Build Artifacts
        uses: actions/upload-artifact@v3
        with:
          name: build-artifacts
          path: ./artifacts
  
  test:
    name: Test Mainframe Components
    runs-on: ubuntu-latest
    needs: build
    steps:
      - uses: actions/checkout@v3
      
      - name: Download Build Artifacts
        uses: actions/download-artifact@v3
        with:
          name: build-artifacts
          path: ./artifacts
      
      - name: Set up Test Environment
        uses: ./.github/actions/setup-test-env
        with:
          config-file: ./config/test/test-config.json
      
      - name: Run Unit Tests
        uses: ./.github/actions/run-mainframe-tests
        with:
          test-dir: ./tests/unit
          artifacts-dir: ./artifacts
          test-report-path: ./test-results/unit
      
      - name: Run Integration Tests
        uses: ./.github/actions/run-mainframe-tests
        with:
          test-dir: ./tests/integration
          artifacts-dir: ./artifacts
          test-report-path: ./test-results/integration
      
      - name: Upload Test Results
        uses: actions/upload-artifact@v3
        with:
          name: test-results
          path: ./test-results
  
  deploy:
    name: Deploy to Mainframe
    runs-on: ubuntu-latest
    needs: test
    if: github.event_name == 'workflow_dispatch' || github.ref == 'refs/heads/main'
    environment:
      name: ${{ github.event.inputs.environment || 'dev' }}
    steps:
      - uses: actions/checkout@v3
      
      - name: Download Build Artifacts
        uses: actions/download-artifact@v3
        with:
          name: build-artifacts
          path: ./artifacts
      
      - name: Deploy to Mainframe
        uses: ./.github/actions/deploy-to-mainframe
        with:
          target-environment: ${{ github.event.inputs.environment || 'dev' }}
          artifacts-dir: ./artifacts
          deploy-config: ./artifacts/deploy
          credentials: ${{ secrets.MAINFRAME_CREDENTIALS }}
      
      - name: Verify Deployment
        uses: ./.github/actions/verify-deployment
        with:
          target-environment: ${{ github.event.inputs.environment || 'dev' }}
          verification-script: ./scripts/deploy/verify.sh
```

### 4. Custom GitHub Actions for Mainframe Operations

Create a custom action for mainframe deployment:

```yaml
# .github/actions/deploy-to-mainframe/action.yml
name: 'Deploy to Mainframe'
description: 'Deploy artifacts to a mainframe environment'
inputs:
  target-environment:
    description: 'Target environment for deployment'
    required: true
    default: 'dev'
  artifacts-dir:
    description: 'Directory containing build artifacts'
    required: true
  deploy-config:
    description: 'Directory containing deployment configuration'
    required: true
  credentials:
    description: 'Mainframe credentials JSON'
    required: true

runs:
  using: 'composite'
  steps:
    - name: Install IBM zOS Connect Client
      shell: bash
      run: |
        pip install zos-connector-client
        
    - name: Prepare Deployment Package
      shell: bash
      run: |
        mkdir -p deploy-package
        cp -r ${{ inputs.artifacts-dir }}/* deploy-package/
        cp -r ${{ inputs.deploy-config }}/* deploy-package/
        
    - name: Deploy COBOL Programs
      shell: bash
      run: |
        echo '${{ inputs.credentials }}' > credentials.json
        python ${{ github.action_path }}/scripts/deploy_cobol.py \
          --env ${{ inputs.target-environment }} \
          --package ./deploy-package \
          --credentials ./credentials.json
          
    - name: Deploy JCL Procedures
      shell: bash
      run: |
        python ${{ github.action_path }}/scripts/deploy_jcl.py \
          --env ${{ inputs.target-environment }} \
          --package ./deploy-package \
          --credentials ./credentials.json
          
    - name: Update DB2 Schemas
      shell: bash
      run: |
        python ${{ github.action_path }}/scripts/update_db2.py \
          --env ${{ inputs.target-environment }} \
          --package ./deploy-package \
          --credentials ./credentials.json
          
    - name: Cleanup Sensitive Data
      shell: bash
      run: |
        rm -f credentials.json
```

Create the deployment Python script:

```python
# .github/actions/deploy-to-mainframe/scripts/deploy_cobol.py
import argparse
import json
import os
import sys
import requests
from zos_connector.client import ZOSConnectClient

def main():
    parser = argparse.ArgumentParser(description='Deploy COBOL programs to z/OS')
    parser.add_argument('--env', required=True, help='Target environment')
    parser.add_argument('--package', required=True, help='Deployment package directory')
    parser.add_argument('--credentials', required=True, help='Credentials file')
    args = parser.parse_args()
    
    # Load credentials
    with open(args.credentials, 'r') as f:
        creds = json.load(f)
    
    # Environment-specific configuration
    env_config = {
        'dev': {
            'host': creds['dev']['host'],
            'port': creds['dev']['port'],
            'user': creds['dev']['user'],
            'password': creds['dev']['password'],
            'dataset_hlq': creds['dev']['dataset_hlq']
        },
        'test': {
            'host': creds['test']['host'],
            'port': creds['test']['port'],
            'user': creds['test']['user'],
            'password': creds['test']['password'],
            'dataset_hlq': creds['test']['dataset_hlq']
        },
        'prod': {
            'host': creds['prod']['host'],
            'port': creds['prod']['port'],
            'user': creds['prod']['user'],
            'password': creds['prod']['password'],
            'dataset_hlq': creds['prod']['dataset_hlq']
        }
    }
    
    config = env_config[args.env]
    
    # Initialize z/OS Connect client
    client = ZOSConnectClient(
        host=config['host'],
        port=config['port'],
        user=config['user'],
        password=config['password']
    )
    
    # Deploy COBOL programs
    cobol_dir = os.path.join(args.package, 'cobol')
    for file in os.listdir(cobol_dir):
        if file.endswith('.cbl') or file.endswith('.cob'):
            program_name = os.path.splitext(file)[0].upper()
            load_dataset = f"{config['dataset_hlq']}.LOADLIB({program_name})"
            source_path = os.path.join(cobol_dir, file)
            
            print(f"Deploying {program_name} to {load_dataset}")
            
            # Upload source code
            source_dataset = f"{config['dataset_hlq']}.COBOL({program_name})"
            client.upload_file(source_path, source_dataset)
            
            # Compile and link
            jcl = f"""//COMPILE JOB (ACCT),'COMPILE COBOL',CLASS=A,
//         MSGCLASS=X,MSGLEVEL=(1,1),NOTIFY=&SYSUID
//COBCOMP EXEC PGM=IGYCRCTL,REGION=0M,
//         PARM='RENT,APOST,MAP,LIST,OFFSET,XREF'
//STEPLIB  DD DSN=IGY.V6R3M0.SIGYCOMP,DISP=SHR
//SYSIN    DD DSN={source_dataset},DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSLIN   DD DSN=&&LOADSET,DISP=(NEW,PASS),
//            UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT1   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT2   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT3   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT4   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT5   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT6   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT7   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT8   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT9   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT10  DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT11  DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT12  DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT13  DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT14  DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT15  DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSMDECK DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//LKED     EXEC PGM=IEWBLINK,COND=(4,LT,COBCOMP),REGION=0M
//SYSLIB   DD DSN=CEE.SCEELKED,DISP=SHR
//SYSLIN   DD DSN=&&LOADSET,DISP=(OLD,DELETE)
//SYSLMOD  DD DSN={load_dataset},DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
"""
            
            # Submit JCL for compilation
            job_id = client.submit_jcl(jcl)
            print(f"Submitted job {job_id}")
            
            # Wait for job completion
            job_status = client.wait_for_job_completion(job_id)
            if job_status['retcode'] == '0000':
                print(f"Successfully deployed {program_name}")
            else:
                print(f"Failed to deploy {program_name}: {job_status}")
                sys.exit(1)
    
    print("COBOL deployment completed successfully")

if __name__ == "__main__":
    main()
```

### 5. GitHub Issue Templates

Create issue templates for mainframe-related work:

```yaml
# .github/ISSUE_TEMPLATE/mainframe-bug.yml
name: Mainframe Bug Report
description: Report a bug in mainframe components
title: "[BUG]: "
labels: ["bug", "mainframe"]
body:
  - type: markdown
    attributes:
      value: |
        Thanks for taking the time to fill out this bug report!
  - type: input
    id: program
    attributes:
      label: Program Name
      description: Which COBOL program or JCL procedure is affected?
      placeholder: e.g., CUSTMGMT
    validations:
      required: true
  - type: dropdown
    id: component
    attributes:
      label: Component Type
      description: What type of component has the issue?
      options:
        - COBOL Program
        - JCL Procedure
        - DB2 Database
        - CICS Transaction
        - IMS Application
        - Other
    validations:
      required: true
  - type: textarea
    id: description
    attributes:
      label: Bug Description
      description: A clear description of what the bug is
      placeholder: Describe the issue in detail
    validations:
      required: true
  - type: textarea
    id: reproduction
    attributes:
      label: Steps to Reproduce
      description: How can we reproduce this issue?
      placeholder: |
        1. Submit JCL '...'
        2. Execute transaction '...'
        3. See error '...'
    validations:
      required: true
  - type: textarea
    id: expected
    attributes:
      label: Expected Behavior
      description: What did you expect to happen?
    validations:
      required: true
  - type: textarea
    id: logs
    attributes:
      label: Relevant Logs
      description: Please copy and paste any relevant log output
      render: shell
  - type: input
    id: environment
    attributes:
      label: Environment
      description: Which environment is this occurring in?
      placeholder: DEV, TEST, PROD
    validations:
      required: true
```

## Azure DevOps Integration Setup

### 1. Project Configuration

Set up an Azure DevOps project with the following structure:

1. **Create Project**:
   - Name: MainframeModernization
   - Description: IBM z/OS Mainframe Modernization Project
   - Visibility: Private
   - Version control: Git
   - Work item process: Agile

2. **Repositories Structure**:
   
   Create the following repositories:
   - mainframe-code: IBM z/OS mainframe code repository
   - modernized-code: Modernized application code
   - deployment-scripts: Deployment automation scripts
   - documentation: Architecture and operational documentation

3. **Configure Branch Policies**:

   For each repository, set up branch policies:
   - Require minimum number of reviewers: 2
   - Check for linked work items
   - Build validation: Enable with appropriate build pipelines
   - Status checks: Enable required status checks

### 2. Azure Pipelines for Mainframe CI/CD

Create a YAML pipeline for mainframe CI/CD:

```yaml
# azure-pipelines.yml
trigger:
  branches:
    include:
      - main
      - dev
  paths:
    include:
      - 'src/**'
      - 'config/**'

pool:
  vmImage: 'ubuntu-latest'

variables:
  - group: mainframe-credentials
  - name: buildConfiguration
    value: 'Release'

stages:
- stage: Analyze
  displayName: Analyze Mainframe Code
  jobs:
  - job: CodeAnalysis
    displayName: AI Foundry Analysis
    steps:
    - checkout: self
    
    - task: AzureCLI@2
      displayName: 'Setup Azure CLI'
      inputs:
        azureSubscription: 'MainframeModernization'
        scriptType: 'bash'
        scriptLocation: 'inlineScript'
        inlineScript: |
          az extension add --name ai-foundry
    
    - task: AzureCLI@2
      displayName: 'Run AI Foundry Analysis'
      inputs:
        azureSubscription: 'MainframeModernization'
        scriptType: 'bash'
        scriptLocation: 'inlineScript'
        inlineScript: |
          az ai-foundry analysis start \
            --resource-group $(resourceGroup) \
            --workspace $(workspaceName) \
            --source-path ./src \
            --output-path ./analysis \
            --config-file ./config/analysis/ai-config.json
    
    - task: PublishPipelineArtifact@1
      displayName: 'Publish Analysis Results'
      inputs:
        targetPath: './analysis'
        artifact: 'code-analysis'

- stage: Build
  displayName: Build Mainframe Components
  dependsOn: Analyze
  jobs:
  - job: BuildComponents
    displayName: Build COBOL and JCL
    steps:
    - checkout: self
    
    - task: DownloadPipelineArtifact@2
      inputs:
        artifactName: 'code-analysis'
        targetPath: './analysis'
    
    - task: Bash@3
      displayName: 'Build COBOL Programs'
      inputs:
        targetType: 'filePath'
        filePath: './scripts/build/build-cobol.sh'
        arguments: '--source-dir ./src/cobol --output-dir ./build --options "$(cobolBuildOptions)"'
    
    - task: CopyFiles@2
      displayName: 'Package Artifacts'
      inputs:
        SourceFolder: '$(Build.SourcesDirectory)'
        Contents: |
          build/**
          src/jcl/**
          config/deploy/**
        TargetFolder: '$(Build.ArtifactStagingDirectory)/artifacts'
    
    - task: PublishPipelineArtifact@1
      displayName: 'Publish Build Artifacts'
      inputs:
        targetPath: '$(Build.ArtifactStagingDirectory)/artifacts'
        artifact: 'build-artifacts'

- stage: Test
  displayName: Test Mainframe Components
  dependsOn: Build
  jobs:
  - job: TestComponents
    displayName: Run Tests
    steps:
    - checkout: self
    
    - task: DownloadPipelineArtifact@2
      inputs:
        artifactName: 'build-artifacts'
        targetPath: './artifacts'
    
    - task: Bash@3
      displayName: 'Set up Test Environment'
      inputs:
        targetType: 'filePath'
        filePath: './scripts/test/setup-test-env.sh'
        arguments: '--config-file ./config/test/test-config.json'
    
    - task: Bash@3
      displayName: 'Run Unit Tests'
      inputs:
        targetType: 'filePath'
        filePath: './scripts/test/run-mainframe-tests.sh'
        arguments: '--test-dir ./tests/unit --artifacts-dir ./artifacts --test-report-path ./test-results/unit'
    
    - task: Bash@3
      displayName: 'Run Integration Tests'
      inputs:
        targetType: 'filePath'
        filePath: './scripts/test/run-mainframe-tests.sh'
        arguments: '--test-dir ./tests/integration --artifacts-dir ./artifacts --test-report-path ./test-results/integration'
    
    - task: PublishTestResults@2
      displayName: 'Publish Test Results'
      inputs:
        testResultsFormat: 'JUnit'
        testResultsFiles: '**/TEST-*.xml'
        searchFolder: '$(System.DefaultWorkingDirectory)/test-results'
        mergeTestResults: true
        testRunTitle: 'Mainframe Component Tests'

- stage: DeployToDev
  displayName: Deploy to Development
  dependsOn: Test
  condition: and(succeeded(), eq(variables['Build.SourceBranch'], 'refs/heads/dev'))
  jobs:
  - deployment: DeployToDevEnvironment
    displayName: Deploy to Dev
    environment: 'Development'
    strategy:
      runOnce:
        deploy:
          steps:
          - checkout: self
          
          - task: DownloadPipelineArtifact@2
            inputs:
              artifactName: 'build-artifacts'
              targetPath: './artifacts'
          
          - task: Bash@3
            displayName: 'Deploy to Mainframe'
            inputs:
              targetType: 'filePath'
              filePath: './scripts/deploy/deploy-to-mainframe.sh'
              arguments: '--env dev --artifacts-dir ./artifacts --deploy-config ./artifacts/deploy'
            env:
              MAINFRAME_HOST: $(dev-host)
              MAINFRAME_PORT: $(dev-port)
              MAINFRAME_USER: $(dev-user)
              MAINFRAME_PASSWORD: $(dev-password)
              MAINFRAME_DATASET_HLQ: $(dev-dataset-hlq)
          
          - task: Bash@3
            displayName: 'Verify Deployment'
            inputs:
              targetType: 'filePath'
              filePath: './scripts/deploy/verify-deployment.sh'
              arguments: '--env dev'

- stage: DeployToProd
  displayName: Deploy to Production
  dependsOn: Test
  condition: and(succeeded(), eq(variables['Build.SourceBranch'], 'refs/heads/main'))
  jobs:
  - deployment: DeployToProdEnvironment
    displayName: Deploy to Production
    environment: 'Production'
    strategy:
      runOnce:
        deploy:
          steps:
          - checkout: self
          
          - task: DownloadPipelineArtifact@2
            inputs:
              artifactName: 'build-artifacts'
              targetPath: './artifacts'
          
          - task: Bash@3
            displayName: 'Deploy to Mainframe'
            inputs:
              targetType: 'filePath'
              filePath: './scripts/deploy/deploy-to-mainframe.sh'
              arguments: '--env prod --artifacts-dir ./artifacts --deploy-config ./artifacts/deploy'
            env:
              MAINFRAME_HOST: $(prod-host)
              MAINFRAME_PORT: $(prod-port)
              MAINFRAME_USER: $(prod-user)
              MAINFRAME_PASSWORD: $(prod-password)
              MAINFRAME_DATASET_HLQ: $(prod-dataset-hlq)
          
          - task: Bash@3
            displayName: 'Verify Deployment'
            inputs:
              targetType: 'filePath'
              filePath: './scripts/deploy/verify-deployment.sh'
              arguments: '--env prod'
```

### 3. Variable Groups for Secure Credentials

Set up a variable group for secure credentials:

1. Navigate to Pipelines > Library > Variable groups
2. Create a new variable group named "mainframe-credentials"
3. Add the following variables, marking them as secret:
   - dev-host: Hostname for development mainframe
   - dev-port: Port number for development mainframe
   - dev-user: Username for development mainframe
   - dev-password: Password for development mainframe
   - dev-dataset-hlq: High-level qualifier for development datasets
   - test-host: Hostname for test mainframe
   - test-port: Port number for test mainframe
   - test-user: Username for test mainframe
   - test-password: Password for test mainframe
   - test-dataset-hlq: High-level qualifier for test datasets
   - prod-host: Hostname for production mainframe
   - prod-port: Port number for production mainframe
   - prod-user: Username for production mainframe
   - prod-password: Password for production mainframe
   - prod-dataset-hlq: High-level qualifier for production datasets
   - resourceGroup: Azure resource group name
   - workspaceName: AI Foundry workspace name
   - cobolBuildOptions: COBOL compiler options

### 4. Deployment Script

Create a deployment script for mainframe integration:

```bash
#!/bin/bash
# scripts/deploy/deploy-to-mainframe.sh

set -e

# Parse arguments
while [[ $# -gt 0 ]]; do
  key="$1"
  case $key in
    --env)
      ENV="$2"
      shift
      shift
      ;;
    --artifacts-dir)
      ARTIFACTS_DIR="$2"
      shift
      shift
      ;;
    --deploy-config)
      DEPLOY_CONFIG="$2"
      shift
      shift
      ;;
    *)
      echo "Unknown option: $key"
      exit 1
      ;;
  esac
done

# Validate required arguments
if [[ -z "$ENV" || -z "$ARTIFACTS_DIR" || -z "$DEPLOY_CONFIG" ]]; then
  echo "Missing required arguments"
  echo "Usage: deploy-to-mainframe.sh --env <environment> --artifacts-dir <artifacts-dir> --deploy-config <deploy-config>"
  exit 1
fi

# Validate environment variables
if [[ -z "$MAINFRAME_HOST" || -z "$MAINFRAME_PORT" || -z "$MAINFRAME_USER" || 
      -z "$MAINFRAME_PASSWORD" || -z "$MAINFRAME_DATASET_HLQ" ]]; then
  echo "Missing required environment variables"
  exit 1
fi

echo "Deploying to $ENV environment on host $MAINFRAME_HOST"

# Install required dependencies
pip install zos-connector-client

# Deploy COBOL programs
echo "Deploying COBOL programs..."
python -m scripts.deploy.deploy_cobol \
  --env "$ENV" \
  --host "$MAINFRAME_HOST" \
  --port "$MAINFRAME_PORT" \
  --user "$MAINFRAME_USER" \
  --password "$MAINFRAME_PASSWORD" \
  --hlq "$MAINFRAME_DATASET_HLQ" \
  --source-dir "$ARTIFACTS_DIR/cobol" \
  --config "$DEPLOY_CONFIG/cobol-config.json"

# Deploy JCL procedures
echo "Deploying JCL procedures..."
python -m scripts.deploy.deploy_jcl \
  --env "$ENV" \
  --host "$MAINFRAME_HOST" \
  --port "$MAINFRAME_PORT" \
  --user "$MAINFRAME_USER" \
  --password "$MAINFRAME_PASSWORD" \
  --hlq "$MAINFRAME_DATASET_HLQ" \
  --source-dir "$ARTIFACTS_DIR/jcl" \
  --config "$DEPLOY_CONFIG/jcl-config.json"

# Update DB2 schemas if needed
if [[ -f "$DEPLOY_CONFIG/db2-config.json" ]]; then
  echo "Updating DB2 schemas..."
  python -m scripts.deploy.update_db2 \
    --env "$ENV" \
    --host "$MAINFRAME_HOST" \
    --port "$MAINFRAME_PORT" \
    --user "$MAINFRAME_USER" \
    --password "$MAINFRAME_PASSWORD" \
    --hlq "$MAINFRAME_DATASET_HLQ" \
    --source-dir "$ARTIFACTS_DIR/db2" \
    --config "$DEPLOY_CONFIG/db2-config.json"
fi

echo "Deployment to $ENV completed successfully"
```

### 5. Work Item Templates

Create custom work item templates for mainframe-specific tasks:

1. Navigate to Project Settings > Work > Process
2. Select your process template
3. Add a new work item type "Mainframe Change"
4. Configure fields:
   - Title: String (required)
   - Description: HTML (required)
   - Programs: String (list of affected programs)
   - Change Type: String (dropdown: New Feature, Enhancement, Bug Fix, Refactoring)
   - Risk Level: String (dropdown: Low, Medium, High)
   - Testing Requirements: HTML (testing instructions)
   - Deployment Instructions: HTML (deployment steps)
   - Rollback Plan: HTML (rollback instructions)

## Branching Strategy

Implement a branching strategy suitable for mainframe modernization:

### 1. Gitflow-Inspired Strategy

```
  main                 ──────────────────────────────────────────────▶
                       │                        │                    │
                       │                        │                    │
  release/v1.0         ─────────────────────────┼──▶                │
                       │                        │                    │
                       │                        │                    │
  develop              ────┬───────┬────────────┴──▶                │
                       │   │       │                                 │
  feature/customer     ────┼───▶   │                                 │
                       │   │       │                                 │
  feature/accounts     ────┼───────┼─▶                              │
                       │   │       │                                 │
  hotfix/v1.0.1        ────┼───────┼─────────────────────────────────┼▶
                       │   │       │                                 │
                       ◀───┴───────┴─────────────────────────────────┘
```

### 2. Branch Naming Conventions

- **main**: Production-ready code
- **develop**: Development integration branch
- **feature/\<name\>**: New features or enhancements
- **bugfix/\<name\>**: Bug fixes
- **hotfix/\<name\>**: Urgent production fixes
- **release/\<version\>**: Release preparation

### 3. Pull Request Process

1. Create a feature/bugfix branch from develop
2. Implement changes with proper testing
3. Create a pull request to merge back to develop
4. Require code reviews from at least 2 team members
5. Run automated testing through CI/CD pipeline
6. Merge to develop after approval
7. Periodically create release branches from develop
8. Test release branches thoroughly
9. Merge release branches to main with a tag
10. Hotfix branches are created from main for urgent fixes
11. Hotfixes are merged to both main and develop

## Code Review Guidelines

Establish code review guidelines specific to mainframe modernization:

### 1. COBOL Code Review Checklist

- [ ] Code adheres to established COBOL standards
- [ ] Variable names are meaningful and consistent
- [ ] Appropriate comments are included
- [ ] Error handling is implemented
- [ ] Efficiency considerations are addressed
- [ ] SQL statements are optimized (if applicable)
- [ ] Security concerns are addressed
- [ ] Testing procedures are documented

### 2. Code Review Process

1. Author submits pull request with detailed description
2. CI system runs automated checks
3. Reviewers analyze code for quality and correctness
4. Feedback is provided as comments on specific lines
5. Author addresses feedback with updates
6. Final approval given when all concerns are addressed
7. Code merged to target branch

## Next Steps

After setting up GitHub or Azure DevOps integration, proceed to:
- [Discovery and Assessment Phase](../02-discovery/README.md) to analyze your existing mainframe environment
- [AI-Powered Code Analysis](../05-code-analysis/README.md) to analyze your mainframe codebase 