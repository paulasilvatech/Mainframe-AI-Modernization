# 🔄 Azure DevOps Integration

This chapter provides technical guidance for integrating IBM z/OS mainframe modernization with Azure DevOps using Azure AI Foundry.

## 📋 Overview

Azure DevOps integration enables comprehensive project management, build automation, testing, and deployment capabilities for mainframe modernization initiatives. By connecting mainframe development with modern DevOps tools, organizations can achieve greater agility, transparency, and collaboration throughout the modernization lifecycle.

## 🎯 Objectives

| Objective | Description |
|-----------|-------------|
| Project Structure | Set up Azure DevOps project structure for mainframe modernization |
| Repository Configuration | Configure repositories for mainframe code management |
| Pipeline Implementation | Implement Azure Pipelines for mainframe CI/CD |
| Work Item Tracking | Establish work item tracking for modernization activities |
| AI Integration | Integrate AI-powered capabilities with Azure DevOps workflows |

## 🏗️ Azure DevOps Project Setup

### Project Structure

1. **Create a New Azure DevOps Project**:

   Navigate to https://dev.azure.com and create a new project:
   - Name: `Mainframe-Modernization`
   - Description: `IBM z/OS modernization with Azure AI Foundry`
   - Visibility: Private (or as per organizational policy)
   - Version control: Git
   - Work item process: Agile (recommended for modernization projects)

2. **Configure Project Settings**:

   - Enable Azure Boards for work tracking
   - Enable Azure Repos for source code management
   - Enable Azure Pipelines for CI/CD
   - Enable Azure Test Plans for test management
   - Enable Azure Artifacts for package management

### Repository Setup

1. **Create Repositories**:

   ```bash
   # Initialize a local repository
   git init mainframe-modernization
   cd mainframe-modernization
   
   # Create basic structure
   mkdir -p src/{cobol,jcl,copybooks} tests docs pipelines
   
   # Create initial README
   echo "# Mainframe Modernization Project" > README.md
   
   # Create .gitattributes for mainframe files
   cat > .gitattributes << EOL
   # Default handling of line endings
   * text=auto
   
   # COBOL source code
   *.cbl text eol=lf working-tree-encoding=ibm1047 zos-working-tree-encoding=ibm1047
   *.cpy text eol=lf working-tree-encoding=ibm1047 zos-working-tree-encoding=ibm1047
   
   # JCL
   *.jcl text eol=lf working-tree-encoding=ibm1047 zos-working-tree-encoding=ibm1047
   EOL
   
   # Initialize repo
   git add .
   git commit -m "Initial project structure"
   
   # Add Azure DevOps remote
   git remote add origin https://dev.azure.com/your-org/Mainframe-Modernization/_git/mainframe-app
   git push -u origin main
   ```

2. **Branch Policies**:

   In Azure DevOps, navigate to Repos > Branches and set up policies for the main branch:
   - Require a minimum number of reviewers
   - Check for linked work items
   - Check for comment resolution
   - Build validation

## 📦 Azure Pipelines for Mainframe Development

### YAML Pipeline Setup

1. **Create a Basic CI Pipeline**:

   Create `azure-pipelines.yml` in the root of your repository:

   ```yaml
   trigger:
     branches:
       include:
         - main
         - develop
     paths:
       include:
         - src/cobol/**
         - src/jcl/**
         - src/copybooks/**
   
   pool:
     vmImage: 'ubuntu-latest'
   
   stages:
     - stage: Analysis
       jobs:
         - job: CodeAnalysis
           steps:
             - task: UseDotNet@2
               displayName: 'Install .NET Core'
               inputs:
                 packageType: 'sdk'
                 version: '6.x'
   
             - task: AzureAIFoundryCodeAnalysis@1
               displayName: 'Analyze Mainframe Code'
               inputs:
                 sourceDirectory: '$(Build.SourcesDirectory)/src'
                 language: 'cobol'
                 outputDirectory: '$(Build.ArtifactStagingDirectory)/analysis'
                 generateReport: true
   
             - task: PublishBuildArtifacts@1
               displayName: 'Publish Analysis Results'
               inputs:
                 pathToPublish: '$(Build.ArtifactStagingDirectory)/analysis'
                 artifactName: 'code-analysis'
   
     - stage: Build
       jobs:
         - job: CompileCode
           steps:
             - script: |
                 sudo apt-get update
                 sudo apt-get install -y gnucobol
               displayName: 'Install GnuCOBOL'
   
             - script: |
                 mkdir -p $(Build.BinariesDirectory)
                 for file in $(Build.SourcesDirectory)/src/cobol/*.cbl; do
                   cobc -x -o "$(Build.BinariesDirectory)/$(basename "$file" .cbl)" "$file" -I $(Build.SourcesDirectory)/src/copybooks
                 done
               displayName: 'Compile COBOL Programs'
   
             - task: PublishBuildArtifacts@1
               displayName: 'Publish Build Artifacts'
               inputs:
                 pathToPublish: '$(Build.BinariesDirectory)'
                 artifactName: 'compiled-programs'
   ```

2. **Set Up Release Pipeline**:

   Create a release pipeline in Azure DevOps:
   
   1. Go to Pipelines > Releases > New Pipeline
   2. Select Empty Job template
   3. Add artifact: Build pipeline
   4. Set up stages:
      - Development
      - Testing
      - Production
   5. Configure tasks for each stage

## 🧠 Integration with AI Foundry

### AI-Powered Code Analysis

Implement AI-powered code analysis in your Azure DevOps pipeline:

```yaml
steps:
- task: AzureAIFoundryCodeAnalysis@1
  displayName: 'AI Code Analysis'
  inputs:
    sourceDirectory: '$(Build.SourcesDirectory)/src'
    language: 'cobol'
    outputDirectory: '$(Build.ArtifactStagingDirectory)/analysis'
    extractBusinessRules: true
    qualityMetrics: true
    generateDocs: true
```

### Automated Test Generation

Generate tests based on code analysis:

```yaml
steps:
- task: AzureAIFoundryTestGenerator@1
  displayName: 'Generate Tests'
  inputs:
    sourceDirectory: '$(Build.SourcesDirectory)/src'
    analysisResults: '$(Build.ArtifactStagingDirectory)/analysis'
    outputDirectory: '$(Build.SourcesDirectory)/tests/generated'
    testFramework: 'junit'
    coverage: 'high'
```

## 📋 Work Item Tracking for Modernization

### Work Item Types

Configure work item tracking for modernization:

1. **Create Custom Work Item Types**:

   Customize the Agile process to include:
   
   | Work Item Type | Purpose |
   |----------------|---------|
   | Modernization Epic | High-level modernization initiative |
   | Mainframe Component | Represents a mainframe application or component |
   | Migration Task | Specific migration/modernization tasks |

2. **Set Up Migration Backlog**:

   Create a dedicated backlog for migration tasks with appropriate states:
   - Discovery
   - Analysis
   - Migration Design
   - Implementation
   - Testing
   - Production Validation

## 💡 Example: End-to-End Modernization Pipeline

```yaml
trigger:
  branches:
    include:
      - main
      - develop

pool:
  vmImage: 'ubuntu-latest'

stages:
  - stage: Analyze
    displayName: 'Analyze Mainframe Code'
    jobs:
      - job: CodeAnalysis
        steps:
          - task: AzureAIFoundryCodeAnalysis@1
            displayName: 'Analyze COBOL Code'
            inputs:
              sourceDirectory: '$(Build.SourcesDirectory)/src'
              language: 'cobol'
              outputDirectory: '$(Build.ArtifactStagingDirectory)/analysis'
              extractBusinessRules: true
              quality: true
          
          - task: PublishBuildArtifacts@1
            displayName: 'Publish Analysis Results'
            inputs:
              pathToPublish: '$(Build.ArtifactStagingDirectory)/analysis'
              artifactName: 'code-analysis'

  - stage: Transform
    displayName: 'Transform Code'
    dependsOn: Analyze
    jobs:
      - job: TransformCode
        steps:
          - task: DownloadBuildArtifacts@1
            inputs:
              buildType: 'current'
              downloadType: 'single'
              artifactName: 'code-analysis'
              downloadPath: '$(System.ArtifactsDirectory)'
          
          - task: AzureAIFoundryTransform@1
            displayName: 'Transform to Java'
            inputs:
              sourceDirectory: '$(Build.SourcesDirectory)/src'
              analysisInput: '$(System.ArtifactsDirectory)/code-analysis'
              targetLanguage: 'java'
              outputDirectory: '$(Build.ArtifactStagingDirectory)/transformed'
          
          - task: PublishBuildArtifacts@1
            displayName: 'Publish Transformed Code'
            inputs:
              pathToPublish: '$(Build.ArtifactStagingDirectory)/transformed'
              artifactName: 'transformed-code'

  - stage: Build
    displayName: 'Build Transformed Code'
    dependsOn: Transform
    jobs:
      - job: BuildJava
        steps:
          - task: DownloadBuildArtifacts@1
            inputs:
              buildType: 'current'
              downloadType: 'single'
              artifactName: 'transformed-code'
              downloadPath: '$(System.ArtifactsDirectory)'
          
          - task: Maven@3
            inputs:
              mavenPomFile: '$(System.ArtifactsDirectory)/transformed-code/pom.xml'
              goals: 'clean package'
              publishJUnitResults: true
              testResultsFiles: '**/surefire-reports/TEST-*.xml'
              javaHomeOption: 'JDKVersion'
              jdkVersionOption: '1.11'
          
          - task: PublishBuildArtifacts@1
            inputs:
              pathToPublish: '$(System.ArtifactsDirectory)/transformed-code/target'
              artifactName: 'java-package'

  - stage: Deploy
    displayName: 'Deploy Application'
    dependsOn: Build
    jobs:
      - deployment: Development
        environment: 'Development'
        strategy:
          runOnce:
            deploy:
              steps:
                - task: DownloadBuildArtifacts@1
                  inputs:
                    buildType: 'current'
                    downloadType: 'single'
                    artifactName: 'java-package'
                    downloadPath: '$(System.ArtifactsDirectory)'
                
                - task: AzureWebApp@1
                  inputs:
                    azureSubscription: 'Azure Subscription'
                    appType: 'webAppLinux'
                    appName: 'modernized-mainframe-app'
                    package: '$(System.ArtifactsDirectory)/java-package/*.jar'
```

## ✅ Best Practices

| Practice | Description |
|----------|-------------|
| Standardized Pipelines | Create reusable templates for common mainframe operations |
| Infrastructure as Code | Manage all pipeline configurations in YAML |
| Security-First Approach | Use secure practices for mainframe credentials |
| Work Item Tracking | Link all code changes to work items for traceability |
| Artifact Management | Maintain consistent artifact management across environments |

## ➡️ Next Steps

After setting up Azure DevOps integration:

1. Implement [🧠 AI-Powered Transformation](../08-ai-transformation/README.md) processes
2. Set up comprehensive [📦 CI/CD Implementation](../09-cicd-implementation/README.md)
3. Implement [⚠️ AI-Powered Risk Management](../10-risk-management/README.md)
4. Establish [🔄 Hybrid Operations Management](../11-hybrid-operations/README.md)

## 📚 References

| Resource | Description |
|----------|-------------|
| [Azure DevOps Documentation](https://docs.microsoft.com/azure/devops) | Official Azure DevOps documentation |
| [Azure Pipelines YAML Reference](https://docs.microsoft.com/azure/devops/pipelines/yaml-schema) | Reference for Azure Pipelines YAML schema |
| [Mainframe DevOps Best Practices](https://learn.microsoft.com/azure/mainframe-migration/devops-best-practices) | Best practices for mainframe DevOps |
| [Azure AI Foundry Documentation](https://docs.microsoft.com/azure/ai-foundry) | Documentation for Azure AI Foundry | 