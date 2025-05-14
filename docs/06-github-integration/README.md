# 🐙 GitHub Integration

This chapter provides technical guidance for integrating mainframe modernization workflows with GitHub using Azure AI Foundry, supporting multiple mainframe platforms (IBM z/OS, Unisys ClearPath, Bull GCOS, NEC ACOS).

## 📋 Overview

GitHub integration enables modern DevOps practices for mainframe application development, providing version control, collaboration tools, automated workflows, and continuous integration capabilities. Azure AI Foundry enhances this integration with AI-powered capabilities specifically designed for mainframe code across different platforms.

## 🎯 Objectives

| Objective | Description |
|-----------|-------------|
| Repository Setup | Set up GitHub repositories for mainframe code management |
| Git Configuration | Configure appropriate Git settings for mainframe code files |
| Workflows | Implement GitHub Actions workflows for mainframe CI/CD |
| Collaboration | Enable collaborative development practices for mainframe teams |
| AI Integration | Integrate AI-powered code analysis and transformation with GitHub |
| Multi-Platform Support | Support various mainframe platforms with specific configurations |

## 📦 GitHub Repository Setup

A well-structured repository helps organize mainframe code effectively:

```
mainframe-app/
├── src/
│   ├── cobol/                  # COBOL source code
│   ├── pl1/                    # PL/I source code
│   ├── natural/                # Natural source code
│   ├── jcl/                    # JCL procedures
│   ├── copybooks/              # Copybooks
│   └── platform-specific/      # Platform-specific code
│       ├── zos/                # IBM z/OS specific code
│       ├── clearpath/          # Unisys ClearPath specific code
│       ├── gcos/               # Bull GCOS specific code
│       └── acos/               # NEC ACOS specific code
├── tests/                      # Test resources
├── docs/                       # Documentation
├── .github/workflows/          # GitHub Actions workflow definitions
├── .gitattributes              # Git attributes for mainframe files
└── README.md                   # Repository documentation
```

Configure Git for mainframe files by creating a `.gitattributes` file with platform-specific settings:

```
# Default handling of line endings
* text=auto

# IBM z/OS specific files
*.cbl text eol=lf working-tree-encoding=ibm1047 zos-working-tree-encoding=ibm1047
*.cpy text eol=lf working-tree-encoding=ibm1047 zos-working-tree-encoding=ibm1047
*.jcl text eol=lf working-tree-encoding=ibm1047 zos-working-tree-encoding=ibm1047

# Unisys ClearPath specific files
*.cob text eol=lf
*.wrk text eol=lf

# Bull GCOS specific files
*.gcos text eol=lf

# NEC ACOS specific files
*.acos text eol=lf
```

## 🔄 GitHub Actions Workflows

Create a basic mainframe CI workflow with platform-specific jobs (`.github/workflows/mainframe-ci.yml`):

```yaml
name: Mainframe CI

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main, develop ]

jobs:
  analyze:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        platform: [zos, clearpath, gcos, acos]
    
    steps:
    - uses: actions/checkout@v3
    
    - name: AI-Powered Code Analysis
      uses: azure/ai-foundry-code-analysis@v1
      with:
        source-dir: ./src
        platform: ${{ matrix.platform }}
        language: auto-detect
        output-dir: ./analysis-results/${{ matrix.platform }}
  
  build:
    needs: analyze
    runs-on: ubuntu-latest
    
    steps:
    - uses: actions/checkout@v3
    
    - name: Download Analysis Results
      uses: actions/download-artifact@v3
      with:
        name: analysis-results
        path: ./analysis-results
        
    - name: Compile Programs
      run: |
        # Platform-specific compilation commands
        if [ -d "src/cobol" ]; then
          echo "Compiling COBOL programs..."
          for file in src/cobol/*.cbl; do
            cobol-compiler "$file" -I src/copybooks -o "$(basename "$file" .cbl).so"
          done
        fi
        
        if [ -d "src/pl1" ]; then
          echo "Compiling PL/I programs..."
          for file in src/pl1/*.pli; do
            pli-compiler "$file" -o "$(basename "$file" .pli).so"
          done
        fi
        
    - name: Run Tests
      run: |
        mainframe-test-runner ./tests
```

## 🔍 Platform-Specific Considerations

### IBM z/OS Integration

For IBM z/OS integration, consider:
- EBCDIC to ASCII conversions
- Dataset naming conventions
- JCL handling
- COBOL copybook management

### Unisys ClearPath Integration

For Unisys ClearPath integration, focus on:
- MCP or OS 2200 specific configurations
- COBOL and Algol source handling
- WFL workflow definitions
- DMSII database integration

### Bull GCOS Integration

For Bull GCOS integration, address:
- JCL equivalents
- TP8 considerations
- COBOL dialect differences
- Database access methods

### NEC ACOS Integration

For NEC ACOS integration, manage:
- ACOS-specific language features
- Transaction processing integration
- Database access patterns
- Special character handling

## ✅ Best Practices

| Practice | Description |
|----------|-------------|
| Incremental Migration | Start with small, well-bounded components |
| Consistent Environments | Use development containers for consistent experiences |
| Comprehensive Testing | Implement automated testing for mainframe code |
| Security First | Protect mainframe credentials with secure practices |
| Knowledge Sharing | Document mainframe-specific workflows and considerations |
| Platform-Specific Branching | Consider separate branches for platform-specific code |
| Multi-Platform Testing | Test across platforms for cross-platform components |

## ➡️ Next Steps

After setting up GitHub integration:

1. Implement [🔄 Azure DevOps Integration](../07-azure-devops-integration/README.md) for project management
2. Set up [🧠 AI-Powered Transformation](../08-ai-transformation/README.md) workflows
3. Establish comprehensive [📦 CI/CD Implementation](../09-cicd-implementation/README.md)

## 📚 References

| Resource | Description |
|----------|-------------|
| [Azure AI Foundry Documentation](https://docs.microsoft.com/azure/ai-foundry) | Official documentation for Azure AI Foundry |
| [GitHub Actions Documentation](https://docs.github.com/actions) | Documentation for GitHub Actions |
| [Mainframe DevOps Best Practices](https://learn.microsoft.com/azure/mainframe-migration/devops-best-practices) | Best practices for mainframe DevOps |
