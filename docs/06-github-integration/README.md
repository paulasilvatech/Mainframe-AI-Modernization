# GitHub Integration

This chapter provides technical guidance for integrating IBM z/OS mainframe modernization workflows with GitHub using Azure AI Foundry.

## Overview

GitHub integration enables modern DevOps practices for mainframe application development, providing version control, collaboration tools, automated workflows, and continuous integration capabilities. Azure AI Foundry enhances this integration with AI-powered capabilities specifically designed for mainframe code.

## Objectives

- Set up GitHub repositories for mainframe code management
- Configure appropriate Git settings for mainframe code files
- Implement GitHub Actions workflows for mainframe CI/CD
- Enable collaborative development practices for mainframe teams
- Integrate AI-powered code analysis and transformation with GitHub

## GitHub Repository Setup

A well-structured repository helps organize mainframe code effectively:

```
mainframe-app/
├── src/
│   ├── cobol/                  # COBOL source code
│   ├── jcl/                    # JCL procedures
│   └── copybooks/              # Copybooks
├── tests/                      # Test resources
├── docs/                       # Documentation
├── .github/workflows/          # GitHub Actions workflow definitions
├── .gitattributes              # Git attributes for mainframe files
└── README.md                   # Repository documentation
```

Configure Git for mainframe files by creating a `.gitattributes` file:

```
# Default handling of line endings
* text=auto

# COBOL source code
*.cbl text eol=lf working-tree-encoding=ibm1047 zos-working-tree-encoding=ibm1047
*.cpy text eol=lf working-tree-encoding=ibm1047 zos-working-tree-encoding=ibm1047

# JCL
*.jcl text eol=lf working-tree-encoding=ibm1047 zos-working-tree-encoding=ibm1047
```

## GitHub Actions Workflows

Create a basic mainframe CI workflow (`.github/workflows/mainframe-ci.yml`):

```yaml
name: Mainframe CI

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main, develop ]

jobs:
  build:
    runs-on: ubuntu-latest
    
    steps:
    - uses: actions/checkout@v3
    
    - name: AI-Powered Code Analysis
      uses: azure/ai-foundry-code-analysis@v1
      with:
        source-dir: ./src
        language: cobol
        output-dir: ./analysis-results
        
    - name: Compile COBOL Programs
      run: |
        for file in src/cobol/*.cbl; do
          cobol-compiler "$file" -I src/copybooks -o "$(basename "$file" .cbl).so"
        done
        
    - name: Run Tests
      run: |
        mainframe-test-runner ./tests
```

## Best Practices

1. **Incremental Migration**: Start with small, well-bounded components
2. **Consistent Environments**: Use development containers for consistent experiences
3. **Comprehensive Testing**: Implement automated testing for mainframe code
4. **Security First**: Protect mainframe credentials with secure practices
5. **Knowledge Sharing**: Document mainframe-specific workflows and considerations

## Next Steps

After setting up GitHub integration:

1. Implement [Azure DevOps Integration](../07-azure-devops-integration/README.md) for project management
2. Set up [AI-Powered Transformation](../08-ai-transformation/README.md) workflows
3. Establish comprehensive [CI/CD Implementation](../09-cicd-implementation/README.md)

## References

- [Azure AI Foundry Documentation](https://docs.microsoft.com/azure/ai-foundry)
- [GitHub Actions Documentation](https://docs.github.com/actions)
- [Mainframe DevOps Best Practices](https://learn.microsoft.com/azure/mainframe-migration/devops-best-practices)
