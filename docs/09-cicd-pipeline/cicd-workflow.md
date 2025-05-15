# CI/CD Pipeline Workflow

This document provides a visual representation of the CI/CD pipeline workflow for mainframe modernization across multiple platforms (IBM z/OS, Unisys ClearPath, Bull GCOS, and NEC ACOS).

## Pipeline Architecture

The following diagram illustrates the CI/CD pipeline architecture:

```mermaid
graph TD
    subgraph "CI/CD Pipeline"
        A["Source Code"] --> B["AI Analysis"]
        B --> C["Platform-Specific Build"]
        C --> D["Automated Testing"]
        C --> E["Security Scan"]
        D --> F["Deployment"]
        E --> F
    end
    
    subgraph "Platform-Specific Components"
        P1["IBM z/OS\n(COBOL, PL/I, JCL)"]
        P2["Unisys ClearPath\n(COBOL, Algol, WFL)"]
        P3["Bull GCOS\n(COBOL, GCL)"]
        P4["NEC ACOS\n(COBOL, NCL)"]
    end
    
    C --> Platform-Specific Components
    Platform-Specific Components --> D
```

## CI/CD Pipeline Stages

Each stage of the pipeline performs specific functions:

### 1. AI Analysis
- Analyzes source code changes using Azure AI Foundry
- Identifies potential impacts and risks
- Generates transformation recommendations
- Ensures platform-specific code quality

### 2. Platform-Specific Build
- Compiles mainframe code for the appropriate platform
- Generates deployable artifacts
- Validates platform-specific syntax
- Creates platform-appropriate deployment packages

### 3. Automated Testing
- Executes unit tests for each component
- Performs integration testing
- Validates business functionality
- Ensures cross-platform compatibility

### 4. Security Scan
- Identifies security vulnerabilities
- Validates compliance requirements
- Analyzes code for security best practices
- Platform-specific security validation

### 5. Deployment
- Deploys to appropriate mainframe environment
- Implements deployment strategy (blue-green, canary, rolling)
- Performs post-deployment verification
- Enables automated rollback if needed

## Cross-Platform Considerations

The CI/CD pipeline accommodates platform-specific requirements while maintaining a consistent workflow:

| Platform | Build Considerations | Testing Approach | Deployment Method |
|----------|---------------------|-----------------|-------------------|
| IBM z/OS | COBOL/PL/I compilation, JCL validation | CICS/Batch testing | Blue-green with Sysplex |
| Unisys ClearPath | COBOL/Algol compilation, WFL validation | COMS/Batch testing | Rolling deployment |
| Bull GCOS | COBOL compilation, GCL validation | TP8/Batch testing | Blue-green with redundancy |
| NEC ACOS | COBOL compilation, NCL validation | AIM/DC testing | Canary release |

## Implementation

See the [mainframe-cicd-workflow.yml](../../templates/workflows/mainframe-cicd-workflow.yml) template for the implementation details of this pipeline.

## Next Steps

- Configure the CI/CD pipeline for your specific mainframe environment
- Customize the pipeline stages based on your modernization approach
- Implement platform-specific validation checks
- Integrate with your existing DevOps toolchain 