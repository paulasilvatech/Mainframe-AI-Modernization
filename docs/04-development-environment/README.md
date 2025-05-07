# Development Environment Configuration

This chapter provides technical guidance for setting up development environments for IBM z/OS mainframe modernization using Azure AI Foundry.

## Overview

An effective development environment is crucial for mainframe modernization success. It enables developers to work efficiently with both legacy mainframe code and modern cloud technologies. This chapter outlines the setup and configuration of development environments that support the entire modernization lifecycle.

## Objectives

- Establish development environments for mainframe code
- Configure modern IDE tooling with mainframe language support
- Set up local testing capabilities for mainframe applications
- Implement consistent development environments across teams
- Integrate Azure AI Foundry tools into the development workflow

## Development Environment Types

### 1. Local Development Environment

**Description**: Developer workstation with all necessary tools and extensions

**Components**:
- Modern IDE (VS Code, Eclipse, etc.)
- Mainframe language extensions
- Local compilation and testing tools
- Source control client
- Cloud development tools
- Azure AI Foundry CLI/extensions

**Best for**:
- Individual developer productivity
- Offline development scenarios
- Lightweight development tasks

### 2. Containerized Development Environment

**Description**: Docker containers with preconfigured development tools

**Components**:
- Base development container
- Language-specific extensions
- Preconfigured tool chains
- Volume mounts for local code
- Network configuration for mainframe access
- CI/CD integration

**Best for**:
- Consistent developer experiences
- Onboarding new team members
- Avoiding "works on my machine" issues

### 3. Cloud-Based Development Environment

**Description**: Development environments hosted in Azure

**Components**:
- Azure DevBox
- GitHub Codespaces
- Virtual machine-based development
- Preconfigured extensions and tools
- Persistent storage
- Network connectivity to mainframe

**Best for**:
- Distributed teams
- High-performance requirements
- Secure mainframe access

## Local Development Environment Setup

### Visual Studio Code Setup

1. **Install Visual Studio Code**:

   Download and install from [https://code.visualstudio.com/](https://code.visualstudio.com/)

2. **Install Required Extensions**:

   ```bash
   # Install VS Code extensions for mainframe development
   code --install-extension IBM.cobol
   code --install-extension broadcomMFD.code4z-extension-pack
   code --install-extension Microsoft.azure-ai-foundry-tools
   code --install-extension ms-azuretools.vscode-docker
   code --install-extension GitHub.vscode-github-actions
   ```

3. **Configure Mainframe Language Settings**:

   Create `.vscode/settings.json` in your project:

   ```json
   {
     "cobol.copybookdirs": [
       "${workspaceFolder}/src/copybooks"
     ],
     "cobol.format": true,
     "cobol.margin": {
       "leftA": 7,
       "leftB": 11,
       "right": 72
     },
     "cobol.tabstops": [7, 11, 15, 19, 23, 27, 31, 35, 39, 43, 47, 51, 55, 59, 63, 67, 71],
     "editor.rulers": [7, 11, 72],
     "files.associations": {
       "*.cbl": "cobol",
       "*.cpy": "cobol",
       "*.jcl": "jcl"
     },
     "terminal.integrated.env.windows": {
       "COBOL_COMPILER_PATH": "C:\\gnucobol\\bin"
     },
     "terminal.integrated.env.linux": {
       "COBOL_COMPILER_PATH": "/usr/bin"
     },
     "terminal.integrated.env.osx": {
       "COBOL_COMPILER_PATH": "/usr/local/bin"
     }
   }
   ```

4. **Install COBOL Compiler**:

   For Windows:
   - Download GnuCOBOL from [https://gnucobol.sourceforge.io/](https://gnucobol.sourceforge.io/)
   - Install to a known location (e.g., C:\gnucobol)
   - Add to PATH environment variable

   For macOS:
   ```bash
   brew install gnu-cobol
   ```

   For Linux:
   ```bash
   sudo apt-get update
   sudo apt-get install -y gnucobol
   ```

5. **Install Azure AI Foundry CLI**:

   ```bash
   npm install -g @azure/ai-foundry-cli
   
   # Configure with your Azure credentials
   az login
   az account set --subscription <subscription-id>
   ai-foundry configure
   ```

### Eclipse Setup

1. **Install Eclipse**:

   Download and install Eclipse IDE from [https://www.eclipse.org/downloads/](https://www.eclipse.org/downloads/)

2. **Install IBM Z Open Editor**:

   1. In Eclipse, go to Help > Eclipse Marketplace
   2. Search for "IBM Z Open Editor"
   3. Install the plugin
   4. Restart Eclipse when prompted

3. **Configure Z Open Editor**:

   1. Go to Window > Preferences > IBM Z Open Editor
   2. Set paths for copybooks and include directories
   3. Configure COBOL syntax highlighting preferences

4. **Install Local Compiler**:

   Follow the same steps as for VS Code to install GnuCOBOL

5. **Configure Launch Configurations**:

   1. Right-click project > Run As > Run Configurations
   2. Create a new configuration for COBOL applications
   3. Set compiler path and arguments

## Containerized Development Environment

### Development Container Setup

1. **Create Dockerfile**:

   Create `devcontainer/Dockerfile`:

   ```dockerfile
   FROM ubuntu:20.04
   
   # Avoid prompts from apt
   ENV DEBIAN_FRONTEND=noninteractive
   
   # Install required packages
   RUN apt-get update && apt-get install -y \
       curl \
       git \
       gnupg2 \
       gnucobol \
       openjdk-11-jdk \
       maven \
       nodejs \
       npm \
       python3 \
       python3-pip \
       wget \
       unzip \
       && apt-get clean \
       && rm -rf /var/lib/apt/lists/*
   
   # Install Azure CLI
   RUN curl -sL https://aka.ms/InstallAzureCLIDeb | bash
   
   # Install Azure AI Foundry CLI
   RUN npm install -g @azure/ai-foundry-cli
   
   # Set up workspace directory
   WORKDIR /workspace
   
   # Configure environment
   ENV JAVA_HOME=/usr/lib/jvm/java-11-openjdk-amd64
   ENV PATH=$PATH:$JAVA_HOME/bin
   
   # Create non-root user for development
   ARG USERNAME=developer
   ARG USER_UID=1000
   ARG USER_GID=$USER_UID
   
   RUN groupadd --gid $USER_GID $USERNAME \
       && useradd --uid $USER_UID --gid $USER_GID -m $USERNAME \
       && mkdir -p /home/$USERNAME/.vscode-server /home/$USERNAME/.vscode-server-insiders \
       && chown -R $USERNAME:$USERNAME /home/$USERNAME
   
   # Set the default user
   USER $USERNAME
   ```

2. **Create devcontainer.json**:

   Create `devcontainer/devcontainer.json`:

   ```json
   {
     "name": "Mainframe Modernization",
     "build": {
       "dockerfile": "Dockerfile",
       "context": ".."
     },
     "settings": {
       "terminal.integrated.shell.linux": "/bin/bash",
       "cobol.copybookdirs": [
         "${workspaceFolder}/src/copybooks"
       ],
       "cobol.format": true,
       "cobol.margin": {
         "leftA": 7,
         "leftB": 11,
         "right": 72
       },
       "editor.rulers": [7, 11, 72],
       "files.associations": {
         "*.cbl": "cobol",
         "*.cpy": "cobol",
         "*.jcl": "jcl"
       }
     },
     "extensions": [
       "IBM.cobol",
       "broadcomMFD.code4z-extension-pack",
       "Microsoft.azure-ai-foundry-tools",
       "ms-azuretools.vscode-docker",
       "GitHub.vscode-github-actions",
       "ms-vscode.azurecli"
     ],
     "forwardPorts": [],
     "remoteUser": "developer",
     "postCreateCommand": "ai-foundry configure --defaults"
   }
   ```

3. **Use with Visual Studio Code**:

   1. Install the Remote - Containers extension
   2. Open the folder containing your devcontainer configuration
   3. Click "Reopen in Container" when prompted

### Docker Compose Setup

For more complex environments with multiple services:

1. **Create docker-compose.yml**:

   ```yaml
   version: '3'
   services:
     dev:
       build:
         context: .
         dockerfile: devcontainer/Dockerfile
       volumes:
         - .:/workspace
         - ~/.azure:/home/developer/.azure
       command: sleep infinity
       environment:
         - AZURE_SUBSCRIPTION_ID=${AZURE_SUBSCRIPTION_ID}
     
     db2:
       image: ibmcom/db2
       environment:
         - LICENSE=accept
         - DB2INSTANCE=db2inst1
         - DB2INST1_PASSWORD=password
         - DBNAME=testdb
       ports:
         - "50000:50000"
       volumes:
         - db2data:/database
   
   volumes:
     db2data:
   ```

2. **Start the Environment**:

   ```bash
   docker-compose up -d
   docker-compose exec dev bash
   ```

## Cloud-Based Development Environment

### Azure DevBox Setup

1. **Create DevBox Definition**:

   In Azure Portal:
   1. Navigate to Azure DevBox
   2. Create a new DevBox Definition
   3. Select appropriate VM size (recommended: at least 4 cores, 16GB RAM)
   4. Add required software packages:
      - Visual Studio Code
      - Git
      - GnuCOBOL
      - Azure CLI
      - Azure AI Foundry CLI
      - Java JDK 11
      - Docker Desktop

2. **Create DevBox Pool**:

   1. Create a new pool using the definition
   2. Set appropriate network settings
   3. Configure access controls

3. **Provision DevBox for Developers**:

   1. Assign DevBox to team members
   2. Set up auto-shutdown and scaling policies
   3. Configure image update policies

### GitHub Codespaces Setup

1. **Create Codespaces Configuration**:

   Add `.devcontainer/devcontainer.json` to your repository:

   ```json
   {
     "name": "Mainframe Modernization",
     "build": {
       "dockerfile": "Dockerfile",
       "context": ".."
     },
     "settings": {
       "terminal.integrated.shell.linux": "/bin/bash",
       "cobol.copybookdirs": [
         "${workspaceFolder}/src/copybooks"
       ],
       "cobol.format": true,
       "editor.rulers": [7, 11, 72],
       "files.associations": {
         "*.cbl": "cobol",
         "*.cpy": "cobol",
         "*.jcl": "jcl"
       }
     },
     "extensions": [
       "IBM.cobol",
       "broadcomMFD.code4z-extension-pack",
       "Microsoft.azure-ai-foundry-tools",
       "ms-azuretools.vscode-docker",
       "GitHub.vscode-github-actions"
     ],
     "forwardPorts": [],
     "remoteUser": "codespace",
     "postCreateCommand": "ai-foundry configure --defaults"
   }
   ```

2. **Use Codespaces**:

   1. Navigate to your repository on GitHub
   2. Click the "Code" button
   3. Select "Open with Codespaces"
   4. Create a new codespace

## Mainframe Connectivity Setup

### Secure Credential Management

1. **Set Up Azure Key Vault**:

   ```bash
   # Create a Key Vault
   az keyvault create --name mainframe-credentials --resource-group mainframe-modernization
   
   # Add mainframe credentials
   az keyvault secret set --vault-name mainframe-credentials --name mainframe-username --value "your-username"
   az keyvault secret set --vault-name mainframe-credentials --name mainframe-password --value "your-password"
   ```

2. **Access Credentials in Development**:

   ```javascript
   // Example: Accessing credentials in Node.js
   const { DefaultAzureCredential } = require("@azure/identity");
   const { SecretClient } = require("@azure/keyvault-secrets");
   
   async function getMainframeCredentials() {
     const credential = new DefaultAzureCredential();
     const secretClient = new SecretClient(
       "https://mainframe-credentials.vault.azure.net/",
       credential
     );
   
     const username = await secretClient.getSecret("mainframe-username");
     const password = await secretClient.getSecret("mainframe-password");
     
     return {
       username: username.value,
       password: password.value
     };
   }
   ```

### Secure Connectivity Configuration

1. **Set Up Azure Private Link**:

   Configure secure connectivity to on-premises mainframe:
   
   1. Create Azure Virtual Network
   2. Set up Azure ExpressRoute or VPN Gateway
   3. Configure Private Endpoint for secure connectivity
   4. Set up network security rules

2. **Configure Local Development Connection**:

   Create a connection profile in `.mainframe-connection.json`:

   ```json
   {
     "connections": [
       {
         "name": "development-mainframe",
         "host": "mainframe.internal.corp",
         "port": 23,
         "type": "tn3270",
         "useKeyVault": true,
         "keyVault": {
           "name": "mainframe-credentials",
           "usernameSecret": "mainframe-username",
           "passwordSecret": "mainframe-password"
         },
         "fallbackCredentials": {
           "usernameEnvironmentVariable": "MAINFRAME_USERNAME",
           "passwordEnvironmentVariable": "MAINFRAME_PASSWORD"
         }
       }
     ]
   }
   ```

## Azure AI Foundry Integration

### Local AI Foundry Tools Setup

1. **Install AI Foundry CLI**:

   ```bash
   npm install -g @azure/ai-foundry-cli
   
   # Configure with your Azure credentials
   az login
   az account set --subscription <subscription-id>
   ai-foundry configure
   ```

2. **Initialize a Project**:

   ```bash
   # Initialize a new AI Foundry project
   ai-foundry init --name mainframe-modernization --type cobol-modernization
   
   # Configure analysis settings
   ai-foundry config set analysis.languages cobol,jcl
   ai-foundry config set analysis.extractBusinessRules true
   ai-foundry config set analysis.generateDocumentation true
   ```

3. **Set Up VS Code Integration**:

   Install the Azure AI Foundry extension from the VS Code marketplace

### IDE Extensions for AI-Powered Development

1. **AI-Powered Code Assistance**:

   Configure `.vscode/settings.json` for AI assistance:

   ```json
   {
     "ai-foundry.assistant.enabled": true,
     "ai-foundry.assistant.languages": ["cobol", "jcl"],
     "ai-foundry.assistant.features": {
       "codeCompletion": true,
       "syntaxHelp": true,
       "businessRuleExtraction": true,
       "transformationSuggestions": true
     }
   }
   ```

2. **Business Rule Visualization**:

   Set up business rule visualization in VS Code:

   ```json
   {
     "ai-foundry.businessRules.visualization": "inline",
     "ai-foundry.businessRules.highlight": true,
     "ai-foundry.businessRules.extractOnSave": true
   }
   ```

## Local Testing Configuration

### Setting Up COBOL Unit Testing

1. **Install COBOL Unit Test Framework**:

   ```bash
   # Clone the repository
   git clone https://github.com/openmainframeproject/cobol-unit-test.git
   cd cobol-unit-test
   
   # Install dependencies
   npm install
   
   # Build the framework
   npm run build
   
   # Link to make available globally
   npm link
   ```

2. **Create a Test Configuration**:

   Create `test-config.json` in your project:

   ```json
   {
     "testRunner": "cobol-unit-test",
     "testDirectory": "tests",
     "sourceDirectory": "src/cobol",
     "copybooks": "src/copybooks",
     "testDataDirectory": "tests/data",
     "reportFormat": "junit"
   }
   ```

3. **Create a Sample Test**:

   Create `tests/CUSTTEST.cbl`:

   ```cobol
   IDENTIFICATION DIVISION.
   PROGRAM-ID. CUSTTEST.
   
   ENVIRONMENT DIVISION.
   CONFIGURATION SECTION.
   
   DATA DIVISION.
   WORKING-STORAGE SECTION.
   01 CUST-ID               PIC X(6) VALUE "C00001".
   01 EXPECTED-NAME         PIC X(30) VALUE "CUSTOMER ONE".
   01 ACTUAL-NAME           PIC X(30).
   01 EXPECTED-STATUS       PIC X(1) VALUE "A".
   01 ACTUAL-STATUS         PIC X(1).
   
   PROCEDURE DIVISION.
   MAIN-PROCEDURE.
       DISPLAY "TESTING CUSTOMER LOOKUP".
       
       CALL 'CUSTMGMT' USING CUST-ID, ACTUAL-NAME, ACTUAL-STATUS.
       
       IF ACTUAL-NAME = EXPECTED-NAME
          DISPLAY "TEST PASSED: CUSTOMER NAME"
       ELSE
          DISPLAY "TEST FAILED: CUSTOMER NAME"
          DISPLAY "  EXPECTED: " EXPECTED-NAME
          DISPLAY "  ACTUAL: " ACTUAL-NAME
       END-IF.
       
       IF ACTUAL-STATUS = EXPECTED-STATUS
          DISPLAY "TEST PASSED: CUSTOMER STATUS"
       ELSE
          DISPLAY "TEST FAILED: CUSTOMER STATUS"
          DISPLAY "  EXPECTED: " EXPECTED-STATUS
          DISPLAY "  ACTUAL: " ACTUAL-STATUS
       END-IF.
       
       STOP RUN.
   ```

4. **Run Tests**:

   ```bash
   cobol-unit-test run --config test-config.json
   ```

### Setting Up JCL Testing

1. **Install JCL Simulator**:

   ```bash
   # Clone the repository
   git clone https://github.com/openmainframeproject/jcl-simulator.git
   cd jcl-simulator
   
   # Install dependencies
   npm install
   
   # Build the simulator
   npm run build
   
   # Link to make available globally
   npm link
   ```

2. **Create a JCL Test Configuration**:

   Create `jcl-test-config.json`:

   ```json
   {
     "jclDirectory": "src/jcl",
     "procDirectory": "src/proc",
     "datasetDirectory": "tests/datasets",
     "outputDirectory": "tests/outputs",
     "mainframeEnv": {
       "simulateJES": true,
       "simulateDBAccess": true,
       "simulateTPAccess": false
     }
   }
   ```

3. **Run JCL Tests**:

   ```bash
   jcl-simulator run --job CUSTJOB --config jcl-test-config.json
   ```

## Continuous Integration Setup

### Local CI Pipeline

1. **Create a Local CI Script**:

   Create `local-ci.sh`:

   ```bash
   #!/bin/bash
   
   # Local CI script for mainframe modernization
   
   # Run COBOL syntax check
   echo "Running COBOL syntax check..."
   for file in src/cobol/*.cbl; do
     cobc -fsyntax-only -I src/copybooks "$file"
     if [ $? -ne 0 ]; then
       echo "Syntax error in $file"
       exit 1
     fi
   done
   
   # Run JCL syntax check
   echo "Running JCL syntax check..."
   for file in src/jcl/*.jcl; do
     jcl-simulator check "$file"
     if [ $? -ne 0 ]; then
       echo "Syntax error in $file"
       exit 1
     fi
   done
   
   # Run AI Foundry code analysis
   echo "Running AI Foundry code analysis..."
   ai-foundry analyze --source-dir src --output-dir analysis-results
   
   # Run unit tests
   echo "Running unit tests..."
   cobol-unit-test run --config test-config.json
   
   echo "CI checks completed successfully!"
   ```

2. **Make the Script Executable**:

   ```bash
   chmod +x local-ci.sh
   ```

3. **Run Local CI**:

   ```bash
   ./local-ci.sh
   ```

### Pre-commit Hooks

1. **Install Pre-commit**:

   ```bash
   pip install pre-commit
   ```

2. **Create Pre-commit Configuration**:

   Create `.pre-commit-config.yaml`:

   ```yaml
   repos:
   - repo: local
     hooks:
     - id: cobol-syntax
       name: COBOL Syntax Check
       entry: bash -c 'cobc -fsyntax-only -I src/copybooks "$FILE"'
       language: system
       files: \.cbl$
     
     - id: jcl-syntax
       name: JCL Syntax Check
       entry: bash -c 'jcl-simulator check "$FILE"'
       language: system
       files: \.jcl$
     
     - id: ai-foundry-analysis
       name: AI Foundry Analysis
       entry: bash -c 'ai-foundry analyze --source-file "$FILE" --quick'
       language: system
       files: \.(cbl|jcl)$
   ```

3. **Install the Hooks**:

   ```bash
   pre-commit install
   ```

## Troubleshooting

| Issue | Resolution |
|-------|------------|
| COBOL compiler not found | Verify installation path and environment variables |
| Character encoding issues | Check `.gitattributes` and editor encoding settings |
| VS Code extensions not activating | Ensure file associations are properly configured |
| Azure AI Foundry CLI auth errors | Run `az login` and verify subscription access |
| Containerized environment connectivity issues | Check network configurations and exposed ports |

## Best Practices

1. **Consistency First**: Use containerized or cloud-based environments to ensure consistency
2. **Secure Credential Handling**: Never store mainframe credentials in code or containers
3. **Local Testing**: Implement comprehensive local testing before pushing to shared environments
4. **Pre-commit Validation**: Use pre-commit hooks to catch issues early
5. **Shared Configurations**: Use version-controlled configuration files for development tools

## Next Steps

After setting up your development environment:

1. Set up [AI-Powered Code Analysis](../05-code-analysis/README.md)
2. Implement [GitHub Integration](../06-github-integration/README.md) or [Azure DevOps Integration](../07-azure-devops-integration/README.md)
3. Begin [AI-Powered Transformation](../08-ai-transformation/README.md) of your mainframe applications

## References

- [Visual Studio Code Documentation](https://code.visualstudio.com/docs)
- [GitHub Codespaces Documentation](https://docs.github.com/en/codespaces)
- [Azure DevBox Documentation](https://docs.microsoft.com/azure/dev-box)
- [Docker Development Environments](https://docs.docker.com/develop)
- [GnuCOBOL Documentation](https://gnucobol.sourceforge.io/guides/GNU%20COBOL%202.0%20Programmers%20Guide.pdf) 