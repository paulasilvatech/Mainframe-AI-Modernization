# 🚀 Quick Start Guide

Get up and running with the Mainframe Modernization Workshop in under 30 minutes!

## 📋 Table of Contents

- [Prerequisites Check](#-prerequisites-check)
- [One-Command Setup](#-one-command-setup)
- [Manual Setup Steps](#-manual-setup-steps)
- [Verify Installation](#-verify-installation)
- [Choose Your Workshop](#-choose-your-workshop)
- [Common Issues](#-common-issues)
- [Next Steps](#-next-steps)

## ✅ Prerequisites Check

Run this command to check if you have all prerequisites:

```bash
curl -fsSL https://raw.githubusercontent.com/mainframe-modernization/workshops/main/scripts/check-prerequisites.sh | bash
```

Expected output:
```
✓ Docker installed (version 20.10.21)
✓ Python installed (version 3.10.8)
✓ Java installed (version 17.0.5)
✓ Git installed (version 2.38.1)
✓ VS Code installed
✓ Azure CLI installed (version 2.44.0)
✓ GitHub CLI installed (version 2.20.2)

All prerequisites satisfied! ✨
```

## 🎯 One-Command Setup

For the fastest setup, run our automated installer:

### macOS/Linux
```bash
curl -fsSL https://raw.githubusercontent.com/mainframe-modernization/workshops/main/scripts/setup.sh | bash
```

### Windows (PowerShell as Administrator)
```powershell
iex ((New-Object System.Net.WebClient).DownloadString('https://raw.githubusercontent.com/mainframe-modernization/workshops/main/scripts/setup.ps1'))
```

This script will:
- ✅ Clone the workshop repository
- ✅ Create Python virtual environment
- ✅ Install all dependencies
- ✅ Pull required Docker images
- ✅ Set up environment variables
- ✅ Configure VS Code extensions

## 🔧 Manual Setup Steps

If you prefer manual setup or the automated script fails:

### 1. Clone the Repository

```bash
git clone https://github.com/mainframe-modernization/workshops.git
cd workshops
```

### 2. Set Up Python Environment

```bash
# Create virtual environment
python -m venv venv

# Activate virtual environment
# On macOS/Linux:
source venv/bin/activate
# On Windows:
.\venv\Scripts\activate

# Install dependencies
pip install -r requirements.txt
```

### 3. Configure Azure Credentials

```bash
# Login to Azure
az login

# Set your subscription (replace with your subscription ID)
az account set --subscription "YOUR_SUBSCRIPTION_ID"

# Create service principal for workshop
az ad sp create-for-rbac --name "mainframe-workshop-sp" --role contributor \
    --scopes /subscriptions/YOUR_SUBSCRIPTION_ID \
    --sdk-auth > azure-credentials.json
```

### 4. Set Up Environment Variables

```bash
# Copy environment template
cp .env.example .env

# Edit .env file with your credentials
# Required variables:
# - AZURE_SUBSCRIPTION_ID
# - AZURE_TENANT_ID
# - AZURE_CLIENT_ID
# - AZURE_CLIENT_SECRET
# - GITHUB_TOKEN
# - OPENAI_API_KEY (or AZURE_OPENAI_ENDPOINT)
```

### 5. Pull Docker Images

```bash
# Pull all required images
docker pull mcr.microsoft.com/azure-functions/python:4-python3.9
docker pull openjdk:17-slim
docker pull postgres:14
docker pull redis:7-alpine
```

### 6. Install VS Code Extensions

Install these recommended extensions:
```bash
code --install-extension ms-python.python
code --install-extension ms-azuretools.vscode-docker
code --install-extension ms-vscode.azure-account
code --install-extension github.vscode-pull-request-github
code --install-extension bitlang.cobol
```

## ✅ Verify Installation

Run the verification script to ensure everything is set up correctly:

```bash
python scripts/verify_setup.py
```

Expected output:
```
🔍 Verifying workshop setup...

✓ Python environment configured
✓ All Python packages installed
✓ Docker daemon running
✓ Required Docker images available
✓ Azure credentials configured
✓ GitHub token valid
✓ OpenAI/Azure OpenAI configured
✓ Workshop data downloaded

🎉 Setup verification complete! You're ready to start.
```

## 🎓 Choose Your Workshop

Based on your verification results, choose your starting point:

### Option 1: COBOL Modernization (Recommended for Beginners)
```bash
cd appendix-a-cobol-modernization
python start_workshop.py --module 1
```

### Option 2: Natural/Adabas Migration (Advanced)
```bash
cd appendix-b-natural-adabas-migration
python start_workshop.py --module 1
```

### Option 3: Introduction Module (Foundation)
```bash
python start_workshop.py --intro
```

## ⚠️ Common Issues

### Docker Not Running
```bash
# macOS
open -a Docker

# Linux
sudo systemctl start docker

# Windows
Start-Service docker
```

### Python Virtual Environment Issues
```bash
# Deactivate and recreate
deactivate
rm -rf venv
python -m venv venv
source venv/bin/activate  # or .\venv\Scripts\activate on Windows
pip install -r requirements.txt
```

### Azure Authentication Failed
```bash
# Clear cached credentials
az account clear
az login --use-device-code
```

### Permission Denied Errors
```bash
# Fix file permissions
chmod +x scripts/*.sh
chmod 755 -R workshop-data/
```

## 📊 Quick Performance Test

Test your setup performance:

```bash
python scripts/performance_test.py
```

This will check:
- Docker container startup time
- Azure API response time
- AI model inference speed
- Database connection speed

## 🛠️ Development Tools Setup

### Optional but Recommended

1. **Postman** for API testing
   ```bash
   # Download from https://www.postman.com/downloads/
   ```

2. **Azure Storage Explorer**
   ```bash
   # Download from https://azure.microsoft.com/features/storage-explorer/
   ```

3. **GitHub Desktop** for easier Git operations
   ```bash
   # Download from https://desktop.github.com/
   ```

## 📚 Next Steps

Now that your environment is ready:

1. **Read the Introduction**: [intro-mainframe-modernization.md](intro-mainframe-modernization.md)
2. **Review Workshop Checklist**: [workshop-checklist.md](workshop-checklist.md)
3. **Start Your First Lab**: Choose from:
   - [COBOL Workshop Lab 1](appendix-a-cobol-modernization/labs/lab1/README.md)
   - [Natural Workshop Lab 1](appendix-b-natural-adabas-migration/labs/lab1/README.md)

## 🆘 Need Help?

- **Troubleshooting Guide**: [TROUBLESHOOTING.md](TROUBLESHOOTING.md)
- **FAQ**: [FAQ.md](FAQ.md)
- **Community Slack**: [Join #workshop-help](https://mainframe-modern.slack.com)
- **Office Hours**: Thursdays 2:00 PM UTC

## 🎯 Pro Tips

1. **Save Time**: Use the provided Docker containers instead of local installations
2. **Stay Organized**: Create a dedicated workspace folder for each workshop
3. **Take Notes**: Use the built-in note-taking templates in `templates/notes/`
4. **Practice**: Each lab has bonus exercises - try them for deeper understanding
5. **Collaborate**: Join study groups in our Slack community

---

<div align="center">

✨ **You're all set! Choose your adventure:**

[📚 **Back to Workshop Overview**](README.md) | [🏃 **Start COBOL Workshop**](appendix-a-cobol-modernization/README.md) | [🚀 **Start Natural Workshop**](appendix-b-natural-adabas-migration/README.md)

</div> 