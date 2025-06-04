# 🔧 Troubleshooting Guide

This guide helps you resolve common issues encountered during the Mainframe Modernization Workshop.

## 📋 Table of Contents

- [Setup Issues](#-setup-issues)
- [Docker Problems](#-docker-problems)
- [Azure Connection Errors](#-azure-connection-errors)
- [Agent Framework Issues](#-agent-framework-issues)
- [COBOL Workshop Issues](#-cobol-workshop-issues)
- [Natural/Adabas Issues](#-naturaladabas-issues)
- [Performance Problems](#-performance-problems)
- [IDE and Tool Issues](#-ide-and-tool-issues)
- [Getting Additional Help](#-getting-additional-help)

## 🛠️ Setup Issues

### Python Virtual Environment Not Activating

**Symptom**: Command not found errors when running Python scripts

**Solution**:
```bash
# Verify Python installation
python --version  # or python3 --version

# Recreate virtual environment
rm -rf venv
python -m venv venv

# Activate (choose based on your system)
source venv/bin/activate      # macOS/Linux
.\venv\Scripts\activate       # Windows PowerShell
venv\Scripts\activate.bat     # Windows CMD
```

### Missing Dependencies

**Symptom**: ImportError or ModuleNotFoundError

**Solution**:
```bash
# Ensure virtual environment is activated
# Update pip first
pip install --upgrade pip

# Reinstall all dependencies
pip install -r requirements.txt --force-reinstall

# If specific package fails
pip install <package-name> --no-cache-dir
```

### Permission Denied Errors

**Symptom**: Cannot execute scripts or access files

**Solution**:
```bash
# Fix script permissions
chmod +x scripts/*.sh
chmod +x scripts/*.py

# Fix directory permissions
chmod -R 755 workshop-data/
chmod -R 755 labs/

# On Windows (Run as Administrator)
icacls scripts /grant Everyone:F /T
```

## 🐳 Docker Problems

### Docker Daemon Not Running

**Symptom**: Cannot connect to Docker daemon

**Solution**:

**macOS**:
```bash
# Start Docker Desktop
open -a Docker
# Wait 30 seconds for daemon to start
sleep 30
docker ps
```

**Linux**:
```bash
# Start Docker service
sudo systemctl start docker
sudo systemctl enable docker

# Add user to docker group
sudo usermod -aG docker $USER
# Log out and back in for group changes
```

**Windows**:
```powershell
# Run as Administrator
Start-Service docker
# Or restart Docker Desktop from system tray
```

### Container Build Failures

**Symptom**: Docker build errors or image not found

**Solution**:
```bash
# Clean Docker cache
docker system prune -a --volumes

# Pull base images manually
docker pull python:3.9-slim
docker pull openjdk:17-slim
docker pull mcr.microsoft.com/azure-functions/python:4-python3.9

# Build with no cache
docker build --no-cache -t workshop-env .

# If behind proxy
docker build --build-arg HTTP_PROXY=$HTTP_PROXY \
             --build-arg HTTPS_PROXY=$HTTPS_PROXY \
             -t workshop-env .
```

### Out of Disk Space

**Symptom**: No space left on device errors

**Solution**:
```bash
# Check Docker disk usage
docker system df

# Clean up unused resources
docker system prune -a --volumes
docker image prune -a
docker container prune
docker volume prune

# Remove specific large images
docker images --format "table {{.Repository}}\t{{.Tag}}\t{{.Size}}"
docker rmi <image-id>
```

## ☁️ Azure Connection Errors

### Authentication Failed

**Symptom**: 401 Unauthorized or credential errors

**Solution**:
```bash
# Clear cached credentials
az account clear
rm -rf ~/.azure

# Re-authenticate
az login --use-device-code

# Verify subscription
az account show
az account list --output table

# Set correct subscription
az account set --subscription "YOUR_SUBSCRIPTION_ID"

# Create new service principal
az ad sp create-for-rbac --name "workshop-sp-$(date +%s)" \
    --role contributor \
    --scopes /subscriptions/$(az account show --query id -o tsv)
```

### Service Principal Issues

**Symptom**: Insufficient privileges or invalid client secret

**Solution**:
```bash
# Verify service principal
az ad sp show --id $AZURE_CLIENT_ID

# Reset credentials
az ad sp credential reset --id $AZURE_CLIENT_ID

# Check role assignments
az role assignment list --assignee $AZURE_CLIENT_ID --output table

# Add required permissions
az role assignment create --assignee $AZURE_CLIENT_ID \
    --role "Contributor" \
    --scope "/subscriptions/$AZURE_SUBSCRIPTION_ID"
```

### API Rate Limiting

**Symptom**: 429 Too Many Requests

**Solution**:
```python
# Add retry logic to your code
import time
from azure.core.exceptions import HttpResponseError

def call_azure_api_with_retry(func, *args, **kwargs):
    max_retries = 3
    for attempt in range(max_retries):
        try:
            return func(*args, **kwargs)
        except HttpResponseError as e:
            if e.status_code == 429 and attempt < max_retries - 1:
                wait_time = 2 ** attempt
                print(f"Rate limited. Waiting {wait_time} seconds...")
                time.sleep(wait_time)
            else:
                raise
```

## 🤖 Agent Framework Issues

### Agent Not Starting

**Symptom**: Agent initialization fails or hangs

**Solution**:
```bash
# Check Redis connection
docker ps | grep redis
redis-cli ping  # Should return PONG

# Restart Redis if needed
docker restart workshop-redis

# Check agent logs
tail -f logs/agent-framework.log

# Verify environment variables
python -c "import os; print(os.environ.get('REDIS_URL', 'Not Set'))"
```

### MCP Server Connection Failed

**Symptom**: Cannot connect to Model Context Protocol server

**Solution**:
```bash
# Check if MCP server is running
curl http://localhost:8000/health

# Start MCP server manually
cd code/agent-framework/mcp-server
python mainframe_mcp_server.py

# Test with different port
export MCP_SERVER_PORT=8001
python mainframe_mcp_server.py --port 8001
```

### Agent Memory Issues

**Symptom**: Agent crashes with memory errors

**Solution**:
```python
# Increase memory limits in agent config
AGENT_CONFIG = {
    "max_memory_mb": 2048,  # Increase from default
    "garbage_collection_interval": 300,  # More frequent GC
    "max_context_size": 10000  # Reduce context window
}

# Monitor memory usage
import psutil
process = psutil.Process()
print(f"Memory usage: {process.memory_info().rss / 1024 / 1024:.2f} MB")
```

## 📘 COBOL Workshop Issues

### COBOL Parser Errors

**Symptom**: Failed to parse COBOL source code

**Solution**:
```bash
# Verify COBOL file encoding
file -i sample.cbl
# Convert if needed
iconv -f EBCDIC -t UTF-8 sample.cbl > sample_utf8.cbl

# Check for special characters
grep -P "[\x80-\xFF]" sample.cbl

# Use the lenient parser mode
python analyze_cobol.py --mode lenient --file sample.cbl
```

### Java Transformation Failures

**Symptom**: Generated Java code doesn't compile

**Solution**:
```bash
# Check Java version
java -version  # Should be 17+

# Clean build directory
rm -rf target/
mvn clean

# Run with detailed errors
mvn compile -X

# Common fixes for generated code
# Add missing imports
sed -i '1i import java.math.BigDecimal;' src/main/java/*.java
```

## 🗄️ Natural/Adabas Issues

### DDM Conversion Errors

**Symptom**: Cannot convert Data Definition Modules

**Solution**:
```bash
# Validate DDM format
python validate_ddm.py --file sample.ddm

# Use compatibility mode
python convert_ddm.py --compatibility-mode --file sample.ddm

# Handle special field types manually
python convert_ddm.py --field-mapping custom_mapping.json --file sample.ddm
```

### MU/PE Field Handling

**Symptom**: Multiple value or periodic group errors

**Solution**:
```sql
-- Create proper array tables
CREATE TABLE customer_phones (
    customer_id INT,
    phone_index INT,
    phone_number VARCHAR(20),
    PRIMARY KEY (customer_id, phone_index),
    FOREIGN KEY (customer_id) REFERENCES customers(id)
);

-- Handle in application code
@Entity
@Table(name = "customer_phones")
public class CustomerPhone {
    @EmbeddedId
    private CustomerPhoneId id;
    private String phoneNumber;
}
```

## ⚡ Performance Problems

### Slow Agent Response

**Symptom**: Agents take too long to respond

**Solution**:
```bash
# Enable performance profiling
export AGENT_PROFILE=true
python run_agent.py

# Optimize model calls
# Use smaller models for simple tasks
export SIMPLE_TASK_MODEL="gpt-3.5-turbo"
export COMPLEX_TASK_MODEL="gpt-4"

# Enable caching
export ENABLE_RESPONSE_CACHE=true
export CACHE_TTL=3600
```

### Docker Container Slow

**Symptom**: Containers take long to start or respond slowly

**Solution**:
```bash
# Allocate more resources to Docker
# Docker Desktop > Settings > Resources
# - CPUs: 4+ cores
# - Memory: 8GB+
# - Disk: 50GB+

# Use volume mounts instead of copying
docker run -v $(pwd):/workspace workshop-env

# Pre-build images
docker build -t workshop-env:cached .
docker tag workshop-env:cached workshop-env:latest
```

## 💻 IDE and Tool Issues

### VS Code Extensions Not Working

**Symptom**: COBOL syntax highlighting or other features missing

**Solution**:
```bash
# Reinstall extensions
code --uninstall-extension bitlang.cobol
code --install-extension bitlang.cobol

# Clear VS Code cache
rm -rf ~/.vscode/extensions/bitlang.cobol*
code --install-extension bitlang.cobol

# Use alternative COBOL extension
code --install-extension broadcommfd.cobol-language-support
```

### Git Line Ending Issues

**Symptom**: Files show as modified without changes

**Solution**:
```bash
# Configure Git for cross-platform
git config --global core.autocrlf input  # macOS/Linux
git config --global core.autocrlf true   # Windows

# Fix existing files
git rm --cached -r .
git reset --hard
```

## 🆘 Getting Additional Help

### Diagnostic Information Collection

Run this script to collect diagnostic information:

```bash
# Download and run diagnostic script
curl -fsSL https://raw.githubusercontent.com/mainframe-modernization/workshops/main/scripts/collect-diagnostics.sh | bash

# This creates diagnostics-TIMESTAMP.tar.gz
# Share this file when requesting help
```

### Community Support

1. **Slack Channel**: [#workshop-troubleshooting](https://mainframe-modern.slack.com)
2. **GitHub Issues**: [Report an Issue](https://github.com/mainframe-modernization/workshops/issues)
3. **Stack Overflow**: Tag with `mainframe-modernization`
4. **Office Hours**: Thursdays 2:00 PM UTC on Zoom

### Emergency Contacts

For critical workshop issues during scheduled sessions:

- **Technical Lead**: workshop-tech@mainframe-modernization.org
- **Emergency Hotline**: +1-555-MOD-HELP (during workshop hours only)

### Useful Commands Reference

```bash
# Quick health check
./scripts/health-check.sh

# Reset workshop environment
./scripts/reset-workshop.sh --preserve-data

# Update workshop materials
git pull origin main
pip install -r requirements.txt --upgrade

# Full system verification
python scripts/verify_setup.py --verbose
```

---

<div align="center">

📚 **Quick Links**

[Back to Workshop](README.md) | [FAQ](FAQ.md) | [Quick Start](QUICK_START.md) | [Report Issue](https://github.com/mainframe-modernization/workshops/issues/new)

</div> 