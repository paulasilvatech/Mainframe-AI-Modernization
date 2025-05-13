# 🔷 IBM z/OS Implementation Guide

This technical guide provides detailed implementation steps for modernizing IBM z/OS mainframe applications using AI and GitHub integration.

## 📋 Overview

IBM z/OS modernization leverages specific IBM technologies combined with cloud capabilities to enable gradual transformation while preserving critical systems functionality.

## 1. 🔌 z/OS Connect Implementation

### Prerequisites
- IBM z/OS Connect EE V3.0 or later
- IBM z/OS V2.3 or later with current maintenance
- TCP/IP connectivity between z/OS and cloud environments
- IBM SDK for Java 8 or higher on z/OS

### Configuration Steps

#### 1.1 Install z/OS Connect

```bash
# Mount the z/OS Connect EE installation files
mount /dev/cdrom /mnt

# Run the installation script
cd /mnt/zosconnect
./install.sh

# Configuration location
cd /var/zosconnect/v3/configuration
```

#### 1.2 Configure Server Instance

Create a `server.xml` configuration file:

```xml
<server description="z/OS Connect EE Server">
    <!-- Enable features -->
    <featureManager>
        <feature>zosconnect:apiRequester-3.0</feature>
        <feature>zosconnect:apiRequester-3.0</feature>
        <feature>zosconnect:swaggerui-1.0</feature>
        <feature>transportSecurity-1.0</feature>
        <feature>appSecurity-2.0</feature>
    </featureManager>

    <!-- HTTP Endpoint -->
    <httpEndpoint id="defaultHttpEndpoint"
                  host="*"
                  httpPort="9080"
                  httpsPort="9443" />

    <!-- Authentication -->
    <basicRegistry id="basic" realm="zOSConnectRealm">
        <user name="apiuser" password="password" />
        <group name="ApiRequesterGroup">
            <member name="apiuser" />
        </group>
    </basicRegistry>

    <!-- Authorization -->
    <authorization-roles id="com.ibm.ws.zosconnect.apiRequester">
        <security-role name="apiRequesterRole">
            <group name="ApiRequesterGroup" />
        </security-role>
    </authorization-roles>

    <!-- CICS Service Provider -->
    <zosconnect_cicsIpicConnection id="cicsConn"
                                 host="cicsappl.example.com"
                                 port="1091"
                                 username="${cics.username}"
                                 password="${cics.password}" />
</server>
```

#### 1.3 Create API Definition

Create a RESTful API from a CICS transaction using the z/OS Connect API Editor:

```json
{
  "swagger": "2.0",
  "info": {
    "title": "Customer API",
    "description": "API for accessing customer information",
    "version": "1.0.0"
  },
  "basePath": "/customers",
  "paths": {
    "/{customerId}": {
      "get": {
        "summary": "Get customer details",
        "parameters": [
          {
            "name": "customerId",
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "responses": {
          "200": {
            "description": "Successful response",
            "schema": {
              "$ref": "#/definitions/Customer"
            }
          }
        }
      }
    }
  },
  "definitions": {
    "Customer": {
      "type": "object",
      "properties": {
        "customerId": {
          "type": "string"
        },
        "name": {
          "type": "string"
        },
        "address": {
          "type": "string"
        },
        "accountBalance": {
          "type": "number",
          "format": "double"
        }
      }
    }
  }
}
```

#### 1.4 Deploy the API

```bash
# Deploy API definition using zconbt command-line tool
zconbt --file=customerAPI.aar --name=CustomerAPI --type=api --wsdir=/var/zosconnect/apis

# Verify deployment
curl -k -u apiuser:password https://zosconnect.example.com:9443/zosConnect/apis
```

## 2. 📦 z/OS Container Extensions (zCX) Implementation

### Prerequisites
- IBM z/OS V2.4 or later
- zCX feature enabled
- Network connectivity to GitHub
- Docker CLI access

### Configuration Steps

#### 2.1 Configure zCX Instance

Create a provisioning script for zCX:

```bash
# Define zCX Instance
ZCXPROV ADD NAME(ZCX01) 
SYS(SYSA) 
DSNAME(ZCX.V24.CONFIG) 
VOLUME(VOL001) 
NETWORK(VLAN123)
IP(10.1.1.100)
GATEWAY(10.1.1.1)
CPUS(4)
MEMORY(8G)
DVIPA(10.1.2.200)
HOSTNAME(zcx01.example.com)
VENDOR_DATA(ZCX.VENDOR.DATA)
```

Execute the provisioning script:

```bash
# Execute provisioning script
MODIFY ZCXPROV,ADD,NAME=ZCX01
```

#### 2.2 Start zCX Instance

```bash
# Start zCX instance
S ZCX01
```

#### 2.3 Install Git and GitHub CLI in zCX

Connect to the zCX instance and install Git and GitHub CLI:

```bash
# Connect to zCX instance
ssh admin@10.1.1.100

# Install Git
apt-get update
apt-get install -y git

# Install GitHub CLI
curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg | sudo dd of=/usr/share/keyrings/githubcli-archive-keyring.gpg
chmod go+r /usr/share/keyrings/githubcli-archive-keyring.gpg
echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" | sudo tee /etc/apt/sources.list.d/github-cli.list > /dev/null
apt-get update
apt-get install -y gh
```

#### 2.4 Create Docker Container for COBOL Applications

```bash
# Create Dockerfile for COBOL application
cat > Dockerfile << 'EOF'
FROM ibmcom/db2
MAINTAINER Your Name <your.email@example.com>

# Copy COBOL binaries and configuration
COPY ./bin /app/bin
COPY ./config /app/config

# Expose API port
EXPOSE 8080

# Start the application
CMD ["/app/bin/start-app.sh"]
EOF

# Build Docker image
docker build -t cobol-app:1.0 .

# Run Docker container
docker run -d -p 8080:8080 --name cobol-app cobol-app:1.0
```

## 3. 🧠 Watson Code Assistant for Z Integration

### Prerequisites
- IBM watsonx Code Assistant for Z subscription
- Access to IBM watsonx platform
- Secure connection between z/OS and watsonx environment
- GitHub Enterprise or github.com account

### Configuration Steps

#### 3.1 Connect to watsonx Code Assistant

```bash
# Install watsonx CLI
curl -sL https://ibm.biz/watsonx-cli | bash

# Login to watsonx
wx login --apikey YOUR_WATSONX_API_KEY
```

#### 3.2 Configure Code Analysis Project

Create a project configuration file:

```json
{
  "project": "mainframe-modernization",
  "source": {
    "type": "github",
    "repo": "your-org/mainframe-code",
    "branch": "main",
    "path": "src/cobol"
  },
  "analysis": {
    "language": "cobol",
    "features": {
      "businessRuleExtraction": true,
      "documentationGeneration": true,
      "codeQuality": true,
      "securityAnalysis": true
    }
  },
  "output": {
    "format": "json",
    "destination": {
      "type": "github",
      "repo": "your-org/modernization-analysis",
      "branch": "main",
      "path": "analysis"
    }
  }
}
```

#### 3.3 Integrate with GitHub Workflows

Create a GitHub workflow for code analysis:

```yaml
name: IBM z/OS Code Analysis

on:
  push:
    branches: [ main ]
    paths:
      - 'src/cobol/**'
  pull_request:
    branches: [ main ]
    paths:
      - 'src/cobol/**'

jobs:
  analyze:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      - name: Setup watsonx CLI
        run: |
          curl -sL https://ibm.biz/watsonx-cli | bash
          wx login --apikey ${{ secrets.WATSONX_API_KEY }}
      
      - name: Run COBOL Code Analysis
        run: |
          wx code analyze --config .wx/analysis-config.json
          
      - name: Upload Analysis Results
        uses: actions/upload-artifact@v3
        with:
          name: code-analysis-results
          path: analysis/
```

#### 3.4 Code Transformation with watsonx

Create a transformation configuration:

```json
{
  "transformation": {
    "source": {
      "language": "cobol",
      "path": "src/cobol"
    },
    "target": {
      "language": "java",
      "framework": "spring-boot",
      "path": "src/java"
    },
    "rules": {
      "copybooks": {
        "processing": "inline",
        "path": "src/cobol/copybooks"
      },
      "cics": {
        "mappingFile": "config/cics-mapping.json"
      },
      "db2": {
        "mappingFile": "config/db2-mapping.json"
      }
    }
  }
}
```

Execute transformation:

```bash
# Run transformation
wx code transform --config .wx/transform-config.json
```

## 4. 🔄 Implementation Example: Complete Modernization Pipeline

### 4.1 Comprehensive GitHub Workflow

Create a `.github/workflows/modernize.yml` file:

```yaml
name: IBM z/OS Modernization Pipeline

on:
  push:
    branches: [ main ]
  workflow_dispatch:

env:
  ZCX_HOST: 10.1.1.100
  ZCX_USER: admin
  ZCONNECT_HOST: zosconnect.example.com
  ZCONNECT_PORT: 9443
  ZCONNECT_USER: apiuser

jobs:
  analyze:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      - name: Setup watsonx CLI
        run: |
          curl -sL https://ibm.biz/watsonx-cli | bash
          wx login --apikey ${{ secrets.WATSONX_API_KEY }}
      
      - name: Run COBOL Code Analysis
        run: |
          wx code analyze --config .wx/analysis-config.json
          
      - name: Upload Analysis Results
        uses: actions/upload-artifact@v3
        with:
          name: code-analysis-results
          path: analysis/
  
  transform:
    needs: analyze
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      - name: Download Analysis Results
        uses: actions/download-artifact@v3
        with:
          name: code-analysis-results
          path: analysis/
      
      - name: Setup watsonx CLI
        run: |
          curl -sL https://ibm.biz/watsonx-cli | bash
          wx login --apikey ${{ secrets.WATSONX_API_KEY }}
      
      - name: Transform COBOL to Java
        run: |
          wx code transform --config .wx/transform-config.json
          
      - name: Commit Transformed Code
        run: |
          git config --local user.email "action@github.com"
          git config --local user.name "GitHub Action"
          git add src/java/
          git commit -m "Transform COBOL to Java [skip ci]"
          git push
  
  build:
    needs: transform
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      - name: Set up JDK 11
        uses: actions/setup-java@v3
        with:
          java-version: '11'
          distribution: 'temurin'
      
      - name: Build with Maven
        run: mvn -B package --file pom.xml
      
      - name: Build Docker Image
        run: |
          docker build -t modernized-app:${GITHUB_SHA::7} .
          docker tag modernized-app:${GITHUB_SHA::7} modernized-app:latest
      
      - name: Upload Docker Image to Registry
        run: |
          echo ${{ secrets.DOCKER_PASSWORD }} | docker login -u ${{ secrets.DOCKER_USERNAME }} --password-stdin
          docker push modernized-app:${GITHUB_SHA::7}
          docker push modernized-app:latest
  
  deploy:
    needs: build
    runs-on: ubuntu-latest
    steps:
      - name: Deploy to zCX
        uses: appleboy/ssh-action@master
        with:
          host: ${{ env.ZCX_HOST }}
          username: ${{ env.ZCX_USER }}
          key: ${{ secrets.ZCX_SSH_KEY }}
          script: |
            docker pull modernized-app:latest
            docker stop modernized-app || true
            docker rm modernized-app || true
            docker run -d --name modernized-app -p 8080:8080 modernized-app:latest
      
      - name: Configure z/OS Connect
        run: |
          curl -k -u ${{ env.ZCONNECT_USER }}:${{ secrets.ZCONNECT_PASSWORD }} \
            -X POST \
            -H "Content-Type: application/json" \
            -d @zconnect/api-config.json \
            https://${{ env.ZCONNECT_HOST }}:${{ env.ZCONNECT_PORT }}/zosConnect/apis
      
      - name: Test API Endpoint
        run: |
          curl -k -u ${{ env.ZCONNECT_USER }}:${{ secrets.ZCONNECT_PASSWORD }} \
            https://${{ env.ZCONNECT_HOST }}:${{ env.ZCONNECT_PORT }}/zosConnect/apis/CustomerAPI/1.0.0/customers/1001
```

## 5. 🧪 Testing and Validation

### 5.1 Create Automated Tests

Create test scripts for API validation:

```javascript
// test/api-tests.js
const axios = require('axios');
const https = require('https');

// Ignore SSL certificate errors for testing
const agent = new https.Agent({  
  rejectUnauthorized: false
});

// Test parameters
const config = {
  baseURL: 'https://zosconnect.example.com:9443/zosConnect/apis/CustomerAPI/1.0.0',
  auth: {
    username: 'apiuser',
    password: process.env.API_PASSWORD
  },
  httpsAgent: agent
};

// Test customer lookup
async function testCustomerLookup() {
  try {
    const response = await axios.get('/customers/1001', config);
    
    console.log('Status:', response.status);
    console.log('Data:', JSON.stringify(response.data, null, 2));
    
    // Basic validation
    if (response.status !== 200) {
      throw new Error(`Expected status 200, got ${response.status}`);
    }
    
    if (!response.data.customerId) {
      throw new Error('Response missing customerId');
    }
    
    console.log('Test passed!');
    return true;
  } catch (error) {
    console.error('Test failed:', error.message);
    if (error.response) {
      console.error('Response status:', error.response.status);
      console.error('Response data:', error.response.data);
    }
    return false;
  }
}

// Run tests
testCustomerLookup().then(result => {
  process.exit(result ? 0 : 1);
});
```

### 5.2 Integration Testing Script

```bash
#!/bin/bash
# integration-test.sh

echo "Starting integration tests..."

# Test z/OS Connect API
echo "Testing z/OS Connect API..."
node test/api-tests.js
if [ $? -ne 0 ]; then
  echo "API test failed!"
  exit 1
fi

# Test containerized application
echo "Testing containerized application..."
response=$(curl -s http://10.1.1.100:8080/api/v1/health)
if [[ "$response" != *"UP"* ]]; then
  echo "Container health check failed!"
  exit 1
fi

echo "All tests passed successfully!"
exit 0
```

## 6. 📊 Monitoring and Management

### 6.1 Configure Prometheus for Monitoring

Create a Prometheus configuration file:

```yaml
# prometheus.yml
global:
  scrape_interval: 15s

scrape_configs:
  - job_name: 'zos-connect'
    scheme: https
    tls_config:
      insecure_skip_verify: true
    basic_auth:
      username: 'prometheus'
      password: 'password'
    metrics_path: '/zosConnect/metrics'
    static_configs:
      - targets: ['zosconnect.example.com:9443']
        labels:
          service: 'z/OS Connect'

  - job_name: 'modernized-app'
    metrics_path: '/actuator/prometheus'
    static_configs:
      - targets: ['10.1.1.100:8080']
        labels:
          service: 'Modernized Application'
```

### 6.2 Configure Grafana Dashboard

Create a dashboard JSON configuration:

```json
{
  "annotations": {
    "list": [
      {
        "builtIn": 1,
        "datasource": "-- Grafana --",
        "enable": true,
        "hide": true,
        "iconColor": "rgba(0, 211, 255, 1)",
        "name": "Annotations & Alerts",
        "type": "dashboard"
      }
    ]
  },
  "editable": true,
  "gnetId": null,
  "graphTooltip": 0,
  "id": 1,
  "links": [],
  "panels": [
    {
      "aliasColors": {},
      "bars": false,
      "dashLength": 10,
      "dashes": false,
      "datasource": "Prometheus",
      "fill": 1,
      "fillGradient": 0,
      "gridPos": {
        "h": 8,
        "w": 12,
        "x": 0,
        "y": 0
      },
      "hiddenSeries": false,
      "id": 2,
      "legend": {
        "avg": false,
        "current": false,
        "max": false,
        "min": false,
        "show": true,
        "total": false,
        "values": false
      },
      "lines": true,
      "linewidth": 1,
      "nullPointMode": "null",
      "options": {
        "dataLinks": []
      },
      "percentage": false,
      "pointradius": 2,
      "points": false,
      "renderer": "flot",
      "seriesOverrides": [],
      "spaceLength": 10,
      "stack": false,
      "steppedLine": false,
      "targets": [
        {
          "expr": "rate(http_server_requests_seconds_count{service=\"z/OS Connect\"}[5m])",
          "legendFormat": "{{method}} {{uri}}",
          "refId": "A"
        }
      ],
      "thresholds": [],
      "timeFrom": null,
      "timeRegions": [],
      "timeShift": null,
      "title": "z/OS Connect API Request Rate",
      "tooltip": {
        "shared": true,
        "sort": 0,
        "value_type": "individual"
      },
      "type": "graph",
      "xaxis": {
        "buckets": null,
        "mode": "time",
        "name": null,
        "show": true,
        "values": []
      },
      "yaxes": [
        {
          "format": "short",
          "label": "Requests / Second",
          "logBase": 1,
          "max": null,
          "min": null,
          "show": true
        },
        {
          "format": "short",
          "label": null,
          "logBase": 1,
          "max": null,
          "min": null,
          "show": true
        }
      ],
      "yaxis": {
        "align": false,
        "alignLevel": null
      }
    }
  ],
  "refresh": "5s",
  "schemaVersion": 22,
  "style": "dark",
  "tags": [],
  "templating": {
    "list": []
  },
  "time": {
    "from": "now-6h",
    "to": "now"
  },
  "timepicker": {},
  "timezone": "",
  "title": "z/OS Modernization Dashboard",
  "uid": "zos-modernization",
  "version": 1
}
```

## 7. 📝 Troubleshooting Guide

### Common Issues and Solutions

| Issue | Solution |
|-------|----------|
| z/OS Connect API returns 401 Unauthorized | Verify user credentials in the basicRegistry configuration |
| zCX container fails to start | Check available resources (CPU, memory) with `F ZCXCTL,DISPLAY` command |
| Watsonx analysis fails | Ensure API key is valid and has proper permissions |
| GitHub workflow fails at deployment | Verify SSH key is correctly configured in GitHub secrets |
| CICS transaction connection issue | Check IPIC connection parameters and network connectivity |

### Diagnostic Commands

```bash
# Verify z/OS Connect status
curl -k -u apiuser:password https://zosconnect.example.com:9443/zosConnect/servers

# Check zCX instance status
F ZCXCTL,DISPLAY,NAME=ZCX01

# Test network connectivity from z/OS
TSO PING 10.1.1.100

# Verify Docker container status in zCX
ssh admin@10.1.1.100 "docker ps -a"

# Check Docker container logs
ssh admin@10.1.1.100 "docker logs modernized-app"
```

## 8. 🚀 Next Steps

- Explore [🔄 GitHub Integration](../../06-github-integration/README.md) for further DevOps implementation
- Learn about [🧠 AI-Powered Transformation](../../08-ai-transformation/README.md) for advanced modernization
- Implement [🔌 API Integration Patterns](../../09-cicd-implementation/api-integration.md) for your specific use cases 