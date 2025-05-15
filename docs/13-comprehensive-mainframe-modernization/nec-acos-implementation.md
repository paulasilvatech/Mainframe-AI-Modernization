# 🔴 NEC ACOS Implementation Guide

This technical guide provides detailed implementation steps for modernizing NEC ACOS mainframe applications using AI and GitHub integration.

## 📋 Overview

NEC ACOS modernization follows a distinct path reflecting its Japanese market focus, with specialized approaches to preserve business logic while enabling modern DevOps practices and cloud integration.

## 1. 🔌 ACOS Integration Framework Implementation

### Prerequisites
- NEC ACOS system (ACOS-4, ACOS-2)
- ACOS iPackage Integration tools
- TCP/IP connectivity between ACOS and cloud environments
- Administrative access to the ACOS system
- Appropriate system privileges

### Configuration Steps

#### 1.1 Install ACOS Integration Framework

```bash
# Mount the installation media
mount /dev/cdrom /mnt/installation

# Navigate to installation directory
cd /mnt/installation

# Run the installer
./acos_integration_framework_install.sh

# Follow the installation wizard
# - Installation directory: /opt/nec/acos-if
# - Configure connection settings
# - Set administrator credentials
```

#### 1.2 Configure Integration Framework

Create a configuration file for the Integration Framework:

```json
{
  "integration_framework": {
    "version": "4.2",
    "framework_id": "ACOS-IF-01",
    "license": {
      "key": "AAAA-BBBB-CCCC-DDDD",
      "customer_id": "CUSTOMER123",
      "expiration": "2025-12-31"
    },
    "acos_systems": [
      {
        "name": "ACOS4_PROD",
        "type": "ACOS-4",
        "version": "XVP PX",
        "host": "acos4-prod.example.com",
        "port": 9023,
        "admin_user": "ADMIN",
        "admin_password_encrypted": "enc:A1B2C3D4E5F6G7H8I9J0",
        "timeout": 300
      },
      {
        "name": "ACOS2_TEST",
        "type": "ACOS-2",
        "version": "10.3",
        "host": "acos2-test.example.com",
        "port": 9023,
        "admin_user": "TEST",
        "admin_password_encrypted": "enc:J0I9H8G7F6E5D4C3B2A1",
        "timeout": 300
      }
    ],
    "network": {
      "security": {
        "allowed_ips": [
          "10.0.0.0/24",
          "192.168.1.0/24"
        ],
        "ssl": {
          "enabled": true,
          "cert_path": "/opt/nec/acos-if/security/acos-if.crt",
          "key_path": "/opt/nec/acos-if/security/acos-if.key"
        }
      }
    }
  }
}
```

#### 1.3 Configure iPackage API Gateway

Create API gateway configuration:

```yaml
apiGateway:
  name: ACOS-API-Gateway
  version: 3.1
  port: 8443
  ssl:
    enabled: true
    keystore: /opt/nec/acos-if/security/keystore.jks
    keystore_password_encrypted: enc:K1L2M3N4O5P6Q7R8S9T0
  
  authentication:
    type: OAuth2
    provider: KeyCloak
    realm: acos-realm
    client_id: api-gateway-client
    client_secret_encrypted: enc:T0S9R8Q7P6O5N4M3L2K1
    token_endpoint: https://keycloak.example.com/auth/realms/acos-realm/protocol/openid-connect/token
    jwks_endpoint: https://keycloak.example.com/auth/realms/acos-realm/protocol/openid-connect/certs
  
  routes:
    - name: batch-api
      path: /api/batch/**
      target: http://localhost:8081
      rateLimit: 100/minute
      
    - name: transaction-api
      path: /api/transaction/**
      target: http://localhost:8082
      rateLimit: 200/minute
      
    - name: data-api
      path: /api/data/**
      target: http://localhost:8083
      rateLimit: 150/minute
```

#### 1.4 Deploy REST APIs

Create API definitions:

```json
{
  "swagger": "2.0",
  "info": {
    "title": "ACOS Transaction API",
    "description": "API for executing transactions on ACOS systems",
    "version": "1.0.0"
  },
  "basePath": "/api/transaction",
  "schemes": ["https"],
  "securityDefinitions": {
    "oauth2": {
      "type": "oauth2",
      "flow": "application",
      "tokenUrl": "https://keycloak.example.com/auth/realms/acos-realm/protocol/openid-connect/token",
      "scopes": {
        "transaction:read": "Read access to transactions",
        "transaction:write": "Write access to transactions"
      }
    }
  },
  "security": [
    {
      "oauth2": ["transaction:read", "transaction:write"]
    }
  ],
  "paths": {
    "/execute": {
      "post": {
        "summary": "Execute a transaction",
        "parameters": [
          {
            "name": "transaction",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/TransactionRequest"
            }
          }
        ],
        "responses": {
          "200": {
            "description": "Transaction executed successfully",
            "schema": {
              "$ref": "#/definitions/TransactionResponse"
            }
          }
        }
      }
    }
  },
  "definitions": {
    "TransactionRequest": {
      "type": "object",
      "required": ["system", "transactionId", "parameters"],
      "properties": {
        "system": {
          "type": "string",
          "description": "ACOS system name"
        },
        "transactionId": {
          "type": "string",
          "description": "Transaction identifier"
        },
        "parameters": {
          "type": "object",
          "description": "Transaction parameters"
        }
      }
    },
    "TransactionResponse": {
      "type": "object",
      "properties": {
        "transactionId": {
          "type": "string"
        },
        "executionId": {
          "type": "string"
        },
        "timestamp": {
          "type": "string",
          "format": "date-time"
        },
        "status": {
          "type": "string",
          "enum": ["SUCCESS", "FAILURE", "PARTIAL"]
        },
        "result": {
          "type": "object"
        }
      }
    }
  }
}
```

## 2. 📦 CASEWORLD/PE Development Environment Integration

### Prerequisites
- CASEWORLD/PE development environment
- Git client compatible with ACOS file formats
- Network connectivity to GitHub
- CASEWORLD/PE API access

### Configuration Steps

#### 2.1 Configure CASEWORLD/PE Git Integration

Create a configuration for Git integration:

```yaml
caseworld_git:
  name: CASEWORLD-Git-Integration
  version: 2.0
  
  repositories:
    - name: ACOS-Main
      type: GitHub
      url: https://github.com/example/acos-modernization.git
      branch: main
      credentials:
        username: caseworld-service
        access_token_encrypted: enc:V1W2X3Y4Z5A6B7C8D9E0
        
    - name: ACOS-Development
      type: GitHub
      url: https://github.com/example/acos-modernization.git
      branch: develop
      credentials:
        username: caseworld-service
        access_token_encrypted: enc:E0D9C8B7A6Z5Y4X3W2V1
  
  mappings:
    - caseworld_project: ACCT
      git_directory: src/acct
      patterns:
        - "*.cbl"
        - "*.jcl"
        - "*.pli"
        
    - caseworld_project: INVT
      git_directory: src/inventory
      patterns:
        - "*.cbl"
        - "*.jcl"
        - "*.pli"
  
  file_conversions:
    - extension: ".cbl"
      from_encoding: "EBCDIC-JP"
      to_encoding: "UTF-8"
      line_ending: "LF"
      
    - extension: ".jcl"
      from_encoding: "EBCDIC-JP"
      to_encoding: "UTF-8"
      line_ending: "LF"
      
    - extension: ".pli"
      from_encoding: "EBCDIC-JP"
      to_encoding: "UTF-8"
      line_ending: "LF"
```

#### 2.2 Configure CASEWORLD/PE IDE Integration

Set up IDE integration:

```bash
# Install the CASEWORLD/PE IDE integration plugin
cd /opt/nec/caseworld/plugins
./install_plugin.sh github-integration-2.0.jar

# Configure the plugin
cat > /opt/nec/caseworld/config/github-integration.properties << EOF
github.api.url=https://api.github.com
github.enterprise.url=
github.token.encrypted=enc:F1G2H3I4J5K6L7M8N9O0
github.username=caseworld-service
github.default.repository=example/acos-modernization
github.default.branch=develop
github.workspace.root=/opt/nec/caseworld/workspace/github
github.sync.interval=5
github.commit.author=CASEWORLD Integration
github.commit.email=caseworld@example.com
EOF

# Restart CASEWORLD/PE
/opt/nec/caseworld/bin/restart.sh
```

## 3. 🔄 ACOS CI/CD Pipeline Integration

### Prerequisites
- ACOS system with batch processing capabilities
- Network connectivity to GitHub
- Authentication credentials for ACOS systems
- ACOS command interface

### Configuration Steps

#### 3.1 Configure GitHub to ACOS Integration

Create a GitHub Actions workflow file for ACOS integration:

```yaml
# .github/workflows/acos-integration.yml
name: ACOS Integration Pipeline

on:
  push:
    branches: [ main, develop ]
    paths:
      - 'src/acos/**'
      - 'jcl/**'
  pull_request:
    branches: [ main ]
    paths:
      - 'src/acos/**'
      - 'jcl/**'
  workflow_dispatch:

jobs:
  analyze:
    name: Analyze ACOS Code
    runs-on: ubuntu-latest
    steps:
      - name: Checkout code
        uses: actions/checkout@v3
        
      - name: Set up JDK 11
        uses: actions/setup-java@v3
        with:
          java-version: '11'
          distribution: 'temurin'
          
      - name: Install ACOS analysis tools
        run: |
          curl -o acos-tools.deb https://nec-download.example.com/acos-tools.deb
          sudo dpkg -i acos-tools.deb
          
      - name: Analyze ACOS code
        run: |
          acos-analyzer \
            --config-file .github/workflows/config/analysis-config.yaml \
            --source-dir ./src/acos \
            --output-dir ./analysis-results
        env:
          ACOS_CONNECTION_STRING: ${{ secrets.ACOS_CONNECTION_STRING }}
          ACOS_USERNAME: ${{ secrets.ACOS_USERNAME }}
          ACOS_PASSWORD: ${{ secrets.ACOS_PASSWORD }}
          
      - name: Upload analysis results
        uses: actions/upload-artifact@v3
        with:
          name: analysis-results
          path: ./analysis-results
          
  build:
    name: Build and Package
    runs-on: ubuntu-latest
    needs: analyze
    steps:
      - name: Checkout code
        uses: actions/checkout@v3
        
      - name: Prepare JCL files
        run: |
          mkdir -p ./build/jcl
          cp ./jcl/*.jcl ./build/jcl/
          # Apply environment-specific substitutions
          for file in ./build/jcl/*.jcl; do
            sed -i "s/__ENV__/${{ github.ref == 'refs/heads/main' && 'PROD' || 'TEST' }}/g" "$file"
            sed -i "s/__DATE__/$(date +%Y%m%d)/g" "$file"
            sed -i "s/__BUILD__/${{ github.run_number }}/g" "$file"
          done
          
      - name: Package code and JCL
        run: |
          mkdir -p ./build/package
          cp -r ./src/acos ./build/package/
          cp -r ./build/jcl ./build/package/
          tar -czvf acos-package.tar.gz -C ./build/package .
          
      - name: Upload package
        uses: actions/upload-artifact@v3
        with:
          name: acos-package
          path: ./acos-package.tar.gz
          
  deploy:
    name: Deploy to ACOS
    runs-on: ubuntu-latest
    needs: build
    if: github.ref == 'refs/heads/main' || github.ref == 'refs/heads/develop'
    environment: ${{ github.ref == 'refs/heads/main' && 'production' || 'test' }}
    steps:
      - name: Download package
        uses: actions/download-artifact@v3
        with:
          name: acos-package
          
      - name: Install ACOS utilities
        run: |
          curl -o acos-utils.deb https://nec-download.example.com/acos-utils.deb
          sudo dpkg -i acos-utils.deb
          
      - name: Transfer files to ACOS
        run: |
          mkdir -p ./deploy
          tar -xzvf acos-package.tar.gz -C ./deploy
          
          # Transfer files to ACOS
          acos-file-transfer \
            --host ${{ secrets.ACOS_HOST }} \
            --port ${{ secrets.ACOS_PORT }} \
            --user ${{ secrets.ACOS_USERNAME }} \
            --password ${{ secrets.ACOS_PASSWORD }} \
            --source-dir ./deploy \
            --dest-dir ${{ github.ref == 'refs/heads/main' && 'PROD.GIT' || 'TEST.GIT' }} \
            --transfer-mode BINARY
            
      - name: Submit JCL jobs
        run: |
          # Submit ACOS batch jobs
          acos-job-submit \
            --host ${{ secrets.ACOS_HOST }} \
            --port ${{ secrets.ACOS_PORT }} \
            --user ${{ secrets.ACOS_USERNAME }} \
            --password ${{ secrets.ACOS_PASSWORD }} \
            --jcl-file ${{ github.ref == 'refs/heads/main' && './deploy/jcl/deploy-prod.jcl' || './deploy/jcl/deploy-test.jcl' }} \
            --wait-for-completion \
            --timeout 1800
```

## 4. 🛡️ Security Integration

### Prerequisites
- ACOS Security Management System
- Identity management system compatible with ACOS
- Network connectivity between systems
- Security administration credentials

### Configuration Steps

#### 4.1 Configure Authentication Integration

Set up unified authentication:

```yaml
authentication:
  provider: ACOS-SSO
  version: 2.5
  
  external_providers:
    - type: SAML
      name: AzureAD
      entity_id: https://login.microsoftonline.com/tenant-id/saml2
      metadata_url: https://login.microsoftonline.com/tenant-id/federationmetadata/2007-06/federationmetadata.xml
      attributes:
        username: http://schemas.xmlsoap.org/ws/2005/05/identity/claims/name
        email: http://schemas.xmlsoap.org/ws/2005/05/identity/claims/emailaddress
        groups: http://schemas.microsoft.com/ws/2008/06/identity/claims/groups
  
  user_mapping:
    acos_user_attribute: USER-ID
    external_attribute: username
    attribute_mappings:
      - acos: USER-NAME
        external: displayName
      - acos: USER-EMAIL
        external: email
```

#### 4.2 Configure Access Control

Set up role-based access control:

```yaml
access_control:
  policy_engine: ACOS-Security
  version: 3.0
  
  roles:
    - name: Developer
      permissions:
        - resource: SOURCE.*
          actions: [READ, WRITE]
        - resource: OBJECT.*
          actions: [READ]
          
    - name: Operator
      permissions:
        - resource: PRODUCTION.*
          actions: [READ, EXECUTE]
        - resource: BATCH.*
          actions: [READ, WRITE, EXECUTE]
          
    - name: Administrator
      permissions:
        - resource: "*"
          actions: [READ, WRITE, EXECUTE, ADMIN]
```

## 5. 📊 Monitoring Integration

### Prerequisites
- ACOS monitoring system
- Log forwarding capabilities
- Central monitoring platform
- Network connectivity

### Configuration Steps

#### 5.1 Configure Performance Monitoring

Set up monitoring integration:

```yaml
monitoring:
  collector: ACOS-Monitor
  version: 2.3
  
  targets:
    - system: ACOS4_PROD
      metrics:
        - name: cpu_utilization
          source: CPU.UTIL
          sampling: 60
          
        - name: memory_utilization
          source: MEM.UTIL
          sampling: 60
          
        - name: transaction_rate
          source: TRANS.RATE
          sampling: 30
  
  output:
    type: Prometheus
    endpoint: http://prometheus.example.com:9091/metrics/job/acos
```

## 6. 🧪 Testing Framework

### Prerequisites
- ACOS testing frameworks
- Test automation tools
- Network connectivity to CI/CD system
- Test data management

### Configuration Steps

#### 6.1 Configure Automated Testing

Set up test automation:

```yaml
testing:
  framework: ACOS-TestAutomation
  version: 2.1
  
  test_suites:
    - name: unit_tests
      path: /tests/unit/*
      timeout: 300
      
    - name: integration_tests
      path: /tests/integration/*
      timeout: 600
      
    - name: system_tests
      path: /tests/system/*
      timeout: 1800
  
  reporting:
    formats:
      - junit
      - html
      - json
```

## 📋 Conclusion

This implementation guide provides a comprehensive approach to modernizing NEC ACOS mainframe applications. By following these steps, organizations can integrate their ACOS systems with modern CI/CD practices using GitHub and cloud services while preserving their critical mainframe applications.

## ➡️ Next Steps

- Explore [🧪 Code Analysis](../05-code-analysis/README.md) for AI-powered application assessment
- Learn about [🐙 GitHub Integration](../06-github-integration/README.md) for mainframe modernization
- Implement [🧠 AI-Powered Transformation](../08-ai-transformation/README.md) for your applications 