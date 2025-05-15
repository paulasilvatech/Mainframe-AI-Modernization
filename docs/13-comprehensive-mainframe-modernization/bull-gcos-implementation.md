# 🔺 Bull GCOS Implementation Guide

This technical guide provides detailed implementation steps for modernizing Bull GCOS mainframe applications using AI and GitHub integration.

## 📋 Overview

Bull GCOS modernization, led by Atos, focuses on hardware virtualization and incremental transformation while preserving business-critical applications and institutional knowledge.

## 1. 🔌 LiberFactory Implementation

### Prerequisites
- Bull GCOS 7 or GCOS 8 system
- LiberFactory Suite (VirtualBull/LiberiOS)
- TCP/IP connectivity between GCOS and cloud environments
- Administrative access to the system
- Appropriate system privileges

### Configuration Steps

#### 1.1 Install VirtualBull/LiberiOS

```bash
# Mount the installation media
mount /dev/cdrom /mnt/installation

# Run the installation script
cd /mnt/installation
./liberfactory_install.sh

# Follow the installation process
# - Select installation directory: /opt/atos/liberfactory
# - Select components: All components
# - Configure network settings
# - Set up administrator credentials
```

#### 1.2 Configure LiberFactory Environment

Create a configuration file for LiberFactory:

```json
{
  "liberfactory": {
    "version": "5.2",
    "environment": "production",
    "license": {
      "key": "XXXX-XXXX-XXXX-XXXX",
      "customer_id": "CUSTOMER001",
      "expiration": "2025-12-31"
    },
    "gcos_systems": [
      {
        "name": "GCOS8_PROD",
        "type": "GCOS8",
        "version": "8.3",
        "host": "gcos8-prod.example.com",
        "port": 3023,
        "admin_user": "ADMIN",
        "admin_password_encrypted": "enc:A1B2C3D4E5F6G7H8I9J0",
        "connection_timeout": 300
      },
      {
        "name": "GCOS7_DEV",
        "type": "GCOS7",
        "version": "7.2",
        "host": "gcos7-dev.example.com",
        "port": 3023,
        "admin_user": "ADMIN",
        "admin_password_encrypted": "enc:J0I9H8G7F6E5D4C3B2A1",
        "connection_timeout": 300
      }
    ],
    "network": {
      "proxy": {
        "enabled": false,
        "host": "",
        "port": 0,
        "username": "",
        "password_encrypted": ""
      },
      "firewall": {
        "allowed_ips": [
          "10.0.0.0/24",
          "192.168.1.0/24"
        ]
      }
    }
  }
}
```

#### 1.3 Configure API Gateway for GCOS Systems

Create a configuration for the API gateway:

```yaml
apiGateway:
  name: GCOS-API-Gateway
  version: 2.1
  port: 8443
  ssl:
    enabled: true
    keystore: /opt/atos/liberfactory/security/keystore.jks
    keystore_password_encrypted: enc:K1L2M3N4O5P6Q7R8S9T0
  
  authentication:
    type: OAuth2
    provider: Keycloak
    realm: gcos-realm
    client_id: api-gateway-client
    client_secret_encrypted: enc:T0S9R8Q7P6O5N4M3L2K1
    token_endpoint: https://keycloak.example.com/auth/realms/gcos-realm/protocol/openid-connect/token
    jwks_endpoint: https://keycloak.example.com/auth/realms/gcos-realm/protocol/openid-connect/certs
  
  routes:
    - name: batch-api
      path: /api/batch/**
      target: http://localhost:8081
      rateLimit: 100/minute
      authentication: required
      
    - name: data-api
      path: /api/data/**
      target: http://localhost:8082
      rateLimit: 200/minute
      authentication: required
      
    - name: admin-api
      path: /api/admin/**
      target: http://localhost:8083
      rateLimit: 50/minute
      authentication: required
      roles: [admin, superuser]
```

#### 1.4 Deploy REST APIs for GCOS Applications

Create API definitions for GCOS applications:

```json
{
  "swagger": "2.0",
  "info": {
    "title": "GCOS Batch API",
    "description": "API for submitting and managing batch jobs on GCOS systems",
    "version": "1.0.0"
  },
  "basePath": "/api/batch",
  "schemes": [
    "https"
  ],
  "securityDefinitions": {
    "oauth2": {
      "type": "oauth2",
      "flow": "application",
      "tokenUrl": "https://keycloak.example.com/auth/realms/gcos-realm/protocol/openid-connect/token",
      "scopes": {
        "batch:read": "Read access to batch jobs",
        "batch:write": "Write access to batch jobs"
      }
    }
  },
  "security": [
    {
      "oauth2": [
        "batch:read",
        "batch:write"
      ]
    }
  ],
  "paths": {
    "/jobs": {
      "post": {
        "summary": "Submit a new batch job",
        "parameters": [
          {
            "name": "job",
            "in": "body",
            "required": true,
            "schema": {
              "$ref": "#/definitions/JobSubmitRequest"
            }
          }
        ],
        "responses": {
          "202": {
            "description": "Job accepted for processing",
            "schema": {
              "$ref": "#/definitions/JobSubmitResponse"
            }
          }
        }
      },
      "get": {
        "summary": "List batch jobs",
        "parameters": [
          {
            "name": "status",
            "in": "query",
            "type": "string",
            "enum": [
              "RUNNING",
              "COMPLETED",
              "FAILED",
              "PENDING",
              "ALL"
            ],
            "default": "ALL"
          }
        ],
        "responses": {
          "200": {
            "description": "List of batch jobs",
            "schema": {
              "type": "array",
              "items": {
                "$ref": "#/definitions/JobInfo"
              }
            }
          }
        }
      }
    },
    "/jobs/{jobId}": {
      "get": {
        "summary": "Get job details",
        "parameters": [
          {
            "name": "jobId",
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "responses": {
          "200": {
            "description": "Job details",
            "schema": {
              "$ref": "#/definitions/JobDetails"
            }
          }
        }
      },
      "delete": {
        "summary": "Cancel a job",
        "parameters": [
          {
            "name": "jobId",
            "in": "path",
            "required": true,
            "type": "string"
          }
        ],
        "responses": {
          "204": {
            "description": "Job cancelled"
          }
        }
      }
    }
  },
  "definitions": {
    "JobSubmitRequest": {
      "type": "object",
      "required": [
        "system",
        "jobScript"
      ],
      "properties": {
        "system": {
          "type": "string",
          "description": "GCOS system name"
        },
        "jobScript": {
          "type": "string",
          "description": "JCL or other job script"
        },
        "priority": {
          "type": "integer",
          "minimum": 1,
          "maximum": 9,
          "default": 5
        },
        "notifyEmail": {
          "type": "string",
          "format": "email"
        }
      }
    },
    "JobSubmitResponse": {
      "type": "object",
      "properties": {
        "jobId": {
          "type": "string"
        },
        "system": {
          "type": "string"
        },
        "submitTime": {
          "type": "string",
          "format": "date-time"
        },
        "status": {
          "type": "string",
          "enum": [
            "PENDING",
            "SUBMITTED"
          ]
        }
      }
    },
    "JobInfo": {
      "type": "object",
      "properties": {
        "jobId": {
          "type": "string"
        },
        "system": {
          "type": "string"
        },
        "submitTime": {
          "type": "string",
          "format": "date-time"
        },
        "status": {
          "type": "string",
          "enum": [
            "RUNNING",
            "COMPLETED",
            "FAILED",
            "PENDING"
          ]
        },
        "priority": {
          "type": "integer"
        }
      }
    },
    "JobDetails": {
      "type": "object",
      "properties": {
        "jobId": {
          "type": "string"
        },
        "system": {
          "type": "string"
        },
        "submitTime": {
          "type": "string",
          "format": "date-time"
        },
        "startTime": {
          "type": "string",
          "format": "date-time"
        },
        "endTime": {
          "type": "string",
          "format": "date-time"
        },
        "status": {
          "type": "string",
          "enum": [
            "RUNNING",
            "COMPLETED",
            "FAILED",
            "PENDING"
          ]
        },
        "exitCode": {
          "type": "integer"
        },
        "outputFiles": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/OutputFile"
          }
        }
      }
    },
    "OutputFile": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string"
        },
        "type": {
          "type": "string",
          "enum": [
            "LISTING",
            "OUTPUT",
            "LOG",
            "OTHER"
          ]
        },
        "url": {
          "type": "string"
        },
        "size": {
          "type": "integer"
        }
      }
    }
  }
}
```

## 2. 🧰 Migration+ Toolset Implementation

### Prerequisites
- Atos Migration+ Toolset
- GCOS application source code
- Target environment specifications
- Network connectivity between GCOS and modernization environment
- Appropriate licenses and permissions

### Configuration Steps

#### 2.1 Configure Migration+ Environment

Set up the Migration+ environment:

```bash
# Install Migration+ package
sudo apt-get update
sudo apt-get install -y migration-plus-toolset

# Configure the environment
sudo migration-plus-config --init
sudo migration-plus-config --set-license-file /path/to/license.key
sudo migration-plus-config --set-gcos-connection GCOS8_PROD
sudo migration-plus-config --set-output-dir /opt/atos/migration-output
```

#### 2.2 Configure Automated Code Analysis

Create a configuration file for code analysis:

```yaml
analysis:
  name: GCOS8-Analysis
  source:
    type: GCOS8
    connection: GCOS8_PROD
    library: PRODLIB
    applications:
      - name: ACCT
        programs:
          - ACCTMAIN
          - ACCTUTIL
          - ACCTRPT
      - name: INVT
        programs:
          - INVTMAIN
          - INVTPRC
          - INVTRPT
  
  rules:
    - category: Performance
      enabled: true
      severity: medium
      
    - category: Maintainability
      enabled: true
      severity: high
      
    - category: Security
      enabled: true
      severity: critical
      
    - category: BestPractices
      enabled: true
      severity: low
  
  output:
    format:
      - JSON
      - HTML
      - CSV
    location: /opt/atos/migration-output/analysis
    reportName: gcos8-analysis-report
```

#### 2.3 Configure Code Transformation

Create a configuration for code transformation:

```yaml
transformation:
  name: GCOS8-Transformation
  source:
    type: GCOS8
    connection: GCOS8_PROD
    library: PRODLIB
    applications:
      - name: ACCT
        programs:
          - ACCTMAIN
          - ACCTUTIL
          - ACCTRPT
  
  target:
    language: Java
    version: 11
    framework: Spring
    database: PostgreSQL
    packaging: Maven
  
  mappings:
    dataTypes:
      - source: COMP-3
        target: BigDecimal
      - source: PIC X
        target: String
      - source: PIC 9
        target: Integer
        
    fileTypes:
      - source: INDEXED
        target: JPA/Hibernate
      - source: SEQUENTIAL
        target: FileInputStream/FileOutputStream
        
    screenFormats:
      - source: TS
        target: Spring MVC + Thymeleaf
      - source: FORM
        target: HTML + JavaScript
  
  options:
    preserveComments: true
    generateDocumentation: true
    optimizeCode: true
    includeUnitTests: true
    
  output:
    location: /opt/atos/migration-output/transformation
    structure:
      type: Maven
      groupId: com.example.legacy
      artifactId: gcos-modernization
      version: 1.0.0
```

#### 2.4 Create Data Migration Configuration

Configure data migration:

```yaml
dataMigration:
  name: GCOS8-DataMigration
  source:
    type: GCOS8
    connection: GCOS8_PROD
    files:
      - name: CUSTOMER
        format: INDEXED
        record: CUSTREC
      - name: ACCOUNT
        format: INDEXED
        record: ACCTREC
      - name: TRANSACTION
        format: SEQUENTIAL
        record: TRANSREC
  
  target:
    type: PostgreSQL
    connection:
      host: db.example.com
      port: 5432
      database: legacy_migration
      username: migration_user
      password_encrypted: enc:U1V2W3X4Y5Z6A7B8C9D0
  
  mappings:
    - sourceFile: CUSTOMER
      targetTable: customers
      fields:
        - source: CUST-ID
          target: id
          type: VARCHAR(20)
          key: true
        - source: CUST-NAME
          target: name
          type: VARCHAR(100)
        - source: CUST-ADDR
          target: address
          type: VARCHAR(200)
        - source: CUST-PHONE
          target: phone
          type: VARCHAR(20)
          
    - sourceFile: ACCOUNT
      targetTable: accounts
      fields:
        - source: ACCT-ID
          target: id
          type: VARCHAR(20)
          key: true
        - source: CUST-ID
          target: customer_id
          type: VARCHAR(20)
          foreignKey:
            table: customers
            column: id
        - source: ACCT-TYPE
          target: type
          type: VARCHAR(10)
        - source: ACCT-BALANCE
          target: balance
          type: DECIMAL(15,2)
  
  options:
    batchSize: 1000
    validateData: true
    errorHandling: "LOG_AND_CONTINUE"
    auditTrail: true
    
  schedule:
    type: INCREMENTAL
    frequency: DAILY
    time: "01:00"
    
  output:
    location: /opt/atos/migration-output/data-migration
    logs: true
    summary: true
    errorReport: true
```

## 3. 🔄 GCOS CI/CD Pipeline Integration

### Prerequisites
- GCOS operations and management software
- Network connectivity to GitHub
- Middleware for job submission and monitoring
- Authentication credentials for GCOS systems

### Configuration Steps

#### 3.1 Configure GitHub to GCOS Integration

Create a GitHub Actions workflow file for GCOS integration:

```yaml
# .github/workflows/gcos-integration.yml
name: GCOS Integration Pipeline

on:
  push:
    branches: [ main, develop ]
    paths:
      - 'src/gcos/**'
      - 'jcl/**'
  pull_request:
    branches: [ main ]
    paths:
      - 'src/gcos/**'
      - 'jcl/**'
  workflow_dispatch:

jobs:
  analyze:
    name: Analyze GCOS Code
    runs-on: ubuntu-latest
    steps:
      - name: Checkout code
        uses: actions/checkout@v3
        
      - name: Set up JDK 11
        uses: actions/setup-java@v3
        with:
          java-version: '11'
          distribution: 'temurin'
          
      - name: Install Migration+ CLI
        run: |
          curl -o migration-plus-cli.deb https://atos-download.example.com/migration-plus-cli.deb
          sudo dpkg -i migration-plus-cli.deb
          
      - name: Analyze GCOS code
        run: |
          migration-plus analyze \
            --config-file .github/workflows/config/analysis-config.yaml \
            --source-dir ./src/gcos \
            --output-dir ./analysis-results
        env:
          GCOS_CONNECTION_STRING: ${{ secrets.GCOS_CONNECTION_STRING }}
          GCOS_USERNAME: ${{ secrets.GCOS_USERNAME }}
          GCOS_PASSWORD: ${{ secrets.GCOS_PASSWORD }}
          
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
        
      - name: Download analysis results
        uses: actions/download-artifact@v3
        with:
          name: analysis-results
          path: ./analysis-results
          
      - name: Set up JDK 11
        uses: actions/setup-java@v3
        with:
          java-version: '11'
          distribution: 'temurin'
          
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
          cp -r ./src/gcos ./build/package/
          cp -r ./build/jcl ./build/package/
          tar -czvf gcos-package.tar.gz -C ./build/package .
          
      - name: Upload package
        uses: actions/upload-artifact@v3
        with:
          name: gcos-package
          path: ./gcos-package.tar.gz
          
  deploy:
    name: Deploy to GCOS
    runs-on: ubuntu-latest
    needs: build
    if: github.ref == 'refs/heads/main' || github.ref == 'refs/heads/develop'
    environment: ${{ github.ref == 'refs/heads/main' && 'production' || 'test' }}
    steps:
      - name: Download package
        uses: actions/download-artifact@v3
        with:
          name: gcos-package
          
      - name: Install GCOS utilities
        run: |
          curl -o gcos-utils.deb https://atos-download.example.com/gcos-utils.deb
          sudo dpkg -i gcos-utils.deb
          
      - name: Transfer files to GCOS
        run: |
          mkdir -p ./deploy
          tar -xzvf gcos-package.tar.gz -C ./deploy
          
          # Transfer files to GCOS
          gcos-file-transfer \
            --host ${{ secrets.GCOS_HOST }} \
            --port ${{ secrets.GCOS_PORT }} \
            --user ${{ secrets.GCOS_USERNAME }} \
            --password ${{ secrets.GCOS_PASSWORD }} \
            --source-dir ./deploy \
            --dest-dir ${{ github.ref == 'refs/heads/main' && 'PROD.GIT' || 'TEST.GIT' }} \
            --transfer-mode BINARY
            
      - name: Submit JCL jobs
        run: |
          # Submit GCOS batch jobs
          gcos-job-submit \
            --host ${{ secrets.GCOS_HOST }} \
            --port ${{ secrets.GCOS_PORT }} \
            --user ${{ secrets.GCOS_USERNAME }} \
            --password ${{ secrets.GCOS_PASSWORD }} \
            --jcl-file ${{ github.ref == 'refs/heads/main' && './deploy/jcl/deploy-prod.jcl' || './deploy/jcl/deploy-test.jcl' }} \
            --wait-for-completion \
            --timeout 1800
            
      - name: Verify deployment
        run: |
          # Check deployment status
          gcos-job-status \
            --host ${{ secrets.GCOS_HOST }} \
            --port ${{ secrets.GCOS_PORT }} \
            --user ${{ secrets.GCOS_USERNAME }} \
            --password ${{ secrets.GCOS_PASSWORD }} \
            --job-name ${{ github.ref == 'refs/heads/main' && 'DEPLOYPRD' || 'DEPLOYTST' }} \
            --check-exit-code
```

#### 3.2 Configure JCL Templates

Create a JCL template for deployment to GCOS:

```
//DEPLOY JOB ACCT=1234,CLASS=A,MSGCLASS=X,NOTIFY=&SYSUID
//*
//* DEPLOYMENT JOB FOR __ENV__ ENVIRONMENT
//* BUILD: __BUILD__
//* DATE: __DATE__
//*
//JOBLIB   DD DSN=SYS1.UTILLIB,DISP=SHR
//         DD DSN=SYS1.PROCLIB,DISP=SHR
//*
//CLEANUP EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE __ENV__.DEPLOY.STAGING PURGE
  IF LASTCC <= 8 THEN SET MAXCC = 0
/*
//*
//CREATE   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE CLUSTER(NAME(__ENV__.DEPLOY.STAGING) -
         VOLUMES(SYSRES) -
         CYLINDERS(10 5) -
         RECORDSIZE(80 32760) -
         KEYS(8 0) -
         INDEXED -
         SHAREOPTIONS(2 3))
  IF LASTCC <= 8 THEN SET MAXCC = 0
/*
//*
//STAGE    EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD DSN=__ENV__.GIT.SOURCE,DISP=SHR
//SYSUT2   DD DSN=__ENV__.DEPLOY.STAGING,DISP=SHR
//SYSIN    DD DUMMY
//*
//COMPILE  EXEC PROC=GCOSCOMP,
//         SOURCE=__ENV__.DEPLOY.STAGING,
//         OBJECT=__ENV__.OBJECT.LIB,
//         OPTIONS='OPTIMIZE,NOADV'
//*
//LINK     EXEC PROC=GCOSLNK,
//         OBJECT=__ENV__.OBJECT.LIB,
//         LOADLIB=__ENV__.LOAD.LIB,
//         OPTIONS='LET,LIST,MAP'
//*
//DBUPDATE EXEC PGM=DBUPDATE,COND=(0,NE)
//STEPLIB  DD DSN=__ENV__.LOAD.LIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  UPDATE DATABASE __ENV__DB
  APPLY CHANGES
  COMMIT
/*
//*
//VERIFY   EXEC PGM=VERIFYDB,COND=(0,NE)
//STEPLIB  DD DSN=__ENV__.LOAD.LIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//SYSIN    DD *
  VERIFY DATABASE __ENV__DB
  CHECK INTEGRITY
  REPORT FULL
/*
```

## 4. 🛡️ Security Implementation

### Prerequisites
- GCOS Security Manager or equivalent
- Identity management system compatible with GCOS
- Network connectivity between security systems
- Appropriate security administration credentials

### Configuration Steps

#### 4.1 Configure User Authentication Integration

Set up authentication integration:

```yaml
authentication:
  provider: Keycloak
  version: 18.0
  realm: gcos-security
  
  serverConfig:
    host: keycloak.example.com
    port: 8443
    useSSL: true
    adminUser: admin
    adminPassword_encrypted: enc:E1F2G3H4I5J6K7L8M9N0
    
  gcosIntegration:
    type: SAML
    entityId: https://gcos.example.com
    assertionConsumerService: https://gcos.example.com/auth/saml/callback
    nameIdFormat: urn:oasis:names:tc:SAML:1.1:nameid-format:unspecified
    signingCertificate: /path/to/signing-cert.pem
    encryptionCertificate: /path/to/encryption-cert.pem
    
  userMapping:
    gcosUserAttribute: USER-ID
    keycloakAttribute: username
    attributeMappings:
      - gcos: USER-NAME
        keycloak: name
      - gcos: USER-EMAIL
        keycloak: email
      - gcos: USER-DEPT
        keycloak: department
        
  groupMapping:
    gcosGroupAttribute: GROUP-ID
    keycloakAttribute: group
    
  roleMapping:
    gcosRoleAttribute: ROLE-ID
    keycloakAttribute: role
```

#### 4.2 Configure Access Control Policies

Set up role-based access control for GCOS resources:

```yaml
accessControl:
  policyEngine: GCOS-RBAC
  version: 2.1
  
  roles:
    - name: Developer
      description: Application developers
      permissions:
        - resource: SOURCE.*
          actions: [READ, WRITE]
        - resource: OBJECT.*
          actions: [READ]
        - resource: TEST.*
          actions: [READ, WRITE, EXECUTE]
          
    - name: Operator
      description: System operators
      permissions:
        - resource: PRODUCTION.*
          actions: [READ, EXECUTE]
        - resource: BACKUP.*
          actions: [READ, WRITE, EXECUTE]
        - resource: MONITOR.*
          actions: [READ, EXECUTE]
          
    - name: Administrator
      description: System administrators
      permissions:
        - resource: "*"
          actions: [READ, WRITE, EXECUTE, ADMIN]
          
  userAssignments:
    - role: Developer
      users: [DEV1, DEV2, DEV3]
      groups: [DEVELOPMENT]
      
    - role: Operator
      users: [OPS1, OPS2]
      groups: [OPERATIONS]
      
    - role: Administrator
      users: [ADMIN1, ADMIN2]
      groups: [SYSADMIN]
      
  resourceDefinitions:
    - pattern: SOURCE.*
      description: Source code libraries
      
    - pattern: OBJECT.*
      description: Object code libraries
      
    - pattern: TEST.*
      description: Test environments
      
    - pattern: PRODUCTION.*
      description: Production systems
      
    - pattern: BACKUP.*
      description: Backup systems
      
    - pattern: MONITOR.*
      description: Monitoring systems
```

## 5. 📊 Monitoring and Integration

### Prerequisites
- GCOS monitoring tools
- Log forwarding agent
- Central monitoring platform
- Network connectivity between systems

### Configuration Steps

#### 5.1 Configure Performance Monitoring

Set up performance monitoring integration:

```yaml
performanceMonitoring:
  collector: GCOS-PerfMon
  version: 3.2
  
  targets:
    - system: GCOS8_PROD
      metrics:
        - name: cpu_utilization
          source: CPU.UTILIZATION
          unit: percent
          sampling: 60  # seconds
          
        - name: memory_utilization
          source: MEMORY.UTILIZATION
          unit: percent
          sampling: 60
          
        - name: disk_io
          source: DISK.IO.RATE
          unit: ops_per_second
          sampling: 60
          
        - name: transaction_rate
          source: TRANSACTION.RATE
          unit: transactions_per_second
          sampling: 30
          
        - name: response_time
          source: RESPONSE.TIME
          unit: milliseconds
          sampling: 30
          
  output:
    type: Prometheus
    endpoint: http://prometheus.example.com:9091/metrics/job/gcos
    authentication:
      username: prometheus
      password_encrypted: enc:O1P2Q3R4S5T6U7V8W9X0
      
  alertRules:
    - name: high_cpu_utilization
      condition: cpu_utilization > 85
      duration: 5m
      severity: warning
      
    - name: critical_cpu_utilization
      condition: cpu_utilization > 95
      duration: 5m
      severity: critical
      
    - name: high_response_time
      condition: response_time > 5000
      duration: 5m
      severity: warning
```

#### 5.2 Configure Log Forwarding

Set up log forwarding for centralized logging:

```yaml
logging:
  collector: GCOS-LogForwarder
  version: 2.4
  
  sources:
    - name: system_logs
      path: /logs/system/*
      format: syslog
      
    - name: application_logs
      path: /logs/application/*
      format: custom
      pattern: "%{TIMESTAMP:timestamp} [%{LOGLEVEL:level}] %{DATA:component} - %{GREEDYDATA:message}"
      
    - name: security_logs
      path: /logs/security/*
      format: cef
      
    - name: transaction_logs
      path: /logs/transactions/*
      format: custom
      pattern: "%{TIMESTAMP:timestamp} %{DATA:transaction_id} %{DATA:user} %{GREEDYDATA:details}"
      
  processors:
    - name: timestamp_converter
      type: date_converter
      field: timestamp
      formats:
        - "yyyy-MM-dd HH:mm:ss,SSS"
        - "yyyy-MM-dd'T'HH:mm:ss.SSSZ"
        
    - name: anonymizer
      type: anonymizer
      fields:
        - account_number
        - credit_card
        - social_security
        
    - name: enricher
      type: enricher
      fields:
        - name: environment
          value: "production"
        - name: system
          value: "GCOS8"
          
  outputs:
    - name: elasticsearch
      type: elasticsearch
      hosts: ["https://elasticsearch.example.com:9200"]
      index: "gcos-logs-%{+YYYY.MM.dd}"
      authentication:
        username: elastic
        password_encrypted: enc:Y1Z2A3B4C5D6E7F8G9H0
        
    - name: s3_archive
      type: s3
      bucket: logs-archive
      region: us-west-2
      prefix: "gcos/logs/%{+YYYY/MM/dd}"
      authentication:
        access_key_encrypted: enc:J1K2L3M4N5O6P7Q8R9S0
        secret_key_encrypted: enc:T1U2V3W4X5Y6Z7A8B9C0
```

## 6. 🧪 Testing Framework Implementation

### Prerequisites
- GCOS testing frameworks
- Test automation tools
- Network connectivity to CI/CD system
- Test data management solution

### Configuration Steps

#### 6.1 Configure Automated Testing Framework

Set up an automated testing framework configuration:

```yaml
testingFramework:
  name: GCOS-TestAutomation
  version: 2.0
  
  testSuites:
    - name: unit_tests
      path: /tests/unit/*
      framework: GCOSUnit
      timeout: 300  # seconds
      
    - name: integration_tests
      path: /tests/integration/*
      framework: GCOSIntegration
      timeout: 600
      
    - name: system_tests
      path: /tests/system/*
      framework: GCOSSystem
      timeout: 1800
      
    - name: performance_tests
      path: /tests/performance/*
      framework: GCOSPerformance
      timeout: 3600
      
  testData:
    management: GCOS-TestData
    sources:
      - name: development
        path: /testdata/dev/*
        refresh: daily
        
      - name: production_sample
        path: /testdata/prod_sample/*
        refresh: weekly
        anonymized: true
        
  execution:
    parallel: true
    maxThreads: 4
    retryCount: 2
    retryDelay: 30  # seconds
    
  reporting:
    formats:
      - junit
      - html
      - json
      
    outputs:
      - type: file
        path: /reports/test-results
        
      - type: http
        url: https://test-results.example.com/api/results
        authentication:
          token_encrypted: enc:D1E2F3G4H5I6J7K8L9M0
```

#### 6.2 Create Test Execution Script

Create a script for executing tests:

```bash
#!/bin/bash

# GCOS Test Execution Script
# Parameters:
# $1 - Test suite name
# $2 - Environment (DEV, TEST, PROD)
# $3 - Output directory

# Configuration
GCOS_HOST="${GCOS_HOST:-gcos.example.com}"
GCOS_PORT="${GCOS_PORT:-3023}"
GCOS_USER="${GCOS_USER:-testuser}"
GCOS_PASSWORD="${GCOS_PASSWORD:-testpassword}"
TEST_SUITE="$1"
ENVIRONMENT="$2"
OUTPUT_DIR="$3"

# Validate parameters
if [[ -z "$TEST_SUITE" || -z "$ENVIRONMENT" || -z "$OUTPUT_DIR" ]]; then
  echo "Error: Missing required parameters"
  echo "Usage: $0 <test_suite> <environment> <output_dir>"
  exit 1
fi

# Create output directory if it doesn't exist
mkdir -p "$OUTPUT_DIR"

# Execute tests
echo "Executing $TEST_SUITE tests in $ENVIRONMENT environment..."

# Connect to GCOS and run tests
gcos-test-runner \
  --host "$GCOS_HOST" \
  --port "$GCOS_PORT" \
  --user "$GCOS_USER" \
  --password "$GCOS_PASSWORD" \
  --suite "$TEST_SUITE" \
  --environment "$ENVIRONMENT" \
  --output-format json \
  --output-file "$OUTPUT_DIR/test-results.json"

# Check execution status
if [ $? -eq 0 ]; then
  echo "Test execution completed successfully."
  
  # Generate HTML report
  gcos-report-generator \
    --input-file "$OUTPUT_DIR/test-results.json" \
    --output-format html \
    --output-file "$OUTPUT_DIR/test-report.html"
    
  echo "Test report generated at $OUTPUT_DIR/test-report.html"
  exit 0
else
  echo "Test execution failed."
  exit 1
fi
```

## 📋 Conclusion

This implementation guide provides a comprehensive approach to modernizing Bull GCOS mainframe applications. By following these steps, organizations can integrate their GCOS systems with modern CI/CD practices using GitHub and cloud services while preserving their critical mainframe applications and knowledge.

## ➡️ Next Steps

- Explore [🧪 Code Analysis](../05-code-analysis/README.md) for AI-powered application assessment
- Learn about [🐙 GitHub Integration](../06-github-integration/README.md) for mainframe modernization
- Implement [🧠 AI-Powered Transformation](../08-ai-transformation/README.md) for your applications 