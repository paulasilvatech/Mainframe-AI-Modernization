# 🔶 Unisys ClearPath Implementation Guide

This technical guide provides detailed implementation steps for modernizing Unisys ClearPath mainframe applications using AI and GitHub integration.

## 📋 Overview

Unisys ClearPath modernization focuses on hardware independence and cloud readiness while preserving critical business logic and institutional knowledge.

## 1. 🔌 ClearPath ePortal Implementation

### Prerequisites
- Unisys ClearPath MCP or OS 2200 system
- ClearPath ePortal 9.0 or later
- TCP/IP connectivity between ClearPath and cloud environments
- Microsoft .NET Framework 4.7 or higher
- IIS 10.0 or higher on Windows Server

### Configuration Steps

#### 1.1 Install ClearPath ePortal

```powershell
# Mount the ClearPath ePortal installation media
# Run the installer with administrative privileges
.\setup.exe

# Follow the installation wizard and select:
# - Destination folder: C:\Program Files\Unisys\ClearPath ePortal
# - Component selection: Server, Designer, and Admin components
# - Authentication: Windows Authentication
```

#### 1.2 Configure ePortal Server Instance

Create a server configuration file:

```xml
<ePortalConfiguration xmlns="http://schemas.unisys.com/eportal/configuration">
  <serverSettings>
    <setting name="ServerName" value="ePortalServer01" />
    <setting name="PortNumber" value="8080" />
    <setting name="SecurePortNumber" value="8443" />
    <setting name="EnableHTTPS" value="true" />
    <setting name="CertificateThumbprint" value="0123456789ABCDEF0123456789ABCDEF01234567" />
  </serverSettings>
  
  <connectionSettings>
    <connection name="MCPConnection1">
      <setting name="HostName" value="mcphost.example.com" />
      <setting name="PortNumber" value="2200" />
      <setting name="AuthenticationType" value="Basic" />
      <setting name="UserIDPlaceholder" value="{UserID}" />
      <setting name="PasswordPlaceholder" value="{Password}" />
      <setting name="DefaultTimeout" value="30000" />
    </connection>
  </connectionSettings>
  
  <securitySettings>
    <setting name="AuthenticationMode" value="Windows" />
    <setting name="EnableRoleBasedAuthorization" value="true" />
    <setting name="SessionTimeoutMinutes" value="30" />
  </securitySettings>
</ePortalConfiguration>
```

#### 1.3 Create Web Service Definition

Use ClearPath ePortal Designer to create a REST API from a ClearPath transaction:

```json
{
  "serviceName": "CustomerService",
  "serviceDescription": "API for accessing customer information on ClearPath MCP",
  "hostConnection": "MCPConnection1",
  "endpoints": [
    {
      "endpointName": "GetCustomer",
      "httpMethod": "GET",
      "route": "/customers/{customerId}",
      "transactionName": "GETCUST",
      "inputParameters": [
        {
          "name": "customerId",
          "sourceType": "Route",
          "sourceName": "customerId",
          "dataType": "String"
        }
      ],
      "outputMapping": {
        "type": "JSON",
        "structureName": "CustomerStructure",
        "dataTransformation": "CUST_TRANSFORM"
      }
    }
  ]
}
```

#### 1.4 Deploy the Web Service

```powershell
# Deploy service using ePortal Administrator
# Connect to ePortal Admin Console
Start-Process "http://localhost:8080/eportaladmin"

# In the console:
# 1. Navigate to Application Deployment
# 2. Click "Deploy New Application"
# 3. Browse for the CustomerService.eportal package
# 4. Select deployment options
# 5. Click "Deploy"

# Verify deployment via PowerShell
Invoke-RestMethod -Uri "http://localhost:8080/api/customers/12345" -Method Get
```

## 2. 📦 ClearPath Forward Integration

### Prerequisites
- ClearPath Forward environment
- Fabric Computing Architecture
- Network connectivity to GitHub
- API connectivity to cloud services

### Configuration Steps

#### 2.1 Configure ClearPath Forward Container Environment

```powershell
# Install ClearPath Container Environment
.\ClearPathContainerEnvironment.exe /silent /install

# Configure container settings
$configFile = "C:\Program Files\Unisys\ClearPath Container\config.xml"
$xml = New-Object System.Xml.XmlDocument
$xml.Load($configFile)

# Set container networking
$networkNode = $xml.SelectSingleNode("//Network")
$networkNode.SetAttribute("IPAddress", "10.1.1.100")
$networkNode.SetAttribute("Subnet", "255.255.255.0")
$networkNode.SetAttribute("Gateway", "10.1.1.1")

# Set resource limits
$resourceNode = $xml.SelectSingleNode("//Resources")
$resourceNode.SetAttribute("CPULimit", "4")
$resourceNode.SetAttribute("MemoryLimit", "8192")

$xml.Save($configFile)
```

#### 2.2 Start Container Environment

```powershell
# Start the container environment
Start-Service ClearPathContainer

# Verify status
Get-Service ClearPathContainer
```

#### 2.3 Install Git and GitHub CLI in Container Environment

```powershell
# Connect to container environment
Enter-PSSession -ComputerName localhost -ConfigurationName ClearPathContainer

# Install Git
Invoke-WebRequest -Uri "https://github.com/git-for-windows/git/releases/download/v2.33.0.windows.1/Git-2.33.0-64-bit.exe" -OutFile "git-installer.exe"
Start-Process -Wait -FilePath ".\git-installer.exe" -ArgumentList "/SILENT"

# Install GitHub CLI
Invoke-WebRequest -Uri "https://github.com/cli/cli/releases/download/v2.0.0/gh_2.0.0_windows_amd64.msi" -OutFile "gh-installer.msi"
Start-Process -Wait -FilePath "msiexec.exe" -ArgumentList "/i gh-installer.msi /quiet"

# Verify installations
git --version
gh --version

# Exit container session
Exit-PSSession
```

## 3. 🔄 ClearPath MCP Integration for CI/CD

### Prerequisites
- ClearPath MCP system with TCP/IP services
- ClearPath COMS services configured
- File transfer services enabled
- User credentials with appropriate access rights

### Configuration Steps

#### 3.1 Configure BIS/OA Integration for Web Services

Create the BIS/OA configuration for web service exposure:

```xml
<bisoa xmlns="http://schemas.unisys.com/bis/oa/config">
  <serviceGroup name="MainframeServices">
    <serviceDefinition name="CustomerLookup">
      <entry point="CUST/GET/V1" />
      <authentication method="Basic" />
      <endpoint type="REST" path="/api/customers" />
      <inputFormat>JSON</inputFormat>
      <outputFormat>JSON</outputFormat>
      <errorHandling retryCount="3" logErrors="true" />
    </serviceDefinition>
    
    <serviceDefinition name="OrderProcessing">
      <entry point="ORD/PROC/V1" />
      <authentication method="OAuth" />
      <endpoint type="REST" path="/api/orders" />
      <inputFormat>JSON</inputFormat>
      <outputFormat>JSON</outputFormat>
      <errorHandling retryCount="3" logErrors="true" />
    </serviceDefinition>
  </serviceGroup>
</bisoa>
```

#### 3.2 Set Up File Transfer for CI/CD Integration

Configure automated file transfer for CI/CD pipeline integration:

```bash
# Create script for automated file transfer from GitHub to MCP
cat > mcp_file_transfer.sh << 'EOF'
#!/bin/bash

# Configuration
MCP_HOST="mcphost.example.com"
MCP_PORT="22"
MCP_USER="mcpuser"
MCP_PASSWORD="mcppassword"
SOURCE_DIR="./build/mcp"
DEST_DIR="(DISK)CICD/SOURCE/"

# Establish connection and transfer files
echo "Connecting to MCP system..."
expect << EOF_EXPECT
spawn sftp -P $MCP_PORT $MCP_USER@$MCP_HOST
expect "password:"
send "$MCP_PASSWORD\r"
expect "sftp>"
send "cd $DEST_DIR\r"
expect "sftp>"
send "put -r $SOURCE_DIR/* .\r"
expect "sftp>"
send "bye\r"
expect eof
EOF_EXPECT

echo "File transfer completed."
EOF

chmod +x mcp_file_transfer.sh
```

#### 3.3 Create CANDE Integration for Compilation

Set up CANDE (Command AND Edit) integration for remote compilation:

```bash
# Create CANDE remote execution script
cat > mcp_compile.sh << 'EOF'
#!/bin/bash

# Configuration
MCP_HOST="mcphost.example.com"
MCP_PORT="23"
MCP_USER="mcpuser"
MCP_PASSWORD="mcppassword"
WORKFILE="CICD/SOURCE/COBOLPGM"

# Establish connection and execute CANDE commands
echo "Connecting to MCP system for compilation..."
expect << EOF_EXPECT
spawn telnet $MCP_HOST $MCP_PORT
expect "LOGON PLEASE"
send "$MCP_USER\r"
expect "PASSWORD:"
send "$MCP_PASSWORD\r"
expect "OK"
send "CANDE\r"
expect "OPTION:"
send "USE $WORKFILE\r"
expect "*"
send "COMPILE COBOL85 LIBRARY = OBJECT/CICD.\r"
expect "*"
send "END\r"
expect "OPTION:"
send "LOGOUT\r"
expect eof
EOF_EXPECT

echo "Compilation completed."
EOF

chmod +x mcp_compile.sh
```

#### 3.4 Deploy Process Automation

Configure an automated deployment process:

```bash
# Create deployment automation script
cat > mcp_deploy.sh << 'EOF'
#!/bin/bash

# Configuration
MCP_HOST="mcphost.example.com"
MCP_PORT="23"
MCP_USER="mcpuser"
MCP_PASSWORD="mcppassword"

# Deploy steps
echo "Connecting to MCP system for deployment..."
expect << EOF_EXPECT
spawn telnet $MCP_HOST $MCP_PORT
expect "LOGON PLEASE"
send "$MCP_USER\r"
expect "PASSWORD:"
send "$MCP_PASSWORD\r"
expect "OK"
send "COPY OBJECT/CICD/COBOLPGM TO PRODUCTION/COBOLPGM\r"
expect "OK"
send "START PRODUCTION/COBOLPGM\r"
expect "OK"
send "LOGOUT\r"
expect eof
EOF_EXPECT

echo "Deployment completed."
EOF

chmod +x mcp_deploy.sh
```

## 4. 🛡️ Security Integration

### Prerequisites
- ClearPath Security Center
- LDAP or Active Directory services
- Network connectivity between ClearPath and cloud security services
- Appropriate security roles and permissions

### Configuration Steps

#### 4.1 Configure LDAP Integration

Set up LDAP integration for unified authentication:

```xml
<ldapConfiguration xmlns="http://schemas.unisys.com/clearpath/security">
  <server host="ldap.example.com" port="389" useSSL="true">
    <authentication bindDN="cn=service-account,ou=service-accounts,dc=example,dc=com" 
                    bindPassword="encrypted:A8B7C6D5E4F3G2H1" />
  </server>
  
  <userMapping>
    <baseDN>ou=users,dc=example,dc=com</baseDN>
    <userFilter>(&amp;(objectClass=person)(sAMAccountName={0}))</userFilter>
    <attributes>
      <username>sAMAccountName</username>
      <displayName>displayName</displayName>
      <email>mail</email>
    </attributes>
  </userMapping>
  
  <groupMapping>
    <baseDN>ou=groups,dc=example,dc=com</baseDN>
    <groupFilter>(&amp;(objectClass=group)(member={0}))</groupFilter>
    <attributes>
      <groupName>cn</groupName>
    </attributes>
  </groupMapping>
</ldapConfiguration>
```

#### 4.2 Configure Role-Based Access Control

Set up RBAC for ClearPath resources:

```xml
<rbacConfiguration xmlns="http://schemas.unisys.com/clearpath/security">
  <roles>
    <role id="developers">
      <member>DevGroup</member>
      <permission resource="CICD/SOURCE/*" action="READ" />
      <permission resource="CICD/SOURCE/*" action="WRITE" />
      <permission resource="OBJECT/CICD/*" action="READ" />
    </role>
    
    <role id="operators">
      <member>OpsGroup</member>
      <permission resource="PRODUCTION/*" action="READ" />
      <permission resource="PRODUCTION/*" action="EXECUTE" />
      <permission resource="OBJECT/CICD/*" action="READ" />
      <permission resource="OBJECT/CICD/*" action="DEPLOY" />
    </role>
    
    <role id="administrators">
      <member>AdminGroup</member>
      <permission resource="*" action="*" />
    </role>
  </roles>
</rbacConfiguration>
```

## 5. 📊 Monitoring and Logging Integration

### Prerequisites
- ClearPath Operations Monitor
- Log aggregation service
- Network connectivity to monitoring systems
- Appropriate system privileges

### Configuration Steps

#### 5.1 Configure Operations Monitoring

Set up operations monitoring integration:

```xml
<monitoringConfiguration xmlns="http://schemas.unisys.com/clearpath/monitoring">
  <endpoints>
    <endpoint type="REST" url="https://monitoring.example.com/api/metrics" 
              authType="OAuth" 
              clientId="monitor-client" 
              clientSecret="encrypted:H1G2F3E4D5C6B7A8" />
  </endpoints>
  
  <metrics>
    <metric name="CPUUtilization" source="/SYSTEM/PERFORMANCE/CPU" interval="60" />
    <metric name="MemoryUtilization" source="/SYSTEM/PERFORMANCE/MEMORY" interval="60" />
    <metric name="DiskIORate" source="/SYSTEM/PERFORMANCE/DISK" interval="60" />
    <metric name="TransactionRate" source="/SYSTEM/COMS/TRANSACTIONS" interval="30" />
    <metric name="ResponseTime" source="/SYSTEM/COMS/RESPONSE" interval="30" />
  </metrics>
  
  <alerts>
    <alert metric="CPUUtilization" threshold="85" duration="300" severity="Warning" />
    <alert metric="CPUUtilization" threshold="95" duration="300" severity="Critical" />
    <alert metric="MemoryUtilization" threshold="90" duration="300" severity="Critical" />
    <alert metric="ResponseTime" threshold="5000" duration="300" severity="Warning" />
  </alerts>
</monitoringConfiguration>
```

#### 5.2 Configure Log Aggregation

Set up log aggregation for centralized logging:

```xml
<loggingConfiguration xmlns="http://schemas.unisys.com/clearpath/logging">
  <sources>
    <source name="SystemLogs" path="/LOGS/SYSTEM/*" />
    <source name="ApplicationLogs" path="/LOGS/APPLICATIONS/*" />
    <source name="SecurityLogs" path="/LOGS/SECURITY/*" />
    <source name="AuditLogs" path="/LOGS/AUDIT/*" />
  </sources>
  
  <destinations>
    <destination type="Syslog" host="logs.example.com" port="514" protocol="TCP" />
    <destination type="REST" url="https://logs.example.com/api/logs" 
                 authType="Bearer" 
                 token="encrypted:Z9Y8X7W6V5U4T3S2R1" />
  </destinations>
  
  <formats>
    <format name="Standard">
      <field name="timestamp" source="$TIMESTAMP" />
      <field name="level" source="$LEVEL" />
      <field name="source" source="$SOURCE" />
      <field name="message" source="$MESSAGE" />
      <field name="details" source="$DETAILS" />
    </format>
  </formats>
  
  <routing>
    <route source="SystemLogs" destination="Syslog" format="Standard" />
    <route source="ApplicationLogs" destination="REST" format="Standard" />
    <route source="SecurityLogs" destination="REST" format="Standard" />
    <route source="AuditLogs" destination="REST" format="Standard" />
  </routing>
</loggingConfiguration>
```

## 6. 🧪 Testing Framework Integration

### Prerequisites
- ClearPath testing frameworks 
- Test automation tools
- Network connectivity to CI/CD system
- Test data management tools

### Configuration Steps

#### 6.1 Configure Automated Testing Framework

Set up an automated testing framework:

```yaml
# Test automation configuration
testConfiguration:
  testSuites:
    - name: UnitTests
      path: /TESTS/UNIT/*
      framework: CPUnitTest
      timeout: 300
      
    - name: IntegrationTests
      path: /TESTS/INTEGRATION/*
      framework: CPIntegTest
      timeout: 600
      
    - name: SystemTests
      path: /TESTS/SYSTEM/*
      framework: CPSysTest
      timeout: 1800
      
  testData:
    sources:
      - name: DevelopmentData
        path: /TESTDATA/DEV/*
        
      - name: ProductionSample
        path: /TESTDATA/PROD/SAMPLE/*
        
  reporting:
    formats:
      - type: JUnit
        output: /TESTS/RESULTS/junit.xml
        
      - type: HTML
        output: /TESTS/RESULTS/report.html
        
      - type: JSON
        output: /TESTS/RESULTS/results.json
        
  automation:
    triggerOnCommit: true
    triggerOnSchedule: "0 0 * * *"  # Daily at midnight
    notifyOnFailure: true
    notificationEmail: "team@example.com"
```

#### 6.2 Create Test Execution Script

Set up a test execution script:

```bash
# Create test execution script
cat > run_mcp_tests.sh << 'EOF'
#!/bin/bash

# Configuration
MCP_HOST="mcphost.example.com"
MCP_PORT="23"
MCP_USER="mcpuser"
MCP_PASSWORD="mcppassword"
TEST_SUITE="UnitTests"

# Execute tests
echo "Connecting to MCP system for test execution..."
expect << EOF_EXPECT
spawn telnet $MCP_HOST $MCP_PORT
expect "LOGON PLEASE"
send "$MCP_USER\r"
expect "PASSWORD:"
send "$MCP_PASSWORD\r"
expect "OK"
send "RUN CPTEST WITH SUITE=$TEST_SUITE;\r"
expect "TEST COMPLETE"
send "COPY /TESTS/RESULTS/results.json TO (USERCODE)RESULTS ON DISK\r"
expect "OK"
send "LOGOUT\r"
expect eof
EOF_EXPECT

# Retrieve test results
sftp -P 22 $MCP_USER@$MCP_HOST:RESULTS ./test-results.json

echo "Test execution completed."
EOF

chmod +x run_mcp_tests.sh
```

## 📋 Conclusion

This implementation guide provides a comprehensive approach to modernizing Unisys ClearPath mainframe applications. By following these steps, organizations can integrate their ClearPath systems with modern CI/CD practices using GitHub and cloud services while preserving their critical mainframe applications.

## ➡️ Next Steps

- Explore [🧪 Code Analysis](../05-code-analysis/README.md) for AI-powered application assessment
- Learn about [🐙 GitHub Integration](../06-github-integration/README.md) for mainframe modernization
- Implement [🧠 AI-Powered Transformation](../08-ai-transformation/README.md) for your applications 