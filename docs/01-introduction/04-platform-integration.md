# 🔄 Mainframe Platform Integration Setup

This guide provides detailed implementation instructions for setting up integration with various mainframe platforms using Azure AI Foundry. It covers specific integration requirements and configuration steps for IBM z/OS, Unisys ClearPath, Bull GCOS, and NEC ACOS mainframe systems.

## 📋 Overview

Integrating Azure AI Foundry with your mainframe environment requires establishing secure and efficient communication channels between your existing systems and the Azure cloud. This guide covers the setup requirements for different mainframe platforms, including network configuration, security settings, and component installation.

## 1. IBM z/OS Integration

### 1.1 Prerequisites for IBM z/OS Integration

Before setting up z/OS integration with Azure AI Foundry, ensure you have:

| Prerequisite | Description |
|--------------|-------------|
| z/OS Version | z/OS V2.2 or later recommended |
| Security Access | Appropriate security permissions to install and configure components |
| Network Connectivity | Network connectivity between your z/OS system and Azure |
| z/OSMF | z/OS Management Facility (if using RESTful API integration) |
| Git on z/OS | Optional but recommended for source control integration |

### 1.2 Integration Architecture Overview

The following diagram illustrates the key components involved in the z/OS integration architecture:

```
┌─────────────────────────┐                   ┌──────────────────────────┐
│   IBM z/OS ENVIRONMENT  │                   │      AZURE CLOUD         │
│                         │                   │                          │
│  ┌─────────────────┐    │  ExpressRoute/    │  ┌──────────────────┐    │
│  │ z/OS Connect    │    │  Private Link     │  │                  │    │
│  │ Enterprise      ├────┼───────────────────┼──┤  Azure API       │    │
│  │ Edition         │    │                   │  │  Management      │    │
│  └─────────────────┘    │                   │  │                  │    │
│                         │                   │  └──────────────────┘    │
│  ┌─────────────────┐    │                   │                          │
│  │ IBM MQ          ├────┼───────────────────┼─┐                        │
│  │                 │    │                   │ │                        │
│  └─────────────────┘    │                   │ │                        │
│                         │                   │ │ ┌──────────────────┐   │
│  ┌─────────────────┐    │                   │ │ │                  │   │
│  │ Connect:Direct  ├────┼───────────────────┼─┼─┤  Azure Logic     │   │
│  │                 │    │                   │ │ │  Apps/Functions  │   │
│  └─────────────────┘    │                   │ │ │                  │   │
│                         │                   │ │ └──────────────────┘   │
│  ┌─────────────────┐    │                   │ │                        │
│  │ TN3270 Services ├────┼───────────────────┼─┘                        │
│  │                 │    │                   │                          │
│  └─────────────────┘    │                   │                          │
│                         │                   │  ┌──────────────────┐    │
│  ┌─────────────────┐    │                   │  │                  │    │
│  │ CICS/IMS/DB2    │    │                   │  │  Azure AI        │    │
│  │ Native Services ├────┼───────────────────┼──┤  Foundry         │    │
│  │                 │    │                   │  │                  │    │
│  └─────────────────┘    │                   │  └──────────────────┘    │
└─────────────────────────┘                   └──────────────────────────┘
```

### 1.3 Integration Component Setup

#### 1.3.1 IBM z/OS Connect Enterprise Edition

IBM z/OS Connect EE serves as a primary integration point for RESTful API access to z/OS services.

**Installation Procedure**

1. **Prepare JCL for Installation**

```jcl
//ZOSINST  JOB (ACCT),'INSTALL Z/OS CONNECT',CLASS=A,
//         MSGCLASS=X,MSGLEVEL=(1,1),NOTIFY=&SYSUID
//STEP1    EXEC PGM=IKJEFT01,DYNAMNBR=20
//SYSTSPRT DD SYSOUT=*
//SYSTSIN  DD *
  PROFILE PREFIX(ZCONNECT)
  RECEIVE INDATASET('ZCONNECT.SMPPTFIN')
  RESTORE S(ZCONHLQ) VOLUME(VOLSER)
/*
```

2. **Configure Server.xml Configuration**

```xml
<server description="z/OS Connect EE Server">
    <featureManager>
        <feature>zosconnect:apiRequester-2.0</feature>
        <feature>zosconnect:apiRequester-2.0</feature>
        <feature>zosconnect:zosConnectManager-1.0</feature>
        <feature>transportSecurity-1.0</feature>
    </featureManager>
    
    <httpEndpoint id="defaultHttpEndpoint" host="*" 
                 httpPort="9080" httpsPort="9443" />
    
    <keyStore id="defaultKeyStore" password="password" />
    
    <sslDefault sslRef="defaultSSLConfig" />
    <ssl id="defaultSSLConfig" keyStoreRef="defaultKeyStore" />
    
    <zosconnect_apiRequester id="apiRequesterGroup1" />
    <zosconnect_apiRequester id="apiRequesterGroup2" />
    
    <zosconnect_zosConnectManager id="zosConnectMgr1" 
                                 adminAPIPort="9443"
                                 adminAPIHost="*"
                                 adminSecurity="true" />
</server>
```

3. **Create API Service Provider**

```json
{
  "name": "CICSCustomerService",
  "description": "Customer Service API hosted in CICS",
  "version": "1.0.0",
  "connection": {
    "type": "cics",
    "connectionRef": "CICSPROD"
  },
  "serviceMapping": {
    "operations": [
      {
        "name": "getCustomer",
        "program": "CUSTINQ",
        "type": "command",
        "structure": {
          "input": "structures/CUSTINQ_input.json",
          "output": "structures/CUSTINQ_output.json"
        }
      }
    ]
  }
}
```

**Security Configuration**

1. **Configure SAF-based Authentication**

```xml
<server>
  <safRegistry id="saf" />
  <safCredentials profilePrefix="BBGZDFLT" />
  
  <administrator-role>
    <user>ZCSADMIN</user>
  </administrator-role>
  
  <apiRequester-role>
    <user>ZCSAPIREQ</user>
  </apiRequester-role>
</server>
```

2. **Create RACF Profiles**

```
RDEFINE FACILITY BBGZDFLT.ZOSCONNECT.ADMIN UACC(NONE)
PERMIT BBGZDFLT.ZOSCONNECT.ADMIN CLASS(FACILITY) ID(ZCSADMIN) ACCESS(READ)

RDEFINE FACILITY BBGZDFLT.ZOSCONNECT.APIREQUESTER UACC(NONE)
PERMIT BBGZDFLT.ZOSCONNECT.APIREQUESTER CLASS(FACILITY) ID(ZCSAPIREQ) ACCESS(READ)

SETROPTS RACLIST(FACILITY) REFRESH
```

#### 1.3.2 IBM MQ Integration

**MQ Channel Configuration**

1. **Create MQ QMgr Definition**

```
DEFINE QMGR CHLAUTH(DISABLED) CONNAUTH(SYSTEM.DEFAULT.AUTHINFO)
```

2. **Create Server Connection Channel**

```
DEFINE CHANNEL(AZURE.SVRCONN) CHLTYPE(SVRCONN) TRPTYPE(TCP) +
       SSLCIPH(ANY_TLS12_OR_HIGHER) SSLCAUTH(OPTIONAL) +
       MCAUSER('MQUSER') MAXMSGL(104857600)
```

3. **Create Client Connection Channel**

```
DEFINE CHANNEL(AZURE.CLNTCONN) CHLTYPE(CLNTCONN) TRPTYPE(TCP) +
       CONNAME('mainframe.example.com(1414)') QMNAME(MQ01) +
       SSLCIPH(ANY_TLS12_OR_HIGHER)
```

4. **Create Queues**

```
DEFINE QLOCAL(AZURE.REQUEST.QUEUE) MAXDEPTH(999999999)
DEFINE QLOCAL(AZURE.RESPONSE.QUEUE) MAXDEPTH(999999999)
DEFINE QLOCAL(AZURE.BACKOUT.QUEUE) MAXDEPTH(999999999)
```

**Azure-Side Integration**

1. **Create Azure Logic App Connector for IBM MQ**

```json
{
  "properties": {
    "displayName": "IBM MQ Connection",
    "stateless": false,
    "connectionParameters": {
      "queueManager": {
        "type": "string",
        "uiDefinition": {
          "displayName": "Queue Manager",
          "description": "The name of the IBM MQ Queue Manager",
          "tooltip": "Provide the MQ Queue Manager name",
          "constraints": {
            "required": "true"
          }
        }
      },
      "hostname": {
        "type": "string",
        "uiDefinition": {
          "displayName": "Host Name",
          "tooltip": "The host name of MQ server",
          "constraints": {
            "required": "true"
          }
        }
      },
      "port": {
        "type": "integer",
        "uiDefinition": {
          "displayName": "Port",
          "tooltip": "MQ connection port (default is 1414)",
          "constraints": {
            "required": "true"
          }
        }
      },
      "channelName": {
        "type": "string",
        "uiDefinition": {
          "displayName": "Channel Name",
          "tooltip": "The SVRCONN channel to connect to",
          "constraints": {
            "required": "true"
          }
        }
      },
      "queueName": {
        "type": "string",
        "uiDefinition": {
          "displayName": "Queue Name",
          "tooltip": "The queue name to connect to",
          "constraints": {
            "required": "true"
          }
        }
      },
      "authentication": {
        "type": "object",
        "properties": {
          "type": {
            "type": "string",
            "enum": ["Basic", "None"],
            "default": "Basic"
          },
          "username": {
            "type": "string"
          },
          "password": {
            "type": "securestring"
          }
        },
        "required": ["type"]
      }
    }
  }
}
```

#### 1.3.3 TN3270 Integration

**Host Configuration**

1. **Configure TN3270 Server**

Here is a sample TN3270 server configuration:

```
TELNETPARMS
  PORT 23
  SECUREPORT 992
  KEYRING TCPIP/TELNET
  INACTIVE 0
  TIMEMARK 600
  SCANINTERVAL 120
  FULLDATATRACE
  SMFINIT 0  SMFTERM 0
  SNAEXT
  MSG07
ENDTELNETPARMS

BEGINVTAM
  PORT 23 992
  DEFAULTLUS
    TCP00001..TCP99999
  ENDDEFAULTLUS

  DEFAULTAPPL TSO

  ALLOWAPPL TSO* DISCONNECTABLE
  ALLOWAPPL CICS* DISCONNECTABLE
  ALLOWAPPL IMS* DISCONNECTABLE

  USSTCP USSTTCP1
ENDVTAM
```

2. **Automation Macro Configuration**

```js
// CICS Customer Inquiry Macro
function inquireCustomer(customerId) {
  // Wait for CICS ready screen
  WaitForString("CICS", -1, -1);
  
  // Type transaction
  SendKeys("CINQ");
  SendKeys("[enter]");
  
  // Wait for inquiry screen
  WaitForString("CUSTOMER INQUIRY", -1, -1);
  
  // Enter customer ID
  SendKeys("[tab]");
  SendKeys("" + customerId);
  SendKeys("[enter]");
  
  // Extract data
  var customerName = GetText(4, 15, 30);
  var accountStatus = GetText(6, 15, 10);
  var accountBalance = GetText(8, 15, 12);
  
  return {
    id: customerId,
    name: customerName.trim(),
    status: accountStatus.trim(),
    balance: accountBalance.trim()
  };
}
```

**Azure Integration**

1. **Azure Function App for TN3270 Integration**

```csharp
using System;
using System.IO;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Azure.WebJobs;
using Microsoft.Azure.WebJobs.Extensions.Http;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.Logging;
using Newtonsoft.Json;
using HODCore; // IBM Host On-Demand Core API

public static class TN3270Integration
{
    [FunctionName("CustomerInquiry")]
    public static async Task<IActionResult> Run(
        [HttpTrigger(AuthorizationLevel.Function, "post", Route = null)] HttpRequest req,
        ILogger log)
    {
        log.LogInformation("C# HTTP trigger function processing a request.");

        string requestBody = await new StreamReader(req.Body).ReadToEndAsync();
        dynamic data = JsonConvert.DeserializeObject(requestBody);
        string customerId = data?.customerId;

        if (string.IsNullOrEmpty(customerId))
        {
            return new BadRequestObjectResult("Please pass a customerId in the request body");
        }

        try
        {
            // Create TN3270 session
            HODSession session = new HODSession();
            session.Connect("mainframe.example.com", 23);
            
            // Log in to CICS
            await session.WaitForStringAsync("WELCOME", 10000);
            await session.SendKeysAsync("CESN");
            await session.SendKeysAsync("[enter]");
            
            // Enter credentials
            await session.WaitForStringAsync("CESN", 5000);
            await session.SendKeysAsync(Environment.GetEnvironmentVariable("CICS_USER"));
            await session.SendKeysAsync("[tab]");
            await session.SendKeysAsync(Environment.GetEnvironmentVariable("CICS_PASSWORD"));
            await session.SendKeysAsync("[enter]");
            
            // Wait for CICS ready and access customer inquiry
            await session.WaitForStringAsync("CICS", 5000);
            await session.SendKeysAsync("CINQ");
            await session.SendKeysAsync("[enter]");
            
            // Enter customer ID
            await session.WaitForStringAsync("CUSTOMER INQUIRY", 5000);
            await session.SendKeysAsync("[tab]");
            await session.SendKeysAsync(customerId);
            await session.SendKeysAsync("[enter]");
            
            // Extract data
            string screen = await session.GetScreenAsTextAsync();
            
            // Parse the screen (simplified)
            string[] lines = screen.Split('\n');
            string name = lines[4].Substring(15, 30).Trim();
            string status = lines[6].Substring(15, 10).Trim();
            string balance = lines[8].Substring(15, 12).Trim();
            
            // Clean up
            await session.SendKeysAsync("[pf3]");
            await session.SendKeysAsync("[pf3]");
            session.Disconnect();
            
            // Return the data
            return new OkObjectResult(new {
                customerId = customerId,
                name = name,
                status = status,
                balance = balance
            });
        }
        catch (Exception ex)
        {
            log.LogError(ex, "Error accessing mainframe");
            return new StatusCodeResult(StatusCodes.Status500InternalServerError);
        }
    }
}
```

#### 1.3.4 DB2 Direct Integration

**z/OS Configuration**

1. **Configure DB2 Distributed Data Facility (DDF)**

```sql
-- Update ZPARM parameters
UPDATE SYSIBM.ZPARMS 
   SET CMTSTAT = 'ACTIVE',
       MAXDBAT = 200,
       CONDBAT = 10000,
       IDTHTOIN = 120;
       
-- Create service
CREATE PROCEDURE SYSPROC.DSNACICS(
    IN PARM_STR VARCHAR(1000))
PROGRAM TYPE MAIN
EXTERNAL NAME DSNACICS
LANGUAGE ASSEMBLE
PARAMETER STYLE GENERAL
RUN OPTIONS 'TRAP(ON),RPTSTG(ON)'
WLM ENVIRONMENT CICSWLM 
SECURITY USER;

-- Grant access to the service
GRANT EXECUTE ON PROCEDURE SYSPROC.DSNACICS TO PUBLIC;
```

2. **Create Stored Procedure for Customer Data**

```sql
CREATE PROCEDURE CUSTOMER.GET_CUSTOMER_DATA(
    IN P_CUSTOMER_ID CHAR(10),
    OUT P_CUSTOMER_NAME VARCHAR(50),
    OUT P_ACCOUNT_STATUS CHAR(10),
    OUT P_ACCOUNT_BALANCE DECIMAL(12,2))
LANGUAGE SQL
WLM ENVIRONMENT WLMENV1
DYNAMIC RESULT SETS 0
DETERMINISTIC
COMMIT ON RETURN YES
BEGIN
    SELECT CUSTOMER_NAME, ACCOUNT_STATUS, ACCOUNT_BALANCE
    INTO P_CUSTOMER_NAME, P_ACCOUNT_STATUS, P_ACCOUNT_BALANCE
    FROM CUSTOMER.ACCOUNTS
    WHERE CUSTOMER_ID = P_CUSTOMER_ID;
END;
```

**Azure Integration**

1. **Configure Azure Key Vault for Credentials**

```bash
# Create Key Vault
az keyvault create --name kv-mainframe-integration --resource-group rg-mainframe-mod --location eastus2

# Add secrets
az keyvault secret set --vault-name kv-mainframe-integration --name DB2-Username --value "db2user"
az keyvault secret set --vault-name kv-mainframe-integration --name DB2-Password --value "securePassword123"
```

2. **Create Azure Function for DB2 Integration**

```csharp
using System;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Azure.WebJobs;
using Microsoft.Azure.WebJobs.Extensions.Http;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.Logging;
using IBM.Data.DB2;
using Azure.Identity;
using Azure.Security.KeyVault.Secrets;

public static class DB2Integration
{
    [FunctionName("GetCustomerData")]
    public static async Task<IActionResult> Run(
        [HttpTrigger(AuthorizationLevel.Function, "get", Route = "customers/{id}")] HttpRequest req,
        string id,
        ILogger log)
    {
        log.LogInformation($"Processing request for customer: {id}");

        if (string.IsNullOrEmpty(id))
        {
            return new BadRequestObjectResult("Customer ID is required");
        }

        try
        {
            // Get credentials from Key Vault
            var kvUri = Environment.GetEnvironmentVariable("KeyVaultUri");
            var secretClient = new SecretClient(new Uri(kvUri), new DefaultAzureCredential());
            var username = (await secretClient.GetSecretAsync("DB2-Username")).Value.Value;
            var password = (await secretClient.GetSecretAsync("DB2-Password")).Value.Value;

            // DB2 connection string
            string connectionString = 
                $"Database=CUSTDB;Server=mainframe.example.com:446;" + 
                $"UID={username};PWD={password};" +
                "Security=SSL;SSLServerCertificate=DigiCertGlobalRoot.arm;";

            using (DB2Connection connection = new DB2Connection(connectionString))
            {
                await connection.OpenAsync();
                
                using (DB2Command command = connection.CreateCommand())
                {
                    command.CommandText = "CALL CUSTOMER.GET_CUSTOMER_DATA(?, ?, ?, ?)";
                    command.CommandType = System.Data.CommandType.StoredProcedure;
                    
                    // Add parameters
                    var p1 = command.CreateParameter();
                    p1.ParameterName = "P_CUSTOMER_ID";
                    p1.DB2Type = DB2Type.Char;
                    p1.Direction = System.Data.ParameterDirection.Input;
                    p1.Value = id.PadRight(10);
                    command.Parameters.Add(p1);
                    
                    var p2 = command.CreateParameter();
                    p2.ParameterName = "P_CUSTOMER_NAME";
                    p2.DB2Type = DB2Type.VarChar;
                    p2.Direction = System.Data.ParameterDirection.Output;
                    p2.Size = 50;
                    command.Parameters.Add(p2);
                    
                    var p3 = command.CreateParameter();
                    p3.ParameterName = "P_ACCOUNT_STATUS";
                    p3.DB2Type = DB2Type.Char;
                    p3.Direction = System.Data.ParameterDirection.Output;
                    p3.Size = 10;
                    command.Parameters.Add(p3);
                    
                    var p4 = command.CreateParameter();
                    p4.ParameterName = "P_ACCOUNT_BALANCE";
                    p4.DB2Type = DB2Type.Decimal;
                    p4.Direction = System.Data.ParameterDirection.Output;
                    p4.Size = 12;
                    command.Parameters.Add(p4);
                    
                    // Execute procedure
                    await command.ExecuteNonQueryAsync();
                    
                    // Get results
                    var result = new {
                        customerId = id,
                        name = p2.Value.ToString().Trim(),
                        status = p3.Value.ToString().Trim(),
                        balance = Convert.ToDecimal(p4.Value)
                    };
                    
                    return new OkObjectResult(result);
                }
            }
        }
        catch (Exception ex)
        {
            log.LogError(ex, $"Error retrieving customer data for ID: {id}");
            return new StatusCodeResult(StatusCodes.Status500InternalServerError);
        }
    }
}
```

### 1.4 End-to-End Testing

After configuring the integration components, perform end-to-end testing:

1. **API Integration Test**

```bash
# Test z/OS Connect API
curl -X GET "https://zconnect.example.com:9443/zosConnect/apis/customerAPI/1.0/customers/1001" \
  -H "Content-Type: application/json" \
  -H "Authorization: Basic dXNlcjpwYXNzd29yZA==" \
  -k
```

2. **MQ Integration Test**

```bash
# Send test message to MQ
echo '{"customerId":"1001","requestType":"INQUIRY"}' > test_message.json
amqsput AZURE.REQUEST.QUEUE MQ01 < test_message.json

# Check response queue
amqsget AZURE.RESPONSE.QUEUE MQ01
```

3. **File Transfer Test**

```bash
# Submit Connect:Direct process
cd /submit,proc=AZURE_FILE_TRANSFER
```

### 1.5 Troubleshooting

| Issue | Diagnostic Steps | Resolution |
|-------|-----------------|------------|
| z/OS Connect API returns 403 | Check server.xml permissions<br>Verify SAF setup<br>Check API requester roles | Update SAF permissions<br>Correct server.xml configuration<br>Assign proper roles |
| MQ Connection Failure | Check channel status<br>Verify network routes<br>Check TLS certificates | Restart channel<br>Update firewall rules<br>Renew certificates |
| File Transfer Failures | Check Connect:Direct logs<br>Verify storage permissions<br>Check network connectivity | Fix process definitions<br>Update Azure Storage permissions<br>Test network path |
| DB2 Connection Timeouts | Review DB2 connection limits<br>Check thread usage<br>Verify network latency | Increase MAXDBAT parameter<br>Optimize queries<br>Implement connection pooling |

### 1.6 Performance Optimization

1. **Connection Pooling**
   - Implement DB2 connection pooling in Azure Functions
   - Configure MQ client connection caching
   - Use persistent sessions for TN3270 access

2. **Batch Processing**
   - Implement batch request processing for high-volume operations
   - Use array inserts/updates for DB2 operations
   - Configure MQ message batching

3. **Caching**
   - Implement Azure Redis Cache for frequently accessed data
   - Configure API Management caching policies
   - Use Azure Front Door for edge caching

## 2. Unisys ClearPath Integration

### 2.1 Prerequisites for Unisys ClearPath Integration

Before setting up Unisys ClearPath integration with Azure AI Foundry, ensure you have:

| Prerequisite | Description |
|--------------|-------------|
| ClearPath MCP or OS 2200 | Current supported version |
| ClearPath ePortal | For web and mobile interface integration |
| ClearPath Forward Connection | For secure communication with Azure |
| Network Connectivity | Network connectivity between your ClearPath system and Azure |

### 2.2 Configuring ClearPath Integration

#### 2.2.1 MCP Environment Setup

```bash
# Example MCP environment setup commands
@CREATE MY_FILE/SETUP/INTEGRATION 
SET APIS ALLOW
ENABLE HTTPS
```

#### 2.2.2 ClearPath ePortal Configuration

1. Access the ClearPath ePortal administration interface
2. Navigate to "External Connections" section
3. Add a new Azure connection with the following settings:
   - Name: AzureAIFoundry
   - URL: Your Azure AI Foundry endpoint
   - Authentication: OAuth2
   - Client ID: Your registered client ID
   - Client Secret: Your registered client secret

#### 2.2.3 Azure Resource Configuration

Create and configure the necessary Azure resources to connect with your ClearPath system:

```bash
# Create an Azure App Registration for ClearPath integration
az ad app create --display-name "ClearPath Integration" \
  --available-to-other-tenants false \
  --oauth2-allow-implicit-flow false

# Get the App ID for use in your ClearPath configuration
APP_ID=$(az ad app list --display-name "ClearPath Integration" --query "[0].appId" -o tsv)
echo $APP_ID
```

## 3. Bull GCOS Integration

### 3.1 Prerequisites for Bull GCOS Integration

Before setting up Bull GCOS integration with Azure AI Foundry, ensure you have:

| Prerequisite | Description |
|--------------|-------------|
| GCOS Version | GCOS 8 SR10 or later recommended |
| LiberTP | For transaction processing integration |
| Web Access Server | For API connectivity |
| Network Connectivity | Network connectivity between your GCOS system and Azure |

### 3.2 Configuring GCOS Integration

#### 3.2.1 GCOS Environment Setup

Configure your GCOS environment to allow external connectivity:

```
// Example GCOS configuration
SET EXTERNALCONNECTION ON
CONFIGURE APIGATEWAY ADDRESS=<azure_foundry_address>
```

#### 3.2.2 Azure Configuration for GCOS

```bash
# Create a resource group for GCOS integration
az group create --name gcos-integration --location westeurope

# Set up Azure API Management for GCOS communication
az apim create --name gcos-api-gateway \
  --resource-group gcos-integration \
  --publisher-name "Your Organization" \
  --publisher-email "admin@example.com" \
  --sku-name Basic

# Configure an API for GCOS communication
az apim api create --resource-group gcos-integration \
  --service-name gcos-api-gateway \
  --api-id gcos-api \
  --display-name "GCOS Integration API" \
  --path gcos \
  --protocols https
```

## 4. NEC ACOS Integration

### 4.1 Prerequisites for NEC ACOS Integration

Before setting up NEC ACOS integration with Azure AI Foundry, ensure you have:

| Prerequisite | Description |
|--------------|-------------|
| ACOS Version | ACOS-4 or later |
| iPackage | For web integration capabilities |
| Network Connectivity | Network connectivity between your ACOS system and Azure |

### 4.2 Configuring ACOS Integration

#### 4.2.1 ACOS Environment Setup

```
// Example ACOS configuration commands
SET EXTERNAL_SERVICE ON
CONFIGURE AZURE_CONNECTION ENDPOINT=<azure_foundry_endpoint>
```

#### 4.2.2 Azure Configuration for ACOS

```bash
# Create a resource group for ACOS integration
az group create --name acos-integration --location japaneast

# Set up Azure API Management for ACOS communication
az apim create --name acos-api-gateway \
  --resource-group acos-integration \
  --publisher-name "Your Organization" \
  --publisher-email "admin@example.com" \
  --sku-name Basic

# Configure an API for ACOS communication
az apim api create --resource-group acos-integration \
  --service-name acos-api-gateway \
  --api-id acos-api \
  --display-name "ACOS Integration API" \
  --path acos \
  --protocols https
```

## 5. Common Integration Patterns

Regardless of your mainframe platform, several common integration patterns can be implemented:

### 5.1 API-First Integration

Expose mainframe functionality as REST APIs:

![API Integration Pattern](../../images/api-integration-pattern.svg)

### 5.2 Event-Driven Integration

Use event-driven architecture to integrate mainframe with modern systems:

![Event-Driven Integration](../../images/event-driven-integration.svg)

### 5.3 Batch Integration

For high-volume data processing between mainframe and Azure:

![Batch Integration](../../images/batch-integration-pattern.svg)

## 6. Security Considerations

Implement these security measures regardless of mainframe platform:

| Security Measure | Description |
|------------------|-------------|
| Network Security | Secure network connections using VPNs or ExpressRoute |
| Authentication | Implement OAuth or certificate-based authentication |
| Data Encryption | Encrypt data in transit and at rest |
| Access Control | Implement least privilege access controls |
| Audit Logging | Enable comprehensive audit logging |

## ➡️ Next Steps

Once you've configured the integration with your mainframe platform:

1. Proceed to [GitHub & Azure DevOps Integration](05-devops-integration.md)
2. Set up your [Development Environment](../04-development-environment/README.md) 