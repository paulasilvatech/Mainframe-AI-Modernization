# IBM z/OS Integration Setup for Azure AI Foundry

This document provides detailed technical instructions for setting up integration between IBM z/OS environments and Azure AI Foundry.

## Integration Architecture Overview

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

## Integration Component Setup

### 1. IBM z/OS Connect Enterprise Edition

IBM z/OS Connect EE serves as a primary integration point for RESTful API access to z/OS services.

#### Installation Procedure

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

#### Security Configuration

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

### 2. IBM MQ Integration

#### MQ Channel Configuration

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

#### Azure-Side Integration

1. **Create Azure Logic App Connector**

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
          "description": "The name of the queue to use",
          "tooltip": "Provide the MQ Queue name",
          "constraints": {
            "required": "true"
          }
        }
      },
      "username": {
        "type": "securestring",
        "uiDefinition": {
          "displayName": "Username",
          "tooltip": "The username to authenticate to MQ (if needed)",
          "constraints": {
            "required": "false"
          }
        }
      },
      "password": {
        "type": "securestring",
        "uiDefinition": {
          "displayName": "Password",
          "tooltip": "The password to authenticate to MQ (if needed)",
          "constraints": {
            "required": "false"
          }
        }
      }
    }
  }
}
```

### 3. Connect:Direct Integration

#### z/OS Setup

1. **Configure Connect:Direct Secure Plus**

```
set secure.pnode=yes
set secure.parameters=(TLSV12,CIPHER64)
set secure.dsn=CD.SECURE.PARMS
```

2. **Create Process to Transfer Files**

```
PROCESS AZURE_FILE_TRANSFER
  SNODE=AZURE_NODE
  STEP01 COPY FROM (
    PNODE
    DSN=PROD.CUSTOMER.EXTRACT
    DISP=SHR
  ) TO (
    SNODE
    FILE=/inbound/customer/extract.dat
    DISP=REPLACE
  )
  PEND
```

#### Azure Integration Setup

1. **Azure Storage Account Configuration**

   Create a dedicated storage account for mainframe file transfers:
   - Name: stmainframeintegration
   - Performance: Standard
   - Replication: ZRS (Zone-redundant storage)
   - Access tier: Hot
   - Enable secure transfer
   - Deploy in the same region as your Azure AI Foundry resources

2. **Azure Logic App for File Processing**

```json
{
  "definition": {
    "$schema": "https://schema.management.azure.com/providers/Microsoft.Logic/schemas/2016-06-01/workflowdefinition.json#",
    "contentVersion": "1.0.0.0",
    "parameters": {},
    "triggers": {
      "When_a_blob_is_added_or_modified": {
        "type": "ApiConnection",
        "inputs": {
          "host": {
            "connection": {
              "name": "@parameters('$connections')['azureblob']['connectionId']"
            }
          },
          "method": "get",
          "path": "/datasets/default/triggers/batch/onupdatedfile",
          "queries": {
            "folderId": "/inbound/customer",
            "maxFileCount": 10
          }
        },
        "recurrence": {
          "frequency": "Minute",
          "interval": 5
        },
        "splitOn": "@triggerBody()",
        "metadata": {
          "JTF_DESIGNER_SPLITON_EXPRESSION": "@triggerBody()"
        }
      }
    },
    "actions": {
      "Process_Mainframe_File": {
        "type": "Function",
        "inputs": {
          "function": {
            "id": "[resourceId('Microsoft.Web/sites/functions', variables('functionAppName'), 'ProcessMainframeFile')]"
          },
          "body": {
            "blobName": "@triggerBody()?['Name']",
            "containerName": "inbound"
          }
        },
        "runAfter": {}
      }
    }
  }
}
```

### 4. TN3270 Integration

#### Installation and Configuration

1. **IBM Host On-Demand Configuration**

```xml
<HOD-Config version="9.5">
  <SessionList>
    <Session name="CICS Production" type="Display">
      <Property name="HostName" value="mainframe.example.com"/>
      <Property name="Port" value="23"/>
      <Property name="HostCodePage" value="037"/>
      <Property name="SecurityType" value="TLS"/>
      <Property name="ConnectionType" value="TN3270"/>
      <Property name="MacroPath" value="/macros"/>
      <AutoSignOn>
        <Property name="UserId" value="${userid}"/>
        <Property name="Password" value="${password}"/>
        <Property name="InitialCommand" value="CESN"/>
      </AutoSignOn>
    </Session>
  </SessionList>
</HOD-Config>
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

#### Azure Integration

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

### 5. DB2 Direct Integration

#### z/OS Configuration

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

#### Azure Integration

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

## End-to-End Testing

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

## Troubleshooting

| Issue | Diagnostic Steps | Resolution |
|-------|-----------------|------------|
| z/OS Connect API returns 403 | Check server.xml permissions<br>Verify SAF setup<br>Check API requester roles | Update SAF permissions<br>Correct server.xml configuration<br>Assign proper roles |
| MQ Connection Failure | Check channel status<br>Verify network routes<br>Check TLS certificates | Restart channel<br>Update firewall rules<br>Renew certificates |
| File Transfer Failures | Check Connect:Direct logs<br>Verify storage permissions<br>Check network connectivity | Fix process definitions<br>Update Azure Storage permissions<br>Test network path |
| DB2 Connection Timeouts | Review DB2 connection limits<br>Check thread usage<br>Verify network latency | Increase MAXDBAT parameter<br>Optimize queries<br>Implement connection pooling |

## Performance Optimization

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

## Next Steps

After completing the IBM z/OS integration setup, proceed to:
- [GitHub & Azure DevOps Integration](05-devops-integration.md) to set up continuous integration and deployment
- [AI-Powered Code Analysis](../05-code-analysis/README.md) to analyze your mainframe codebase 