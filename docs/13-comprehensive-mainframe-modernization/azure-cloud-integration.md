# ☁️ Azure Cloud Integration Guide

This technical guide provides detailed implementation steps for integrating mainframe applications with Azure cloud services across multiple mainframe platforms.

## 📋 Overview

Microsoft Azure offers a comprehensive set of services for mainframe modernization that enable organizations to:

- 🔄 Migrate mainframe workloads to Azure
- 🔌 Integrate existing mainframe systems with cloud services
- 🌉 Create hybrid environments spanning mainframe and cloud
- 🧠 Leverage AI services for legacy application enhancement

## 🏗️ Integration Architectures

### 1. 🔄 Azure Logic Apps Integration Pattern

**Description**: This pattern uses Azure Logic Apps to create workflows that integrate mainframe systems with cloud services and other applications.

**Key Components**:
- **🔄 Logic Apps**: For orchestration and workflow
- **🔌 API Connectors**: For integration with mainframe systems
- **📦 Service Bus**: For asynchronous messaging
- **🔍 Azure Monitor**: For monitoring and alerting

**Sample Implementation**:

```json
{
  "definition": {
    "$schema": "https://schema.management.azure.com/providers/Microsoft.Logic/schemas/2016-06-01/workflowdefinition.json#",
    "actions": {
      "Process_Mainframe_Transaction": {
        "type": "ApiConnection",
        "inputs": {
          "host": {
            "connection": {
              "name": "@parameters('$connections')['ibmmq']['connectionId']"
            }
          },
          "method": "post",
          "path": "/message",
          "queries": {
            "queueManager": "MVS1.QMGR",
            "queue": "TRANSACTION.QUEUE",
            "messageType": "MQMT_REQUEST"
          },
          "body": "@{triggerBody()}"
        },
        "runAfter": {}
      },
      "Parse_Response": {
        "type": "ParseJson",
        "inputs": {
          "content": "@body('Process_Mainframe_Transaction')",
          "schema": {
            "type": "object",
            "properties": {
              "transactionId": { "type": "string" },
              "status": { "type": "string" },
              "data": {
                "type": "object",
                "properties": {
                  "customerId": { "type": "string" },
                  "amount": { "type": "number" },
                  "timestamp": { "type": "string" }
                }
              }
            }
          }
        },
        "runAfter": {
          "Process_Mainframe_Transaction": [ "Succeeded" ]
        }
      },
      "Update_SQL_Database": {
        "type": "SqlServerStoredProcedure",
        "inputs": {
          "host": {
            "connection": {
              "name": "@parameters('$connections')['sql']['connectionId']"
            }
          },
          "method": "post",
          "path": "/datasets/default/procedures/UpdateTransactionHistory",
          "queries": {
            "parameters": {
              "TransactionId": "@body('Parse_Response')?['transactionId']",
              "CustomerId": "@body('Parse_Response')?['data']?['customerId']",
              "Amount": "@body('Parse_Response')?['data']?['amount']",
              "TransactionDate": "@body('Parse_Response')?['data']?['timestamp']",
              "Status": "@body('Parse_Response')?['status']"
            }
          }
        },
        "runAfter": {
          "Parse_Response": [ "Succeeded" ]
        }
      }
    },
    "triggers": {
      "When_a_HTTP_request_is_received": {
        "type": "Request",
        "kind": "Http",
        "inputs": {
          "schema": {
            "type": "object",
            "properties": {
              "customerId": { "type": "string" },
              "transactionType": { "type": "string" },
              "amount": { "type": "number" }
            },
            "required": ["customerId", "transactionType", "amount"]
          }
        }
      }
    },
    "outputs": {}
  },
  "parameters": {
    "$connections": {
      "value": {
        "ibmmq": {
          "connectionId": "/subscriptions/{subscription-id}/resourceGroups/{resource-group}/providers/Microsoft.Web/connections/ibmmq",
          "connectionName": "ibmmq",
          "id": "/subscriptions/{subscription-id}/providers/Microsoft.Web/locations/{location}/managedApis/ibmmq"
        },
        "sql": {
          "connectionId": "/subscriptions/{subscription-id}/resourceGroups/{resource-group}/providers/Microsoft.Web/connections/sql",
          "connectionName": "sql",
          "id": "/subscriptions/{subscription-id}/providers/Microsoft.Web/locations/{location}/managedApis/sql"
        }
      }
    }
  }
}
```

### 2. 🌉 Azure API Management Gateway Pattern

**Description**: This pattern uses Azure API Management to create a unified API gateway that exposes mainframe services as RESTful APIs.

**Key Components**:
- **🚪 API Management**: For API hosting, documentation, and security
- **🔗 Host Integration Server**: For mainframe connectivity
- **🌐 Application Gateway**: For network security and load balancing
- **🔒 Key Vault**: For secure credential management

**Implementation Example - API Definition**:

```yaml
openapi: 3.0.1
info:
  title: Mainframe Customer API
  description: API for accessing customer data from IBM z/OS mainframe
  version: 1.0.0
servers:
  - url: https://mainframe-api.azure-api.net/customers
paths:
  /customers/{customerId}:
    get:
      summary: Get customer details
      description: Retrieves customer information from the mainframe
      operationId: getCustomer
      parameters:
        - name: customerId
          in: path
          required: true
          schema:
            type: string
      responses:
        '200':
          description: Successful operation
          content:
            application/json:
              schema:
                $ref: '#/components/schemas/Customer'
        '404':
          description: Customer not found
        '500':
          description: Internal server error
      x-mainframe-integration:
        system: "IMS"
        transaction: "CUSTINQ"
        mappings:
          request:
            - source: "customerId"
              target: "CUSTID"
          response:
            - source: "CUSTNAME"
              target: "name"
            - source: "CUSTADDR"
              target: "address"
            - source: "CUSTPHONE"
              target: "phoneNumber"
            - source: "CUSTYPE"
              target: "customerType"
            - source: "CUSTBAL"
              target: "accountBalance"
components:
  schemas:
    Customer:
      type: object
      properties:
        customerId:
          type: string
        name:
          type: string
        address:
          type: string
        phoneNumber:
          type: string
        customerType:
          type: string
          enum: [Regular, Premium]
        accountBalance:
          type: number
          format: double
```

**Implementing the Backend Policy in API Management**:

```xml
<policies>
    <inbound>
        <base />
        <set-backend-service backend-id="mainframe-connector" />
        <set-body template="liquid">
        {
            "transaction": "CUSTINQ",
            "data": {
                "CUSTID": "{{context.Request.UrlParameters["customerId"]}}"
            }
        }
        </set-body>
    </inbound>
    <backend>
        <base />
    </backend>
    <outbound>
        <base />
        <choose>
            <when condition="@(context.Response.StatusCode == 200)">
                <set-body template="liquid">
                {
                    "customerId": "{{context.Request.UrlParameters["customerId"]}}",
                    "name": "{{body.CUSTNAME}}",
                    "address": "{{body.CUSTADDR}}",
                    "phoneNumber": "{{body.CUSTPHONE}}",
                    "customerType": "{% if body.CUSTYPE == "P" %}Premium{% else %}Regular{% endif %}",
                    "accountBalance": {{body.CUSTBAL}}
                }
                </set-body>
            </when>
        </choose>
    </outbound>
    <on-error>
        <base />
    </on-error>
</policies>
```

### 3. 📦 Azure Functions Integration Pattern

**Description**: This pattern uses Azure Functions to create serverless applications that integrate with mainframe systems.

**Key Components**:
- **⚡ Azure Functions**: For serverless business logic
- **🔌 Host Integration Server**: For mainframe connectivity
- **📦 Event Grid**: For event-driven architecture
- **💾 Cosmos DB**: For modern data storage

**Implementation Example - Azure Function with Mainframe Integration**:

```csharp
using System;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Azure.WebJobs;
using Microsoft.Azure.WebJobs.Extensions.Http;
using Microsoft.Extensions.Logging;
using Newtonsoft.Json;
using IBM.Data.DB2.iSeries;

public static class MainframeIntegrationFunction
{
    [FunctionName("GetCustomerData")]
    public static async Task<IActionResult> Run(
        [HttpTrigger(AuthorizationLevel.Function, "get", Route = "customers/{customerId}")] HttpRequest req,
        string customerId,
        ILogger log)
    {
        log.LogInformation($"Processing request for customer ID: {customerId}");

        try
        {
            // Get connection string from configuration
            string connectionString = Environment.GetEnvironmentVariable("MainframeConnectionString");

            // Create customer object to hold data
            var customer = new CustomerModel();

            // Connect to mainframe DB2 database
            using (iDB2Connection connection = new iDB2Connection(connectionString))
            {
                await connection.OpenAsync();

                // Create command to query customer data
                string sql = "SELECT CUSTNAME, CUSTADDR, CUSTPHONE, CUSTTYPE, CUSTBAL " +
                             "FROM CUSTMAST WHERE CUSTID = @CustomerId";

                using (iDB2Command command = new iDB2Command(sql, connection))
                {
                    // Add parameter to prevent SQL injection
                    command.Parameters.Add("@CustomerId", iDB2DbType.iDB2Char).Value = customerId;

                    // Execute command and process results
                    using (var reader = await command.ExecuteReaderAsync())
                    {
                        if (await reader.ReadAsync())
                        {
                            customer.CustomerId = customerId;
                            customer.Name = reader.GetString(0).Trim();
                            customer.Address = reader.GetString(1).Trim();
                            customer.PhoneNumber = reader.GetString(2).Trim();
                            customer.CustomerType = reader.GetString(3) == "P" ? "Premium" : "Regular";
                            customer.AccountBalance = reader.GetDecimal(4);
                        }
                        else
                        {
                            return new NotFoundResult();
                        }
                    }
                }
            }

            return new OkObjectResult(customer);
        }
        catch (Exception ex)
        {
            log.LogError($"Error processing request: {ex.Message}");
            return new StatusCodeResult(StatusCodes.Status500InternalServerError);
        }
    }
}

public class CustomerModel
{
    public string CustomerId { get; set; }
    public string Name { get; set; }
    public string Address { get; set; }
    public string PhoneNumber { get; set; }
    public string CustomerType { get; set; }
    public decimal AccountBalance { get; set; }
}
```

## 🔄 Data Integration Options

Azure provides several options for integrating mainframe data with cloud services:

### 1. 🔄 Azure Data Factory with Mainframe Connectors

**Implementation Example - Data Factory Pipeline**:

```json
{
    "name": "MainframeDataSyncPipeline",
    "properties": {
        "activities": [
            {
                "name": "CopyCustomerData",
                "type": "Copy",
                "inputs": [
                    {
                        "referenceName": "MainframeCustomerDataset",
                        "type": "DatasetReference"
                    }
                ],
                "outputs": [
                    {
                        "referenceName": "AzureSqlCustomerDataset",
                        "type": "DatasetReference"
                    }
                ],
                "typeProperties": {
                    "source": {
                        "type": "Db2Source",
                        "query": "SELECT CUSTID, CUSTNAME, CUSTADDR, CUSTPHONE, CUSTTYPE, CUSTBAL FROM CUSTMAST"
                    },
                    "sink": {
                        "type": "SqlSink",
                        "writeBehavior": "upsert",
                        "upsertSettings": {
                            "useTempDB": true,
                            "keys": ["CustomerId"]
                        },
                        "sqlWriterStoredProcedureName": "sp_upsert_customer_data"
                    }
                }
            }
        ],
        "frequency": "Hour",
        "interval": 1
    }
}
```

### 2. 📦 Event Hubs for Real-time Data Integration

**Implementation Example - Mainframe Adapter Publishing to Event Hubs**:

```java
package com.example.mainframe.integration;

import com.microsoft.azure.eventhubs.ConnectionStringBuilder;
import com.microsoft.azure.eventhubs.EventData;
import com.microsoft.azure.eventhubs.EventHubClient;
import com.microsoft.azure.eventhubs.EventHubException;

import java.io.IOException;
import java.nio.charset.Charset;
import java.time.Instant;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;

public class MainframeEventPublisher {

    private final ScheduledExecutorService executorService;
    private final EventHubClient eventHubClient;

    public MainframeEventPublisher(String connectionString) throws EventHubException, IOException {
        // Create executor service for EventHub client
        executorService = Executors.newScheduledThreadPool(4);
        
        // Create EventHub client
        eventHubClient = EventHubClient.createFromConnectionStringSync(
            connectionString, executorService);
    }

    public void publishTransactionEvent(String transactionId, String customerId, 
                                       String transactionType, double amount) 
                                       throws EventHubException {
        try {
            // Create JSON payload
            String payload = String.format(
                "{\"transactionId\":\"%s\",\"customerId\":\"%s\"," +
                "\"transactionType\":\"%s\",\"amount\":%.2f,\"timestamp\":\"%s\"}",
                transactionId, customerId, transactionType, amount, 
                Instant.now().toString());
            
            // Create event data
            EventData eventData = EventData.create(
                payload.getBytes(Charset.defaultCharset()));
            
            // Add properties
            eventData.getProperties().put("source", "mainframe");
            eventData.getProperties().put("transactionType", transactionType);
            
            // Send event to Event Hub
            eventHubClient.sendSync(eventData);
            
            System.out.println("Event sent: " + payload);
        } catch (Exception e) {
            System.err.println("Error sending event: " + e.getMessage());
            throw e;
        }
    }

    public void close() throws EventHubException {
        eventHubClient.closeSync();
        executorService.shutdown();
    }

    public static void main(String[] args) {
        try {
            // Connection string with SAS key
            String connectionString = "Endpoint=sb://mainframe-events.servicebus.windows.net/;" +
                "EntityPath=transactions;SharedAccessKeyName=send;SharedAccessKey=<key>";
            
            MainframeEventPublisher publisher = new MainframeEventPublisher(connectionString);
            
            // Publish sample event
            publisher.publishTransactionEvent("TX123456", "CUST100", "PURCHASE", 125.50);
            
            // Close connections
            publisher.close();
        } catch (Exception e) {
            System.err.println("Error in main: " + e.getMessage());
        }
    }
}
```

## 🧠 AI Integration with Azure OpenAI Service

Azure OpenAI Service can be used to augment mainframe modernization:

### AI-Enhanced Mainframe Documentation Generator

**Implementation Example - Azure Function with OpenAI Integration**:

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
using Azure;
using Azure.AI.OpenAI;
using System.Text;

public static class CobolAnalyzerFunction
{
    [FunctionName("AnalyzeCobol")]
    public static async Task<IActionResult> Run(
        [HttpTrigger(AuthorizationLevel.Function, "post", Route = null)] HttpRequest req,
        ILogger log)
    {
        log.LogInformation("COBOL analysis request received");

        string requestBody = await new StreamReader(req.Body).ReadToEndAsync();
        var data = JsonConvert.DeserializeObject<AnalysisRequest>(requestBody);
        
        if (string.IsNullOrEmpty(data?.CobolCode))
        {
            return new BadRequestObjectResult("Please provide COBOL code for analysis");
        }

        try
        {
            // Configure OpenAI client
            string endpoint = Environment.GetEnvironmentVariable("OpenAIEndpoint");
            string key = Environment.GetEnvironmentVariable("OpenAIKey");
            string deploymentName = Environment.GetEnvironmentVariable("OpenAIModel");

            var client = new OpenAIClient(new Uri(endpoint), new AzureKeyCredential(key));

            // Define prompt
            var prompt = new StringBuilder();
            prompt.AppendLine("Analyze the following COBOL program and provide comprehensive documentation:");
            prompt.AppendLine("1. Describe the business purpose of the program");
            prompt.AppendLine("2. Identify and explain key business rules");
            prompt.AppendLine("3. List data definitions and their business meaning");
            prompt.AppendLine("4. Explain the program flow and logic");
            prompt.AppendLine("5. Identify any potential issues or areas for improvement");
            prompt.AppendLine("\nCOBOL Code:");
            prompt.AppendLine(data.CobolCode);

            // Call Azure OpenAI
            var options = new ChatCompletionsOptions
            {
                DeploymentName = deploymentName,
                Messages =
                {
                    new ChatMessage(ChatRole.System, "You are an expert COBOL analyst with deep mainframe knowledge."),
                    new ChatMessage(ChatRole.User, prompt.ToString())
                },
                Temperature = 0.3f,
                MaxTokens = 2000
            };

            var response = await client.GetChatCompletionsAsync(options);
            var analysis = response.Value.Choices[0].Message.Content;

            return new OkObjectResult(new AnalysisResponse
            {
                Analysis = analysis,
                InputCodeLength = data.CobolCode.Length,
                AnalysisDate = DateTime.UtcNow
            });
        }
        catch (Exception ex)
        {
            log.LogError($"Error analyzing COBOL: {ex.Message}");
            return new StatusCodeResult(StatusCodes.Status500InternalServerError);
        }
    }
}

public class AnalysisRequest
{
    public string CobolCode { get; set; }
}

public class AnalysisResponse
{
    public string Analysis { get; set; }
    public int InputCodeLength { get; set; }
    public DateTime AnalysisDate { get; set; }
}
```

## 🔒 Security Integration

Azure provides comprehensive security solutions for mainframe integration:

### Secure Credential Management with Azure Key Vault

**Implementation Example - Retrieving Mainframe Credentials**:

```csharp
using Azure.Identity;
using Azure.Security.KeyVault.Secrets;
using Microsoft.Extensions.Logging;
using System;
using System.Threading.Tasks;

public class MainframeCredentialManager
{
    private readonly string _keyVaultUrl;
    private readonly ILogger _logger;
    private readonly SecretClient _secretClient;

    public MainframeCredentialManager(string keyVaultUrl, ILogger logger)
    {
        _keyVaultUrl = keyVaultUrl;
        _logger = logger;
        
        // Create secret client with managed identity authentication
        var credential = new DefaultAzureCredential();
        _secretClient = new SecretClient(new Uri(_keyVaultUrl), credential);
    }

    public async Task<MainframeCredentials> GetMainframeCredentialsAsync(string environment)
    {
        try
        {
            // Get username from Key Vault
            string userSecretName = $"mainframe-{environment}-username";
            KeyVaultSecret userSecret = await _secretClient.GetSecretAsync(userSecretName);
            
            // Get password from Key Vault
            string passwordSecretName = $"mainframe-{environment}-password";
            KeyVaultSecret passwordSecret = await _secretClient.GetSecretAsync(passwordSecretName);
            
            // Return credentials
            return new MainframeCredentials
            {
                Username = userSecret.Value,
                Password = passwordSecret.Value
            };
        }
        catch (Exception ex)
        {
            _logger.LogError($"Error retrieving mainframe credentials: {ex.Message}");
            throw;
        }
    }
}

public class MainframeCredentials
{
    public string Username { get; set; }
    public string Password { get; set; }
}
```

## 📦 Deployment Examples

### Azure Resource Manager Template for Mainframe Integration Environment

```json
{
  "$schema": "https://schema.management.azure.com/schemas/2019-04-01/deploymentTemplate.json#",
  "contentVersion": "1.0.0.0",
  "parameters": {
    "apiManagementServiceName": {
      "type": "string",
      "metadata": {
        "description": "Name of the API Management service"
      }
    },
    "logicAppName": {
      "type": "string",
      "metadata": {
        "description": "Name of the Logic App"
      }
    },
    "keyVaultName": {
      "type": "string",
      "metadata": {
        "description": "Name of the Key Vault"
      }
    },
    "sqlServerName": {
      "type": "string",
      "metadata": {
        "description": "Name of the SQL Server"
      }
    },
    "sqlDatabaseName": {
      "type": "string",
      "metadata": {
        "description": "Name of the SQL Database"
      }
    },
    "administratorLogin": {
      "type": "string",
      "metadata": {
        "description": "SQL Server administrator login"
      }
    },
    "administratorLoginPassword": {
      "type": "securestring",
      "metadata": {
        "description": "SQL Server administrator password"
      }
    },
    "location": {
      "type": "string",
      "defaultValue": "[resourceGroup().location]",
      "metadata": {
        "description": "Location for all resources"
      }
    }
  },
  "resources": [
    {
      "type": "Microsoft.ApiManagement/service",
      "apiVersion": "2021-08-01",
      "name": "[parameters('apiManagementServiceName')]",
      "location": "[parameters('location')]",
      "sku": {
        "name": "Developer",
        "capacity": 1
      },
      "properties": {
        "publisherEmail": "admin@contoso.com",
        "publisherName": "Contoso Mainframe Integration"
      }
    },
    {
      "type": "Microsoft.Logic/workflows",
      "apiVersion": "2019-05-01",
      "name": "[parameters('logicAppName')]",
      "location": "[parameters('location')]",
      "properties": {
        "state": "Enabled",
        "definition": {
          "$schema": "https://schema.management.azure.com/providers/Microsoft.Logic/schemas/2016-06-01/workflowdefinition.json#",
          "contentVersion": "1.0.0.0",
          "parameters": {},
          "triggers": {
            "manual": {
              "type": "Request",
              "kind": "Http",
              "inputs": {
                "schema": {}
              }
            }
          },
          "actions": {},
          "outputs": {}
        }
      }
    },
    {
      "type": "Microsoft.KeyVault/vaults",
      "apiVersion": "2021-10-01",
      "name": "[parameters('keyVaultName')]",
      "location": "[parameters('location')]",
      "properties": {
        "enabledForDeployment": false,
        "enabledForDiskEncryption": false,
        "enabledForTemplateDeployment": false,
        "tenantId": "[subscription().tenantId]",
        "accessPolicies": [],
        "sku": {
          "name": "standard",
          "family": "A"
        }
      }
    },
    {
      "type": "Microsoft.Sql/servers",
      "apiVersion": "2021-05-01-preview",
      "name": "[parameters('sqlServerName')]",
      "location": "[parameters('location')]",
      "properties": {
        "administratorLogin": "[parameters('administratorLogin')]",
        "administratorLoginPassword": "[parameters('administratorLoginPassword')]",
        "version": "12.0"
      },
      "resources": [
        {
          "type": "databases",
          "apiVersion": "2021-05-01-preview",
          "name": "[parameters('sqlDatabaseName')]",
          "location": "[parameters('location')]",
          "sku": {
            "name": "Basic",
            "tier": "Basic"
          },
          "dependsOn": [
            "[resourceId('Microsoft.Sql/servers', parameters('sqlServerName'))]"
          ]
        },
        {
          "type": "firewallRules",
          "apiVersion": "2021-05-01-preview",
          "name": "AllowAllAzureIPs",
          "dependsOn": [
            "[resourceId('Microsoft.Sql/servers', parameters('sqlServerName'))]"
          ],
          "properties": {
            "startIpAddress": "0.0.0.0",
            "endIpAddress": "0.0.0.0"
          }
        }
      ]
    }
  ],
  "outputs": {
    "apiManagementUrl": {
      "type": "string",
      "value": "[concat('https://', parameters('apiManagementServiceName'), '.azure-api.net')]"
    },
    "sqlServerFqdn": {
      "type": "string",
      "value": "[reference(resourceId('Microsoft.Sql/servers', parameters('sqlServerName'))).fullyQualifiedDomainName]"
    },
    "keyVaultUri": {
      "type": "string",
      "value": "[reference(resourceId('Microsoft.KeyVault/vaults', parameters('keyVaultName'))).vaultUri]"
    }
  }
}
```

## 📊 Monitoring and Management

### Application Insights for Cross-Platform Monitoring

**Implementation Example - Custom Metrics for Mainframe Integration**:

```csharp
using Microsoft.ApplicationInsights;
using Microsoft.ApplicationInsights.Extensibility;
using Microsoft.ApplicationInsights.DataContracts;
using System;
using System.Collections.Generic;

public class MainframeIntegrationTelemetry
{
    private readonly TelemetryClient _telemetryClient;
    
    public MainframeIntegrationTelemetry(string instrumentationKey)
    {
        var config = new TelemetryConfiguration
        {
            InstrumentationKey = instrumentationKey
        };
        
        _telemetryClient = new TelemetryClient(config);
    }
    
    public void TrackMainframeTransaction(string transactionId, string transactionType, 
                                        string customerId, long mainframeResponseTimeMs)
    {
        // Track as custom event
        var properties = new Dictionary<string, string>
        {
            { "TransactionId", transactionId },
            { "TransactionType", transactionType },
            { "CustomerId", customerId }
        };
        
        var metrics = new Dictionary<string, double>
        {
            { "ResponseTimeMs", mainframeResponseTimeMs }
        };
        
        _telemetryClient.TrackEvent("MainframeTransaction", properties, metrics);
        
        // Track response time as metric
        _telemetryClient.TrackMetric(
            new MetricTelemetry("MainframeResponseTime", mainframeResponseTimeMs)
            {
                Properties = 
                {
                    { "TransactionType", transactionType }
                }
            });
        
        // Ensure telemetry is sent
        _telemetryClient.Flush();
    }
    
    public void TrackMainframeError(string transactionId, string errorCode, 
                                  string errorMessage, string stackTrace = null)
    {
        var properties = new Dictionary<string, string>
        {
            { "TransactionId", transactionId },
            { "ErrorCode", errorCode }
        };
        
        var exception = new Exception(errorMessage);
        _telemetryClient.TrackException(exception, properties);
        
        // Ensure telemetry is sent
        _telemetryClient.Flush();
    }
}
```

## 🧩 Best Practices for Azure Mainframe Integration

1. **🔒 Security First**: Always implement proper authentication and encryption for mainframe connectivity
2. **📊 Comprehensive Monitoring**: Implement end-to-end monitoring across mainframe and Azure
3. **🔄 Idempotent Operations**: Design integration points to handle retries and duplicates safely
4. **📈 Scalability Planning**: Implement throttling and scaling mechanisms to protect mainframe resources
5. **🔍 Detailed Logging**: Maintain transaction correlation across systems for traceability
6. **⚡ Performance Optimization**: Minimize mainframe resource usage through caching and batching
7. **🧪 Thorough Testing**: Implement comprehensive integration testing with mainframe simulators
8. **🔄 Hybrid Operations**: Plan for long-term hybrid operations with proper governance

## 🗺️ Implementation Roadmap

1. **🔍 Assessment**: Evaluate mainframe systems for integration opportunities
2. **🏗️ Foundation**: Set up Azure landing zone with appropriate network connectivity
3. **🧪 Proof of Concept**: Implement small-scale integration for validation
4. **📋 Standards and Patterns**: Define integration patterns and governance standards
5. **🔄 Implementation**: Roll out integration patterns incrementally
6. **📊 Monitoring**: Establish comprehensive monitoring and alerting
7. **🔒 Security Reviews**: Conduct regular security assessments
8. **📈 Optimization**: Continuously improve performance and reliability 