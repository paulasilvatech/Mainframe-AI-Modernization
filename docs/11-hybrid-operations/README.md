# Hybrid Operations Management

This chapter provides detailed technical guidance for implementing hybrid operations management for IBM z/OS mainframe applications integrated with Azure AI Foundry.

## Overview

Hybrid operations management addresses the unique challenges of operating mainframe applications in a modernized environment where both traditional IBM z/OS systems and cloud components coexist. Azure AI Foundry provides intelligent capabilities to streamline operations, enhance monitoring, and facilitate effective management across the hybrid landscape.

This chapter outlines a comprehensive approach to managing hybrid mainframe operations with a focus on automation, intelligent monitoring, and integrated management.

## Objectives

- Implement unified monitoring across mainframe and cloud environments
- Establish AI-powered incident management for hybrid operations
- Create automated operational workflows spanning z/OS and Azure
- Develop intelligent capacity management across platforms
- Implement unified security operations for hybrid environments

## Hybrid Operations Architecture

The hybrid operations architecture consists of several integrated components:

```
┌─────────────────────────────────────────────────────────────────────────────────────┐
│                         HYBRID OPERATIONS MANAGEMENT                                 │
├─────────────────────────────────────────────┬───────────────────────────────────────┤
│                                             │                                       │
│           IBM z/OS ENVIRONMENT              │           AZURE ENVIRONMENT           │
│                                             │                                       │
│  ┌─────────────────────────────────────┐    │    ┌─────────────────────────────┐    │
│  │                                     │    │    │                             │    │
│  │  ┌─────────────┐   ┌─────────────┐  │    │    │  ┌─────────────┐            │    │
│  │  │             │   │             │  │    │    │  │             │            │    │
│  │  │  Mainframe  │   │  Mainframe  │  │    │    │  │   Azure     │            │    │
│  │  │  Apps       │   │  Monitoring │  │    │    │  │   Services  │            │    │
│  │  │             │   │             │  │    │    │  │             │            │    │
│  │  └──────┬──────┘   └──────┬──────┘  │    │    │  └──────┬──────┘            │    │
│  │         │                 │         │    │    │         │                   │    │
│  │  ┌──────▼──────┐   ┌──────▼──────┐  │    │    │  ┌──────▼──────┐            │    │
│  │  │             │   │             │  │    │    │  │             │            │    │
│  │  │  z/OS       │   │  SMF        │  │    │    │  │  Azure      │            │    │
│  │  │  Services   │   │  Records    │  │    │    │  │  Monitor    │            │    │
│  │  │             │   │             │  │    │    │  │             │            │    │
│  │  └──────┬──────┘   └──────┬──────┘  │    │    │  └──────┬──────┘            │    │
│  │         │                 │         │    │    │         │                   │    │
│  └─────────┼─────────────────┼─────────┘    │    └─────────┼───────────────────┘    │
│            │                 │              │              │                        │
│     ┌──────▼─────────────────▼──────┐       │       ┌──────▼───────────────────┐    │
│     │                              │        │       │                          │    │
│     │      Integration Agents      │◄───────┼─────► │   Integration Services   │    │
│     │                              │        │       │                          │    │
│     └──────────────┬───────────────┘        │       └────────────┬─────────────┘    │
│                    │                        │                    │                   │
└────────────────────┼────────────────────────┼────────────────────┼───────────────────┘
                     │                        │                    │                    
                     │                        │                    │                    
┌────────────────────▼────────────────────────▼────────────────────▼───────────────────┐
│                                                                                      │
│                              AZURE AI FOUNDRY                                        │
│                                                                                      │
│  ┌─────────────────────┐   ┌─────────────────────┐   ┌─────────────────────┐         │
│  │                     │   │                     │   │                     │         │
│  │  Unified Monitoring │   │  Intelligent        │   │  Automated          │         │
│  │  & Observability    │   │  Incident Response  │   │  Operations         │         │
│  │                     │   │                     │   │                     │         │
│  └─────────────────────┘   └─────────────────────┘   └─────────────────────┘         │
│                                                                                      │
│  ┌─────────────────────┐   ┌─────────────────────┐   ┌─────────────────────┐         │
│  │                     │   │                     │   │                     │         │
│  │  Capacity           │   │  Security           │   │  Hybrid             │         │
│  │  Intelligence       │   │  Operations         │   │  Operations Console │         │
│  │                     │   │                     │   │                     │         │
│  └─────────────────────┘   └─────────────────────┘   └─────────────────────┘         │
│                                                                                      │
└──────────────────────────────────────────────────────────────────────────────────────┘
```

## Technical Implementation

### 1. Unified Monitoring Implementation

Implement unified monitoring across IBM z/OS and Azure environments:

1. **Deploy Mainframe Integration Agents**:

   ```bash
   az ai-foundry agent deploy \
     --resource-group mainframe-modernization-rg \
     --aifoundry-name mainframe-ai-foundry \
     --agent-type monitoring \
     --target-host $(MAINFRAME_HOST) \
     --target-port $(MAINFRAME_PORT) \
     --credentials-file mainframe-credentials.json \
     --configuration-file monitoring-config.json
   ```

2. **Configure Monitoring Integration**:

   Create `monitoring-config.json`:
   ```json
   {
     "monitoringConfiguration": {
       "smfRecords": {
         "enabled": true,
         "recordTypes": [30, 70, 72, 89, 120],
         "collectionInterval": 300,
         "processingMode": "real-time"
       },
       "rmfMonitoring": {
         "enabled": true,
         "metrics": ["CPU", "MEMORY", "IO", "WORKLOAD"],
         "collectionInterval": 60
       },
       "applicationLogs": {
         "enabled": true,
         "sources": [
           { "name": "CICS", "logName": "DFHLOG" },
           { "name": "DB2", "logName": "DSNLOG" },
           { "name": "IMS", "logName": "IMSLOG" },
           { "name": "COBOL", "ddName": "SYSOUT" }
         ]
       },
       "transactionTracking": {
         "enabled": true,
         "transactionClasses": ["FINANCIAL", "CUSTOMER", "BATCH"],
         "correlationIdentifiers": ["USERID", "TRANSID", "TASKNUM"]
       }
     }
   }
   ```

3. **Configure Azure Monitor Integration**:

   ```bash
   az monitor diagnostic-settings create \
     --name mainframe-integration \
     --resource $(AIFOUNDRY_ID) \
     --workspace $(LOG_ANALYTICS_WORKSPACE_ID) \
     --logs '[{"category": "MainframeOperations", "enabled": true}]' \
     --metrics '[{"category": "AllMetrics", "enabled": true}]'
   ```

4. **Create Unified Dashboards**:

   ```bash
   az portal dashboard create \
     --name HybridMainframeOperations \
     --resource-group mainframe-modernization-rg \
     --location eastus \
     --template-file templates/dashboards/hybrid-operations-dashboard.json
   ```

### 2. Intelligent Incident Management

Implement AI-powered incident management for hybrid environments:

1. **Deploy Incident Management Configuration**:

   ```bash
   az ai-foundry incident-management create \
     --resource-group mainframe-modernization-rg \
     --aifoundry-name mainframe-ai-foundry \
     --configuration-file incident-management-config.json
   ```

   Create `incident-management-config.json`:
   ```json
   {
     "incidentManagement": {
       "aiCapabilities": {
         "anomalyDetection": true,
         "rootCauseAnalysis": true,
         "impactAssessment": true,
         "recommendationEngine": true
       },
       "correlation": {
         "enabled": true,
         "correlationWindow": 300,
         "crossPlatformCorrelation": true,
         "suppressionRules": [
           {
             "pattern": "DB2LOCK*",
             "deduplicationWindow": 600
           }
         ]
       },
       "notification": {
         "channels": [
           {
             "type": "email",
             "recipients": ["operations@example.com"]
           },
           {
             "type": "teams",
             "webhook": "https://teams.webhook.url"
           },
           {
             "type": "serviceNow",
             "connectionString": "$(SERVICE_NOW_CONNECTION)"
           }
         ],
         "escalation": {
           "levels": [
             {
               "level": 1,
               "waitTime": 1800,
               "recipients": ["tier1@example.com"]
             },
             {
               "level": 2,
               "waitTime": 3600,
               "recipients": ["tier2@example.com"]
             }
           ]
         }
       }
     }
   }
   ```

2. **Configure Incident Response Playbooks**:

   ```bash
   az ai-foundry playbook create \
     --resource-group mainframe-modernization-rg \
     --aifoundry-name mainframe-ai-foundry \
     --name DB2Deadlock \
     --template-file templates/playbooks/db2-deadlock-resolution.json
   ```

3. **Implement Hybrid Runbooks**:

   Create `db2-deadlock-resolution.json`:
   ```json
   {
     "trigger": {
       "type": "alert",
       "alertRule": "DB2Deadlock",
       "source": "Mainframe"
     },
     "actions": [
       {
         "name": "CollectDiagnostics",
         "type": "MainframeCommand",
         "parameters": {
           "command": "-DISPLAY THREAD(*) LOCKINFO",
           "subsystem": "DB2"
         },
         "output": "diagnosticResults"
       },
       {
         "name": "AnalyzeDeadlock",
         "type": "AIAnalysis",
         "parameters": {
           "analysisType": "DeadlockPattern",
           "input": "diagnosticResults"
         },
         "output": "analysisResults"
       },
       {
         "name": "TakeRemediationAction",
         "type": "ConditionalExecution",
         "conditions": [
           {
             "condition": "analysisResults.confidence > 0.7",
             "actions": [
               {
                 "name": "ApplyFix",
                 "type": "MainframeCommand",
                 "parameters": {
                   "command": "-CANCEL THREAD(${analysisResults.threadId})",
                   "subsystem": "DB2"
                 }
               }
             ]
           },
           {
             "condition": "true",
             "actions": [
               {
                 "name": "EscalateToHuman",
                 "type": "Notification",
                 "parameters": {
                   "channel": "teams",
                   "message": "DB2 deadlock detected, manual intervention required. Analysis results: ${analysisResults.summary}",
                   "severity": "High"
                 }
               }
             ]
           }
         ]
       }
     ]
   }
   ```

### 3. Automated Operations Workflows

Implement automated operational workflows spanning z/OS and Azure:

1. **Deploy Integration Automation**:

   ```bash
   az resource create \
     --resource-group mainframe-modernization-rg \
     --namespace Microsoft.Logic --resource-type workflows \
     --name BatchProcessingWorkflow \
     --properties @batch-processing-workflow.json \
     --api-version 2019-05-01
   ```

2. **Configure Hybrid Batch Processing Workflow**:

   Create `batch-processing-workflow.json`:
   ```json
   {
     "properties": {
       "state": "Enabled",
       "definition": {
         "$schema": "https://schema.management.azure.com/providers/Microsoft.Logic/schemas/2016-06-01/workflowdefinition.json#",
         "contentVersion": "1.0.0.0",
         "parameters": {
           "mainframeJobName": {
             "type": "string",
             "defaultValue": "DAILYBAT"
           },
           "mainframeSubsystem": {
             "type": "string",
             "defaultValue": "JES2"
           }
         },
         "triggers": {
           "schedule": {
             "type": "Recurrence",
             "recurrence": {
               "frequency": "Day",
               "interval": 1,
               "schedule": {
                 "hours": [22],
                 "minutes": [0]
               }
             }
           }
         },
         "actions": {
           "CheckPrerequisites": {
             "type": "Http",
             "inputs": {
               "method": "GET",
               "uri": "https://ai-foundry-api.example.com/api/mainframe-operations/prerequisites",
               "queries": {
                 "jobName": "@parameters('mainframeJobName')"
               }
             },
             "runAfter": {}
           },
           "SubmitMainframeJob": {
             "type": "Http",
             "inputs": {
               "method": "POST",
               "uri": "https://ai-foundry-api.example.com/api/mainframe-operations/jobs",
               "body": {
                 "jobName": "@parameters('mainframeJobName')",
                 "subsystem": "@parameters('mainframeSubsystem')",
                 "parameters": {
                   "DATE": "@utcNow('yyyy-MM-dd')"
                 }
               }
             },
             "runAfter": {
               "CheckPrerequisites": [
                 "Succeeded"
               ]
             }
           },
           "MonitorJobExecution": {
             "type": "Until",
             "actions": {
               "GetJobStatus": {
                 "type": "Http",
                 "inputs": {
                   "method": "GET",
                   "uri": "https://ai-foundry-api.example.com/api/mainframe-operations/jobs/@{body('SubmitMainframeJob').jobId}/status"
                 }
               },
               "WaitForNextCheck": {
                 "type": "Wait",
                 "inputs": {
                   "interval": {
                     "count": 30,
                     "unit": "Second"
                   }
                 },
                 "runAfter": {
                   "GetJobStatus": [
                     "Succeeded"
                   ]
                 }
               }
             },
             "expression": "@equals(body('GetJobStatus').status, 'COMPLETED')",
             "limit": {
               "count": 60,
               "timeout": "PT1H"
             },
             "runAfter": {
               "SubmitMainframeJob": [
                 "Succeeded"
               ]
             }
           },
           "ProcessResults": {
             "type": "Http",
             "inputs": {
               "method": "POST",
               "uri": "https://ai-foundry-api.example.com/api/mainframe-operations/jobs/@{body('SubmitMainframeJob').jobId}/results",
               "body": {
                 "destinationStorage": "mainframeresults",
                 "processType": "DataTransformation"
               }
             },
             "runAfter": {
               "MonitorJobExecution": [
                 "Succeeded"
               ]
             }
           },
           "RunDataAnalytics": {
             "type": "AzureFunction",
             "inputs": {
               "function": {
                 "id": "[resourceId('Microsoft.Web/sites/functions', variables('functionAppName'), 'ProcessMainframeData')]"
               },
               "body": {
                 "jobId": "@body('SubmitMainframeJob').jobId",
                 "executionDate": "@utcNow('yyyy-MM-dd')",
                 "storageContainer": "mainframeresults"
               }
             },
             "runAfter": {
               "ProcessResults": [
                 "Succeeded"
               ]
             }
           },
           "UpdateOperationalDashboard": {
             "type": "Http",
             "inputs": {
               "method": "PATCH",
               "uri": "https://ai-foundry-api.example.com/api/dashboards/hybrid-operations",
               "body": {
                 "lastBatchRun": "@utcNow()",
                 "jobId": "@body('SubmitMainframeJob').jobId",
                 "processingResults": "@body('RunDataAnalytics')"
               }
             },
             "runAfter": {
               "RunDataAnalytics": [
                 "Succeeded"
               ]
             }
           }
         },
         "outputs": {}
       }
     }
   }
   ```

### 4. Capacity Intelligence

Implement AI-powered capacity management across platforms:

1. **Deploy Capacity Intelligence Service**:

   ```bash
   az ai-foundry capacity create \
     --resource-group mainframe-modernization-rg \
     --aifoundry-name mainframe-ai-foundry \
     --configuration-file capacity-intelligence-config.json
   ```

2. **Configure Capacity Intelligence**:

   Create `capacity-intelligence-config.json`:
   ```json
   {
     "capacityIntelligence": {
       "data": {
         "sources": [
           {
             "type": "mainframe",
             "metrics": [
               "CPU_UTILIZATION",
               "MEMORY_UTILIZATION",
               "IO_RATE",
               "WORKLOAD_ACTIVITY"
             ],
             "collectionInterval": 300
           },
           {
             "type": "azure",
             "metrics": [
               "Percentage CPU",
               "Memory Available",
               "Disk Operations/Sec",
               "Network In/Out"
             ],
             "collectionInterval": 60
           }
         ],
         "historicalPeriod": 90,
         "retentionPeriod": 365
       },
       "analysis": {
         "forecastingHorizon": 90,
         "seasonalityDetection": true,
         "anomalyDetection": true,
         "correlationAnalysis": true,
         "workloadPatternIdentification": true
       },
       "optimization": {
         "recommendations": {
           "enabled": true,
           "types": [
             "RESOURCE_ALLOCATION",
             "WORKLOAD_SCHEDULING",
             "COST_OPTIMIZATION"
           ],
           "autoImplementation": false
         },
         "simulationEnabled": true
       },
       "reporting": {
         "dashboards": ["CAPACITY_OVERVIEW", "TREND_ANALYSIS", "FORECAST_VIEW"],
         "alerting": {
           "enabled": true,
           "thresholds": {
             "CPU_UTILIZATION": 85,
             "MEMORY_UTILIZATION": 90,
             "FORECAST_CAPACITY_BREACH": 30
           }
         }
       }
     }
   }
   ```

3. **Create Capacity Forecasting Job**:

   ```bash
   az ai-foundry job create \
     --resource-group mainframe-modernization-rg \
     --aifoundry-name mainframe-ai-foundry \
     --name CapacityForecast \
     --job-type forecast \
     --schedule "0 0 * * 0" \
     --parameters @capacity-forecast-params.json
   ```

   Create `capacity-forecast-params.json`:
   ```json
   {
     "forecastingParameters": {
       "timeHorizon": 90,
       "confidenceInterval": 95,
       "granularity": "DAILY",
       "metrics": [
         "CPU_UTILIZATION",
         "MEMORY_UTILIZATION",
         "IO_RATE",
         "WORKLOAD_ACTIVITY"
       ],
       "scenarios": [
         {
           "name": "BaselineGrowth",
           "growthRate": 0.05
         },
         {
           "name": "AcceleratedGrowth",
           "growthRate": 0.15
         },
         {
           "name": "ModernizationImpact",
           "customModel": "MODERNIZATION_SHIFT"
         }
       ],
       "outputFormat": "DASHBOARD_AND_REPORT",
       "notificationRecipients": ["capacity-planning@example.com"]
     }
   }
   ```

### 5. Security Operations

Implement unified security operations across hybrid environments:

1. **Deploy Security Operations Integration**:

   ```bash
   az ai-foundry security create \
     --resource-group mainframe-modernization-rg \
     --aifoundry-name mainframe-ai-foundry \
     --configuration-file security-operations-config.json
   ```

2. **Configure Security Operations**:

   Create `security-operations-config.json`:
   ```json
   {
     "securityOperations": {
       "dataSources": {
         "mainframe": {
           "racfLogs": true,
           "acfLogs": true,
           "topSecretLogs": true,
           "smfSecurityRecords": true,
           "systemLogs": true
         },
         "azure": {
           "azureActivityLogs": true,
           "azureSecurityCenter": true,
           "azureSentinel": true,
           "microsoftDefender": true
         }
       },
       "threatDetection": {
         "enabled": true,
         "analysisRules": [
           "PRIVILEGED_ACCESS_ANOMALY",
           "UNUSUAL_ACCESS_PATTERN",
           "CREDENTIAL_MISUSE",
           "DATA_EXFILTRATION",
           "CROSS_PLATFORM_THREATS"
         ]
       },
       "complianceMonitoring": {
         "frameworks": [
           "PCI-DSS",
           "HIPAA",
           "SOX",
           "GDPR",
           "NIST"
         ],
         "automatedAssessment": true,
         "reportingFrequency": "WEEKLY"
       },
       "responseAutomation": {
         "enabled": true,
         "playbooks": [
           "ACCOUNT_LOCKOUT",
           "SUSPICIOUS_ACCESS_INVESTIGATION",
           "MALWARE_CONTAINMENT"
         ]
       },
       "securityDashboard": {
         "components": [
           "THREAT_OVERVIEW",
           "COMPLIANCE_STATUS",
           "VULNERABILITY_SUMMARY",
           "INCIDENT_TRACKER"
         ],
         "updateFrequency": 300
       }
     }
   }
   ```

3. **Configure Security Alerts Integration**:

   ```bash
   az monitor activity-log alert create \
     --name MainframeSecurityAlerts \
     --resource-group mainframe-modernization-rg \
     --condition category=Security \
     --action-group /subscriptions/{subscriptionId}/resourceGroups/mainframe-modernization-rg/providers/Microsoft.Insights/actionGroups/SecurityResponseTeam
   ```

## Hybrid Operations Console

Implement a unified operations console for hybrid environments:

1. **Deploy Hybrid Operations Console**:

   ```bash
   az deployment group create \
     --resource-group mainframe-modernization-rg \
     --template-file templates/hybrid-operations-console.json \
     --parameters @console-parameters.json
   ```

2. **Configure Console Components**:

   Create `console-parameters.json`:
   ```json
   {
     "consoleName": {
       "value": "MainframeHybridOps"
     },
     "consoleComponents": {
       "value": [
         "MONITORING_DASHBOARD",
         "ALERT_MANAGER",
         "INCIDENT_RESPONSE",
         "CAPACITY_INSIGHTS",
         "SECURITY_OPERATIONS",
         "AUTOMATION_CONTROL"
       ]
     },
     "userRoles": {
       "value": [
         {
           "name": "OperationsEngineer",
           "permissions": [
             "VIEW_ALL",
             "MANAGE_INCIDENTS",
             "RUN_AUTOMATIONS"
           ]
         },
         {
           "name": "SecurityAnalyst",
           "permissions": [
             "VIEW_SECURITY",
             "MANAGE_SECURITY_INCIDENTS",
             "VIEW_AUDITS"
           ]
         },
         {
           "name": "MainframeSpecialist",
           "permissions": [
             "VIEW_ALL",
             "MANAGE_MAINFRAME",
             "VIEW_CAPACITY"
           ]
         }
       ]
     },
     "consoleSettings": {
       "value": {
         "refreshInterval": 60,
         "defaultView": "MONITORING_DASHBOARD",
         "theme": "CORPORATE",
         "telemetryEnabled": true
       }
     }
   }
   ```

## Practical Example: Hybrid Batch Processing Monitoring

This example demonstrates how to set up hybrid monitoring for a mainframe batch process that has dependencies in Azure:

1. **Configure the Monitoring Integration**:

   ```json
   {
     "hybridBatchMonitoring": {
       "batchProcess": {
         "name": "DAILYACCT",
         "mainframeJobName": "ACCTPROC",
         "schedule": "0 22 * * 1-5",
         "expectedDuration": 7200,
         "dependencies": [
           {
             "type": "mainframe",
             "name": "DB2.ACCOUNTS.DAILY",
             "checkCommand": "-DISPLAY DATABASE(ACCOUNTS)",
             "status": "available"
           },
           {
             "type": "azure",
             "name": "DataLakeInput",
             "resource": "StorageAccount",
             "resourceName": "acctdatalake",
             "status": "filesAvailable"
           }
         ]
       },
       "monitoring": {
         "checkpoints": [
           {
             "name": "JobSubmission",
             "type": "mainframe",
             "command": "$D J,ACCTPROC",
             "expectedOutput": "JOB STARTED",
             "timeout": 300
           },
           {
             "name": "DB2Processing",
             "type": "mainframe",
             "command": "SELECT COUNT(*) FROM SYSIBM.SYSPROCS WHERE NAME='ACCTPROC'",
             "expectedOutput": "> 0",
             "timeout": 3600
           },
           {
             "name": "DataExport",
             "type": "hybrid",
             "mainframeCommand": "$D J,ACCTPROC",
             "azureCheck": {
               "type": "Blob",
               "container": "exports",
               "filePattern": "ACCT_*.csv"
             },
             "timeout": 5400
           },
           {
             "name": "DataLakeProcessing",
             "type": "azure",
             "resource": "DataFactory",
             "pipelineName": "ProcessAccountExport",
             "status": "Succeeded",
             "timeout": 1800
           }
         ],
         "alerts": {
           "delayThreshold": 900,
           "failureNotification": {
             "channels": ["email", "teams"],
             "recipients": ["batch-ops@example.com"]
           }
         }
       },
       "remediation": {
         "autoRetry": {
           "enabled": true,
           "maxRetries": 3,
           "retryInterval": 600
         },
         "playbooks": [
           {
             "trigger": "DB2Processing.Timeout",
             "playbook": "RestartDB2Process"
           },
           {
             "trigger": "DataExport.Failure",
             "playbook": "RecoverExportProcess"
           }
         ]
       }
     }
   }
   ```

2. **Implement the Monitoring Dashboard**:

   ```bash
   az portal dashboard create \
     --name HybridBatchMonitoring \
     --resource-group mainframe-modernization-rg \
     --location eastus \
     --template-file templates/dashboards/hybrid-batch-dashboard.json
   ```

## Validation Steps

After implementing hybrid operations management, validate the implementation using these steps:

1. **Validate Unified Monitoring**:
   - Confirm metric collection from both mainframe and Azure sources
   - Verify proper visualization in dashboards
   - Check alert generation and correlation across platforms

2. **Test Incident Management**:
   - Simulate a hybrid incident (e.g., DB2 issue affecting Azure services)
   - Verify proper detection, correlation, and notification
   - Confirm playbook execution and remediation steps

3. **Verify Automated Operations**:
   - Test end-to-end automation of hybrid workflows
   - Confirm proper error handling and notification
   - Validate cross-platform coordination

4. **Check Capacity Intelligence**:
   - Verify data collection and analysis across platforms
   - Test forecasting accuracy with historical data
   - Review recommendation quality and relevance

5. **Validate Security Operations**:
   - Test threat detection across platforms
   - Verify security playbook execution
   - Confirm compliance monitoring and reporting

## Troubleshooting

| Issue | Resolution |
|-------|------------|
| Missing mainframe metrics | Verify mainframe agent installation and permissions; check SMF record types configuration |
| Alert correlation failure | Adjust correlation rules and timeframe; check for clock synchronization issues |
| Automation playbook failures | Verify mainframe command permissions; check network connectivity between platforms |
| Capacity forecast inaccuracy | Increase historical data period; adjust seasonality detection parameters |
| Cross-platform security alerting delays | Check log ingestion frequency; optimize security agent configuration |

## Next Steps

After implementing hybrid operations management, continue to:

1. [Case Studies and Examples](../12-case-studies/README.md) - Real-world examples and lessons learned

## Additional Resources

- [Mainframe Monitoring Best Practices](../resources/guides/mainframe-monitoring-best-practices.md)
- [Hybrid Incident Response Playbooks](../resources/templates/incident-response-playbooks.md)
- [Capacity Planning Guidelines](../resources/guides/hybrid-capacity-planning.md)
- [Security Operations Reference Architecture](../resources/architecture/security-operations-architecture.md) 