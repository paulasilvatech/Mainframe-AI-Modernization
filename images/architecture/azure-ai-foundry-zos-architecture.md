# Azure AI Foundry Architecture for IBM z/OS

The following ASCII diagram illustrates the comprehensive reference architecture for Azure AI Foundry integration with IBM z/OS:

```
┌───────────────────────────────────────────────────────────────────────────────────────────────────────────┐
│                                          AZURE CLOUD                                                       │
├───────────────────────────────────────────────────────────────────────────────────────────────────────────┤
│                                                                                                           │
│  ┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐  │
│  │                                      AZURE AI FOUNDRY                                                │  │
│  │                                                                                                     │  │
│  │  ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────────────────┐    │  │
│  │  │                 │  │                 │  │                 │  │                             │    │  │
│  │  │ Code            │  │ Translation     │  │ Risk            │  │ Operational                 │    │  │
│  │  │ Intelligence    │  │ Services        │  │ Intelligence    │  │ Intelligence                │    │  │
│  │  │                 │  │                 │  │                 │  │                             │    │  │
│  │  └────────┬────────┘  └────────┬────────┘  └────────┬────────┘  └──────────────┬──────────────┘    │  │
│  │           │                    │                    │                           │                   │  │
│  │           └──────────┬─────────┴──────────┬─────────┴───────────┬──────────────┘                   │  │
│  │                      │                    │                     │                                   │  │
│  │                      ▼                    ▼                     ▼                                   │  │
│  │  ┌────────────────────────────────────────────────────────────────────────────────────┐           │  │
│  │  │                             AI FOUNDATION SERVICES                                  │           │  │
│  │  │                                                                                     │           │  │
│  │  │  ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────────────────────┐     │           │  │
│  │  │  │                 │  │                 │  │                                 │     │           │  │
│  │  │  │ Azure OpenAI    │  │ Azure Cognitive │  │ Azure Machine Learning          │     │           │  │
│  │  │  │ Service         │  │ Services        │  │ Service                         │     │           │  │
│  │  │  │                 │  │                 │  │                                 │     │           │  │
│  │  │  └─────────────────┘  └─────────────────┘  └─────────────────────────────────┘     │           │  │
│  │  │                                                                                     │           │  │
│  │  └────────────────────────────────────────────────────────────────────────────────────┘           │  │
│  │                                                                                                     │  │
│  └─────────────────────────────────────────────────────────────────────────────────────────────────────┘  │
│                                                                                                           │
│  ┌─────────────────────────────────────────────────────┐   ┌─────────────────────────────────────────────┐│
│  │                INTEGRATION SERVICES                  │   │               PLATFORM SERVICES             ││
│  │                                                     │   │                                             ││
│  │  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  │   │  ┌─────────────┐  ┌─────────────────────┐  ││
│  │  │             │  │             │  │             │  │   │  │             │  │                     │  ││
│  │  │ Azure       │  │ Azure       │  │ Azure API   │  │   │  │ GitHub or   │  │ Azure               │  ││
│  │  │ Functions   │  │ Logic Apps  │  │ Management  │  │   │  │ Azure DevOps│  │ Monitor             │  ││
│  │  │             │  │             │  │             │  │   │  │             │  │                     │  ││
│  │  └─────────────┘  └─────────────┘  └─────────────┘  │   │  └─────────────┘  └─────────────────────┘  ││
│  │                                                     │   │                                             ││
│  │  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  │   │  ┌─────────────┐  ┌─────────────────────┐  ││
│  │  │             │  │             │  │             │  │   │  │             │  │                     │  ││
│  │  │ Host        │  │ Azure       │  │ Azure       │  │   │  │ Azure Data  │  │ Azure Security      │  ││
│  │  │ Integration │  │ Service Bus │  │ Event Grid  │  │   │  │ Factory     │  │ Center              │  ││
│  │  │             │  │             │  │             │  │   │  │             │  │                     │  ││
│  │  └─────────────┘  └─────────────┘  └─────────────┘  │   │  └─────────────┘  └─────────────────────┘  ││
│  │                                                     │   │                                             ││
│  └─────────────────────────────────────────────────────┘   └─────────────────────────────────────────────┘│
│                                                                                                           │
└───────────────────────────────────────────────────────┬───────────────────────────────────────────────────┘
                                                         │
                                                         │ Secure Connection
                                                         │ (ExpressRoute/Private Link)
                                                         │
┌───────────────────────────────────────────────────────┴───────────────────────────────────────────────────┐
│                                    IBM z/OS ENVIRONMENT                                                    │
├───────────────────────────────────────────────────────────────────────────────────────────────────────────┤
│                                                                                                           │
│  ┌─────────────────────────────────┐   ┌────────────────────────────────┐   ┌─────────────────────────┐   │
│  │      APPLICATION LAYER          │   │         DATA LAYER             │   │     INTEGRATION LAYER    │   │
│  │                                 │   │                                │   │                         │   │
│  │  ┌─────────────┐ ┌────────────┐ │   │  ┌─────────────┐ ┌───────────┐ │   │  ┌─────────────────┐   │   │
│  │  │             │ │            │ │   │  │             │ │           │ │   │  │                 │   │   │
│  │  │ Enterprise  │ │ Enterprise │ │   │  │ DB2         │ │ VSAM      │ │   │  │ IBM Connect:    │   │   │
│  │  │ COBOL       │ │ PL/I       │ │   │  │ Databases   │ │ Files     │ │   │  │ Direct          │   │   │
│  │  │             │ │            │ │   │  │             │ │           │ │   │  │                 │   │   │
│  │  └─────────────┘ └────────────┘ │   │  └─────────────┘ └───────────┘ │   │  └─────────────────┘   │   │
│  │                                 │   │                                │   │                         │   │
│  │  ┌─────────────┐ ┌────────────┐ │   │  ┌─────────────┐ ┌───────────┐ │   │  ┌─────────────────┐   │   │
│  │  │             │ │            │ │   │  │             │ │           │ │   │  │                 │   │   │
│  │  │ Assembler   │ │ JCL        │ │   │  │ IMS         │ │ QSAM/BDAM │ │   │  │ IBM Host        │   │   │
│  │  │ Programs    │ │ Procedures │ │   │  │ Databases   │ │ Files     │ │   │  │ Access Transform│   │   │
│  │  │             │ │            │ │   │  │             │ │           │ │   │  │                 │   │   │
│  │  └─────────────┘ └────────────┘ │   │  └─────────────┘ └───────────┘ │   │  └─────────────────┘   │   │
│  │                                 │   │                                │   │                         │   │
│  │  ┌─────────────┐ ┌────────────┐ │   │  ┌─────────────────────────┐   │   │  ┌─────────────────┐   │   │
│  │  │             │ │            │ │   │  │                         │   │   │  │                 │   │   │
│  │  │ CICS        │ │ IMS        │ │   │  │ System Catalogs and     │   │   │  │ MQ Series       │   │   │
│  │  │ Transactions│ │ Applications│ │   │  │ Metadata                │   │   │  │                 │   │   │
│  │  │             │ │            │ │   │  │                         │   │   │  │                 │   │   │
│  │  └─────────────┘ └────────────┘ │   │  └─────────────────────────┘   │   │  └─────────────────┘   │   │
│  └─────────────────────────────────┘   └────────────────────────────────┘   └─────────────────────────┘   │
│                                                                                                           │
└───────────────────────────────────────────────────────────────────────────────────────────────────────────┘
```

## Component Details

### Azure AI Foundry Core Components

1. **Code Intelligence**: Deep analysis of IBM z/OS mainframe code
2. **Translation Services**: Code conversion and API transformation
3. **Risk Intelligence**: Risk assessment and mitigation strategies
4. **Operational Intelligence**: Monitoring and optimization for hybrid environments

### Integration Layer Components

1. **Host Integration Server**: Connectivity between Azure and IBM z/OS
2. **Azure Logic Apps and Functions**: Workflow automation and event processing
3. **Azure API Management**: Modern API façade for mainframe services

### IBM z/OS Integration Components

1. **IBM Connect:Direct**: Secure, reliable file transfer
2. **IBM Host Access Transformation Services**: Modernizing terminal-based applications
3. **IBM MQ Series**: Message queuing between z/OS and Azure 