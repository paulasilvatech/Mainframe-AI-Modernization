# Technical Architecture: Azure AI Foundry for IBM z/OS Integration

This document provides a detailed technical architecture for Azure AI Foundry integration with IBM z/OS environments.

![Architecture Overview](../../images/architecture-overview.svg)

## Reference Architecture

The following diagram illustrates the comprehensive reference architecture for Azure AI Foundry integration with IBM z/OS:

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
│  │                                 │   │                                │   │                         │   │
│  │  └─────────────────────────────────┘   └────────────────────────────────┘   └─────────────────────────┘   │
│                                                                                                           │
└───────────────────────────────────────────────────────────────────────────────────────────────────────────┘
```

## Component Details

### Azure AI Foundry Core Components

#### 1. Code Intelligence

The Code Intelligence component provides deep analysis of IBM z/OS mainframe code:

| Feature | Technical Capabilities |
|---------|------------------------|
| COBOL Analysis | - Enterprise COBOL syntax parsing<br>- Variable and data structure tracking<br>- Control flow analysis<br>- Procedure division mapping<br>- Copybook resolution and linking |
| PL/I Analysis | - Enterprise PL/I syntax parsing<br>- Structure and procedure mapping<br>- Include file resolution<br>- External call tracking |
| Assembler Analysis | - IBM High-Level Assembler support<br>- Macro expansion tracking<br>- Register usage analysis<br>- System services mapping |
| JCL Analysis | - JCL procedure parsing<br>- Step dependencies tracking<br>- Dataset usage mapping<br>- Symbolic parameter resolution |
| CICS/IMS Analysis | - CICS command identification<br>- CICS program flow analysis<br>- IMS segment and PCB mapping<br>- Screen mapping identification |

Implementation technologies:
- Custom language parsers built on ANTLR
- Azure OpenAI large language models
- Neural program understanding models
- Graph-based dependency tracking

#### 2. Translation Services

The Translation Services component assists in code conversion and API transformation:

| Feature | Technical Capabilities |
|---------|------------------------|
| COBOL to Java | - Syntax-directed translation<br>- Data structure mapping<br>- Procedural to OO transformation<br>- Built-in runtime libraries |
| COBOL to C# | - .NET-optimized translation<br>- Paragraphs to methods mapping<br>- PERFORM to function call translation<br>- Compatibility libraries |
| CICS to REST | - CICS program to API conversion<br>- BMS map to JSON transformation<br>- Stateful session management<br>- Security context propagation |
| IMS to Microservices | - IMS transaction conversion<br>- PCB to service interface mapping<br>- Hierarchical to relational data mapping |
| JCL to Pipelines | - JCL to workflow conversion<br>- Condition code handling<br>- Step dependencies preservation<br>- Resource allocation translation |

Implementation technologies:
- Abstract syntax tree transformations
- AI-guided translation models
- Compatibility runtime libraries
- Azure Logic Apps workflow generation

#### 3. Risk Intelligence

The Risk Intelligence component provides risk assessment and mitigation strategies:

| Feature | Technical Capabilities |
|---------|------------------------|
| Change Impact Analysis | - Inter-program dependency analysis<br>- Data flow impact tracking<br>- Integration point risk assessment<br>- Business function impact mapping |
| Deployment Risk Scoring | - Complexity-based risk scoring<br>- Change scope assessment<br>- Critical path analysis<br>- Historical deployment correlation |
| Test Coverage Analysis | - Code path coverage analysis<br>- Test case sufficiency evaluation<br>- Critical condition identification<br>- Data variation test recommendations |
| Rollback Planning | - Point-in-time recovery planning<br>- Database consistency assurance<br>- Transaction integrity verification<br>- Operational procedure generation |

Implementation technologies:
- Graph-based impact analysis
- Machine learning risk models
- Pattern recognition from historical data
- Azure DevOps/GitHub pipeline integration

#### 4. Operational Intelligence

The Operational Intelligence component provides monitoring and optimization for hybrid environments:

| Feature | Technical Capabilities |
|---------|------------------------|
| Performance Monitoring | - z/OS RMF/SMF data integration<br>- Cross-platform transaction tracking<br>- Response time breakdown analysis<br>- Resource utilization correlation |
| Anomaly Detection | - Baseline deviation detection<br>- Pattern-based anomaly identification<br>- Multi-dimensional analysis<br>- Auto-correlation of related metrics |
| Capacity Planning | - Workload growth prediction<br>- Resource requirement forecasting<br>- Batch window optimization<br>- Cost optimization recommendations |
| Incident Management | - Root cause analysis assistance<br>- Problem pattern recognition<br>- Resolution recommendation<br>- Knowledge base integration |

Implementation technologies:
- Azure Monitor custom metrics integration
- Azure Log Analytics with SMF log ingestion
- Azure Application Insights for transaction tracking
- Machine learning for anomaly detection

### Integration Layer Components

#### 1. Host Integration Server

Host Integration Server (HIS) provides connectivity between Azure and IBM z/OS:

| Feature | Technical Implementation |
|---------|--------------------------|
| TN3270 Integration | - TN3270 terminal emulation<br>- Screen scraping with pattern recognition<br>- Session management and pooling<br>- SSL/TLS encryption support |
| Data Integration | - DB2 for z/OS direct access<br>- VSAM record retrieval and update<br>- Transaction coordination (2PC)<br>- Schema mapping and transformation |
| Transaction Integration | - CICS transaction invocation<br>- IMS transaction processing<br>- Security credential mapping<br>- Transaction integrity management |

#### 2. Azure Logic Apps and Functions

These serverless components enable workflow automation and event processing:

| Feature | Technical Implementation |
|---------|--------------------------|
| Mainframe Process Orchestration | - JCL-like workflow orchestration<br>- Conditional processing support<br>- Parallel step execution<br>- Error handling and recovery |
| Event-Driven Processing | - Mainframe event detection<br>- Real-time processing triggers<br>- Custom connector integration<br>- Stateful process execution |
| Integration Automation | - File transfer automation<br>- Format conversion processing<br>- Validation and verification<br>- Notification and alerting |

#### 3. Azure API Management

API Management provides a modern API façade for mainframe services:

| Feature | Technical Implementation |
|---------|--------------------------|
| Mainframe API Façade | - REST API for CICS/IMS services<br>- JSON transformation of legacy data formats<br>- OpenAPI (Swagger) definition generation<br>- API versioning and lifecycle management |
| API Security | - OAuth/OIDC integration<br>- JWT validation and claims mapping<br>- Rate limiting and quota enforcement<br>- IP filtering and network isolation |
| API Gateway | - Request/response transformation<br>- Protocol conversion<br>- Caching for performance optimization<br>- Logging and monitoring integration |

### IBM z/OS Integration Components

#### 1. IBM Connect:Direct

Connect:Direct enables secure, reliable file transfer between z/OS and Azure:

| Feature | Technical Implementation |
|---------|--------------------------|
| Secure File Transfer | - Checkpoint/restart capability<br>- Compression and encryption<br>- Secure+ protocol support<br>- Transfer validation and verification |
| Scheduled Transfers | - Event-driven transfer initiation<br>- Calendar-based scheduling<br>- Prerequisite dependency support<br>- Completion notification and alerting |
| Data Format Handling | - EBCDIC to ASCII conversion<br>- Record format preservation<br>- Unicode transformation<br>- Code page mapping |

#### 2. IBM Host Access Transformation Services (HATS)

HATS helps modernize terminal-based applications:

| Feature | Technical Implementation |
|---------|--------------------------|
| Screen Transformation | - 3270 screen to HTML/JSON conversion<br>- Screen pattern recognition<br>- Macro recording and playback<br>- Screen customization and styling |
| Web Service Creation | - Terminal interaction as services<br>- Session state management<br>- SOAP and REST interface generation<br>- Transaction integration |

#### 3. IBM MQ Series

MQ provides message queuing between z/OS and Azure:

| Feature | Technical Implementation |
|---------|--------------------------|
| Queue Management | - Queue definition and configuration<br>- Message persistence and durability<br>- Transaction coordination<br>- Dead letter queue handling |
| Channel Security | - TLS channel encryption<br>- Mutual authentication<br>- Channel exit support<br>- Certificate management |

## Security Architecture

### Network Security

The network security architecture follows defense-in-depth principles:

1. **Physical Connectivity**
   - ExpressRoute premium circuit for dedicated connectivity
   - Redundant connections for high availability
   - BGP routing for dynamic path selection

2. **Network Controls**
   - Network Security Groups with deny-by-default rules
   - Application Security Groups for service isolation
   - Service Tags for Azure service access control
   - Azure DDoS Protection Standard

3. **Service Endpoints and Private Link**
   - Azure Private Link for PaaS services access
   - Service Endpoints for Azure services
   - Private endpoints for secure resource access
   - DNS integration for private resolution

### Identity and Access Management

Identity and access management integrates Azure AD with mainframe security:

1. **Azure Active Directory**
   - Azure AD as primary identity provider
   - Multi-factor authentication enforcement
   - Conditional access policies
   - Privileged Identity Management

2. **Mainframe Security Integration**
   - RACF/ACF2/Top Secret user mapping
   - Security credential propagation
   - Enterprise certificate management
   - Secure password vaulting

3. **Application Authentication**
   - Service principal authentication
   - Managed identities for Azure services
   - Application registration and API permissions
   - JWT token-based authentication

### Data Protection

Data protection covers both at-rest and in-transit scenarios:

1. **Encryption at Rest**
   - Azure Storage encryption
   - Azure SQL/Cosmos DB encryption
   - Disk encryption for VMs
   - Customer-managed keys option

2. **Encryption in Transit**
   - TLS 1.2+ for all communications
   - IPsec for ExpressRoute connections
   - Secure protocol enforcement
   - Certificate-based authentication

3. **Key Management**
   - Azure Key Vault for key storage
   - HSM-backed protection
   - Key rotation automation
   - Access control with RBAC

## Deployment Architecture

Azure AI Foundry deployment follows a hub-and-spoke model:

1. **Hub Virtual Network**
   - Central connectivity and security services
   - ExpressRoute/VPN gateways
   - Shared services (DNS, monitoring)
   - Network Virtual Appliances

2. **Spoke Virtual Networks**
   - Workload-specific virtual networks
   - Peered to hub for connectivity
   - Isolated security boundaries
   - Purpose-specific NSGs

3. **Cross-Premises Connectivity**
   - ExpressRoute for z/OS connectivity
   - VPN backup options
   - BGP route exchange
   - Traffic filtering

## Scaling and Performance

1. **Compute Scaling**
   - Virtual Machine Scale Sets for HIS
   - Azure App Service scaling for web front-ends
   - Azure Functions consumption plan
   - Container instances for processing nodes

2. **Storage Performance**
   - Premium SSD for high I/O workloads
   - Ultra Disk for highest performance
   - Azure NetApp Files for shared file storage
   - Storage account partitioning

3. **Network Performance**
   - ExpressRoute circuit sizing (1-10 Gbps)
   - ExpressRoute Global Reach
   - Accelerated networking for VMs
   - Azure Front Door for global distribution

## Disaster Recovery

1. **Regional Pairs**
   - Primary and secondary region deployment
   - Geo-redundant storage for state data
   - Cross-region replication

2. **Recovery Options**
   - Azure Site Recovery for VM workloads
   - Database geo-replication
   - Traffic Manager for failover routing
   - Recovery point and time objectives

## Next Steps

For detailed implementation steps, continue to:
- [Installation Prerequisites](03-prerequisites.md)
- [IBM z/OS Integration Setup](04-zos-integration.md)
- [GitHub & Azure DevOps Integration](05-devops-integration.md) 