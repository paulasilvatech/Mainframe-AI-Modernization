# Technical Overview: Azure AI Foundry for IBM z/OS

This document provides a technical overview of Azure AI Foundry for IBM z/OS mainframe modernization.

## Architecture Components

Azure AI Foundry integrates with IBM z/OS environments through a secure, multi-layered architecture:

1. **AI Intelligence Layer**
   - Azure OpenAI Service
   - Azure Cognitive Services
   - Azure Machine Learning

2. **Integration Layer**
   - Azure Logic Apps
   - Azure Functions
   - Azure API Management
   - Host Integration Server
   - IBM Connect:Direct/NDM

3. **Platform Layer**
   - GitHub or Azure DevOps
   - Azure Monitor
   - Azure Security Center
   - Azure Data Services

4. **IBM z/OS Components**
   - Enterprise COBOL Applications
   - PL/I Applications
   - Assembler Programs
   - JCL Procedures
   - CICS Transactions
   - IMS Applications
   - DB2 Databases
   - VSAM Files

## Technical Requirements

| Component | Requirement |
|-----------|-------------|
| Azure Subscription | Pay-As-You-Go or Enterprise Agreement |
| Azure OpenAI Service | General availability in your region |
| Network Connectivity | ExpressRoute or S2S VPN |
| IBM z/OS | Version 2.1 or higher |
| IBM Enterprise COBOL | Version 5.1 or higher |
| IBM Enterprise PL/I | Version 5.1 or higher |

## Integration Pattern

Azure AI Foundry connects to IBM z/OS environments using these technical patterns:

1. **Batch File Transfer**
   - Transfer mainframe artifacts to Azure for analysis
   - Uses IBM Connect:Direct or secure FTP
   - Handles EBCDIC to ASCII conversion

2. **Real-time API Integration**
   - Connect to live z/OS systems for operational intelligence
   - Uses Host Integration Server with TN3270 connectivity
   - Implements Azure API Management for interface modernization

3. **Data Synchronization**
   - Replicates mainframe data to Azure for analysis and testing
   - Uses Azure Data Factory with IBM CDC integration
   - Maintains referential integrity across platforms

## Security Architecture

Azure AI Foundry implements a defense-in-depth approach for mainframe integration:

1. **Network Security**
   - ExpressRoute private connection
   - Azure Private Link for PaaS services
   - Network Security Groups with restricted access

2. **Identity and Access**
   - Azure Active Directory integration
   - RACF/ACF2/Top Secret integration
   - Managed Identities for service-to-service authentication

3. **Data Protection**
   - At-rest encryption with Azure-managed keys
   - In-transit encryption using TLS 1.2+
   - Key Vault integration for secret management

## Technical Benefits

| Benefit | Technical Implementation |
|---------|--------------------------|
| Automated Analysis | AI-powered analysis of COBOL, PL/I, and JCL code |
| Risk Reduction | Predictive risk models for deployment planning |
| Accelerated Migration | Automated translation services for code conversion |
| Knowledge Extraction | Business rule identification and documentation generation |
| Hybrid Operations | Cross-platform monitoring and management |

## Next Steps

For detailed implementation steps, continue to:
- [Technical Architecture Detail](02-architecture.md)
- [Installation Prerequisites](03-prerequisites.md)
- [IBM z/OS Integration Setup](04-zos-integration.md) 