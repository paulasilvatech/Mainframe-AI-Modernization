# Azure AI Foundry Components for Multiple Mainframe Platforms

This document provides a detailed breakdown of Azure AI Foundry components for integrating with various mainframe platforms, including IBM z/OS, Unisys ClearPath, Bull GCOS, and NEC ACOS.

## Core AI Components

### Azure OpenAI Service

The Azure OpenAI Service powers the core intelligence capabilities of Azure AI Foundry across all mainframe platforms:

| Feature | Technical Details |
|---------|------------------|
| Code Understanding | Large language models trained on COBOL, PL/I, JCL, Assembler, and platform-specific languages |
| Pattern Recognition | Identification of common patterns and anti-patterns across mainframe platforms |
| Knowledge Extraction | Extraction of business rules and domain knowledge from diverse mainframe codebases |
| Documentation Generation | Automated generation of technical documentation for all platforms |
| Modernization Suggestions | Platform-specific recommendations for code refactoring and modernization |

**Integration Points:**
- Accessed via Azure OpenAI API
- Custom models fine-tuned for various mainframe languages and dialects
- Configurable context windows for large codebases
- Platform-specific prompt engineering techniques

### Azure Cognitive Services

Cognitive Services provide specialized AI capabilities for all mainframe platforms:

| Service | Mainframe Application |
|---------|----------------------|
| Form Recognizer | Digitizing mainframe documentation and printouts from all vendors |
| Custom Vision | Screen pattern recognition for various terminal interfaces |
| Text Analytics | Analysis of comments and documentation across platforms |
| Anomaly Detector | Identifying abnormal patterns in operational data from any mainframe |
| Translator | Converting technical documentation between languages |

**Integration Points:**
- REST APIs with batch processing capabilities
- Specialized models for platform-specific content
- Integration with documentation repositories
- Support for multiple character encodings (EBCDIC, ASCII, EBCDIC-JP, etc.)

### Azure Machine Learning

Azure Machine Learning provides cross-platform capabilities:

| Capability | Technical Implementation |
|------------|--------------------------|
| Custom Model Training | Training specialized models for specific mainframe environments (IBM, Unisys, Bull, NEC) |
| Automated ML | Discovering optimal models for prediction tasks across platforms |
| MLOps | Managing model lifecycle across heterogeneous environments |
| Responsible AI | Ensuring ethical use of AI in modernization |
| Real-time Inference | Processing operational data for instant insights across platforms |

**Integration Points:**
- Python SDK for custom model development
- REST API for model inference
- Integration with Azure Databricks for large-scale data processing
- Platform-specific data connectors

## Platform-Specific Components

### Code Analysis Engine

The Code Analysis Engine provides deep analysis of code across multiple mainframe platforms:

| Platform | Language Support |
|----------|-----------------|
| IBM z/OS | Enterprise COBOL, Enterprise PL/I, HLASM, JCL, CICS, IMS |
| Unisys ClearPath | COBOL-74/85, ALGOL, ClearPath TACL, WFL, DMSII |
| Bull GCOS | GCOS COBOL, JCL, TPR, IDS/II |
| NEC ACOS | ACOS COBOL, NCL, AIM |

**Technical Specifications:**
- Multi-encoding support (EBCDIC, ASCII, EBCDIC-JP)
- Fixed-format and free-format parsing for all platforms
- Platform-specific language extensions
- Complete program flow analysis
- Dialect-aware parsing for regional variants

### Dependency Mapper

The Dependency Mapper identifies relationships between components across platforms:

| Platform | Dependency Analysis Capabilities |
|----------|----------------------------------|
| IBM z/OS | Program calls, DB2, VSAM, JCL flow, CICS/IMS transactions |
| Unisys ClearPath | Program calls, DMSII databases, COMS transactions |
| Bull GCOS | Program links, IDS/II databases, TP8 transactions |
| NEC ACOS | Program linkage, AIM databases, AIM/DC transactions |

**Implementation Architecture:**
- Graph database for relationship storage
- Visualization engine for dependency diagrams
- Query API for programmatic access
- Impact analysis algorithms
- Change propagation prediction
- Cross-platform relationship mapping

### Translation Engine

The Translation Engine converts mainframe assets to modern equivalents:

| Platform | Transformation Capabilities |
|----------|----------------------------|
| IBM z/OS | COBOL→Java/C#/Node.js, PL/I→Java/C#, JCL→DevOps pipelines, DB2→Modern DBs |
| Unisys ClearPath | COBOL→Java/C#, ALGOL→C#/Java, WFL→DevOps pipelines, DMSII→Modern DBs |
| Bull GCOS | GCOS COBOL→Java/C#, JCL→DevOps pipelines, IDS/II→Modern DBs |
| NEC ACOS | ACOS COBOL→Java/C#, NCL→DevOps pipelines, AIM→Modern DBs |

**Technical Capabilities:**
- Abstract Syntax Tree (AST) based transformation
- Semantic preservation guarantees
- Runtime library support for compatibility
- Code style customization
- Documentation generation
- Platform-specific idiom translation

### Operational Intelligence Service

The Operational Intelligence Service monitors and optimizes hybrid environments across platforms:

| Platform | Monitoring Capabilities |
|----------|------------------------|
| IBM z/OS | Integration with RMF/SMF data |
| Unisys ClearPath | Integration with CPMS data |
| Bull GCOS | Integration with GM3/GM8 monitoring |
| NEC ACOS | Integration with ACOSMON data |

**Integration Architecture:**
- Azure Monitor custom metrics
- Log Analytics with custom connectors for each platform
- Application Insights for transaction tracking
- Azure Data Explorer for historical analysis
- Azure Alerts for proactive monitoring
- Cross-platform correlation engine

## Integration Components

### Mainframe Connectivity Services

Connectivity services provide access to various mainframe platforms:

| Platform | Connectivity Components |
|----------|------------------------|
| IBM z/OS | HIS, Connect:Direct, MQ Series, TN3270 |
| Unisys ClearPath | ClearPath ePortal, ClearPath MCP TN6530, Open/OLTP |
| Bull GCOS | Liber/Access, GCOS-X, TN3270 |
| NEC ACOS | iPackage, DCnet/SNA, AIM/Connect |

**Technical Specifications:**
- Windows or Linux deployment
- TLS 1.2+ encryption
- Managed identity support
- Connection pooling
- High availability configurations
- Platform-specific protocol support

### Azure API Management

Azure API Management modernizes interfaces for all mainframe platforms:

| Platform | API Capabilities |
|----------|-----------------|
| IBM z/OS | CICS/IMS transactions as REST APIs |
| Unisys ClearPath | COMS transactions as REST APIs |
| Bull GCOS | TP8 transactions as REST APIs |
| NEC ACOS | AIM/DC transactions as REST APIs |

**Implementation Architecture:**
- Premium tier for virtual network integration
- Custom policies for platform-specific transformations
- Backend services integration with connectivity components
- Caching for performance optimization
- Developer portal customization
- Multi-platform authentication

### Azure Logic Apps

Azure Logic Apps orchestrates processes across mainframe platforms:

| Platform | Orchestration Capabilities |
|----------|----------------------------|
| IBM z/OS | JCL workflow automation, CICS/IMS integration |
| Unisys ClearPath | WFL workflow automation, COMS integration |
| Bull GCOS | JCL workflow automation, TP8 integration |
| NEC ACOS | NCL workflow automation, AIM/DC integration |

**Integration Points:**
- Custom connectors for each mainframe platform
- Standard connectors for cloud services
- B2B protocols support (AS2, X12, EDIFACT)
- Enterprise Integration Pack
- Integration with Event Grid for event-driven architecture
- Cross-platform transaction coordination

## Deployment Models

Azure AI Foundry components can be deployed in multiple configurations for any mainframe platform:

1. **Cloud-Only Model**
   - All components deployed in Azure
   - Connection to mainframes via ExpressRoute or VPN
   - Suitable for environments with good connectivity

2. **Hybrid Model**
   - AI components in Azure
   - Integration components on-premises
   - Data synchronization between environments
   - Suitable for environments with security constraints

3. **Multi-Platform Model**
   - Support for heterogeneous mainframe environments
   - Unified management across platforms
   - Consolidated DevOps pipelines
   - Suitable for organizations with multiple mainframe types

4. **Disconnected Model**
   - Batch processing of mainframe assets
   - Offline analysis and transformation
   - Results synchronized back to mainframes
   - Suitable for air-gapped environments

## Platform-Specific Considerations

### IBM z/OS

- Integration with IBM Z and Cloud Modernization Stack
- Support for z/OS Connect for API enablement
- Specialized handling for EBCDIC encoding
- Compatibility with IBM watsonx Code Assistant for Z

### Unisys ClearPath

- Integration with ClearPath MCP Software Series
- Support for ClearPath ePortal for web and mobile interfaces
- Special handling for ClearPath Forward Fabric architecture
- Data transformation for DMSII database structures

### Bull GCOS

- Integration with LiberFactory Suite
- Support for Atos Migration+ toolset
- Hardware virtualization capabilities
- Emulation layer integration

### NEC ACOS

- Integration with CASEWORLD/PE development environment
- Support for iPackage integration tools
- Special handling for EBCDIC-JP encoding
- ACOS-specific parallel processing support

## Next Steps

For detailed implementation guidance, continue to:
- [Implementation Roadmap](04-implementation-roadmap.md)
- [Discovery and Assessment](../02-discovery/README.md)
- [Foundation Setup](../03-foundation/README.md)
- [Platform-Specific Implementation Guides](../13-comprehensive-mainframe-modernization/README.md) 