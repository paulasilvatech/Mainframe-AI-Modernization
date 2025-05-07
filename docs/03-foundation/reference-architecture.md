# Reference Architecture

This document provides a comprehensive reference architecture for IBM z/OS mainframe modernization using Azure AI Foundry with GitHub and Azure DevOps integration.

## Architecture Overview

The reference architecture establishes a blueprint for implementing a hybrid modernization approach that enables organizations to modernize IBM z/OS mainframe applications while maintaining operational integrity and business continuity.

![Reference Architecture](../../images/reference-architecture.png)

This architecture is designed to support multiple modernization patterns, including:
- Rehosting (lift and shift)
- Refactoring (code transformation)
- Rearchitecting (application restructuring)
- Rebuilding (application rewrite)
- Replacing (COTS/SaaS implementation)

## Core Architecture Principles

The reference architecture adheres to these core technical principles:

1. **Hybrid by Design**
   - Support hybrid operation between mainframe and cloud
   - Enable phased migration approaches
   - Maintain interoperability during transition

2. **Cloud-Native Where Appropriate**
   - Leverage cloud-native capabilities where beneficial
   - Apply containerization for appropriate components
   - Implement microservices architectures selectively

3. **Security-First Approach**
   - Maintain or enhance current security posture
   - Implement defense-in-depth security strategy
   - Address compliance requirements by design

4. **DevOps Enablement**
   - Integrate CI/CD across mainframe and cloud
   - Automate build, test, and deployment processes
   - Implement infrastructure as code

5. **AI-Powered Transformation**
   - Leverage AI for code analysis and transformation
   - Apply AI for testing and quality assurance
   - Utilize AI for operational insights

## Architecture Components

### 1. IBM z/OS Environment

The architecture integrates with existing IBM z/OS environments through:

| Component | Purpose | Implementation |
|-----------|---------|----------------|
| z/OS Connect | REST API enablement for mainframe applications | z/OS Connect EE with API Gateway |
| Batch Integration | Integration with batch processing systems | Azure Logic Apps with FTP/SFTP connectors |
| Data Integration | Data synchronization and replication | Azure Data Factory with CDC connectors |
| Security Integration | Authentication and authorization | Azure AD integration with RACF/ACF2/TopSecret |

### 2. Azure Landing Zone

The Azure landing zone provides the foundational infrastructure:

```
┌───────────────────────────────────────────────────────────────┐
│                     Management Group                          │
└───────────────────────────────┬───────────────────────────────┘
                                │
                                ▼
┌───────────────────────────────────────────────────────────────┐
│                        Subscription                           │
└───────────────────────────────┬───────────────────────────────┘
                                │
                                ▼
┌───────────────────────────────────────────────────────────────┐
│                        Resource Groups                        │
├─────────────────┬─────────────────┬─────────────────┬─────────┤
│ Connectivity    │ Identity        │ Management      │ Security│
└─────────────────┴─────────────────┴─────────────────┴─────────┘
```

Key components include:

- **Connectivity Hub**: ExpressRoute, Azure Firewall, DNS
- **Identity Services**: Azure AD, Azure AD DS, Managed Identities
- **Management Services**: Azure Monitor, Log Analytics, Azure Automation
- **Security Services**: Azure Security Center, Azure Sentinel, Key Vault

### 3. AI Foundry Platform

The Azure AI Foundry platform provides these capabilities:

```
┌───────────────────────────────────────────────────────────────┐
│                    AI Foundry Components                      │
├─────────────────┬─────────────────┬─────────────────┬─────────┤
│ Code Analysis   │ Knowledge       │ Transformation  │Operation│
│ & Intelligence  │ Extraction      │ Services        │Analytics│
└─────────────────┴─────────────────┴─────────────────┴─────────┘
```

| Component | Purpose | Implementation |
|-----------|---------|----------------|
| Code Analysis | Analyze mainframe code structure | Azure AI Language Understanding, Custom ML models |
| Knowledge Extraction | Extract business rules and logic | Azure OpenAI, Knowledge Mining |
| Transformation Services | Automated code transformation | Custom ML transformation models, Azure OpenAI |
| Operational Analytics | Runtime insights and optimization | Azure Monitor, Application Insights |

### 4. DevOps Integration

The architecture implements a unified DevOps approach:

```
┌───────────────────────────────────────────────────────────────┐
│                      DevOps Platform                          │
├─────────────────┬─────────────────┬─────────────────┬─────────┤
│ Source Control  │ CI/CD Pipelines │ Test Automation │ Release │
│ Management      │                 │                 │ Mgmt    │
└─────────────────┴─────────────────┴─────────────────┴─────────┘
```

The architecture supports both GitHub and Azure DevOps:

| Component | GitHub Implementation | Azure DevOps Implementation |
|-----------|----------------------|----------------------------|
| Source Control | GitHub Repositories | Azure Repos |
| CI/CD Pipelines | GitHub Actions | Azure Pipelines |
| Test Automation | GitHub Actions + 3rd party | Azure Test Plans + Pipelines |
| Artifact Management | GitHub Packages | Azure Artifacts |
| Work Management | GitHub Issues | Azure Boards |

### 5. Application Patterns

The reference architecture supports these application patterns:

#### Pattern 1: API-Enabled Mainframe

![API-Enabled Pattern](../../images/api-enabled-pattern.png)

- Expose mainframe functionality via REST APIs
- Modern frontends accessing mainframe services
- Retain core business logic on mainframe
- Implementation: z/OS Connect + Azure API Management

#### Pattern 2: Hybrid Data Processing

![Hybrid Data Pattern](../../images/hybrid-data-pattern.png)

- Replicate mainframe data to cloud platforms
- Process data in cloud while maintaining mainframe source
- Enable cloud analytics on mainframe data
- Implementation: CDC tools + Azure Data Factory + Azure Synapse

#### Pattern 3: Incremental Refactoring

![Incremental Refactoring Pattern](../../images/incremental-refactoring-pattern.png)

- Incrementally refactor components to cloud-native
- Maintain interoperability with remaining mainframe components
- Phased migration approach
- Implementation: AI-powered code transformation + Azure Functions/App Service

#### Pattern 4: Containerized Rehosting

![Containerized Rehosting Pattern](../../images/containerized-rehosting-pattern.png)

- Rehost mainframe applications in containers
- Maintain application logic with minimal changes
- Enable DevOps practices and horizontal scaling
- Implementation: Containerization tools + Azure Kubernetes Service

## Network Architecture

The reference architecture implements a secure networking approach:

```
┌──────────────────┐    ┌─────────────────┐    ┌───────────────────┐
│ Enterprise       │    │ Azure ExpressRoute│    │ Azure Virtual     │
│ Network          │◄──►│ Circuit          │◄──►│ Network           │
└──────────────────┘    └─────────────────┘    └───────┬───────────┘
                                                       │
                                                       ▼
                                            ┌───────────────────────┐
                                            │ Network Security      │
                                            │ (NSGs, Firewall)      │
                                            └───────┬───────────────┘
                                                   │
                        ┌───────────────────────┐ │ ┌───────────────────────┐
                        │ Web Tier Subnet       │◄┼─►│ App Tier Subnet       │
                        └───────────────────────┘ │ └───────────────────────┘
                                                  │
                                                  ▼
                                       ┌───────────────────────┐
                                       │ Data Tier Subnet      │
                                       └───────────────────────┘
```

Key elements include:
- ExpressRoute connectivity to on-premises
- Hub-and-spoke network topology
- Segmented network subnets by function
- Network security groups and Azure Firewall
- Private endpoints for PaaS services

## Security Architecture

The reference architecture implements a comprehensive security model:

### Identity and Access

- Azure AD for cloud identity management
- Integration with mainframe security (RACF/ACF2/TopSecret)
- Role-based access control (RBAC)
- Just-in-time access for privileged operations
- Managed identities for service-to-service authentication

### Network Security

- Network isolation with NSGs and Azure Firewall
- Private endpoints for PaaS services
- Encrypted communications using TLS 1.2+
- DDoS protection
- Web Application Firewall for internet-facing services

### Data Security

- Encryption at rest for all storage
- Encryption in transit using TLS
- Key management through Azure Key Vault
- Data classification and protection
- Access auditing and monitoring

### Application Security

- DevSecOps integration in CI/CD
- Automated security scanning
- Secret management through Key Vault
- Secure coding practices
- Runtime application protection

## Deployment Topologies

The reference architecture supports multiple deployment topologies:

### Development Topology

```
┌───────────────────────────────────────────────────────────────┐
│                 Development Subscription                       │
├────────────────┬────────────────┬───────────────┬─────────────┤
│ Dev Environment│ Test Environment│ Tools        │ Shared      │
│ (Per Team)     │ (Integration)   │ & Services   │ Services    │
└────────────────┴────────────────┴───────────────┴─────────────┘
```

### Production Topology

```
┌───────────────────────────────────────────────────────────────┐
│              Production Subscription                           │
├────────────────┬────────────────┬───────────────┬─────────────┤
│ Pre-Production │ Production     │ DR            │ Monitoring  │
│ Environment    │ Environment    │ Environment   │ & Management│
└────────────────┴────────────────┴───────────────┴─────────────┘
```

## Integration Patterns

The reference architecture supports these integration patterns:

### 1. Synchronous API Integration

- REST API calls between systems
- Real-time integration
- Implementation: Azure API Management + z/OS Connect

### 2. Asynchronous Message Integration

- Message-based integration
- Event-driven architecture
- Implementation: Azure Service Bus + IBM MQ integration

### 3. Batch Data Integration

- Scheduled data transfers
- Bulk processing operations
- Implementation: Azure Data Factory + SFTP/Connect:Direct

### 4. Hybrid Transaction Processing

- Distributed transaction management
- Cross-platform transaction integrity
- Implementation: Custom transaction coordination services

## Implementation Considerations

When implementing this reference architecture, consider:

### Scalability

- Implement auto-scaling for cloud components
- Maintain capacity planning for mainframe components
- Consider scaling limitations in hybrid operations

### Resilience

- Implement redundancy across all critical components
- Design for graceful degradation of services
- Implement comprehensive monitoring and alerting

### Performance

- Address network latency in hybrid integration
- Optimize data transfer operations
- Implement performance testing across platforms

### Cost Optimization

- Implement right-sizing for cloud resources
- Consider reserved instances for predictable workloads
- Implement cost monitoring and optimization

## Next Steps

After reviewing the reference architecture:
- Align your [Modernization Strategy](modernization-strategy.md) with this architecture
- Review the [Governance Framework](governance-framework.md) for implementation controls
- Develop your [Team Organization](team-organization.md) model
- Begin implementing your [Development Environment](../04-development-environment/README.md) 