# Installation Prerequisites for Azure AI Foundry

This document outlines the prerequisites and requirements for implementing Azure AI Foundry with IBM z/OS integration.

## Azure Environment Requirements

### Azure Subscription Requirements

| Requirement | Details |
|-------------|---------|
| Subscription Type | Enterprise Agreement, Pay-As-You-Go, or CSP |
| Required Role | Owner or Contributor at subscription level |
| Resource Providers | Must register: Microsoft.Compute, Microsoft.Storage, Microsoft.Network, Microsoft.CognitiveServices, Microsoft.MachineLearningServices |
| Quota Requirements | At least 20 cores for initial deployment<br>Standard_Dsv3 and Standard_Esv3 series VM availability |

### Networking Requirements

| Requirement | Details |
|-------------|---------|
| ExpressRoute Circuit | Premium SKU recommended for global routing<br>Bandwidth: Minimum 50 Mbps, recommended 200+ Mbps<br>BGP peering configured for Microsoft and private peering |
| Virtual Network | Address space: /23 or larger recommended<br>Multiple subnets for component isolation<br>Network security groups with proper rule configuration |
| DNS Configuration | Private DNS zones for private endpoints<br>Conditional forwarding for on-premises resolution<br>DNS resolution with on-premises DNS servers |

### Azure Active Directory

| Requirement | Details |
|-------------|---------|
| Azure AD Tenant | Enterprise tenant with directory synchronization<br>Multi-factor authentication configured<br>Conditional access policies established |
| Service Principals | Service principals for automation and integration<br>Certificate-based authentication recommended<br>Proper role assignments with least privilege |
| Application Registrations | Registered applications for API access<br>API permission configuration<br>Redirect URI configuration for web applications |

## IBM z/OS Environment Requirements

### Operating System Requirements

| Requirement | Details |
|-------------|---------|
| z/OS Version | z/OS 2.3 or later recommended<br>Current service pack and PTFs applied<br>TCP/IP stack properly configured |
| Language Environment | LE level compatible with installed compilers<br>COBOL, PL/I, and Assembler runtime libraries<br>Current maintenance applied |
| Security System | RACF, ACF2, or Top Secret with current maintenance<br>Security profiles for API and integration access<br>Digital certificate support configured |

### Middleware Requirements

| Requirement | Details |
|-------------|---------|
| IBM CICS | CICS TS 5.5 or later recommended<br>Web services support enabled<br>JSON capabilities configured |
| IBM IMS | IMS 14 or later recommended<br>IMS Connect configured for TCP/IP access<br>Open Database functionality enabled |
| IBM DB2 | DB2 for z/OS 12 or later<br>DRDA and JDBC support enabled<br>Distributed access configured |
| IBM MQ | MQ 9.0 or later recommended<br>Channel security configured<br>TLS/SSL encryption enabled |

### Integration Components

| Requirement | Details |
|-------------|---------|
| IBM Host Integration Server | Most current version installed<br>Service packs applied<br>TN3270 services configured |
| IBM Connect:Direct | Current version with latest PTFs<br>TCP/IP transport configured<br>Secure+ protocol enabled for transfers |
| IBM z/OS Connect | Current version with latest service<br>REST APIs configured for CICS/IMS/DB2<br>API provider configured |

## Development Environment Requirements

### Developer Workstation Requirements

| Requirement | Details |
|-------------|---------|
| Operating System | Windows 10/11 Enterprise or macOS<br>Current updates applied<br>PowerShell 7.1+ (Windows) or zsh/bash (macOS) |
| Tools | VS Code with Azure, GitHub, and mainframe extensions<br>Azure CLI 2.30.0 or higher<br>Git 2.30.0 or higher |
| Terminal Emulator | TN3270 emulator with API capabilities<br>Macro support for automation<br>File transfer capabilities |

### Source Control and DevOps

| Requirement | Details |
|-------------|---------|
| GitHub | Enterprise account with private repositories<br>Organization-level security settings<br>Branch protection rules |
| Azure DevOps | Azure DevOps Organization<br>Project created with Git repositories<br>Pipeline agents configured |
| GitHub Actions | Self-hosted runners (if behind firewall)<br>Secret management configured<br>Environment configurations defined |

## Network Connectivity Requirements

### Firewall Configuration

| Connection Type | Ports/Protocols |
|-----------------|-----------------|
| TN3270 Access | TCP port 23 or 992 (TLS)<br>Stateful inspection<br>Session timeout settings |
| Database Access | DB2: TCP port 446 or custom<br>DRDA protocol support<br>Connection encryption |
| MQ Connections | TCP ports 1414 or custom<br>MQIPT configuration if needed<br>Channel security exits support |
| File Transfer | Connect:Direct ports (typically 1363)<br>FTP/FTPS: ports 21, 990 (control), dynamic (data)<br>Passive mode support for FTP |

### Network Performance

| Requirement | Details |
|-------------|---------|
| Latency | Maximum 100ms round-trip time for interactive<br>Maximum 250ms for batch processing<br>Consistent performance with minimal jitter |
| Bandwidth | Minimum 10 Mbps dedicated for development<br>50+ Mbps for production workloads<br>QoS markings honored across network path |
| Reliability | 99.9% uptime for connectivity<br>Redundant circuits preferred<br>Automated failover configuration |

## Security Requirements

### Encryption and Compliance

| Requirement | Details |
|-------------|---------|
| Data Encryption | TLS 1.2+ for all communications<br>AES-256 for data at rest<br>Key management procedures |
| Compliance | Industry and regulatory compliance requirements<br>Data residency restrictions<br>Audit logging requirements |
| Identity Management | Multi-factor authentication for all admin access<br>Just-in-time privileged access<br>Separation of duties enforcement |

## Installation Preparation Checklist

- [ ] Azure subscription configured with required resource providers
- [ ] ExpressRoute or secure VPN connectivity established
- [ ] Virtual networks and subnets created
- [ ] Network security groups configured
- [ ] Azure AD tenant prepared with required users and groups
- [ ] Service principals created with proper permissions
- [ ] IBM z/OS environment prepared with current maintenance
- [ ] Middleware configured for external access
- [ ] Firewall rules implemented and tested
- [ ] Developer environments configured with required tools
- [ ] Source control repositories created
- [ ] Security and compliance requirements documented
- [ ] Backup and recovery procedures established

## Next Steps

After ensuring all prerequisites are met, proceed to:
- [IBM z/OS Integration Setup](04-zos-integration.md)
- [GitHub & Azure DevOps Integration](05-devops-integration.md) 