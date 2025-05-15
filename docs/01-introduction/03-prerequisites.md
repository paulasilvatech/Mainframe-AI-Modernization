# 📋 Installation Prerequisites for Azure AI Foundry

This document outlines the prerequisites and requirements for implementing Azure AI Foundry with multiple mainframe platforms (IBM z/OS, Unisys ClearPath, Bull GCOS, and NEC ACOS).

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

## Unisys ClearPath Environment Requirements

### Operating System Requirements

| Requirement | Details |
|-------------|---------|
| ClearPath MCP/OS 2200 | MCP 19.0 or later / OS 2200 18.0 or later<br>Current interim corrections (IC) applied<br>TCP/IP networking properly configured |
| Language Environment | ClearPath COBOL 85 environment<br>Algol runtime libraries<br>ClearPath C/C++ environment (if used) |
| Security System | ClearPath MCP security / OS 2200 security<br>Security profiles for API access<br>Public Key infrastructure support |

### Middleware Requirements

| Requirement | Details |
|-------------|---------|
| ClearPath COMS | COMS 6.0 or later recommended<br>Web services extensions enabled<br>JSON support configured |
| ClearPath TIP | TIP 8.0 or later<br>External connectivity configured<br>Transaction routing enabled |
| DMSII/DMS | Current DMSII version<br>DMSII OPEN/ODBC configured<br>Enterprise Database Server enabled |
| ClearPath MQ | Current version<br>TLS/SSL configured<br>Channel security enabled |

### Integration Components

| Requirement | Details |
|-------------|---------|
| ClearPath ePortal | Current version installed<br>Updates applied<br>Web services configured |
| ClearPath Forward | Latest version with latest updates<br>Connection profiles configured<br>Security settings established |
| ClearPath Enterprise/Suite | Core components installed<br>Integration modules configured<br>API gateway services established |

## Bull GCOS Environment Requirements

### Operating System Requirements

| Requirement | Details |
|-------------|---------|
| GCOS Version | GCOS 8 SR10 or later / GCOS 7 V8 or later<br>Current maintenance applied<br>TCP/IP services properly configured |
| Language Environment | COBOL 85 environment<br>Current runtime libraries<br>Compatible compiler versions |
| Security System | GCOS security system<br>Security profiles for external access<br>Certificate support configured |

### Middleware Requirements

| Requirement | Details |
|-------------|---------|
| TP8 | TP8 with current maintenance<br>Web services capabilities<br>External access configured |
| TPR | Current TPR version<br>Connection capabilities enabled<br>Transaction routing configured |
| IDS/II | Current IDS/II version<br>External database access<br>ODBC/JDBC connectivity |
| JCL Batch System | Current version<br>Integration extensions<br>External submission capabilities |

### Integration Components

| Requirement | Details |
|-------------|---------|
| Liber/Access | Current version installed<br>Service packs applied<br>Access profiles configured |
| GCOS-X Integration | Current components<br>Connection profiles<br>Security settings |
| LiberFactory Suite | Integration modules installed<br>API services configured<br>Security settings established |

## NEC ACOS Environment Requirements

### Operating System Requirements

| Requirement | Details |
|-------------|---------|
| ACOS Version | ACOS-4 with latest service packs / ACOS-2 current version<br>System maintenance applied<br>Network subsystem properly configured |
| Language Environment | ACOS COBOL environment<br>Runtime libraries<br>System utilities current version |
| Security System | ACOS security framework<br>External access profiles<br>Authentication mechanisms configured |

### Middleware Requirements

| Requirement | Details |
|-------------|---------|
| AIM/DC | Current version with latest updates<br>External services enabled<br>JSON capability configured |
| ACOS Database | Current AIM database version<br>External access configured<br>Connection protocols enabled |
| ACOS Batch System | NCL current version<br>External submission capability<br>Integration extensions |
| ACOS MQSeries | Current version<br>Channel security<br>TLS/SSL configuration |

### Integration Components

| Requirement | Details |
|-------------|---------|
| iPackage Tools | Current version installed<br>Updates applied<br>Connection profiles configured |
| DCnet/SNA | Current components<br>TCP/IP integration<br>Security configuration |
| AIM/Connect | Integration modules<br>API services<br>Connection security |

## Development Environment Requirements

### Developer Workstation Requirements

| Requirement | Details |
|-------------|---------|
| Operating System | Windows 10/11 Enterprise or macOS<br>Current updates applied<br>PowerShell 7.1+ (Windows) or zsh/bash (macOS) |
| Tools | VS Code with Azure, GitHub, and mainframe extensions<br>Azure CLI 2.30.0 or higher<br>Git 2.30.0 or higher |
| Terminal Emulators | TN3270 emulator for IBM z/OS<br>TN6530 for Unisys ClearPath<br>VT terminal for Bull GCOS<br>ACOS terminal emulator |

### Source Control and DevOps

| Requirement | Details |
|-------------|---------|
| GitHub | Enterprise account with private repositories<br>Organization-level security settings<br>Branch protection rules |
| Azure DevOps | Azure DevOps Organization<br>Project created with Git repositories<br>Pipeline agents configured |
| GitHub Actions | Self-hosted runners (if behind firewall)<br>Secret management configured<br>Environment configurations defined |

## Network Connectivity Requirements

### Firewall Configuration

| Platform | Ports/Protocols |
|-----------------|-----------------|
| IBM z/OS | TN3270: TCP port 23 or 992 (TLS)<br>DB2: TCP port 446 or custom<br>MQ: TCP ports 1414 or custom<br>Connect:Direct ports (typically 1363) |
| Unisys ClearPath | TN6530: TCP port 102 or 23<br>COMS/SILAS: Custom ports<br>DMSII ODBC: Custom ports<br>ePortal: TCP port 443 |
| Bull GCOS | VT: TCP ports 23 or 992<br>Liber/Access: Custom ports<br>GCOS-X: Custom ports<br>IDS/II: Database specific ports |
| NEC ACOS | Terminal: TCP port 23 or custom<br>iPackage: Custom ports<br>AIM/Connect: Custom ports<br>Database access: Platform-specific ports |

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
- [ ] Mainframe environments prepared with current maintenance:
  - [ ] IBM z/OS environment
  - [ ] Unisys ClearPath environment
  - [ ] Bull GCOS environment
  - [ ] NEC ACOS environment
- [ ] Middleware configured for external access on all platforms
- [ ] Firewall rules implemented and tested for all mainframe connectivity
- [ ] Developer environments configured with required tools and emulators
- [ ] Source control repositories created
- [ ] Security and compliance requirements documented
- [ ] Backup and recovery procedures established

## Next Steps

After ensuring all prerequisites are met, proceed to:
- [Mainframe Platform Integration Setup](04-platform-integration.md)
- [GitHub & Azure DevOps Integration](05-devops-integration.md) 