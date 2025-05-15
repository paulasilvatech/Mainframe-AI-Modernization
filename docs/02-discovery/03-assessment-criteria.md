# 📊 AI-Powered Assessment Criteria

This document details the technical assessment criteria used by Azure AI Foundry to evaluate mainframe applications across multiple platforms (IBM z/OS, Unisys ClearPath, Bull GCOS, and NEC ACOS) for modernization.

```mermaid
graph TB
    subgraph "Multi-Platform Code Analysis Engine"
        CA1["Lexical & Syntactic Parsing"]
        CA2["Control Flow Analysis"]
        CA3["Pattern Recognition"]
        CA4["Technical Debt Quantification"]
    end
    
    subgraph "Platform-Specific Analytics"
        PA1["z/OS: SMF Records"]
        PA2["ClearPath: CPMS Data"]
        PA3["GCOS: GM3/GM8 Monitoring"]
        PA4["ACOS: ACOSMON Data"]
    end
    
    subgraph "Business Impact Analyzer"
        BI1["Business Criticality"]
        BI2["Revenue/Cost Impact"]
        BI3["Regulatory Compliance"]
        BI4["Strategic Alignment"]
    end
    
    Multi-Platform Code Analysis Engine --> MCI["Modernization Complexity Index"]
    Platform-Specific Analytics --> MCI
    Business Impact Analyzer --> MCI
    
    MCI --> MOD["Modernization Approach"]
```

## Assessment Framework

Azure AI Foundry uses a multi-dimensional assessment framework to evaluate mainframe applications across technical, operational, and business dimensions for all supported mainframe platforms.

The framework employs these key technical components:

1. **Multi-Platform Code Analysis Engine**
   - Deep lexical and syntactic parsing of:
     - IBM z/OS: COBOL, PL/I, Assembler, JCL
     - Unisys ClearPath: COBOL, Algol, WFL
     - Bull GCOS: GCOS COBOL, JCL
     - NEC ACOS: ACOS COBOL, NCL
   - Static analysis of control flow and data dependencies
   - Pattern recognition for common architectural patterns
   - Technical debt quantification
   - Cross-platform integration analysis

2. **Platform-Specific Operational Analytics**
   - IBM z/OS: Performance metrics from SMF records
   - Unisys ClearPath: Performance metrics from CPMS data
   - Bull GCOS: Performance metrics from GM3/GM8 monitoring
   - NEC ACOS: Performance metrics from ACOSMON data
   - Resource consumption analysis (CPU, memory, I/O)
   - Batch window impact assessment
   - Transaction volume and response time analysis

3. **Business Impact Analyzer**
   - Business criticality assessment
   - Revenue and cost impact modeling
   - Regulatory and compliance implications
   - Strategic alignment evaluation
   - Cross-platform dependencies and business workflows

## Technical Assessment Dimensions

The assessment process evaluates applications across these technical dimensions:

| Dimension | Assessment Criteria | Weight |
|-----------|---------------------|--------|
| Code Complexity | Cyclomatic complexity, control flow density | 15% |
| Technical Debt | Dead code, duplicate logic, obsolete patterns | 10% |
| Dependencies | External system dependencies, internal coupling | 15% |
| Performance | CPU consumption, I/O efficiency, resource usage | 15% |
| Architecture | Modular design, separation of concerns | 10% |
| Data Complexity | Data structure complexity, access patterns | 15% |
| Documentation | Code comments, external documentation | 5% |
| Maintenance History | Change frequency, defect density | 15% |

## Platform-Specific Assessment Considerations

Each mainframe platform has unique characteristics that are considered in the assessment:

| Platform | Specific Assessment Considerations |
|----------|-----------------------------------|
| **IBM z/OS** | CICS/IMS transaction complexity, DB2 data access patterns, 31-bit addressing constraints, Assembler usage, JCL complexity |
| **Unisys ClearPath** | COMS transaction complexity, DMSII database structures, MCP/OS 2200 specific features, WFL complexity, Algol code assessment |
| **Bull GCOS** | TP8 transaction complexity, IDS/II database structures, GCOS-specific features, JCL variations, TPR subsystem integration |
| **NEC ACOS** | AIM/DC transaction complexity, AIM database structures, ACOS-specific features, NCL complexity, ACOS dialect assessment |

## Complexity Scoring Algorithm

The assessment uses this algorithm to calculate the Modernization Complexity Index (MCI):

```
MCI = (CCW × CC) + (TDW × TD) + (DW × D) + (PW × P) + (AW × A) + (DCW × DC) + (DOW × DO) + (MHW × MH) + (PSW × PS)

Where:
- CCW = Code Complexity Weight
- CC = Code Complexity Score
- TDW = Technical Debt Weight
- TD = Technical Debt Score
- DW = Dependencies Weight
- D = Dependencies Score
- PW = Performance Weight
- P = Performance Score
- AW = Architecture Weight
- A = Architecture Score
- DCW = Data Complexity Weight
- DC = Data Complexity Score
- DOW = Documentation Weight
- DO = Documentation Score
- MHW = Maintenance History Weight
- MH = Maintenance History Score
- PSW = Platform-Specific Weight
- PS = Platform-Specific Score
```

Each score is normalized to a scale of 1-10, with 10 representing the highest complexity.

## Implementation Process

### Data Collection and Analysis

The assessment process follows this technical workflow:

```
┌────────────────┐     ┌─────────────────┐     ┌──────────────────┐
│ Source Metrics │────▶│ Algorithm      │────▶│ Modernization    │
│  Collection    │     │  Processing     │     │  Scoring         │
└────────────────┘     └─────────────────┘     └──────────────────┘
        │                                               │
        ▼                                               ▼
┌────────────────┐     ┌─────────────────┐     ┌──────────────────┐
│   Operational  │────▶│ Multi-factor    │────▶│  Recommendation  │
│   Metrics      │     │  Analysis       │     │  Engine          │
└────────────────┘     └─────────────────┘     └──────────────────┘
```

### Technical Implementation Steps

To implement the assessment process:

1. **Configure Assessment Parameters**
   ```bash
   # Configure Assessment Parameters
   az ai-foundry assessment configure --config-file assessment-config.json
   
   # Set Assessment Weights
   az ai-foundry assessment set-weights --complexity 15 --technical-debt 10 --dependencies 15 --performance 15 --architecture 10 --data-complexity 15 --documentation 5 --maintenance-history 15
   
   # Configure Platform-Specific Assessment
   az ai-foundry assessment configure-platform --platform zos --config-file zos-assessment-config.json
   az ai-foundry assessment configure-platform --platform clearpath --config-file clearpath-assessment-config.json
   az ai-foundry assessment configure-platform --platform gcos --config-file gcos-assessment-config.json
   az ai-foundry assessment configure-platform --platform acos --config-file acos-assessment-config.json
   ```

2. **Run Initial Assessment**
   ```bash
   # Execute Assessment for All Platforms
   az ai-foundry assessment run --inventory-id <inventory-id> --platform all --output-format json
   
   # Execute Assessment for Specific Platform
   az ai-foundry assessment run --inventory-id <inventory-id> --platform zos --output-format json
   
   # Generate Assessment Report
   az ai-foundry assessment report --assessment-id <assessment-id> --format html
   ```

3. **Calibrate Assessment**
   ```bash
   # Calibrate Results with Expert Feedback
   az ai-foundry assessment calibrate --assessment-id <assessment-id> --calibration-file expert-feedback.json
   
   # Re-run Assessment with Calibration
   az ai-foundry assessment run --inventory-id <inventory-id> --platform all --calibration-id <calibration-id> --output-format json
   ```

## Platform-Specific Modernization Approach Recommendations

Based on the assessment scores, the AI engine recommends modernization approaches with platform-specific considerations:

### IBM z/OS

| MCI Range | Recommended Approach | Technical Considerations |
|-----------|---------------------|-------------------------|
| 1.0 - 3.0 | Replace | Low complexity, good candidate for replacement with COTS or SaaS |
| 3.1 - 5.0 | Rearchitect | Moderate complexity, suitable for cloud-native transformation |
| 5.1 - 7.0 | Refactor | High complexity with good modular structure, incremental modernization |
| 7.1 - 8.5 | Replatform | High complexity with significant technical debt, containerize first |
| 8.6 - 10.0 | Rehost | Extremely high complexity, lift-and-shift to minimize risk |

### Unisys ClearPath

| MCI Range | Recommended Approach | Technical Considerations |
|-----------|---------------------|-------------------------|
| 1.0 - 3.0 | Replace | Low complexity, good candidate for replacement with COTS or SaaS |
| 3.1 - 5.0 | Rearchitect | Moderate complexity, suitable for cloud-native transformation |
| 5.1 - 7.0 | Refactor | High complexity with good modular structure, evaluate ClearPath Forward migration |
| 7.1 - 8.5 | Replatform | High complexity, consider ClearPath MCP Software Series for cloud |
| 8.6 - 10.0 | Rehost | Extremely high complexity, leverage ClearPath Forward Hardware |

### Bull GCOS

| MCI Range | Recommended Approach | Technical Considerations |
|-----------|---------------------|-------------------------|
| 1.0 - 3.0 | Replace | Low complexity, good candidate for replacement with COTS or SaaS |
| 3.1 - 5.0 | Rearchitect | Moderate complexity, suitable for cloud-native transformation |
| 5.1 - 7.0 | Refactor | High complexity with good modular structure, consider LiberFactory migration |
| 7.1 - 8.5 | Replatform | High complexity, evaluate Migration+ toolset for emulation |
| 8.6 - 10.0 | Rehost | Extremely high complexity, hardware virtualization approach |

### NEC ACOS

| MCI Range | Recommended Approach | Technical Considerations |
|-----------|---------------------|-------------------------|
| 1.0 - 3.0 | Replace | Low complexity, good candidate for replacement with COTS or SaaS |
| 3.1 - 5.0 | Rearchitect | Moderate complexity, suitable for cloud-native transformation |
| 5.1 - 7.0 | Refactor | High complexity with good modular structure, consider iPackage integration |
| 7.1 - 8.5 | Replatform | High complexity, evaluate CASEWORLD/PE toolset for migration |
| 8.6 - 10.0 | Rehost | Extremely high complexity, ACOS-4/XVP PX implementation |

## Assessment Deliverables

The assessment process produces these technical deliverables:

1. **Platform-Specific Application Assessment Reports**
   - Application-level scores across all dimensions for each platform
   - Component-level detailed analysis with platform context
   - Technical risk profile and modernization impact
   - Cross-platform integration complexity assessment

2. **Platform-Aware Modernization Recommendations**
   - Recommended modernization approach per application with platform context
   - Technical justification for recommendations specific to each platform
   - Implementation risk assessment with platform considerations
   - Cross-platform modernization strategy

3. **Modernization Effort Estimation**
   - Estimated effort for recommended approach by platform
   - Technical skills requirements for each platform
   - Implementation timeline projections with platform dependencies
   - Cross-platform coordination effort estimates

4. **Modernization Roadmap**
   - Phased implementation recommendation across platforms
   - Technical dependencies and critical path with platform context
   - Risk mitigation strategies for platform-specific challenges
   - Cross-platform integration approach and sequencing

## Next Steps

After completing the assessment:
- Review [Dependency Mapping](02-dependency-mapping.md) results to validate findings
- Use assessment results to inform your [Modernization Strategy](../03-foundation/modernization-strategy.md)
- Start planning for your [Foundation Setup](../03-foundation/README.md) based on platform-specific assessment recommendations 