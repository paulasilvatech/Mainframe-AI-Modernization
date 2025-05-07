# AI-Powered Assessment Criteria

This document details the technical assessment criteria used by Azure AI Foundry to evaluate IBM z/OS mainframe applications for modernization.

## Assessment Framework

Azure AI Foundry uses a multi-dimensional assessment framework to evaluate mainframe applications across technical, operational, and business dimensions.

![Assessment Framework](../../images/assessment-framework.png)

The framework employs these key technical components:

1. **Code Analysis Engine**
   - Deep lexical and syntactic parsing of COBOL, PL/I, and Assembler
   - Static analysis of control flow and data dependencies
   - Pattern recognition for common architectural patterns
   - Technical debt quantification

2. **Operational Analytics**
   - Performance metrics evaluation from SMF records
   - Resource consumption analysis (CPU, memory, I/O)
   - Batch window impact assessment
   - Transaction volume and response time analysis

3. **Business Impact Analyzer**
   - Business criticality assessment
   - Revenue and cost impact modeling
   - Regulatory and compliance implications
   - Strategic alignment evaluation

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

## Complexity Scoring Algorithm

The assessment uses this algorithm to calculate the Modernization Complexity Index (MCI):

```
MCI = (CCW × CC) + (TDW × TD) + (DW × D) + (PW × P) + (AW × A) + (DCW × DC) + (DOW × DO) + (MHW × MH)

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
   ```

2. **Run Initial Assessment**
   ```bash
   # Execute Assessment
   az ai-foundry assessment run --inventory-id <inventory-id> --output-format json
   
   # Generate Assessment Report
   az ai-foundry assessment report --assessment-id <assessment-id> --format html
   ```

3. **Calibrate Assessment**
   ```bash
   # Calibrate Results with Expert Feedback
   az ai-foundry assessment calibrate --assessment-id <assessment-id> --calibration-file expert-feedback.json
   
   # Re-run Assessment with Calibration
   az ai-foundry assessment run --inventory-id <inventory-id> --calibration-id <calibration-id> --output-format json
   ```

## Modernization Approach Recommendation

Based on the assessment scores, the AI engine recommends modernization approaches:

| MCI Range | Recommended Approach | Technical Considerations |
|-----------|---------------------|-------------------------|
| 1.0 - 3.0 | Replace | Low complexity, good candidate for replacement with COTS or SaaS |
| 3.1 - 5.0 | Rearchitect | Moderate complexity, suitable for cloud-native transformation |
| 5.1 - 7.0 | Refactor | High complexity with good modular structure, incremental modernization |
| 7.1 - 8.5 | Replatform | High complexity with significant technical debt, containerize first |
| 8.6 - 10.0 | Rehost | Extremely high complexity, lift-and-shift to minimize risk |

## Assessment Deliverables

The assessment process produces these technical deliverables:

1. **Application Assessment Report**
   - Application-level scores across all dimensions
   - Component-level detailed analysis
   - Technical risk profile and modernization impact

2. **Modernization Recommendation**
   - Recommended modernization approach per application
   - Technical justification for recommendations
   - Implementation risk assessment

3. **Modernization Effort Estimation**
   - Estimated effort for recommended approach
   - Technical skills requirements
   - Implementation timeline projections

4. **Modernization Roadmap**
   - Phased implementation recommendation
   - Technical dependencies and critical path
   - Risk mitigation strategies

## Next Steps

After completing the assessment:
- Review [Dependency Mapping](02-dependency-mapping.md) results to validate findings
- Use assessment results to inform your [Modernization Strategy](../03-foundation/modernization-strategy.md)
- Start planning for your [Foundation Setup](../03-foundation/README.md) based on assessment recommendations 