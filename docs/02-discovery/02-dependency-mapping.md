# Mainframe Dependency Mapping

This guide explains how to use the dependency mapping tool to analyze IBM z/OS mainframe applications and identify relationships between components for modernization planning.

## Overview

Dependency mapping is a critical step in the mainframe modernization process. It helps identify relationships between various components such as COBOL programs, JCL jobs, copybooks, DB2 tables, and CICS transactions. Understanding these dependencies is essential for:

- Impact analysis during code changes
- Phased modernization planning
- Risk assessment and mitigation
- Test planning and prioritization
- Deployment strategies

The Azure AI Foundry enhanced dependency mapping tool uses advanced analysis techniques to identify both explicit and implicit dependencies in mainframe code, creating a comprehensive dependency graph.

## Prerequisites

- Access to mainframe source code (COBOL, JCL, PL/I, Copybooks, etc.)
- Python 3.8 or higher installed
- Required Python packages (see requirements.txt)
- Basic understanding of mainframe components and their relationships

## How to Run Dependency Mapping

### Installation

1. Navigate to the dependency mapping tool directory:

```bash
cd code/dependency-mapping
```

2. Install required dependencies:

```bash
pip install -r requirements.txt
```

### Basic Usage

Run the dependency mapping tool on your mainframe source code:

```bash
python map_dependencies.py --source-dir /path/to/mainframe/code --output dependency-map.json
```

### Advanced Options

The tool provides several options for customizing the dependency analysis:

```bash
python map_dependencies.py \
  --source-dir /path/to/mainframe/code \
  --output dependency-map.json \
  --format json \
  --exclude "test/*" \
  --exclude "archive/*" \
  --report \
  --verbose
```

### CI/CD Integration

For automated dependency analysis in CI/CD pipelines, use the provided integration script:

```bash
./ci_integration.sh \
  --source-dir /path/to/mainframe/code \
  --output-dir /path/to/output \
  --integration github \
  --changes-only
```

## Analyzing Dependency Results

### Understanding the Dependency Graph

The dependency graph represents relationships between mainframe components:

- **Nodes**: Individual components (programs, copybooks, JCL jobs, etc.)
- **Edges**: Dependencies between components (calls, includes, executes, etc.)
- **Direction**: Source component depends on target component

### Dependency Types

The tool identifies various types of dependencies:

| Dependency Type | Description | Example |
|----------------|-------------|---------|
| Calls | Program calls another program | `CALL 'SUBPGM'` |
| Includes | Program includes a copybook | `COPY CUSTCPY` |
| Executes | JCL job executes a program | `//STEP1 EXEC PGM=MAINPGM` |
| Reads | Program reads from a data source | `SELECT * FROM CUSTTBL` |
| Writes | Program writes to a data source | `INSERT INTO ORDERTBL` |
| Uses | Component uses another component | JCL procedure usage |
| References | Component references another | Dataset references |

### Critical Path Analysis

Identify the most critical components in your application based on dependency centrality measures:

```bash
python map_dependencies.py -s /path/to/mainframe/code --report
```

The report will identify components with high dependency counts, high centrality, or other risk factors.

### Impact Analysis

To analyze the impact of changes to a specific component:

```bash
python map_dependencies.py -s /path/to/mainframe/code -c CUSTMGMT
```

This will show:
- Components that would be impacted by changes to the specified component
- Components that the specified component depends on

## Integration with AI-Powered Risk Assessment

The dependency mapping results can be integrated with the AI-powered risk assessment tool:

```bash
python ../risk-assessment/assess_risk.py \
  --source-dir /path/to/mainframe/code \
  --dependency-map dependency-map.json \
  --profile financial-core \
  --output risk-assessment.json
```

This integration enables:
- Risk assessment based on dependency complexity
- Identification of critical components in the dependency chain
- Impact-based risk scoring
- More accurate deployment strategy recommendations

## Visualization Options

The tool can generate visual dependency graphs in various formats:

```bash
python map_dependencies.py -s /path/to/mainframe/code -o dependency-graph.png -f png
```

Supported formats include:
- PNG, PDF, SVG for visual representations
- JSON, CSV for data analysis
- GraphML for import into specialized graph visualization tools

## Best Practices

1. **Start Small**: Begin with a manageable subset of your mainframe application
2. **Focus on High-Value Areas**: Prioritize business-critical components
3. **Validate Results**: Verify dependencies with domain experts
4. **Iterative Analysis**: Run multiple analyses as you refine your understanding
5. **Integrate with Modernization Planning**: Use dependency insights to guide modernization phases
6. **Track Changes Over Time**: Run dependency analysis regularly to monitor evolution

## Troubleshooting

| Issue | Resolution |
|-------|------------|
| Missing dependencies | Ensure all code artifacts are available in the source directory |
| Unexpected dependencies | Review code for dynamic calls or conditional execution paths |
| Memory errors | For large codebases, run on a more powerful machine or analyze in smaller parts |
| Visualization issues | For complex graphs, use filtering or export to specialized graph tools |

## Next Steps

After completing dependency mapping:

1. Review the dependency graph and critical component analysis
2. Incorporate findings into your modernization strategy
3. Develop a phased approach based on dependency clusters
4. Implement appropriate risk mitigation strategies
5. Set up continuous dependency analysis in your CI/CD pipeline

## References

- [Mainframe Dependency Mapping Tool README](../code/dependency-mapping/README.md)
- [Risk Assessment Integration](../10-risk-management/README.md)
- [Modernization Planning Guide](../03-foundation/modernization-strategy.md) 