# Mainframe Dependency Mapping Tool

This tool analyzes IBM z/OS mainframe applications to identify dependencies between components such as COBOL programs, JCL jobs, copybooks, DB2 tables, CICS transactions, and more. It produces a dependency graph that can be used for impact analysis, modernization planning, and risk assessment.

## Features

- Identifies dependencies between mainframe components
- Supports multiple mainframe artifacts (COBOL, JCL, PL/I, Copybooks, etc.)
- Detects various dependency types (CALL, COPY, EXEC, etc.)
- Creates visual dependency graphs
- Performs impact analysis to identify affected components
- Identifies critical components based on dependency analysis
- Generates comprehensive reports in multiple formats

## Requirements

- Python 3.8 or higher
- Dependencies listed in `requirements.txt`

## Installation

1. Ensure Python 3.8+ is installed
2. Install dependencies:

```bash
pip install -r requirements.txt
```

## Usage

Basic usage to analyze a mainframe codebase:

```bash
python map_dependencies.py --source-dir /path/to/mainframe/code --output dependency-map.json
```

### Command-line Arguments

- `--source-dir`, `-s`: Directory containing mainframe source code (required)
- `--output`, `-o`: Output file for dependency map (default: dependency-map.json)
- `--exclude`, `-e`: Glob patterns to exclude (can be used multiple times)
- `--format`, `-f`: Output format [json, csv, graphml, png, pdf, svg] (default: json)
- `--component`, `-c`: Analyze impact for a specific component
- `--verbose`, `-v`: Enable verbose logging
- `--report`, `-r`: Generate summary report

### Examples

Generate a JSON dependency map:

```bash
python map_dependencies.py -s ./src/mainframe -o dependencies.json
```

Create a visual graph in PNG format:

```bash
python map_dependencies.py -s ./src/mainframe -o dependencies.png -f png
```

Generate a CSV dependency map:

```bash
python map_dependencies.py -s ./src/mainframe -o dependencies.csv -f csv
```

Analyze impact of changes to a specific component:

```bash
python map_dependencies.py -s ./src/mainframe -c CUSTMGMT
```

Generate a comprehensive report:

```bash
python map_dependencies.py -s ./src/mainframe -r
```

## Output Files

The tool can generate various output files:

- **JSON**: Complete dependency information including component details and dependencies
- **CSV**: Two CSV files containing component information and dependency relationships
- **GraphML**: Graph format that can be imported into visualization tools
- **PNG/PDF/SVG**: Visual representation of the dependency graph
- **TXT Report**: Text report summarizing dependency analysis findings

## Integration with Modernization Workflows

This dependency mapping tool is designed to integrate with the Azure AI Foundry mainframe modernization workflow:

1. **Discovery Phase**: Map dependencies to understand application structure
2. **Risk Assessment**: Identify critical components and dependencies for risk evaluation
3. **Planning**: Use dependency insights to plan phased modernization
4. **Development**: Incorporate dependency awareness into code transformations
5. **Testing**: Focus testing efforts on impacted components
6. **Deployment**: Make informed deployment decisions based on component dependencies

## Examples of Insights

The tool provides valuable insights such as:

- Which components have the most dependencies
- Which components would be impacted by changes to a specific module
- What external systems a component depends on
- The complexity of a component's dependency network
- Historical patterns in component development and maintenance 