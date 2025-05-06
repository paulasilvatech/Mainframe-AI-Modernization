# Discovery and Assessment

## Overview

This chapter guides you through the process of systematically discovering, analyzing, and assessing your mainframe environment to develop a data-driven modernization strategy. Using Azure AI tools, you'll gain insights into your mainframe applications, data, and dependencies.

## Prerequisites

- Access to mainframe system documentation and application inventories
- Permission to analyze mainframe code and configurations
- Completed [Introduction to Mainframe Modernization](../01-introduction/README.md)
- Azure subscription for assessment tools

## Step 1: Inventory Collection Methodology

Begin by collecting a comprehensive inventory of your mainframe environment. This inventory will form the foundation of your assessment.

### Mainframe Components to Inventory

| Component | Description | Collection Approach |
|-----------|-------------|---------------------|
| Applications | Business applications running on the mainframe | Extract application listings from system catalogs |
| Programs | COBOL, PL/I, Assembler programs | Extract from source code management systems |
| JCL | Job Control Language procedures | Gather from procedure libraries |
| Data | Databases, files, and data structures | Extract metadata from system catalogs |
| Interfaces | APIs, file transfers, screen interfaces | Document from system configurations |
| Infrastructure | Hardware, system software, middleware | Collect from system documentation |

```bash
# Example script to extract COBOL program inventory
EXTRACT-PROG-LIST LANG=COBOL OUTPUT=COBOL.LIST
```

### Key Considerations

- Be thorough in your inventory collection - missing components can impact the assessment
- Document not just the "what" but also the "why" and "how" of existing systems
- Pay special attention to interfaces between systems

## Step 2: Azure AI Code Analysis

Azure OpenAI Service can help analyze and understand mainframe code, particularly for applications with limited documentation or tribal knowledge.

### Setting Up Azure OpenAI for COBOL Analysis

1. Create an Azure OpenAI Service resource
2. Configure appropriate permissions and access controls
3. Prepare your COBOL code for analysis:

```bash
# Example script to prepare COBOL files for Azure OpenAI analysis
cat << 'EOF' > prepare-cobol.sh
#!/bin/bash
mkdir -p cobol-analysis
find . -name "*.cbl" -o -name "*.cob" | while read file; do
  # Create a clean version with line numbers and stripped comments
  cat -n "$file" | sed 's/^\s*\*.\+$//' > "cobol-analysis/$(basename "$file").txt"
done
EOF

chmod +x prepare-cobol.sh
./prepare-cobol.sh
```

4. Use Azure OpenAI to analyze code structure, dependencies, and business rules:

```python
# Example Python code for Azure OpenAI COBOL analysis
import os
import openai
from azure.identity import DefaultAzureCredential
from azure.keyvault.secrets import SecretClient

# Set up Azure OpenAI
openai.api_type = "azure"
openai.api_base = os.getenv("AZURE_OPENAI_ENDPOINT")
openai.api_version = "2023-05-15"

# Get API key from Key Vault
credential = DefaultAzureCredential()
key_vault_url = os.getenv("KEY_VAULT_URL")
secret_client = SecretClient(vault_url=key_vault_url, credential=credential)
openai.api_key = secret_client.get_secret("azure-openai-key").value

def analyze_cobol_file(file_path):
    with open(file_path, 'r') as file:
        cobol_code = file.read()
    
    response = openai.ChatCompletion.create(
        engine="gpt-4",
        messages=[
            {"role": "system", "content": "You are a COBOL expert. Analyze the following COBOL program and identify its main functions, business rules, data structures, and external dependencies."},
            {"role": "user", "content": cobol_code}
        ],
        temperature=0.1,
        max_tokens=4000
    )
    
    return response.choices[0].message.content

# Process all prepared COBOL files
analysis_dir = "cobol-analysis"
results_dir = "cobol-analysis-results"
os.makedirs(results_dir, exist_ok=True)

for filename in os.listdir(analysis_dir):
    if filename.endswith(".txt"):
        file_path = os.path.join(analysis_dir, filename)
        print(f"Analyzing {filename}...")
        analysis = analyze_cobol_file(file_path)
        
        # Save the analysis results
        with open(os.path.join(results_dir, f"{filename}.analysis.md"), 'w') as f:
            f.write(analysis)
```

### Key Considerations

- Ensure proper data governance when analyzing sensitive code
- Remember that AI analysis is a starting point - human expertise is still valuable
- Focus on business rules and logic rather than just technical components

## Step 3: Dependency Mapping

Understanding dependencies between mainframe components is crucial for developing a modernization strategy that minimizes disruption.

### Types of Dependencies to Map

1. **Program-to-Program Dependencies**: Which programs call other programs
2. **Program-to-Data Dependencies**: Which programs access which data resources
3. **Job Flow Dependencies**: The sequence and relationships between jobs
4. **Interface Dependencies**: Dependencies on external systems

Use a combination of automated tools and manual analysis to create a comprehensive dependency map:

```plaintext
┌─────────────────┐     ┌─────────────────┐     ┌─────────────────┐
│                 │     │                 │     │                 │
│  Customer       │     │  Account        │     │  Transaction    │
│  Management     │────►│  Processing     │────►│  Handling       │
│  System         │     │  System         │     │  System         │
│                 │     │                 │     │                 │
└─────────────────┘     └─────────────────┘     └─────────────────┘
        │                       │                       │
        │                       │                       │
        ▼                       ▼                       ▼
┌─────────────────┐     ┌─────────────────┐     ┌─────────────────┐
│                 │     │                 │     │                 │
│  Customer       │     │  Account        │     │  Transaction    │
│  Database       │     │  Database       │     │  Log            │
│                 │     │                 │     │                 │
└─────────────────┘     └─────────────────┘     └─────────────────┘
```

### Key Considerations

- Pay special attention to implicit dependencies that may not be documented
- Identify critical paths and high-risk dependencies
- Document data flow as well as control flow

## Step 4: Business Rule Extraction

Business rules embedded in mainframe code represent valuable intellectual property that must be preserved during modernization.

Azure OpenAI can help extract and document these rules:

```python
# Example Python code for business rule extraction
def extract_business_rules(file_path):
    with open(file_path, 'r') as file:
        cobol_code = file.read()
    
    response = openai.ChatCompletion.create(
        engine="gpt-4",
        messages=[
            {"role": "system", "content": "You are a COBOL expert specialized in identifying business rules. For the given COBOL program, extract all business rules in a structured format. A business rule is a specific calculation, validation, condition, or process that implements a business policy or requirement."},
            {"role": "user", "content": cobol_code}
        ],
        temperature=0.1,
        max_tokens=4000
    )
    
    return response.choices[0].message.content
```

Document the extracted rules in a business rule catalog:

| Rule ID | Description | Source Component | Complexity | Criticality |
|---------|-------------|------------------|------------|-------------|
| BR001 | Calculate customer credit score based on payment history | CUSTCRED.CBL | Medium | High |
| BR002 | Apply 5% discount for orders over $1000 | ORDERPROC.CBL | Low | Medium |
| BR003 | Validate account numbers using mod-10 algorithm | ACCTVALID.CBL | Medium | High |

### Key Considerations

- Validate extracted business rules with subject matter experts
- Prioritize rules based on business value and complexity
- Document context and rationale for business rules

## Step 5: Modernization Readiness Assessment

Based on your inventory, code analysis, and dependency mapping, assess the readiness of each component for modernization.

### Sample Assessment Framework

| Criterion | Weight | Scoring Guidelines |
|-----------|--------|-------------------|
| Business Value | 25% | 1: Low value to business<br>5: Critical to business operations |
| Technical Complexity | 20% | 1: Simple, well-structured<br>5: Complex, poorly documented |
| Dependency Risk | 20% | 1: Few dependencies<br>5: Many complex dependencies |
| Modernization Benefit | 20% | 1: Limited benefit from modernization<br>5: Significant benefit |
| Resource Requirements | 15% | 1: Minimal resources required<br>5: Substantial resources required |

Apply this framework to each application or component to develop a modernization readiness score:

```python
# Example Python function to calculate modernization readiness
def calculate_readiness_score(business_value, technical_complexity, 
                             dependency_risk, modernization_benefit, resource_requirements):
    score = (business_value * 0.25 + 
             (6 - technical_complexity) * 0.20 +  # Invert scale for complexity
             (6 - dependency_risk) * 0.20 +       # Invert scale for risk
             modernization_benefit * 0.20 + 
             (6 - resource_requirements) * 0.15)  # Invert scale for resources
    
    return score * 20  # Scale to 0-100
```

### Key Considerations

- Involve both business and technical stakeholders in the assessment
- Consider both quantitative and qualitative factors
- Use the assessment to prioritize modernization efforts

## Step 6: Prioritization Framework

With the assessment complete, develop a prioritization framework to sequence your modernization efforts:

| Quadrant | Characteristics | Modernization Approach |
|----------|-----------------|------------------------|
| High Value, Low Complexity | - Critical business functions<br>- Relatively simple to modernize<br>- Limited dependencies | Prioritize for early modernization, using API-first approach |
| High Value, High Complexity | - Critical business functions<br>- Complex logic or many dependencies<br>- High risk if disrupted | Careful phased modernization with comprehensive testing |
| Low Value, Low Complexity | - Less critical to business<br>- Simple implementation<br>- Few dependencies | Consider quick modernization when resources are available |
| Low Value, High Complexity | - Limited business value<br>- Complex implementation<br>- Potentially high modernization cost | Consider replacing, retiring, or deferring modernization |

Use this quadrant approach to develop a modernization roadmap that balances business value, technical feasibility, and risk:

```plaintext
       ┌─────────────────────┬─────────────────────┐
       │                     │                     │
       │  QUICK WINS         │  STRATEGIC PROJECTS │
       │                     │                     │
       │  - High Value       │  - High Value       │
High   │  - Low Complexity   │  - High Complexity  │
Value  │                     │                     │
       │  Approach:          │  Approach:          │
       │  API-First          │  Phased Hybrid      │
       │                     │                     │
       ├─────────────────────┼─────────────────────┤
       │                     │                     │
       │  FILL-INS           │  LAST PRIORITY      │
       │                     │                     │
       │  - Low Value        │  - Low Value        │
Low    │  - Low Complexity   │  - High Complexity  │
Value  │                     │                     │
       │  Approach:          │  Approach:          │
       │  Opportunistic      │  Retire or Retain   │
       │                     │                     │
       └─────────────────────┴─────────────────────┘
              Low Complexity        High Complexity
```

### Key Considerations

- Balance short-term wins with long-term strategic goals
- Consider resource constraints and dependencies between components
- Reevaluate prioritization as you progress through modernization

## Troubleshooting

| Issue | Cause | Solution |
|-------|-------|----------|
| Incomplete or inaccurate inventory | Limited access to systems or documentation | Conduct interviews with system owners and operators to fill gaps |
| AI analysis produces limited insights | Poor quality source code or unusual coding patterns | Supplement AI analysis with manual code reviews and SME interviews |
| Conflicting prioritization inputs | Different stakeholders have different priorities | Use a weighted scoring system that incorporates multiple perspectives |

## Next Steps

Now that you've completed your discovery and assessment, you can proceed to:

1. [Infrastructure Setup](../03-infrastructure/README.md): Set up the foundational infrastructure for your modernization journey
2. [Hybrid Development Implementation](../04-hybrid-development/README.md): Configure your development environment for hybrid mainframe-cloud development

## Additional Resources

- [Azure OpenAI Documentation](https://learn.microsoft.com/en-us/azure/cognitive-services/openai/)
- [Mainframe Assessment Tools](https://learn.microsoft.com/en-us/azure/architecture/reference-architectures/migration/mainframe-migration-assessment-tools)
- [Example Dependency Mapping Code](../../code/risk-assessment/dependency-mapping.py) 