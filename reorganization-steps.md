# Implementation Steps for Repository Reorganization

This document outlines the step-by-step process to reorganize the repository structure according to the enhanced Azure AI Foundry Mainframe Modernization Playbook structure.

## Preparation

1. Create a backup branch of the current repository structure
   ```bash
   git checkout -b backup-current-structure
   git push origin backup-current-structure
   git checkout main
   ```

2. Create a new working branch for the reorganization
   ```bash
   git checkout -b reorganize-structure
   ```

## Directory Restructuring

### 1. Create the new directory structure

```bash
# Create documentation directories
mkdir -p docs/{01-introduction,02-discovery,03-foundation,04-development-environment,05-code-analysis,06-github-integration,07-azure-devops-integration,08-ai-transformation,09-cicd-implementation,10-risk-management,11-hybrid-operations,12-case-studies}

# Create subdirectories in each documentation chapter
for dir in docs/*/; do
  mkdir -p "$dir"
done

# Create code directories
mkdir -p code/github/{workflows,templates,hooks,scripts}
mkdir -p code/azure-devops/{pipelines,templates,extensions,scripts}
mkdir -p code/ai-foundry/{analysis,transformation,knowledge,monitoring}
mkdir -p code/mainframe/{cobol,pl1,jcl,db2,cics}
mkdir -p code/hybrid/{api-integration,data-integration,deployment,monitoring}

# Create template directories
mkdir -p templates/gitattributes
mkdir -p templates/workflows/{discovery,build,test,deploy}
mkdir -p templates/pipelines/{discovery,build,test,deploy}
mkdir -p templates/deployment

# Create image directories
mkdir -p images/{architecture,implementation,workflows,screenshots}
```

### 2. Migrate existing content

1. For each existing file, identify the appropriate location in the new structure
2. Move files to their new locations, preserving content and history:

```bash
# Example: Move introduction files
git mv docs/01-introduction/README.md docs/01-introduction/README.md
git mv docs/01-introduction/01-overview.md docs/01-introduction/01-overview.md
git mv docs/01-introduction/02-architecture.md docs/01-introduction/02-architecture.md

# Continue with similar commands for all existing files
```

3. For directories with conflicting numbering or names:
   - Identify the content focus first
   - Move to the appropriate chapter based on content, not just numbering
   - Update internal references and links

### 3. Create README files for new directories

Create README.md files for all new directories that don't have them:

```bash
# Example for a directory without a README
echo "# AI-Powered Transformation\n\nThis chapter covers AI-powered transformation strategies for IBM z/OS mainframe applications using Azure AI Foundry." > docs/08-ai-transformation/README.md
```

### 4. Update cross-references

1. Update all internal links in documentation to reflect the new structure
2. Check and update all relative paths in code examples
3. Ensure all links in the main README.md are correct

## Content Updates

### 1. Update chapter titles and descriptions

1. Review and update the title and description in each chapter's README.md
2. Ensure alignment with the enhanced focus on Azure AI Foundry for IBM z/OS

### 2. Update content for equal coverage

1. Review GitHub Integration and Azure DevOps Integration chapters
2. Ensure both have equivalent depth and coverage
3. Update any examples to provide parallel implementation approaches

### 3. Enhance AI Foundry focus

1. Update content to emphasize AI capabilities throughout
2. Add IBM z/OS specific details to all relevant sections
3. Ensure terminology is consistent with IBM z/OS environments

## Testing and Validation

1. Verify all links work correctly in the new structure
2. Validate that all code examples run successfully
3. Check that all images display correctly
4. Test navigation flow through the chapters

## Finalization

1. Commit all changes with descriptive commit messages
   ```bash
   git add .
   git commit -m "Reorganize repository structure for enhanced Azure AI Foundry for IBM z/OS focus"
   ```

2. Create a pull request for review
3. After approval, merge to main branch
4. Update any external references to the repository structure

## Post-Reorganization Tasks

1. Update documentation index pages
2. Enhance chapter introduction pages
3. Add new content aligned with the enhanced structure
4. Consider implementing GitHub Pages for better documentation browsing

This reorganization maintains existing content while enhancing the structure to better reflect the focus on Azure AI Foundry for IBM z/OS with equal coverage for GitHub and Azure DevOps integration paths. 