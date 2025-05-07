#!/bin/bash
# Mainframe Dependency Mapping CI/CD Integration Script
# This script shows how to integrate the dependency mapping tool into CI/CD pipelines

set -e  # Exit on error

# Default settings
SOURCE_DIR="./mainframe"
OUTPUT_DIR="./dependency-analysis"
COMPONENT_NAME=""
GENERATE_REPORT=true
FORMAT="json"
INTEGRATION_TYPE="github"  # "github" or "azure"
ANALYZE_CHANGES_ONLY=false
CHANGES_SINCE="HEAD~1"

# Parse command-line arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    --source-dir)
      SOURCE_DIR="$2"
      shift 2
      ;;
    --output-dir)
      OUTPUT_DIR="$2"
      shift 2
      ;;
    --component)
      COMPONENT_NAME="$2"
      shift 2
      ;;
    --format)
      FORMAT="$2"
      shift 2
      ;;
    --integration)
      INTEGRATION_TYPE="$2"
      shift 2
      ;;
    --changes-only)
      ANALYZE_CHANGES_ONLY=true
      shift
      ;;
    --changes-since)
      CHANGES_SINCE="$2"
      shift 2
      ;;
    --help)
      echo "Usage: $0 [options]"
      echo "Options:"
      echo "  --source-dir DIR       Source directory containing mainframe code"
      echo "  --output-dir DIR       Output directory for dependency analysis"
      echo "  --component NAME       Focus analysis on a specific component"
      echo "  --format FORMAT        Output format (json, csv, png, pdf, svg)"
      echo "  --integration TYPE     CI/CD integration type (github, azure)"
      echo "  --changes-only         Only analyze changed files"
      echo "  --changes-since REF    Analyze changes since this Git reference"
      echo "  --help                 Show this help message"
      exit 0
      ;;
    *)
      echo "Unknown option: $1"
      exit 1
      ;;
  esac
done

# Create output directory if it doesn't exist
mkdir -p "$OUTPUT_DIR"

# Install required dependencies
echo "Installing dependencies..."
pip install -r "$(dirname "$0")/requirements.txt"

# If analyzing changes only, create a list of changed mainframe files
if [ "$ANALYZE_CHANGES_ONLY" = true ]; then
  echo "Identifying changed mainframe files since $CHANGES_SINCE..."
  
  CHANGED_FILES_DIR="${OUTPUT_DIR}/changed_files"
  mkdir -p "$CHANGED_FILES_DIR"
  
  # Get list of changed files with mainframe extensions
  git diff --name-only "$CHANGES_SINCE" | grep -E '\.(cbl|cob|jcl|pli|pl1|cpy|asm|s|proc|inc)$' > "${CHANGED_FILES_DIR}/changed_files.txt"
  
  # Copy changed files to temporary directory for analysis
  if [ -s "${CHANGED_FILES_DIR}/changed_files.txt" ]; then
    echo "Found changed mainframe files, copying to temporary directory..."
    while IFS= read -r file; do
      # Create directory structure
      mkdir -p "${CHANGED_FILES_DIR}/$(dirname "$file")"
      # Copy file
      cp "$file" "${CHANGED_FILES_DIR}/$file"
    done < "${CHANGED_FILES_DIR}/changed_files.txt"
    
    # Update source directory to only analyze changed files
    SOURCE_DIR="$CHANGED_FILES_DIR"
  else
    echo "No mainframe files changed, using full source directory."
  fi
fi

# Run dependency mapping
echo "Analyzing dependencies in $SOURCE_DIR..."

# Base command
CMD="python $(dirname "$0")/map_dependencies.py -s \"$SOURCE_DIR\" -o \"$OUTPUT_DIR/dependency-map.$FORMAT\" -f \"$FORMAT\" --verbose"

# Add component analysis if specified
if [ -n "$COMPONENT_NAME" ]; then
  CMD="$CMD -c \"$COMPONENT_NAME\""
fi

# Add report generation
if [ "$GENERATE_REPORT" = true ]; then
  CMD="$CMD -r"
fi

# Execute command
echo "Running: $CMD"
eval "$CMD"

# Additional CI/CD specific integrations
if [ "$INTEGRATION_TYPE" = "github" ]; then
  # GitHub Actions specific integration
  echo "Integrating with GitHub Actions..."
  
  # Create GitHub Actions annotations for critical components
  if [ -f "$OUTPUT_DIR/dependency-map_report.txt" ]; then
    echo "Generating GitHub Actions annotations..."
    grep -A 10 "CRITICAL COMPONENTS" "$OUTPUT_DIR/dependency-map_report.txt" | 
    grep -E '^\s+[A-Z0-9]+:' |
    while IFS=':' read -r component deps; do
      echo "::warning file=$component::Critical component with $deps"
    done
  fi
  
  # Output dependency info as GitHub Actions output variable
  if [ -f "$OUTPUT_DIR/dependency-map.json" ]; then
    echo "Storing dependency count as output variable..."
    DEP_COUNT=$(jq '.metadata.dependency_count' "$OUTPUT_DIR/dependency-map.json")
    echo "::set-output name=dependency_count::$DEP_COUNT"
  fi
  
elif [ "$INTEGRATION_TYPE" = "azure" ]; then
  # Azure DevOps specific integration
  echo "Integrating with Azure DevOps..."
  
  # Create Azure DevOps warning annotations for critical components
  if [ -f "$OUTPUT_DIR/dependency-map_report.txt" ]; then
    echo "Generating Azure DevOps warnings..."
    grep -A 10 "CRITICAL COMPONENTS" "$OUTPUT_DIR/dependency-map_report.txt" | 
    grep -E '^\s+[A-Z0-9]+:' |
    while IFS=':' read -r component deps; do
      echo "##vso[task.logissue type=warning;]Critical component: $component with $deps"
    done
  fi
  
  # Upload artifacts to Azure DevOps
  echo "##vso[artifact.upload containerfolder=dependency-analysis;artifactname=Dependency Analysis]$OUTPUT_DIR"
  
  # Set Azure DevOps variables
  if [ -f "$OUTPUT_DIR/dependency-map.json" ]; then
    DEP_COUNT=$(jq '.metadata.dependency_count' "$OUTPUT_DIR/dependency-map.json")
    echo "##vso[task.setvariable variable=dependencyCount;isOutput=true]$DEP_COUNT"
  fi
fi

# Output summary
echo "Dependency analysis complete."
echo "Output files available in $OUTPUT_DIR:"
ls -la "$OUTPUT_DIR"

if [ -f "$OUTPUT_DIR/dependency-map_report.txt" ]; then
  echo "Summary Report:"
  head -n 20 "$OUTPUT_DIR/dependency-map_report.txt"
  echo "..."
fi

exit 0 