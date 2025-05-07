#!/usr/bin/env python3
"""
Azure AI Foundry - Mainframe Dependency Mapping Tool

This script analyzes mainframe applications to identify dependencies between components
such as COBOL programs, JCL jobs, copybooks, DB2 tables, CICS transactions, and more.
It produces a dependency graph that can be used for impact analysis, modernization planning,
and risk assessment.

Usage:
    python map_dependencies.py --source-dir <directory> --output <file> [options]

Requirements:
    - Python 3.8+
    - See requirements.txt for dependencies
"""

import argparse
import csv
import json
import logging
import os
import re
import sys
from collections import defaultdict
from dataclasses import dataclass
from datetime import datetime
from enum import Enum
from pathlib import Path
from typing import Dict, List, Optional, Set, Tuple, Union

import networkx as nx
import matplotlib.pyplot as plt
from rich.console import Console
from rich.progress import Progress

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger("dependency-mapper")
console = Console()

class ComponentType(str, Enum):
    """Types of mainframe components that can be mapped."""
    COBOL_PROGRAM = "COBOL Program"
    COPYBOOK = "Copybook"
    JCL_JOB = "JCL Job"
    JCL_PROC = "JCL Procedure"
    DB2_TABLE = "DB2 Table"
    CICS_TRANSACTION = "CICS Transaction"
    IMS_DATABASE = "IMS Database"
    IMS_TRANSACTION = "IMS Transaction"
    VSAM_FILE = "VSAM File"
    ASSEMBLER_PROGRAM = "Assembler Program"
    PL1_PROGRAM = "PL/I Program"
    INCLUDE = "Include File"
    SYSTEM_LIBRARY = "System Library"

class DependencyType(str, Enum):
    """Types of dependencies between components."""
    CALLS = "Calls"
    INCLUDES = "Includes"
    EXECUTES = "Executes"
    READS = "Reads"
    WRITES = "Writes"
    USES = "Uses"
    REFERENCES = "References"
    INHERITS = "Inherits"
    TRIGGERS = "Triggers"
    DEPENDS_ON = "Depends On"

@dataclass
class Component:
    """Represents a component in the mainframe application."""
    name: str
    type: ComponentType
    path: str
    size: int = 0
    lines: int = 0
    modified_date: Optional[datetime] = None
    attributes: Dict = None
    
    def __post_init__(self):
        if self.attributes is None:
            self.attributes = {}

@dataclass
class Dependency:
    """Represents a dependency between two components."""
    source: str
    target: str
    type: DependencyType
    count: int = 1
    locations: List[Tuple[str, int]] = None
    
    def __post_init__(self):
        if self.locations is None:
            self.locations = []

class DependencyMapper:
    """Main class for mapping dependencies in mainframe applications."""
    
    def __init__(self, source_dir: str, exclusion_patterns: List[str] = None):
        """Initialize the dependency mapper.
        
        Args:
            source_dir: Directory containing mainframe source code
            exclusion_patterns: List of glob patterns to exclude
        """
        self.source_dir = os.path.abspath(source_dir)
        self.exclusion_patterns = exclusion_patterns or []
        self.components: Dict[str, Component] = {}
        self.dependencies: List[Dependency] = []
        self.graph = nx.DiGraph()
        
        # Regular expressions for analysis
        self._init_regex_patterns()
        
        logger.info(f"Initialized dependency mapper for {source_dir}")
    
    def _init_regex_patterns(self):
        """Initialize regular expression patterns for code analysis."""
        # COBOL patterns
        self.call_pattern = re.compile(r'\bCALL\s+[\'"]?([A-Za-z0-9#@$-]+)[\'"]?', re.IGNORECASE)
        self.copy_pattern = re.compile(r'\bCOPY\s+([A-Za-z0-9#@$-]+)', re.IGNORECASE)
        self.exec_cics_pattern = re.compile(r'\bEXEC\s+CICS\s+([A-Za-z0-9\s]+)\b', re.IGNORECASE)
        self.exec_sql_pattern = re.compile(r'\bEXEC\s+SQL\b(.*?);', re.IGNORECASE | re.DOTALL)
        self.exec_sql_select_pattern = re.compile(r'\bSELECT\b.*?\bFROM\b\s+([A-Za-z0-9#@$_.]+)', re.IGNORECASE)
        self.exec_sql_insert_pattern = re.compile(r'\bINSERT\s+INTO\s+([A-Za-z0-9#@$_.]+)', re.IGNORECASE)
        self.exec_sql_update_pattern = re.compile(r'\bUPDATE\s+([A-Za-z0-9#@$_.]+)', re.IGNORECASE)
        self.exec_sql_delete_pattern = re.compile(r'\bDELETE\s+FROM\s+([A-Za-z0-9#@$_.]+)', re.IGNORECASE)
        
        # JCL patterns
        self.jcl_exec_pattern = re.compile(r'//\s*[A-Za-z0-9#@$]+\s+EXEC\s+(?:PGM=([A-Za-z0-9#@$]+)|PROC=([A-Za-z0-9#@$]+))', re.IGNORECASE)
        self.jcl_include_pattern = re.compile(r'//\s*INCLUDE\s+MEMBER=([A-Za-z0-9#@$]+)', re.IGNORECASE)
        self.jcl_dd_dsn_pattern = re.compile(r'//\s*[A-Za-z0-9#@$]+\s+DD\s+DSN=([A-Za-z0-9#@$\\.()]+)', re.IGNORECASE)
        
        # PL/I patterns
        self.pli_include_pattern = re.compile(r'\bINCLUDE\s+([A-Za-z0-9#@$]+)\s*;', re.IGNORECASE)
        self.pli_call_pattern = re.compile(r'\bCALL\s+([A-Za-z0-9#@$]+)\s*[;(]', re.IGNORECASE)
    
    def map_dependencies(self) -> Tuple[Dict[str, Component], List[Dependency]]:
        """Map all dependencies in the source directory.
        
        Returns:
            Tuple containing component dictionary and dependency list
        """
        console.print("[bold blue]Starting dependency mapping...[/bold blue]")
        
        # Find all relevant files
        files = self._find_files()
        
        # Map components
        with Progress() as progress:
            task = progress.add_task("[green]Identifying components...", total=len(files))
            
            for file_path in files:
                self._process_file(file_path)
                progress.update(task, advance=1)
        
        # Build dependency graph
        self._build_graph()
        
        console.print(f"[bold green]Mapping complete![/bold green] Found {len(self.components)} components and {len(self.dependencies)} dependencies.")
        
        return self.components, self.dependencies
    
    def _find_files(self) -> List[str]:
        """Find all relevant files in the source directory."""
        extensions = {
            ".cbl", ".cob", ".cobol",  # COBOL
            ".jcl",                    # JCL
            ".pli", ".pl1",            # PL/I
            ".cpy",                    # Copybooks
            ".asm", ".s",              # Assembler
            ".inc", ".incl",           # Include files
            ".proc",                   # Procedures
        }
        
        files = []
        for root, _, filenames in os.walk(self.source_dir):
            for filename in filenames:
                file_path = os.path.join(root, filename)
                
                # Skip excluded patterns
                if any(re.match(pattern, file_path) for pattern in self.exclusion_patterns):
                    continue
                
                # Include files with known extensions
                ext = os.path.splitext(filename)[1].lower()
                if ext in extensions:
                    files.append(file_path)
                # Include files without extensions that might be JCL or other mainframe artifacts
                elif '.' not in filename and self._is_text_file(file_path):
                    files.append(file_path)
        
        return files
    
    def _is_text_file(self, file_path: str) -> bool:
        """Check if a file is a text file by reading a sample."""
        try:
            with open(file_path, 'rb') as f:
                sample = f.read(1024)
                return not bool(sample.translate(None, bytes(range(32, 127)) + b'\r\n\t'))
        except Exception:
            return False
    
    def _process_file(self, file_path: str):
        """Process a single file to identify components and dependencies."""
        rel_path = os.path.relpath(file_path, self.source_dir)
        filename = os.path.basename(file_path)
        name, ext = os.path.splitext(filename)
        ext = ext.lower()
        
        file_size = os.path.getsize(file_path)
        modified_time = datetime.fromtimestamp(os.path.getmtime(file_path))
        
        try:
            with open(file_path, 'r', encoding='utf-8', errors='replace') as f:
                content = f.read()
                line_count = len(content.splitlines())
        except Exception as e:
            logger.warning(f"Could not read {file_path}: {str(e)}")
            return
        
        # Identify component type
        component_type = self._identify_component_type(file_path, ext, content)
        
        # Create component
        component = Component(
            name=name,
            type=component_type,
            path=rel_path,
            size=file_size,
            lines=line_count,
            modified_date=modified_time
        )
        
        # Add extra attributes based on component type
        if component_type == ComponentType.COBOL_PROGRAM:
            self._analyze_cobol_attributes(component, content)
        elif component_type == ComponentType.JCL_JOB:
            self._analyze_jcl_attributes(component, content)
        
        # Add component to dictionary
        self.components[name] = component
        
        # Extract dependencies
        self._extract_dependencies(name, component_type, content, file_path)
    
    def _identify_component_type(self, file_path: str, ext: str, content: str) -> ComponentType:
        """Identify the type of component based on file extension and content."""
        # By extension
        if ext in ['.cbl', '.cob', '.cobol']:
            return ComponentType.COBOL_PROGRAM
        elif ext == '.cpy':
            return ComponentType.COPYBOOK
        elif ext == '.jcl':
            return ComponentType.JCL_JOB
        elif ext in ['.pli', '.pl1']:
            return ComponentType.PL1_PROGRAM
        elif ext in ['.asm', '.s']:
            return ComponentType.ASSEMBLER_PROGRAM
        
        # By content inspection
        if 'IDENTIFICATION DIVISION' in content.upper():
            return ComponentType.COBOL_PROGRAM
        if '//JOBLIB' in content.upper() or '//STEPLIB' in content.upper():
            return ComponentType.JCL_JOB
        if '//PROC ' in content.upper():
            return ComponentType.JCL_PROC
        if 'EXEC CICS' in content.upper():
            return ComponentType.CICS_TRANSACTION
        
        # Default
        return ComponentType.INCLUDE
    
    def _analyze_cobol_attributes(self, component: Component, content: str):
        """Extract additional attributes from COBOL program."""
        # Check for CICS
        if 'EXEC CICS' in content.upper():
            component.attributes['has_cics'] = True
        
        # Check for DB2
        if 'EXEC SQL' in content.upper():
            component.attributes['has_db2'] = True
        
        # Check for file I/O
        if re.search(r'\bOPEN\b', content, re.IGNORECASE):
            component.attributes['has_file_io'] = True
    
    def _analyze_jcl_attributes(self, component: Component, content: str):
        """Extract additional attributes from JCL job."""
        # Count steps
        steps = re.findall(r'//\s*[A-Za-z0-9#@$]+\s+EXEC\s+', content, re.IGNORECASE)
        component.attributes['step_count'] = len(steps)
        
        # Check for DB2
        if 'EXEC IKJEFT01' in content.upper() and 'DSN SYSTEM' in content.upper():
            component.attributes['has_db2'] = True
        
        # Check for SORT utility
        if 'EXEC PGM=SORT' in content.upper():
            component.attributes['has_sort'] = True
    
    def _extract_dependencies(self, component_name: str, component_type: ComponentType, 
                             content: str, file_path: str):
        """Extract dependencies from file content."""
        lines = content.splitlines()
        
        if component_type == ComponentType.COBOL_PROGRAM:
            self._extract_cobol_dependencies(component_name, content, lines, file_path)
        elif component_type in [ComponentType.JCL_JOB, ComponentType.JCL_PROC]:
            self._extract_jcl_dependencies(component_name, content, lines, file_path)
        elif component_type == ComponentType.PL1_PROGRAM:
            self._extract_pli_dependencies(component_name, content, lines, file_path)
    
    def _extract_cobol_dependencies(self, component_name: str, content: str, lines: List[str], file_path: str):
        """Extract dependencies from COBOL program."""
        # Extract CALL statements
        for i, line in enumerate(lines, 1):
            for match in self.call_pattern.finditer(line):
                target = match.group(1)
                self._add_dependency(
                    component_name, target, DependencyType.CALLS,
                    file_path, i, line.strip()
                )
        
        # Extract COPY statements
        for i, line in enumerate(lines, 1):
            for match in self.copy_pattern.finditer(line):
                target = match.group(1)
                self._add_dependency(
                    component_name, target, DependencyType.INCLUDES,
                    file_path, i, line.strip()
                )
        
        # Extract EXEC CICS statements
        for i, line in enumerate(lines, 1):
            for match in self.exec_cics_pattern.finditer(line):
                cics_command = match.group(1).strip()
                if 'LINK' in cics_command:
                    # Extract program name from LINK command
                    prog_match = re.search(r'PROGRAM\s*\(\s*[\'"]?([A-Za-z0-9#@$-]+)[\'"]?\s*\)', cics_command)
                    if prog_match:
                        target = prog_match.group(1)
                        self._add_dependency(
                            component_name, target, DependencyType.CALLS,
                            file_path, i, line.strip()
                        )
        
        # Extract SQL statements and table references
        sql_blocks = []
        current_block = []
        in_sql_block = False
        
        for i, line in enumerate(lines, 1):
            if 'EXEC SQL' in line.upper():
                in_sql_block = True
                current_block = [(i, line)]
            elif in_sql_block:
                current_block.append((i, line))
                if ';' in line:
                    in_sql_block = False
                    sql_blocks.append(current_block)
                    current_block = []
        
        for block in sql_blocks:
            block_text = ' '.join(line for _, line in block)
            line_num = block[0][0]
            
            # Extract table references
            select_matches = self.exec_sql_select_pattern.finditer(block_text)
            for match in select_matches:
                table = match.group(1)
                self._add_dependency(
                    component_name, table, DependencyType.READS,
                    file_path, line_num, block_text[:60] + "..."
                )
            
            insert_matches = self.exec_sql_insert_pattern.finditer(block_text)
            for match in insert_matches:
                table = match.group(1)
                self._add_dependency(
                    component_name, table, DependencyType.WRITES,
                    file_path, line_num, block_text[:60] + "..."
                )
            
            update_matches = self.exec_sql_update_pattern.finditer(block_text)
            for match in update_matches:
                table = match.group(1)
                self._add_dependency(
                    component_name, table, DependencyType.WRITES,
                    file_path, line_num, block_text[:60] + "..."
                )
            
            delete_matches = self.exec_sql_delete_pattern.finditer(block_text)
            for match in delete_matches:
                table = match.group(1)
                self._add_dependency(
                    component_name, table, DependencyType.WRITES,
                    file_path, line_num, block_text[:60] + "..."
                )
    
    def _extract_jcl_dependencies(self, component_name: str, content: str, lines: List[str], file_path: str):
        """Extract dependencies from JCL job or procedure."""
        # Extract EXEC statements
        for i, line in enumerate(lines, 1):
            for match in self.jcl_exec_pattern.finditer(line):
                program = match.group(1)
                procedure = match.group(2)
                
                if program:
                    self._add_dependency(
                        component_name, program, DependencyType.EXECUTES,
                        file_path, i, line.strip()
                    )
                if procedure:
                    self._add_dependency(
                        component_name, procedure, DependencyType.USES,
                        file_path, i, line.strip()
                    )
        
        # Extract INCLUDE statements
        for i, line in enumerate(lines, 1):
            for match in self.jcl_include_pattern.finditer(line):
                target = match.group(1)
                self._add_dependency(
                    component_name, target, DependencyType.INCLUDES,
                    file_path, i, line.strip()
                )
        
        # Extract DD DSN references
        for i, line in enumerate(lines, 1):
            for match in self.jcl_dd_dsn_pattern.finditer(line):
                dataset = match.group(1)
                # Only include dataset names that look like program names or datasets
                if not dataset.startswith('&') and '.' in dataset:
                    # Use the last part of the dataset name as the target
                    target = dataset.split('.')[-1]
                    self._add_dependency(
                        component_name, target, DependencyType.REFERENCES,
                        file_path, i, line.strip()
                    )
    
    def _extract_pli_dependencies(self, component_name: str, content: str, lines: List[str], file_path: str):
        """Extract dependencies from PL/I program."""
        # Extract INCLUDE statements
        for i, line in enumerate(lines, 1):
            for match in self.pli_include_pattern.finditer(line):
                target = match.group(1)
                self._add_dependency(
                    component_name, target, DependencyType.INCLUDES,
                    file_path, i, line.strip()
                )
        
        # Extract CALL statements
        for i, line in enumerate(lines, 1):
            for match in self.pli_call_pattern.finditer(line):
                target = match.group(1)
                self._add_dependency(
                    component_name, target, DependencyType.CALLS,
                    file_path, i, line.strip()
                )
    
    def _add_dependency(self, source: str, target: str, dep_type: DependencyType, 
                       file_path: str, line_number: int, line_content: str):
        """Add a dependency to the list, or increment count if it exists."""
        # Normalize target name
        target = target.strip().upper()
        source = source.strip().upper()
        
        # Skip self-dependencies
        if source == target:
            return
        
        # Find existing dependency or create new one
        existing = next((d for d in self.dependencies 
                        if d.source == source and d.target == target and d.type == dep_type), None)
        
        if existing:
            existing.count += 1
            existing.locations.append((os.path.relpath(file_path, self.source_dir), line_number))
        else:
            new_dep = Dependency(
                source=source,
                target=target,
                type=dep_type,
                locations=[(os.path.relpath(file_path, self.source_dir), line_number)]
            )
            self.dependencies.append(new_dep)
    
    def _build_graph(self):
        """Build a directed graph from components and dependencies."""
        # Add nodes (components)
        for name, component in self.components.items():
            self.graph.add_node(name, 
                                type=component.type,
                                path=component.path,
                                size=component.size,
                                lines=component.lines)
        
        # Add edges (dependencies)
        for dep in self.dependencies:
            if not self.graph.has_edge(dep.source, dep.target):
                self.graph.add_edge(dep.source, dep.target, 
                                   type=dep.type,
                                   count=dep.count)
            else:
                # Update existing edge with additional information
                self.graph[dep.source][dep.target]['count'] += dep.count
                
                # If the dependency types are different, use a combined type
                if self.graph[dep.source][dep.target].get('type') != dep.type:
                    combined = f"{self.graph[dep.source][dep.target].get('type')}, {dep.type}"
                    self.graph[dep.source][dep.target]['type'] = combined
    
    def save_graph(self, output_file: str):
        """Save the dependency graph to a file."""
        base, ext = os.path.splitext(output_file)
        
        if ext.lower() == '.json':
            self._save_json(output_file)
        elif ext.lower() == '.csv':
            self._save_csv(output_file)
        elif ext.lower() == '.graphml':
            nx.write_graphml(self.graph, output_file)
        elif ext.lower() in ['.png', '.pdf', '.svg']:
            self._save_image(output_file)
        else:
            # Default to JSON if extension is unknown
            self._save_json(f"{base}.json")
    
    def _save_json(self, output_file: str):
        """Save dependency information as JSON."""
        # Convert components and dependencies to dictionaries
        components_dict = {name: {
            'name': comp.name,
            'type': comp.type,
            'path': comp.path,
            'size': comp.size,
            'lines': comp.lines,
            'modified_date': comp.modified_date.isoformat() if comp.modified_date else None,
            'attributes': comp.attributes
        } for name, comp in self.components.items()}
        
        dependencies_list = [{
            'source': dep.source,
            'target': dep.target,
            'type': dep.type,
            'count': dep.count,
            'locations': dep.locations
        } for dep in self.dependencies]
        
        # Create output structure
        output = {
            'metadata': {
                'generated_at': datetime.now().isoformat(),
                'source_directory': self.source_dir,
                'component_count': len(components_dict),
                'dependency_count': len(dependencies_list)
            },
            'components': components_dict,
            'dependencies': dependencies_list
        }
        
        # Write to file
        with open(output_file, 'w') as f:
            json.dump(output, f, indent=2)
        
        logger.info(f"Saved dependency graph to {output_file}")
    
    def _save_csv(self, output_file: str):
        """Save dependency information as CSV."""
        base = os.path.splitext(output_file)[0]
        
        # Save components
        components_file = f"{base}_components.csv"
        with open(components_file, 'w', newline='') as f:
            writer = csv.writer(f)
            writer.writerow(['Name', 'Type', 'Path', 'Size (bytes)', 'Lines', 'Modified Date'])
            for name, comp in self.components.items():
                writer.writerow([
                    comp.name,
                    comp.type,
                    comp.path,
                    comp.size,
                    comp.lines,
                    comp.modified_date.isoformat() if comp.modified_date else None
                ])
        
        # Save dependencies
        dependencies_file = f"{base}_dependencies.csv"
        with open(dependencies_file, 'w', newline='') as f:
            writer = csv.writer(f)
            writer.writerow(['Source', 'Target', 'Type', 'Count'])
            for dep in self.dependencies:
                writer.writerow([
                    dep.source,
                    dep.target,
                    dep.type,
                    dep.count
                ])
        
        logger.info(f"Saved component data to {components_file}")
        logger.info(f"Saved dependency data to {dependencies_file}")
    
    def _save_image(self, output_file: str):
        """Save dependency graph as an image."""
        plt.figure(figsize=(20, 20))
        
        # Position nodes using spring layout
        pos = nx.spring_layout(self.graph)
        
        # Create node colors based on component type
        node_colors = []
        for node in self.graph.nodes():
            if node in self.components:
                if self.components[node].type == ComponentType.COBOL_PROGRAM:
                    node_colors.append('blue')
                elif self.components[node].type == ComponentType.JCL_JOB:
                    node_colors.append('red')
                elif self.components[node].type == ComponentType.COPYBOOK:
                    node_colors.append('green')
                elif self.components[node].type == ComponentType.DB2_TABLE:
                    node_colors.append('purple')
                else:
                    node_colors.append('gray')
            else:
                node_colors.append('yellow')  # External references
        
        # Draw the graph
        nx.draw(
            self.graph, 
            pos, 
            with_labels=True, 
            node_color=node_colors,
            node_size=800, 
            font_size=8,
            font_weight='bold',
            alpha=0.8
        )
        
        plt.title("Mainframe Component Dependencies")
        plt.tight_layout()
        plt.savefig(output_file, dpi=300)
        plt.close()
        
        logger.info(f"Saved dependency visualization to {output_file}")
    
    def find_impacted_components(self, component_name: str) -> Set[str]:
        """Find all components that would be impacted by changes to a component.
        
        Args:
            component_name: Name of the component to analyze
            
        Returns:
            Set of component names that depend on the specified component
        """
        if not self.graph.has_node(component_name):
            logger.warning(f"Component not found: {component_name}")
            return set()
        
        # Use BFS to find all ancestors (components that depend on this one)
        impacted = set()
        queue = [component_name]
        
        while queue:
            current = queue.pop(0)
            
            for predecessor in self.graph.predecessors(current):
                if predecessor not in impacted and predecessor != component_name:
                    impacted.add(predecessor)
                    queue.append(predecessor)
        
        return impacted
    
    def find_required_components(self, component_name: str) -> Set[str]:
        """Find all components that are required by a specified component.
        
        Args:
            component_name: Name of the component to analyze
            
        Returns:
            Set of component names that the specified component depends on
        """
        if not self.graph.has_node(component_name):
            logger.warning(f"Component not found: {component_name}")
            return set()
        
        # Use BFS to find all descendants (components this one depends on)
        required = set()
        queue = [component_name]
        
        while queue:
            current = queue.pop(0)
            
            for successor in self.graph.successors(current):
                if successor not in required and successor != component_name:
                    required.add(successor)
                    queue.append(successor)
        
        return required
    
    def analyze_critical_path(self) -> List[str]:
        """Identify critical components based on dependency analysis.
        
        Returns:
            List of component names that are most critical (highest dependency count)
        """
        if not self.graph.nodes():
            return []
        
        # Calculate centrality measures
        betweenness = nx.betweenness_centrality(self.graph)
        in_degree = dict(self.graph.in_degree())
        
        # Combine measures for scoring
        scores = {}
        for node in self.graph.nodes():
            scores[node] = (betweenness.get(node, 0) * 0.7) + (in_degree.get(node, 0) * 0.3)
        
        # Get top 10 critical components
        critical = sorted(scores.items(), key=lambda x: x[1], reverse=True)[:10]
        return [name for name, _ in critical]
    
    def generate_summary_report(self) -> str:
        """Generate a summary report of the dependency analysis.
        
        Returns:
            String containing the summary report
        """
        if not self.components:
            return "No components found."
        
        # Count component types
        type_counts = defaultdict(int)
        for comp in self.components.values():
            type_counts[comp.type] += 1
        
        # Count dependency types
        dep_counts = defaultdict(int)
        for dep in self.dependencies:
            dep_counts[dep.type] += 1
        
        # Get some graph statistics
        avg_deps = len(self.dependencies) / len(self.components) if self.components else 0
        
        # Format the report
        report = [
            "MAINFRAME DEPENDENCY ANALYSIS SUMMARY",
            "=" * 40,
            f"Source Directory: {self.source_dir}",
            f"Analysis Date: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}",
            "",
            "COMPONENT STATISTICS",
            "-" * 20,
            f"Total Components: {len(self.components)}",
        ]
        
        for comp_type, count in sorted(type_counts.items(), key=lambda x: x[1], reverse=True):
            report.append(f"  {comp_type}: {count}")
        
        report.extend([
            "",
            "DEPENDENCY STATISTICS",
            "-" * 20,
            f"Total Dependencies: {len(self.dependencies)}",
            f"Average Dependencies per Component: {avg_deps:.2f}",
        ])
        
        for dep_type, count in sorted(dep_counts.items(), key=lambda x: x[1], reverse=True):
            report.append(f"  {dep_type}: {count}")
        
        report.extend([
            "",
            "CRITICAL COMPONENTS",
            "-" * 20,
        ])
        
        for comp in self.analyze_critical_path():
            in_degree = len(list(self.graph.predecessors(comp)))
            out_degree = len(list(self.graph.successors(comp)))
            report.append(f"  {comp}: {in_degree} incoming, {out_degree} outgoing dependencies")
        
        return "\n".join(report)


def parse_args():
    """Parse command line arguments."""
    parser = argparse.ArgumentParser(
        description='Map dependencies in mainframe applications'
    )
    
    parser.add_argument(
        '--source-dir', '-s',
        required=True,
        help='Directory containing mainframe source code'
    )
    
    parser.add_argument(
        '--output', '-o',
        default='dependency-map.json',
        help='Output file for dependency map'
    )
    
    parser.add_argument(
        '--exclude', '-e',
        action='append',
        default=[],
        help='Glob patterns to exclude (can be used multiple times)'
    )
    
    parser.add_argument(
        '--format', '-f',
        choices=['json', 'csv', 'graphml', 'png', 'pdf', 'svg'],
        default='json',
        help='Output format'
    )
    
    parser.add_argument(
        '--component', '-c',
        help='Analyze impact for a specific component'
    )
    
    parser.add_argument(
        '--verbose', '-v',
        action='store_true',
        help='Enable verbose logging'
    )
    
    parser.add_argument(
        '--report', '-r',
        action='store_true',
        help='Generate summary report'
    )
    
    return parser.parse_args()


def main():
    """Main entry point."""
    args = parse_args()
    
    # Configure logging level
    if args.verbose:
        logger.setLevel(logging.DEBUG)
    
    try:
        # Create mapper
        mapper = DependencyMapper(args.source_dir, args.exclude)
        
        # Map dependencies
        mapper.map_dependencies()
        
        # Determine output file with correct extension
        output_file = args.output
        if not any(output_file.endswith(ext) for ext in ['.json', '.csv', '.graphml', '.png', '.pdf', '.svg']):
            output_file = f"{output_file}.{args.format}"
        
        # Save results
        mapper.save_graph(output_file)
        
        # Generate report if requested
        if args.report:
            report = mapper.generate_summary_report()
            report_file = f"{os.path.splitext(output_file)[0]}_report.txt"
            with open(report_file, 'w') as f:
                f.write(report)
            print(f"\nSummary report written to: {report_file}")
            print("\n" + report)
        
        # Analyze specific component if requested
        if args.component:
            component = args.component.upper()
            
            impacted = mapper.find_impacted_components(component)
            required = mapper.find_required_components(component)
            
            print(f"\nImpact Analysis for '{component}':")
            print(f"  Components impacted by changes to {component}: {len(impacted)}")
            for i, comp in enumerate(sorted(impacted)[:10], 1):
                print(f"    {i}. {comp}")
            if len(impacted) > 10:
                print(f"    ... and {len(impacted) - 10} more")
            
            print(f"\n  Components required by {component}: {len(required)}")
            for i, comp in enumerate(sorted(required)[:10], 1):
                print(f"    {i}. {comp}")
            if len(required) > 10:
                print(f"    ... and {len(required) - 10} more")
        
        return 0
        
    except Exception as e:
        logger.error(f"Dependency mapping failed: {str(e)}")
        if args.verbose:
            import traceback
            traceback.print_exc()
        return 1


if __name__ == "__main__":
    sys.exit(main()) 