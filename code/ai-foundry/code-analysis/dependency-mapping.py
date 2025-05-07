#!/usr/bin/env python3
"""
Dependency Mapping for Mainframe Applications

This script analyzes COBOL programs and JCL to identify dependencies between
components. It creates a dependency graph that can be used for impact analysis
during modernization planning.

Usage:
  python dependency-mapping.py [--dir <directory>] [--output <output_dir>]

Options:
  --dir     Directory containing mainframe code to analyze (default: code/cobol)
  --output  Output directory for dependency artifacts (default: ./dependency-output)
"""

import os
import sys
import re
import argparse
import json
from pathlib import Path
import logging
from typing import Dict, List, Set, Tuple

import networkx as nx
import matplotlib.pyplot as plt

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger('dependency_mapping')

class DependencyMapper:
    """Analyzes COBOL programs and JCL to map dependencies between components."""
    
    def __init__(self, code_dir: str = 'code/cobol', output_dir: str = './dependency-output'):
        """Initialize the dependency mapper.
        
        Args:
            code_dir: Directory containing mainframe code to analyze
            output_dir: Directory to store dependency mapping outputs
        """
        self.code_dir = code_dir
        self.output_dir = output_dir
        self.dependency_graph = nx.DiGraph()
        self.program_details = {}
        
    def collect_files(self) -> Tuple[List[str], List[str]]:
        """Collect all COBOL and JCL files from the specified directory.
        
        Returns:
            Tuple of (cobol_files, jcl_files)
        """
        cobol_files = []
        jcl_files = []
        
        for root, _, files in os.walk(self.code_dir):
            for file in files:
                file_path = os.path.join(root, file)
                
                if file.endswith(('.cbl', '.cob', '.CBL', '.COB')):
                    cobol_files.append(file_path)
                elif file.endswith(('.jcl', '.JCL')):
                    jcl_files.append(file_path)
        
        logger.info(f"Found {len(cobol_files)} COBOL files and {len(jcl_files)} JCL files")
        return cobol_files, jcl_files
    
    def analyze_cobol_dependencies(self, cobol_files: List[str]):
        """Analyze COBOL programs to identify dependencies.
        
        Args:
            cobol_files: List of COBOL file paths to analyze
        """
        for file_path in cobol_files:
            try:
                with open(file_path, 'r') as f:
                    content = f.read()
                
                program_id = self._extract_program_id(content)
                called_programs = self._extract_called_programs(content)
                copybooks = self._extract_copybooks(content)
                file_accesses = self._extract_file_accesses(content)
                
                if program_id:
                    self.dependency_graph.add_node(program_id, type='program', file=file_path)
                    self.program_details[program_id] = {
                        'file': file_path,
                        'calls': called_programs,
                        'copybooks': copybooks,
                        'file_accesses': file_accesses
                    }
                    
                    # Add dependencies for called programs
                    for called in called_programs:
                        self.dependency_graph.add_node(called, type='program')
                        self.dependency_graph.add_edge(program_id, called, type='calls')
                    
                    # Add dependencies for copybooks
                    for copybook in copybooks:
                        self.dependency_graph.add_node(copybook, type='copybook')
                        self.dependency_graph.add_edge(program_id, copybook, type='includes')
                    
                    # Add dependencies for file accesses
                    for file_access in file_accesses:
                        self.dependency_graph.add_node(file_access, type='file')
                        self.dependency_graph.add_edge(program_id, file_access, type='accesses')
                
            except Exception as e:
                logger.error(f"Error analyzing {file_path}: {e}")
    
    def analyze_jcl_dependencies(self, jcl_files: List[str]):
        """Analyze JCL to identify job dependencies.
        
        Args:
            jcl_files: List of JCL file paths to analyze
        """
        for file_path in jcl_files:
            try:
                with open(file_path, 'r') as f:
                    content = f.read()
                
                job_name = self._extract_job_name(content)
                executed_programs = self._extract_executed_programs(content)
                job_dependencies = self._extract_job_dependencies(content)
                
                if job_name:
                    self.dependency_graph.add_node(job_name, type='job', file=file_path)
                    
                    # Add dependencies for executed programs
                    for program in executed_programs:
                        self.dependency_graph.add_node(program, type='program')
                        self.dependency_graph.add_edge(job_name, program, type='executes')
                    
                    # Add dependencies for job dependencies
                    for dep_job in job_dependencies:
                        self.dependency_graph.add_node(dep_job, type='job')
                        self.dependency_graph.add_edge(job_name, dep_job, type='depends-on')
                
            except Exception as e:
                logger.error(f"Error analyzing JCL {file_path}: {e}")
    
    def _extract_program_id(self, content: str) -> str:
        """Extract the PROGRAM-ID from COBOL code.
        
        Args:
            content: COBOL source code
            
        Returns:
            Program ID or empty string if not found
        """
        match = re.search(r'PROGRAM-ID\.\s+([A-Za-z0-9-]+)', content)
        if match:
            return match.group(1)
        return ""
    
    def _extract_called_programs(self, content: str) -> Set[str]:
        """Extract programs called from COBOL code.
        
        Args:
            content: COBOL source code
            
        Returns:
            Set of called program names
        """
        # Look for CALL statements
        call_matches = re.finditer(r'CALL\s+[\'"]([A-Za-z0-9-]+)[\'"]', content, re.IGNORECASE)
        called_programs = {match.group(1) for match in call_matches}
        
        # Also look for CALL identifier where identifier is defined earlier
        dynamic_calls = re.finditer(r'CALL\s+([A-Za-z0-9-]+)', content, re.IGNORECASE)
        for match in dynamic_calls:
            if match.group(1) not in ["'", '"']:  # Skip calls we already processed
                # This is a potential dynamic call
                identifier = match.group(1)
                # Try to find where this identifier might be set
                id_def = re.search(rf'{identifier}\s+PIC.*VALUE\s+[\'"]([A-Za-z0-9-]+)[\'"]', content, re.IGNORECASE)
                if id_def:
                    called_programs.add(id_def.group(1))
        
        return called_programs
    
    def _extract_copybooks(self, content: str) -> Set[str]:
        """Extract copybooks included in COBOL code.
        
        Args:
            content: COBOL source code
            
        Returns:
            Set of copybook names
        """
        copy_matches = re.finditer(r'COPY\s+([A-Za-z0-9-]+)', content, re.IGNORECASE)
        return {match.group(1) for match in copy_matches}
    
    def _extract_file_accesses(self, content: str) -> Set[str]:
        """Extract files accessed in COBOL code.
        
        Args:
            content: COBOL source code
            
        Returns:
            Set of file names
        """
        # Look for SELECT statements in FILE-CONTROL
        select_matches = re.finditer(r'SELECT\s+([A-Za-z0-9-]+)\s+ASSIGN\s+TO\s+([A-Za-z0-9-]+)', content, re.IGNORECASE)
        return {match.group(2) for match in select_matches}
    
    def _extract_job_name(self, content: str) -> str:
        """Extract job name from JCL.
        
        Args:
            content: JCL content
            
        Returns:
            Job name or empty string if not found
        """
        match = re.search(r'//([A-Za-z0-9#@$]+)\s+JOB', content)
        if match:
            return match.group(1)
        return ""
    
    def _extract_executed_programs(self, content: str) -> Set[str]:
        """Extract programs executed in JCL.
        
        Args:
            content: JCL content
            
        Returns:
            Set of program names
        """
        # Look for EXEC PGM=program statements
        pgm_matches = re.finditer(r'EXEC\s+PGM=([A-Za-z0-9]+)', content, re.IGNORECASE)
        return {match.group(1) for match in pgm_matches}
    
    def _extract_job_dependencies(self, content: str) -> Set[str]:
        """Extract job dependencies from JCL.
        
        Args:
            content: JCL content
            
        Returns:
            Set of dependent job names
        """
        # Look for COND= and other job dependency indicators
        cond_matches = re.finditer(r'IF\s+\(\s*([A-Za-z0-9]+)\s*', content, re.IGNORECASE)
        return {match.group(1) for match in cond_matches}
    
    def generate_impact_analysis(self, starting_component: str) -> Set[str]:
        """Generate impact analysis for a specific component.
        
        Args:
            starting_component: Component to analyze impact for
        
        Returns:
            Set of components impacted by changes to starting_component
        """
        if starting_component not in self.dependency_graph:
            logger.warning(f"Component {starting_component} not found in dependency graph")
            return set()
        
        # Find all components that depend on this component
        # These are the "affected" components if starting_component changes
        impacted = set()
        for node in self.dependency_graph.nodes():
            # Check if there's a path from node to starting_component
            try:
                if node != starting_component and nx.has_path(self.dependency_graph, node, starting_component):
                    impacted.add(node)
            except nx.NetworkXNoPath:
                continue
        
        return impacted
    
    def analyze_dependencies(self):
        """Analyze all files and build dependency graph."""
        os.makedirs(self.output_dir, exist_ok=True)
        
        cobol_files, jcl_files = self.collect_files()
        
        logger.info("Analyzing COBOL dependencies...")
        self.analyze_cobol_dependencies(cobol_files)
        
        logger.info("Analyzing JCL dependencies...")
        self.analyze_jcl_dependencies(jcl_files)
        
        logger.info(f"Dependency graph built with {self.dependency_graph.number_of_nodes()} nodes "
                   f"and {self.dependency_graph.number_of_edges()} edges")
    
    def generate_dependency_report(self):
        """Generate a report of dependencies."""
        if not self.dependency_graph.nodes():
            logger.warning("No dependencies found to report")
            return
        
        report_path = os.path.join(self.output_dir, 'dependency-report.md')
        
        with open(report_path, 'w') as f:
            f.write("# Mainframe Component Dependency Report\n\n")
            
            # Program dependencies
            f.write("## Program Dependencies\n\n")
            f.write("| Program | Calls | Uses Copybooks | Accesses Files |\n")
            f.write("|---------|-------|----------------|----------------|\n")
            
            for program, details in self.program_details.items():
                calls = ', '.join(details['calls']) if details['calls'] else 'None'
                copybooks = ', '.join(details['copybooks']) if details['copybooks'] else 'None'
                files = ', '.join(details['file_accesses']) if details['file_accesses'] else 'None'
                
                f.write(f"| {program} | {calls} | {copybooks} | {files} |\n")
            
            # Program usage (what programs call this program)
            f.write("\n## Program Usage\n\n")
            f.write("| Program | Called By |\n")
            f.write("|---------|----------|\n")
            
            for node in self.dependency_graph.nodes():
                if self.dependency_graph.nodes[node].get('type') == 'program':
                    called_by = []
                    for pred in self.dependency_graph.predecessors(node):
                        edge_data = self.dependency_graph.get_edge_data(pred, node)
                        if edge_data.get('type') == 'calls':
                            called_by.append(pred)
                    
                    called_by_str = ', '.join(called_by) if called_by else 'None'
                    f.write(f"| {node} | {called_by_str} |\n")
            
            logger.info(f"Dependency report generated: {report_path}")
    
    def visualize_dependencies(self):
        """Generate visualizations of the dependency graph."""
        if not self.dependency_graph.nodes():
            logger.warning("No dependencies found to visualize")
            return
        
        # Create a simplified graph for visualization
        vis_graph = nx.DiGraph()
        
        # Only include program-to-program calls for clarity
        for u, v, data in self.dependency_graph.edges(data=True):
            if data.get('type') == 'calls':
                vis_graph.add_edge(u, v)
        
        # Only visualize if we have edges
        if vis_graph.number_of_edges() > 0:
            # Determine node colors based on type
            node_colors = []
            for node in vis_graph.nodes():
                node_type = self.dependency_graph.nodes[node].get('type', 'unknown')
                if node_type == 'program':
                    node_colors.append('lightblue')
                elif node_type == 'job':
                    node_colors.append('lightgreen')
                else:
                    node_colors.append('lightgray')
            
            plt.figure(figsize=(12, 10))
            pos = nx.spring_layout(vis_graph, seed=42)
            nx.draw(vis_graph, pos, with_labels=True, node_color=node_colors, 
                   node_size=1500, font_size=8, arrows=True, arrowsize=15)
            
            plt.title("Program Call Dependencies")
            plt.tight_layout()
            plt.savefig(os.path.join(self.output_dir, 'program_dependencies.png'), dpi=300)
            plt.close()
            
            logger.info(f"Dependency visualization saved to {self.output_dir}/program_dependencies.png")
        else:
            logger.warning("No program-to-program dependencies found to visualize")
    
    def export_dependency_data(self):
        """Export dependency data as JSON for further analysis."""
        # Convert networkx graph to JSON serializable format
        graph_data = {
            'nodes': [],
            'edges': []
        }
        
        for node in self.dependency_graph.nodes():
            node_data = self.dependency_graph.nodes[node].copy()
            node_data['id'] = node
            graph_data['nodes'].append(node_data)
        
        for u, v, data in self.dependency_graph.edges(data=True):
            edge_data = data.copy()
            edge_data['source'] = u
            edge_data['target'] = v
            graph_data['edges'].append(edge_data)
        
        with open(os.path.join(self.output_dir, 'dependency_data.json'), 'w') as f:
            json.dump(graph_data, f, indent=2)
        
        logger.info(f"Dependency data exported to {self.output_dir}/dependency_data.json")

def main():
    """Main entry point for the dependency mapping script."""
    parser = argparse.ArgumentParser(description='Dependency Mapping for Mainframe Applications')
    parser.add_argument('--dir', default='code/cobol', help='Directory containing mainframe code to analyze')
    parser.add_argument('--output', default='./dependency-output', help='Output directory for dependency artifacts')
    parser.add_argument('--component', help='Generate impact analysis for a specific component')
    
    args = parser.parse_args()
    
    mapper = DependencyMapper(code_dir=args.dir, output_dir=args.output)
    mapper.analyze_dependencies()
    mapper.generate_dependency_report()
    mapper.visualize_dependencies()
    mapper.export_dependency_data()
    
    if args.component:
        impacted = mapper.generate_impact_analysis(args.component)
        print(f"\nComponents impacted by changes to {args.component}:")
        for component in sorted(impacted):
            print(f"- {component}")

if __name__ == "__main__":
    main() 