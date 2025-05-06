#!/usr/bin/env python3
"""
Azure AI-based Risk Assessment for Mainframe Deployments

This script analyzes mainframe code changes to assess deployment risk
using Azure OpenAI services. It evaluates:
- Complexity of changes
- Impact on critical systems
- Potential performance impacts
- Data integrity risks
- Test coverage adequacy

Usage:
  python assess_risk.py [--dir <directory>] [--output <report_file>]

Options:
  --dir     Directory containing mainframe code to analyze (default: code/cobol)
  --output  Output report file (default: risk-assessment-report.md)
"""

import os
import sys
import argparse
import json
import re
from datetime import datetime
import logging
from typing import Dict, List, Any, Tuple

import openai
from azure.identity import DefaultAzureCredential
from azure.keyvault.secrets import SecretClient

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger('risk_assessment')

# Risk level definitions
RISK_LEVELS = {
    'LOW': '🟢 Low Risk',
    'MEDIUM': '🟡 Medium Risk',
    'HIGH': '🔴 High Risk',
    'CRITICAL': '⚠️ Critical Risk'
}

class RiskAssessor:
    """Main class for assessing deployment risk for mainframe code changes."""
    
    def __init__(self, code_dir: str = 'code/cobol'):
        """Initialize the risk assessor.
        
        Args:
            code_dir: Directory containing mainframe code to analyze
        """
        self.code_dir = code_dir
        self.setup_azure_openai()
        self.results = {
            'timestamp': datetime.now().isoformat(),
            'summary': {},
            'details': [],
            'recommendation': ''
        }
        
    def setup_azure_openai(self):
        """Configure Azure OpenAI client using environment variables."""
        try:
            # Set up Azure OpenAI
            openai.api_type = "azure"
            openai.api_base = os.getenv("AZURE_OPENAI_ENDPOINT")
            openai.api_version = "2023-05-15"
            
            # Get API key from Key Vault
            credential = DefaultAzureCredential()
            key_vault_url = os.getenv("KEY_VAULT_URL")
            secret_client = SecretClient(vault_url=key_vault_url, credential=credential)
            openai.api_key = secret_client.get_secret("azure-openai-key").value
            
            logger.info("Azure OpenAI configuration successful")
        except Exception as e:
            logger.error(f"Failed to configure Azure OpenAI: {e}")
            sys.exit(1)
    
    def collect_files(self) -> List[str]:
        """Collect all COBOL files from the specified directory.
        
        Returns:
            List of file paths to analyze
        """
        cobol_files = []
        for root, _, files in os.walk(self.code_dir):
            for file in files:
                if file.endswith(('.cbl', '.cob', '.CBL', '.COB')):
                    cobol_files.append(os.path.join(root, file))
        
        logger.info(f"Found {len(cobol_files)} COBOL files to analyze")
        return cobol_files
    
    def analyze_file(self, file_path: str) -> Dict[str, Any]:
        """Analyze a single COBOL file for risk factors.
        
        Args:
            file_path: Path to the COBOL file
        
        Returns:
            Dictionary with risk analysis results
        """
        try:
            with open(file_path, 'r') as f:
                code = f.read()
            
            # Extract key metrics from the file
            loc = len(code.splitlines())
            program_id = self._extract_program_id(code)
            
            # Analyze code using Azure OpenAI
            analysis = self._analyze_with_openai(code)
            
            # Determine risk level based on analysis
            risk_level = self._determine_risk_level(analysis)
            
            return {
                'file_path': file_path,
                'program_id': program_id,
                'loc': loc,
                'risk_level': risk_level,
                'analysis': analysis
            }
            
        except Exception as e:
            logger.error(f"Error analyzing {file_path}: {e}")
            return {
                'file_path': file_path,
                'program_id': 'UNKNOWN',
                'loc': 0,
                'risk_level': 'HIGH',
                'analysis': {
                    'complexity': 'Unknown (analysis error)',
                    'critical_systems_impact': 'Unknown (analysis error)',
                    'performance_impact': 'Unknown (analysis error)',
                    'data_integrity_risk': 'Unknown (analysis error)',
                    'test_coverage': 'Unknown (analysis error)',
                    'risk_factors': ['Analysis failed'],
                    'risk_details': f'Error during analysis: {str(e)}'
                }
            }
    
    def _extract_program_id(self, code: str) -> str:
        """Extract the PROGRAM-ID from COBOL code.
        
        Args:
            code: COBOL source code
            
        Returns:
            Program ID or 'UNKNOWN' if not found
        """
        match = re.search(r'PROGRAM-ID\.\s+([A-Za-z0-9-]+)', code)
        if match:
            return match.group(1)
        return 'UNKNOWN'
    
    def _analyze_with_openai(self, code: str) -> Dict[str, Any]:
        """Use Azure OpenAI to analyze COBOL code for risk factors.
        
        Args:
            code: COBOL source code
            
        Returns:
            Dictionary with analysis results
        """
        prompt = """
        You are a COBOL expert specializing in risk assessment for mainframe deployments.
        Analyze the following COBOL program for deployment risks.
        
        Provide your assessment in JSON format with the following fields:
        - complexity: Text describing code complexity (Low/Medium/High)
        - critical_systems_impact: Text describing potential impact on critical systems
        - performance_impact: Text describing potential performance impacts
        - data_integrity_risk: Text describing data integrity risks
        - test_coverage: Assessment of required test coverage
        - risk_factors: Array of specific risk factors identified
        - risk_details: Detailed explanation of risks
        
        COBOL CODE:
        
        """
        
        try:
            response = openai.ChatCompletion.create(
                engine="gpt-4",
                messages=[
                    {"role": "system", "content": "You are a COBOL expert specializing in mainframe modernization risk assessment."},
                    {"role": "user", "content": prompt + code}
                ],
                temperature=0.1,
                max_tokens=2000,
                response_format={"type": "json_object"}
            )
            
            analysis_text = response.choices[0].message.content
            analysis = json.loads(analysis_text)
            return analysis
            
        except Exception as e:
            logger.error(f"OpenAI analysis failed: {e}")
            return {
                'complexity': 'Unknown (API error)',
                'critical_systems_impact': 'Unknown (API error)',
                'performance_impact': 'Unknown (API error)',
                'data_integrity_risk': 'Unknown (API error)',
                'test_coverage': 'Unknown (API error)',
                'risk_factors': ['API error'],
                'risk_details': f'Error calling Azure OpenAI API: {str(e)}'
            }
    
    def _determine_risk_level(self, analysis: Dict[str, Any]) -> str:
        """Determine the overall risk level based on the analysis.
        
        Args:
            analysis: Analysis results from Azure OpenAI
            
        Returns:
            Risk level string ('LOW', 'MEDIUM', 'HIGH', or 'CRITICAL')
        """
        risk_score = 0
        
        # Evaluate complexity
        if 'high' in analysis.get('complexity', '').lower():
            risk_score += 3
        elif 'medium' in analysis.get('complexity', '').lower():
            risk_score += 2
        elif 'low' in analysis.get('complexity', '').lower():
            risk_score += 1
        
        # Evaluate critical systems impact
        if 'high' in analysis.get('critical_systems_impact', '').lower():
            risk_score += 3
        elif 'medium' in analysis.get('critical_systems_impact', '').lower():
            risk_score += 2
        elif 'low' in analysis.get('critical_systems_impact', '').lower():
            risk_score += 1
        
        # Evaluate performance impact
        if 'high' in analysis.get('performance_impact', '').lower():
            risk_score += 2
        elif 'medium' in analysis.get('performance_impact', '').lower():
            risk_score += 1
        
        # Evaluate data integrity risk
        if 'high' in analysis.get('data_integrity_risk', '').lower():
            risk_score += 4  # Data integrity is critical
        elif 'medium' in analysis.get('data_integrity_risk', '').lower():
            risk_score += 2
        elif 'low' in analysis.get('data_integrity_risk', '').lower():
            risk_score += 1
        
        # Determine risk level based on score
        if risk_score >= 10:
            return 'CRITICAL'
        elif risk_score >= 7:
            return 'HIGH'
        elif risk_score >= 4:
            return 'MEDIUM'
        else:
            return 'LOW'
    
    def assess_all_files(self):
        """Analyze all COBOL files and compile results."""
        files = self.collect_files()
        
        if not files:
            logger.warning(f"No COBOL files found in {self.code_dir}")
            self.results['summary'] = {
                'total_files': 0,
                'risk_levels': {},
                'overall_risk': 'UNKNOWN'
            }
            self.results['recommendation'] = "No COBOL files found to analyze."
            return
        
        risk_counts = {'LOW': 0, 'MEDIUM': 0, 'HIGH': 0, 'CRITICAL': 0}
        
        for file_path in files:
            logger.info(f"Analyzing {file_path}")
            analysis = self.analyze_file(file_path)
            self.results['details'].append(analysis)
            risk_counts[analysis['risk_level']] += 1
        
        # Calculate overall risk level
        if risk_counts['CRITICAL'] > 0:
            overall_risk = 'CRITICAL'
        elif risk_counts['HIGH'] > 0:
            overall_risk = 'HIGH'
        elif risk_counts['MEDIUM'] > 0:
            overall_risk = 'MEDIUM'
        else:
            overall_risk = 'LOW'
        
        self.results['summary'] = {
            'total_files': len(files),
            'risk_levels': risk_counts,
            'overall_risk': overall_risk
        }
        
        # Generate recommendation based on risk level
        self._generate_recommendation()
    
    def _generate_recommendation(self):
        """Generate deployment recommendations based on risk assessment."""
        overall_risk = self.results['summary']['overall_risk']
        
        if overall_risk == 'CRITICAL':
            self.results['recommendation'] = """
            🛑 DO NOT DEPLOY
            
            Critical risks have been identified. Deployment is not recommended until these issues are resolved.
            Review the detailed findings and address all critical risk factors before attempting deployment.
            """
        elif overall_risk == 'HIGH':
            self.results['recommendation'] = """
            ⚠️ HIGH RISK DEPLOYMENT
            
            Significant risks have been identified. Deploy only with:
            - Full executive approval
            - Complete rollback plan tested and verified
            - Extended testing in pre-production environment
            - Incremental deployment with monitoring at each stage
            """
        elif overall_risk == 'MEDIUM':
            self.results['recommendation'] = """
            🟡 PROCEED WITH CAUTION
            
            Moderate risks have been identified. Proceed with:
            - Additional testing focused on the identified risk areas
            - Monitoring plan in place for post-deployment
            - Rollback plan prepared
            - Deployment during off-peak hours recommended
            """
        else:  # LOW
            self.results['recommendation'] = """
            🟢 PROCEED WITH STANDARD PRECAUTIONS
            
            Low risk deployment. Proceed with standard deployment procedures:
            - Normal testing protocols
            - Standard monitoring
            - Regular deployment timing acceptable
            """
    
    def generate_report(self, output_file: str = 'risk-assessment-report.md'):
        """Generate a detailed Markdown report of the risk assessment.
        
        Args:
            output_file: Path to the output report file
        """
        with open(output_file, 'w') as f:
            # Write header
            f.write("# Mainframe Deployment Risk Assessment\n\n")
            f.write(f"Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n\n")
            
            # Write summary
            f.write("## Summary\n\n")
            summary = self.results['summary']
            f.write(f"Total Files Analyzed: {summary['total_files']}\n\n")
            f.write("Risk Level Distribution:\n\n")
            
            for level, count in summary['risk_levels'].items():
                if count > 0:
                    f.write(f"- {RISK_LEVELS[level]}: {count} file(s)\n")
            
            f.write(f"\n**Overall Deployment Risk: {RISK_LEVELS[summary['overall_risk']]}**\n\n")
            
            # Write recommendation
            f.write("## Recommendation\n\n")
            f.write(self.results['recommendation'].strip())
            f.write("\n\n")
            
            # Write detailed findings
            f.write("## Detailed Findings\n\n")
            
            for analysis in self.results['details']:
                f.write(f"### {analysis['program_id']} ({os.path.basename(analysis['file_path'])})\n\n")
                f.write(f"- **Risk Level**: {RISK_LEVELS[analysis['risk_level']]}\n")
                f.write(f"- **Lines of Code**: {analysis['loc']}\n")
                f.write(f"- **Complexity**: {analysis['analysis']['complexity']}\n")
                f.write(f"- **Critical Systems Impact**: {analysis['analysis']['critical_systems_impact']}\n")
                f.write(f"- **Performance Impact**: {analysis['analysis']['performance_impact']}\n")
                f.write(f"- **Data Integrity Risk**: {analysis['analysis']['data_integrity_risk']}\n")
                f.write(f"- **Test Coverage Needed**: {analysis['analysis']['test_coverage']}\n\n")
                
                f.write("**Risk Factors:**\n\n")
                for factor in analysis['analysis']['risk_factors']:
                    f.write(f"- {factor}\n")
                
                f.write("\n**Details:**\n\n")
                f.write(analysis['analysis']['risk_details'])
                f.write("\n\n---\n\n")
        
        logger.info(f"Report generated: {output_file}")

def main():
    """Main entry point for the risk assessment script."""
    parser = argparse.ArgumentParser(description='Azure AI-based Risk Assessment for Mainframe Deployments')
    parser.add_argument('--dir', default='code/cobol', help='Directory containing mainframe code to analyze')
    parser.add_argument('--output', default='risk-assessment-report.md', help='Output report file')
    
    args = parser.parse_args()
    
    assessor = RiskAssessor(code_dir=args.dir)
    assessor.assess_all_files()
    assessor.generate_report(output_file=args.output)
    
    # Print summary to console
    summary = assessor.results['summary']
    if 'overall_risk' in summary:
        risk_level = summary['overall_risk']
        print(f"\nOverall Deployment Risk: {RISK_LEVELS[risk_level]}")
        print(f"See {args.output} for detailed report")
    else:
        print("\nRisk assessment could not be completed.")

if __name__ == "__main__":
    main() 