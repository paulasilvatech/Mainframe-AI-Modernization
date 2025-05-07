#!/usr/bin/env python3
"""
Azure AI Foundry - Mainframe Risk Assessment Tool

This script analyzes changes to mainframe applications and evaluates the
deployment risk across multiple dimensions (security, performance, reliability, 
business impact).

Usage:
    python assess_risk.py --source-dir <directory> --profile <profile> --output <file>

Requirements:
    - Python 3.8+
    - See requirements.txt for dependencies
"""

import argparse
import json
import logging
import os
import sys
from datetime import datetime
from typing import Dict, List, Optional, Tuple, Union

import numpy as np
import pandas as pd
from azure.identity import DefaultAzureCredential
from azure.ai.foundry import AIFoundryClient  # Placeholder for actual SDK

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger("risk-assessment")

# Risk categories and weights
RISK_CATEGORIES = {
    "security": {
        "description": "Security and vulnerability risk",
        "factors": [
            "authentication_changes", 
            "authorization_changes", 
            "sensitive_data_handling",
            "input_validation",
            "security_patches"
        ]
    },
    "performance": {
        "description": "Performance and scalability risk",
        "factors": [
            "resource_intensive_operations",
            "database_changes",
            "batch_window_impact",
            "transaction_path_changes",
            "resource_contention"
        ]
    },
    "reliability": {
        "description": "Reliability and stability risk",
        "factors": [
            "error_handling_changes",
            "critical_path_changes",
            "transaction_integrity",
            "recovery_procedures",
            "dependency_changes"
        ]
    },
    "business_impact": {
        "description": "Business impact risk",
        "factors": [
            "business_process_changes",
            "regulatory_compliance",
            "customer_facing_impact",
            "financial_transaction_impact",
            "reporting_impact"
        ]
    }
}

# Default risk profiles
DEFAULT_PROFILES = {
    "default": {
        "securityWeight": 0.3,
        "performanceWeight": 0.2,
        "reliabilityWeight": 0.3,
        "businessImpactWeight": 0.2,
        "thresholds": {
            "low": 30,
            "medium": 60,
            "high": 80
        }
    },
    "financial-core": {
        "securityWeight": 0.4,
        "performanceWeight": 0.2,
        "reliabilityWeight": 0.3,
        "businessImpactWeight": 0.1,
        "thresholds": {
            "low": 20,
            "medium": 40,
            "high": 60
        },
        "criticalComponents": [
            "ACCTPROC.cbl",
            "TRANSACT.cbl",
            "BALUPDT.cbl",
            "SECCHECK.cbl"
        ]
    },
    "customer-facing": {
        "securityWeight": 0.3,
        "performanceWeight": 0.3,
        "reliabilityWeight": 0.2,
        "businessImpactWeight": 0.2,
        "thresholds": {
            "low": 30,
            "medium": 60,
            "high": 80
        }
    }
}


class RiskAssessor:
    """Main class for assessing deployment risk of mainframe applications."""

    def __init__(self, profile_name: str = "default", custom_profile: Optional[Dict] = None):
        """Initialize the risk assessor with a risk profile.
        
        Args:
            profile_name: Name of built-in profile to use
            custom_profile: Custom risk profile (overrides profile_name if provided)
        """
        self.profile = custom_profile if custom_profile else DEFAULT_PROFILES.get(
            profile_name, DEFAULT_PROFILES["default"])
        self.ai_client = self._initialize_ai_client()
        logger.info(f"Initialized risk assessor with profile: {profile_name}")

    def _initialize_ai_client(self) -> Optional[AIFoundryClient]:
        """Initialize Azure AI Foundry client for enhanced analysis."""
        try:
            credential = DefaultAzureCredential()
            # This is a placeholder - actual initialization would depend on the real SDK
            client = AIFoundryClient(credential=credential)
            return client
        except Exception as e:
            logger.warning(f"Failed to initialize AI client: {str(e)}")
            logger.warning("Running in local-only mode with reduced capabilities")
            return None

    def assess_source_directory(self, source_dir: str) -> Dict:
        """Perform risk assessment on a source directory.
        
        Args:
            source_dir: Path to source code directory
            
        Returns:
            Dict containing risk assessment results
        """
        if not os.path.isdir(source_dir):
            raise ValueError(f"Source directory not found: {source_dir}")
        
        logger.info(f"Assessing risk for source directory: {source_dir}")
        
        # Collect files for analysis
        cobol_files = self._find_files(source_dir, [".cbl", ".cob"])
        jcl_files = self._find_files(source_dir, [".jcl"])
        copybook_files = self._find_files(source_dir, [".cpy"])
        pli_files = self._find_files(source_dir, [".pli"])
        
        # Perform analysis
        security_risk = self._assess_security_risk(cobol_files, copybook_files, jcl_files, pli_files)
        performance_risk = self._assess_performance_risk(cobol_files, jcl_files, pli_files)
        reliability_risk = self._assess_reliability_risk(cobol_files, copybook_files, jcl_files, pli_files)
        business_risk = self._assess_business_impact(cobol_files, jcl_files)
        
        # Calculate weighted overall risk
        weighted_risk = (
            security_risk * self.profile["securityWeight"] +
            performance_risk * self.profile["performanceWeight"] +
            reliability_risk * self.profile["reliabilityWeight"] +
            business_risk * self.profile["businessImpactWeight"]
        )
        
        # Determine risk level based on thresholds
        risk_level = self._determine_risk_level(weighted_risk)
        
        # Create findings
        findings = self._generate_findings(
            security_risk, performance_risk, reliability_risk, business_risk,
            cobol_files, jcl_files, copybook_files, pli_files
        )
        
        # Generate recommended deployment strategy
        deployment_strategy = self._recommend_deployment_strategy(weighted_risk, risk_level)
        
        return {
            "timestamp": datetime.now().isoformat(),
            "sourceDirectory": source_dir,
            "profile": self.profile.get("name", "default"),
            "riskScores": {
                "security": security_risk,
                "performance": performance_risk,
                "reliability": reliability_risk,
                "businessImpact": business_risk
            },
            "overallScore": weighted_risk,
            "riskLevel": risk_level,
            "findings": findings,
            "recommendedDeploymentStrategy": deployment_strategy,
            "approvalRequired": risk_level in ["medium", "high"],
            "approvalLevel": "senior" if risk_level == "high" else "standard"
        }

    def _find_files(self, directory: str, extensions: List[str]) -> List[str]:
        """Find files with specified extensions in directory."""
        found_files = []
        for root, _, files in os.walk(directory):
            for file in files:
                if any(file.lower().endswith(ext.lower()) for ext in extensions):
                    found_files.append(os.path.join(root, file))
        return found_files

    def _assess_security_risk(self, cobol_files, copybook_files, jcl_files, pli_files) -> float:
        """Assess security risk of changes."""
        # This is a simplified implementation - a real implementation would 
        # perform detailed code analysis
        
        risk_score = 0
        
        # Check for critical security patterns in COBOL code
        for file in cobol_files:
            try:
                with open(file, 'r') as f:
                    content = f.read().upper()
                    # Check for authentication/authorization related code
                    if 'PASSWORD' in content or 'SECURITY' in content:
                        risk_score += 15
                    # Check for input validation
                    if 'VALIDATE' in content and risk_score < 85:
                        risk_score += 10
                    # Check for database access
                    if 'SQL' in content and risk_score < 90:
                        risk_score += 5
            except Exception as e:
                logger.error(f"Error processing {file}: {str(e)}")
        
        # Enhance with AI analysis if available
        if self.ai_client:
            try:
                # This is a placeholder for actual AI client usage
                ai_security_score = self.ai_client.analyze_security(cobol_files)
                risk_score = (risk_score + ai_security_score) / 2
            except Exception as e:
                logger.error(f"AI security analysis failed: {str(e)}")
        
        return min(risk_score, 100)  # Cap at 100

    def _assess_performance_risk(self, cobol_files, jcl_files, pli_files) -> float:
        """Assess performance risk of changes."""
        # This is a simplified implementation
        
        risk_score = 0
        
        # Check for performance-critical patterns in COBOL
        for file in cobol_files:
            try:
                with open(file, 'r') as f:
                    content = f.read().upper()
                    # Check for database operations
                    if 'SELECT' in content or 'EXEC SQL' in content:
                        risk_score += 15
                    # Check for loops
                    if 'PERFORM UNTIL' in content and risk_score < 85:
                        risk_score += 10
                    # Check for sorting
                    if 'SORT' in content and risk_score < 90:
                        risk_score += 20
            except Exception as e:
                logger.error(f"Error processing {file}: {str(e)}")
        
        # Check JCL for resource-intensive steps
        for file in jcl_files:
            try:
                with open(file, 'r') as f:
                    content = f.read().upper()
                    if 'SORT' in content:
                        risk_score += 10
                    if 'DB2' in content and risk_score < 90:
                        risk_score += 5
            except Exception as e:
                logger.error(f"Error processing {file}: {str(e)}")
                
        return min(risk_score, 100)  # Cap at 100

    def _assess_reliability_risk(self, cobol_files, copybook_files, jcl_files, pli_files) -> float:
        """Assess reliability risk of changes."""
        # This is a simplified implementation
        
        risk_score = 0
        
        # Check for critical components
        critical_components = self.profile.get("criticalComponents", [])
        for file in cobol_files + jcl_files + pli_files:
            filename = os.path.basename(file)
            if filename in critical_components:
                risk_score += 30
                break
        
        # Check for error handling in COBOL
        for file in cobol_files:
            try:
                with open(file, 'r') as f:
                    content = f.read().upper()
                    # Check for error handling
                    if 'ON ERROR' in content or 'ON EXCEPTION' in content:
                        risk_score += 10
                    # Check for file I/O
                    if 'OPEN' in content and 'CLOSE' in content:
                        risk_score += 5
            except Exception as e:
                logger.error(f"Error processing {file}: {str(e)}")
        
        return min(risk_score, 100)  # Cap at 100

    def _assess_business_impact(self, cobol_files, jcl_files) -> float:
        """Assess business impact risk of changes."""
        # This is a simplified implementation
        
        risk_score = 0
        
        # Check for business-critical patterns
        for file in cobol_files:
            try:
                with open(file, 'r') as f:
                    content = f.read().upper()
                    # Financial calculations
                    if 'COMPUTE' in content and ('BALANCE' in content or 'AMOUNT' in content):
                        risk_score += 20
                    # Customer data
                    if 'CUSTOMER' in content:
                        risk_score += 15
            except Exception as e:
                logger.error(f"Error processing {file}: {str(e)}")
        
        # Check JCL for critical jobs
        for file in jcl_files:
            try:
                with open(file, 'r') as f:
                    content = f.read().upper()
                    if 'DAILY' in content or 'MONTH' in content:
                        risk_score += 10
            except Exception as e:
                logger.error(f"Error processing {file}: {str(e)}")
        
        return min(risk_score, 100)  # Cap at 100

    def _determine_risk_level(self, risk_score: float) -> str:
        """Determine risk level based on risk score and profile thresholds."""
        thresholds = self.profile.get("thresholds", {"low": 30, "medium": 60, "high": 80})
        
        if risk_score < thresholds["low"]:
            return "low"
        elif risk_score < thresholds["medium"]:
            return "medium"
        else:
            return "high"

    def _generate_findings(self, security_risk, performance_risk, reliability_risk, 
                          business_risk, cobol_files, jcl_files, copybook_files, pli_files) -> List[Dict]:
        """Generate findings based on risk assessment."""
        findings = []
        
        # This is a simplified implementation - a real implementation would 
        # perform detailed code analysis to identify specific issues
        
        # Example security finding
        if security_risk > 50:
            findings.append({
                "category": "security",
                "severity": "high" if security_risk > 75 else "medium",
                "title": "Authentication code changes detected",
                "description": "Changes to authentication logic were detected which may impact security.",
                "location": self._identify_security_files(cobol_files),
                "remediation": "Review authentication changes and ensure proper security testing."
            })
        
        # Example performance finding
        if performance_risk > 40:
            findings.append({
                "category": "performance",
                "severity": "medium",
                "title": "Database access patterns may impact performance",
                "description": "Changes to database access patterns were detected that may affect performance.",
                "location": self._identify_db_files(cobol_files),
                "remediation": "Review SQL statements and ensure efficient query patterns."
            })
        
        # Example reliability finding
        if reliability_risk > 60:
            findings.append({
                "category": "reliability",
                "severity": "high",
                "title": "Critical component changes with limited error handling",
                "description": "Changes to critical components with insufficient error handling were detected.",
                "location": self._identify_critical_files(cobol_files, self.profile.get("criticalComponents", [])),
                "remediation": "Add comprehensive error handling to critical components."
            })
        
        # Example business impact finding
        if business_risk > 30:
            findings.append({
                "category": "businessImpact",
                "severity": "medium",
                "title": "Financial calculation changes",
                "description": "Changes to financial calculation logic were detected.",
                "location": self._identify_financial_files(cobol_files),
                "remediation": "Ensure financial calculations are validated with business stakeholders."
            })
        
        return findings

    def _identify_security_files(self, files: List[str]) -> str:
        """Identify files with security concerns."""
        for file in files:
            try:
                with open(file, 'r') as f:
                    if 'PASSWORD' in f.read().upper() or 'SECURITY' in f.read().upper():
                        return os.path.basename(file)
            except:
                pass
        return ""

    def _identify_db_files(self, files: List[str]) -> str:
        """Identify files with database operations."""
        for file in files:
            try:
                with open(file, 'r') as f:
                    if 'SQL' in f.read().upper():
                        return os.path.basename(file)
            except:
                pass
        return ""

    def _identify_critical_files(self, files: List[str], critical_components: List[str]) -> str:
        """Identify critical files."""
        for file in files:
            if os.path.basename(file) in critical_components:
                return os.path.basename(file)
        return ""

    def _identify_financial_files(self, files: List[str]) -> str:
        """Identify files with financial calculations."""
        for file in files:
            try:
                with open(file, 'r') as f:
                    content = f.read().upper()
                    if 'COMPUTE' in content and ('BALANCE' in content or 'AMOUNT' in content):
                        return os.path.basename(file)
            except:
                pass
        return ""

    def _recommend_deployment_strategy(self, risk_score: float, risk_level: str) -> Dict:
        """Recommend a deployment strategy based on risk assessment."""
        if risk_level == "low":
            return {
                "strategy": "rolling",
                "description": "Standard rolling deployment with automated validation",
                "approvalRequired": False
            }
        elif risk_level == "medium":
            return {
                "strategy": "blue-green",
                "description": "Blue-green deployment with manual validation steps",
                "approvalRequired": True,
                "approvalLevel": "standard"
            }
        else:  # high risk
            return {
                "strategy": "canary",
                "description": "Canary deployment with gradual traffic shifting and enhanced monitoring",
                "approvalRequired": True,
                "approvalLevel": "senior",
                "additionalSteps": [
                    "Pre-deployment validation meeting",
                    "Enhanced monitoring setup",
                    "Rollback rehearsal"
                ]
            }


def parse_args():
    """Parse command line arguments."""
    parser = argparse.ArgumentParser(
        description='Assess deployment risk for mainframe applications'
    )
    
    parser.add_argument(
        '--source-dir', '-s',
        required=True,
        help='Directory containing source code to assess'
    )
    
    parser.add_argument(
        '--profile', '-p',
        default='default',
        choices=list(DEFAULT_PROFILES.keys()),
        help='Risk profile to use for assessment'
    )
    
    parser.add_argument(
        '--custom-profile', '-c',
        help='Path to custom risk profile JSON file'
    )
    
    parser.add_argument(
        '--output', '-o',
        default='risk-assessment.json',
        help='Output file for assessment results'
    )
    
    parser.add_argument(
        '--verbose', '-v',
        action='store_true',
        help='Enable verbose logging'
    )
    
    return parser.parse_args()


def main():
    """Main entry point."""
    args = parse_args()
    
    # Configure logging level
    if args.verbose:
        logger.setLevel(logging.DEBUG)
    
    # Load custom profile if specified
    custom_profile = None
    if args.custom_profile:
        try:
            with open(args.custom_profile, 'r') as f:
                custom_profile = json.load(f)
        except Exception as e:
            logger.error(f"Failed to load custom profile: {str(e)}")
            return 1

    try:
        # Create risk assessor
        assessor = RiskAssessor(args.profile, custom_profile)
        
        # Perform assessment
        results = assessor.assess_source_directory(args.source_dir)
        
        # Write results to file
        with open(args.output, 'w') as f:
            json.dump(results, f, indent=2)
        
        # Print summary to console
        print(f"\nRisk Assessment Summary:")
        print(f"Overall Risk Score: {results['overallScore']:.2f}")
        print(f"Risk Level: {results['riskLevel'].upper()}")
        print(f"Recommended Deployment Strategy: {results['recommendedDeploymentStrategy']['strategy'].upper()}")
        
        if results['findings']:
            print("\nKey Findings:")
            for i, finding in enumerate(results['findings'], 1):
                print(f"{i}. [{finding['severity'].upper()}] {finding['title']}")
        
        print(f"\nDetailed results written to: {args.output}")
        
        # Return non-zero exit code for high risk to facilitate CI/CD pipeline decisions
        return 1 if results['riskLevel'] == 'high' else 0
        
    except Exception as e:
        logger.error(f"Assessment failed: {str(e)}")
        return 1


if __name__ == "__main__":
    sys.exit(main()) 