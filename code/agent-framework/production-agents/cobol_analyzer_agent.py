"""
COBOL Analyzer Agent
Production-ready implementation for analyzing COBOL programs
Handles syntax analysis, complexity metrics, and business logic extraction
"""

import re
import asyncio
from typing import Dict, Any, List, Optional, Tuple
from dataclasses import dataclass, field
from collections import defaultdict

from .base_agent import BaseAgent, TaskContext, TaskExecutionError


@dataclass
class COBOLStructure:
    """COBOL program structure representation"""
    divisions: List[Dict[str, Any]] = field(default_factory=list)
    sections: List[Dict[str, Any]] = field(default_factory=list)
    paragraphs: List[Dict[str, Any]] = field(default_factory=list)
    data_items: List[Dict[str, Any]] = field(default_factory=list)
    procedures: List[Dict[str, Any]] = field(default_factory=list)
    copy_statements: List[Dict[str, Any]] = field(default_factory=list)
    file_operations: List[Dict[str, Any]] = field(default_factory=list)
    database_calls: List[Dict[str, Any]] = field(default_factory=list)
    screen_operations: List[Dict[str, Any]] = field(default_factory=list)


@dataclass
class ComplexityMetrics:
    """COBOL complexity metrics"""
    lines_of_code: int = 0
    comment_lines: int = 0
    cyclomatic_complexity: int = 0
    cognitive_complexity: int = 0
    nesting_depth: int = 0
    data_complexity: float = 0.0
    maintainability_index: float = 0.0
    technical_debt_ratio: float = 0.0


@dataclass
class BusinessRule:
    """Business rule extracted from COBOL"""
    rule_id: str
    name: str
    type: str  # calculation, validation, workflow, etc.
    description: str
    location: Dict[str, Any]  # file, start_line, end_line
    code_snippet: str
    variables: List[Dict[str, Any]]
    conditions: List[str]
    actions: List[str]
    dependencies: List[str]
    confidence: float  # 0.0 to 1.0


class COBOLAnalyzerAgent(BaseAgent):
    """
    Specialized agent for analyzing COBOL programs
    Provides deep analysis of structure, complexity, and business logic
    """
    
    def __init__(self, agent_id: Optional[str] = None, config: Dict[str, Any] = None):
        super().__init__(
            agent_id=agent_id,
            agent_type="cobol_analyzer",
            capabilities=[
                "syntax_analysis",
                "structure_extraction",
                "complexity_calculation",
                "business_logic_extraction",
                "dependency_mapping",
                "quality_assessment",
                "pattern_detection",
                "modernization_readiness"
            ],
            config=config or {}
        )
        
        # COBOL-specific patterns
        self._init_patterns()
        
        # Analysis cache
        self._analysis_cache = {}
        self._cache_ttl = self.config.get('cache_ttl', 3600)
        
    def _init_patterns(self):
        """Initialize COBOL pattern definitions"""
        self.patterns = {
            'division': re.compile(r'^\s*(\w+)\s+DIVISION\s*\.', re.IGNORECASE),
            'section': re.compile(r'^\s*(\w+(?:\s+\w+)*)\s+SECTION\s*\.', re.IGNORECASE),
            'paragraph': re.compile(r'^(\w+(?:-\w+)*)\s*\.', re.MULTILINE),
            'data_level': re.compile(r'^\s*(\d{2})\s+(\S+)', re.MULTILINE),
            'pic_clause': re.compile(r'PIC(?:TURE)?\s+(?:IS\s+)?([^\s.]+)', re.IGNORECASE),
            'perform': re.compile(r'PERFORM\s+(\S+)(?:\s+THROUGH\s+(\S+))?', re.IGNORECASE),
            'call': re.compile(r'CALL\s+["\'](\w+)["\']', re.IGNORECASE),
            'copy': re.compile(r'COPY\s+(\w+)', re.IGNORECASE),
            'sql': re.compile(r'EXEC\s+SQL(.*?)END-EXEC', re.IGNORECASE | re.DOTALL),
            'cics': re.compile(r'EXEC\s+CICS(.*?)END-EXEC', re.IGNORECASE | re.DOTALL),
            'file_op': re.compile(r'(OPEN|CLOSE|READ|WRITE|REWRITE|DELETE)\s+(\w+)', re.IGNORECASE),
            'if_statement': re.compile(r'IF\s+(.*?)\s+(?:THEN|$)', re.IGNORECASE | re.DOTALL),
            'evaluate': re.compile(r'EVALUATE\s+(.*?)\s+END-EVALUATE', re.IGNORECASE | re.DOTALL),
            'compute': re.compile(r'COMPUTE\s+(.*?)(?:END-COMPUTE|$)', re.IGNORECASE | re.DOTALL),
            'move': re.compile(r'MOVE\s+(.*?)\s+TO\s+(.*?)(?:\.|$)', re.IGNORECASE),
        }
        
    async def _initialize(self) -> None:
        """Initialize COBOL analyzer"""
        self.logger.info("Initializing COBOL analyzer agent")
        
        # Load custom patterns if configured
        if self.config.get('custom_patterns_file'):
            await self._load_custom_patterns()
            
        # Initialize analysis modules
        self._complexity_calculator = ComplexityCalculator()
        self._business_extractor = BusinessLogicExtractor()
        
    async def _shutdown(self) -> None:
        """Shutdown COBOL analyzer"""
        self.logger.info("Shutting down COBOL analyzer agent")
        
        # Clear cache
        self._analysis_cache.clear()
        
    async def _execute_task(self, task_type: str, params: Dict[str, Any], 
                           context: TaskContext) -> Any:
        """Execute COBOL analysis task"""
        
        if task_type == "analyze_program":
            return await self._analyze_program(params)
        elif task_type == "extract_structure":
            return await self._extract_structure(params)
        elif task_type == "calculate_complexity":
            return await self._calculate_complexity(params)
        elif task_type == "extract_business_logic":
            return await self._extract_business_logic(params)
        elif task_type == "assess_quality":
            return await self._assess_quality(params)
        elif task_type == "detect_patterns":
            return await self._detect_patterns(params)
        elif task_type == "analyze_dependencies":
            return await self._analyze_dependencies(params)
        elif task_type == "assess_modernization_readiness":
            return await self._assess_modernization_readiness(params)
        else:
            raise TaskExecutionError(f"Unknown task type: {task_type}")
    
    async def _analyze_program(self, params: Dict[str, Any]) -> Dict[str, Any]:
        """Comprehensive COBOL program analysis"""
        code = params.get('code', '')
        file_path = params.get('file_path', 'unknown')
        options = params.get('options', {})
        
        # Check cache
        cache_key = self._generate_cache_key(code)
        if cache_key in self._analysis_cache and not options.get('force_refresh'):
            self.logger.info(f"Returning cached analysis for {file_path}")
            return self._analysis_cache[cache_key]
        
        self.logger.info(f"Analyzing COBOL program: {file_path}")
        
        # Perform parallel analysis
        tasks = [
            self._extract_structure({'code': code}),
            self._calculate_complexity({'code': code}),
            self._extract_business_logic({'code': code}),
            self._analyze_dependencies({'code': code}),
            self._detect_patterns({'code': code})
        ]
        
        results = await asyncio.gather(*tasks, return_exceptions=True)
        
        # Handle any errors
        for i, result in enumerate(results):
            if isinstance(result, Exception):
                self.logger.error(f"Analysis task {i} failed: {result}")
                results[i] = {}
        
        # Combine results
        analysis = {
            'file_path': file_path,
            'structure': results[0],
            'complexity': results[1],
            'business_logic': results[2],
            'dependencies': results[3],
            'patterns': results[4],
            'timestamp': context.created_at.isoformat(),
            'analysis_duration': (context.completed_at - context.started_at).total_seconds() if context.completed_at else None
        }
        
        # Add quality assessment
        quality = await self._assess_quality({'analysis': analysis})
        analysis['quality'] = quality
        
        # Add modernization readiness
        readiness = await self._assess_modernization_readiness({'analysis': analysis})
        analysis['modernization_readiness'] = readiness
        
        # Cache results
        self._analysis_cache[cache_key] = analysis
        
        return analysis
    
    async def _extract_structure(self, params: Dict[str, Any]) -> Dict[str, Any]:
        """Extract COBOL program structure"""
        code = params.get('code', '')
        
        structure = COBOLStructure()
        lines = code.split('\n')
        
        current_division = None
        current_section = None
        current_paragraph = None
        line_number = 0
        
        for line_number, line in enumerate(lines, 1):
            # Skip comments
            if line.strip().startswith('*') or line[6:7] == '*':
                continue
                
            # Check for division
            division_match = self.patterns['division'].match(line)
            if division_match:
                current_division = {
                    'name': division_match.group(1),
                    'line': line_number,
                    'sections': []
                }
                structure.divisions.append(current_division)
                continue
            
            # Check for section
            section_match = self.patterns['section'].match(line)
            if section_match:
                current_section = {
                    'name': section_match.group(1),
                    'line': line_number,
                    'division': current_division['name'] if current_division else None,
                    'paragraphs': []
                }
                structure.sections.append(current_section)
                if current_division:
                    current_division['sections'].append(current_section['name'])
                continue
            
            # Check for paragraph (in PROCEDURE DIVISION)
            if current_division and current_division['name'] == 'PROCEDURE':
                paragraph_match = self.patterns['paragraph'].match(line)
                if paragraph_match and not any(keyword in line.upper() for keyword in ['DIVISION', 'SECTION']):
                    paragraph_name = paragraph_match.group(1)
                    if paragraph_name not in ['END-IF', 'END-PERFORM', 'END-EVALUATE']:
                        current_paragraph = {
                            'name': paragraph_name,
                            'line': line_number,
                            'section': current_section['name'] if current_section else None,
                            'statements': []
                        }
                        structure.paragraphs.append(current_paragraph)
                        if current_section:
                            current_section['paragraphs'].append(paragraph_name)
            
            # Extract data items (in DATA DIVISION)
            if current_division and current_division['name'] == 'DATA':
                data_match = self.patterns['data_level'].match(line)
                if data_match:
                    level = data_match.group(1)
                    name = data_match.group(2)
                    pic_match = self.patterns['pic_clause'].search(line)
                    
                    data_item = {
                        'level': level,
                        'name': name,
                        'line': line_number,
                        'picture': pic_match.group(1) if pic_match else None,
                        'section': current_section['name'] if current_section else None
                    }
                    structure.data_items.append(data_item)
            
            # Extract COPY statements
            copy_match = self.patterns['copy'].search(line)
            if copy_match:
                structure.copy_statements.append({
                    'name': copy_match.group(1),
                    'line': line_number,
                    'context': current_division['name'] if current_division else None
                })
            
            # Extract file operations
            file_op_match = self.patterns['file_op'].search(line)
            if file_op_match:
                structure.file_operations.append({
                    'operation': file_op_match.group(1),
                    'file': file_op_match.group(2),
                    'line': line_number,
                    'paragraph': current_paragraph['name'] if current_paragraph else None
                })
            
            # Extract database calls
            sql_match = self.patterns['sql'].search(line)
            if sql_match:
                structure.database_calls.append({
                    'type': 'SQL',
                    'statement': sql_match.group(1).strip(),
                    'line': line_number,
                    'paragraph': current_paragraph['name'] if current_paragraph else None
                })
            
            cics_match = self.patterns['cics'].search(line)
            if cics_match:
                structure.database_calls.append({
                    'type': 'CICS',
                    'command': cics_match.group(1).strip(),
                    'line': line_number,
                    'paragraph': current_paragraph['name'] if current_paragraph else None
                })
        
        return {
            'divisions': structure.divisions,
            'sections': structure.sections,
            'paragraphs': structure.paragraphs,
            'data_items': len(structure.data_items),
            'copy_statements': structure.copy_statements,
            'file_operations': self._summarize_file_operations(structure.file_operations),
            'database_calls': self._summarize_database_calls(structure.database_calls)
        }
    
    async def _calculate_complexity(self, params: Dict[str, Any]) -> Dict[str, Any]:
        """Calculate COBOL program complexity metrics"""
        code = params.get('code', '')
        
        metrics = ComplexityMetrics()
        lines = code.split('\n')
        
        # Basic metrics
        metrics.lines_of_code = len(lines)
        metrics.comment_lines = sum(1 for line in lines if line.strip().startswith('*') or (len(line) > 6 and line[6] == '*'))
        
        # Cyclomatic complexity
        metrics.cyclomatic_complexity = 1  # Base complexity
        
        # Count decision points
        for line in lines:
            if self.patterns['if_statement'].search(line):
                metrics.cyclomatic_complexity += 1
            elif 'WHEN' in line.upper():
                metrics.cyclomatic_complexity += 1
            elif 'PERFORM' in line.upper() and 'UNTIL' in line.upper():
                metrics.cyclomatic_complexity += 1
            elif 'EVALUATE' in line.upper():
                metrics.cyclomatic_complexity += 1
        
        # Cognitive complexity (simplified)
        nesting_level = 0
        max_nesting = 0
        cognitive_score = 0
        
        for line in lines:
            # Track nesting
            if any(keyword in line.upper() for keyword in ['IF', 'PERFORM', 'EVALUATE']):
                nesting_level += 1
                cognitive_score += nesting_level
                max_nesting = max(max_nesting, nesting_level)
            elif any(keyword in line.upper() for keyword in ['END-IF', 'END-PERFORM', 'END-EVALUATE']):
                nesting_level = max(0, nesting_level - 1)
        
        metrics.cognitive_complexity = cognitive_score
        metrics.nesting_depth = max_nesting
        
        # Data complexity (based on data items and operations)
        data_items = len(self.patterns['data_level'].findall(code))
        file_ops = len(self.patterns['file_op'].findall(code))
        db_calls = len(self.patterns['sql'].findall(code)) + len(self.patterns['cics'].findall(code))
        
        metrics.data_complexity = (data_items + file_ops * 2 + db_calls * 3) / max(metrics.lines_of_code, 1) * 100
        
        # Maintainability index (simplified Microsoft formula)
        # MI = 171 - 5.2 * ln(Halstead Volume) - 0.23 * (Cyclomatic Complexity) - 16.2 * ln(Lines of Code)
        import math
        halstead_volume = metrics.lines_of_code * math.log2(max(data_items + 10, 1))  # Simplified
        
        metrics.maintainability_index = max(
            0,
            min(100, 171 - 5.2 * math.log(halstead_volume) - 0.23 * metrics.cyclomatic_complexity - 16.2 * math.log(metrics.lines_of_code))
        )
        
        # Technical debt ratio
        if metrics.maintainability_index < 20:
            metrics.technical_debt_ratio = 0.9
        elif metrics.maintainability_index < 40:
            metrics.technical_debt_ratio = 0.7
        elif metrics.maintainability_index < 60:
            metrics.technical_debt_ratio = 0.5
        elif metrics.maintainability_index < 80:
            metrics.technical_debt_ratio = 0.3
        else:
            metrics.technical_debt_ratio = 0.1
        
        return {
            'lines_of_code': metrics.lines_of_code,
            'comment_lines': metrics.comment_lines,
            'comment_ratio': metrics.comment_lines / max(metrics.lines_of_code, 1),
            'cyclomatic_complexity': metrics.cyclomatic_complexity,
            'cognitive_complexity': metrics.cognitive_complexity,
            'max_nesting_depth': metrics.nesting_depth,
            'data_complexity': round(metrics.data_complexity, 2),
            'maintainability_index': round(metrics.maintainability_index, 2),
            'technical_debt_ratio': round(metrics.technical_debt_ratio, 2),
            'complexity_rating': self._rate_complexity(metrics)
        }
    
    async def _extract_business_logic(self, params: Dict[str, Any]) -> Dict[str, Any]:
        """Extract business logic and rules from COBOL"""
        code = params.get('code', '')
        
        rules = []
        rule_counter = 1
        
        # Extract calculations
        compute_matches = self.patterns['compute'].finditer(code)
        for match in compute_matches:
            rule = BusinessRule(
                rule_id=f"BR-CALC-{rule_counter:03d}",
                name=f"Calculation Rule {rule_counter}",
                type="calculation",
                description="Business calculation",
                location=self._get_location(code, match.start()),
                code_snippet=match.group(0),
                variables=self._extract_variables(match.group(0)),
                conditions=[],
                actions=[match.group(1).strip()],
                dependencies=[],
                confidence=0.9
            )
            rules.append(rule)
            rule_counter += 1
        
        # Extract validations (IF statements with business logic)
        if_matches = self.patterns['if_statement'].finditer(code)
        for match in if_matches:
            condition = match.group(1).strip()
            if self._is_business_validation(condition):
                rule = BusinessRule(
                    rule_id=f"BR-VAL-{rule_counter:03d}",
                    name=f"Validation Rule {rule_counter}",
                    type="validation",
                    description=f"Business validation: {self._describe_condition(condition)}",
                    location=self._get_location(code, match.start()),
                    code_snippet=match.group(0),
                    variables=self._extract_variables(condition),
                    conditions=[condition],
                    actions=self._extract_if_actions(code, match.end()),
                    dependencies=[],
                    confidence=0.85
                )
                rules.append(rule)
                rule_counter += 1
        
        # Extract workflows (PERFORM sequences)
        perform_matches = self.patterns['perform'].finditer(code)
        workflow_patterns = self._identify_workflows(list(perform_matches))
        
        for workflow in workflow_patterns:
            rule = BusinessRule(
                rule_id=f"BR-WFL-{rule_counter:03d}",
                name=f"Workflow {rule_counter}",
                type="workflow",
                description=f"Business process workflow",
                location=workflow['location'],
                code_snippet=workflow['code'],
                variables=[],
                conditions=workflow['conditions'],
                actions=workflow['steps'],
                dependencies=workflow['dependencies'],
                confidence=0.8
            )
            rules.append(rule)
            rule_counter += 1
        
        return {
            'total_rules': len(rules),
            'rules_by_type': self._categorize_rules(rules),
            'rules': [self._rule_to_dict(rule) for rule in rules],
            'business_complexity': self._calculate_business_complexity(rules)
        }
    
    async def _assess_quality(self, params: Dict[str, Any]) -> Dict[str, Any]:
        """Assess COBOL code quality"""
        analysis = params.get('analysis', {})
        
        quality_scores = {
            'readability': 0,
            'maintainability': 0,
            'testability': 0,
            'security': 0,
            'performance': 0,
            'documentation': 0
        }
        
        # Readability score
        complexity = analysis.get('complexity', {})
        if complexity.get('cognitive_complexity', 100) < 10:
            quality_scores['readability'] = 90
        elif complexity.get('cognitive_complexity', 100) < 20:
            quality_scores['readability'] = 70
        elif complexity.get('cognitive_complexity', 100) < 50:
            quality_scores['readability'] = 50
        else:
            quality_scores['readability'] = 30
        
        # Adjust for comments
        comment_ratio = complexity.get('comment_ratio', 0)
        if comment_ratio > 0.15:
            quality_scores['readability'] += 10
        
        # Maintainability score (from metrics)
        quality_scores['maintainability'] = complexity.get('maintainability_index', 50)
        
        # Testability score
        structure = analysis.get('structure', {})
        paragraphs = len(structure.get('paragraphs', []))
        if paragraphs > 0:
            avg_paragraph_size = complexity.get('lines_of_code', 0) / paragraphs
            if avg_paragraph_size < 50:
                quality_scores['testability'] = 80
            elif avg_paragraph_size < 100:
                quality_scores['testability'] = 60
            else:
                quality_scores['testability'] = 40
        
        # Security score (basic checks)
        security_issues = self._check_security_issues(analysis)
        quality_scores['security'] = max(0, 100 - len(security_issues) * 20)
        
        # Performance score
        performance_issues = self._check_performance_issues(analysis)
        quality_scores['performance'] = max(0, 100 - len(performance_issues) * 15)
        
        # Documentation score
        quality_scores['documentation'] = min(100, comment_ratio * 500)
        
        # Overall quality score
        overall_score = sum(quality_scores.values()) / len(quality_scores)
        
        return {
            'overall_score': round(overall_score, 2),
            'scores': quality_scores,
            'rating': self._get_quality_rating(overall_score),
            'issues': {
                'security': security_issues,
                'performance': performance_issues,
                'maintainability': self._get_maintainability_issues(complexity)
            },
            'recommendations': self._generate_quality_recommendations(quality_scores)
        }
    
    async def _detect_patterns(self, params: Dict[str, Any]) -> Dict[str, Any]:
        """Detect common COBOL patterns and anti-patterns"""
        code = params.get('code', '')
        
        patterns_found = {
            'design_patterns': [],
            'anti_patterns': [],
            'code_smells': []
        }
        
        # Design patterns
        if self._has_pattern(code, 'modular_structure'):
            patterns_found['design_patterns'].append({
                'name': 'Modular Structure',
                'description': 'Well-organized program with clear separation of concerns',
                'occurrences': 1,
                'confidence': 0.85
            })
        
        if self._has_pattern(code, 'error_handling'):
            patterns_found['design_patterns'].append({
                'name': 'Systematic Error Handling',
                'description': 'Consistent error handling throughout the program',
                'occurrences': len(re.findall(r'ON\s+ERROR', code, re.IGNORECASE)),
                'confidence': 0.8
            })
        
        # Anti-patterns
        goto_count = len(re.findall(r'GO\s+TO', code, re.IGNORECASE))
        if goto_count > 5:
            patterns_found['anti_patterns'].append({
                'name': 'Excessive GOTO Usage',
                'description': 'Overuse of GOTO statements leading to spaghetti code',
                'occurrences': goto_count,
                'severity': 'high',
                'recommendation': 'Refactor to use structured programming constructs'
            })
        
        # Code smells
        duplicate_code = self._detect_duplicate_code(code)
        if duplicate_code:
            patterns_found['code_smells'].append({
                'name': 'Duplicate Code',
                'description': 'Similar code blocks found in multiple locations',
                'locations': duplicate_code,
                'severity': 'medium',
                'recommendation': 'Extract common code into reusable paragraphs'
            })
        
        dead_code = self._detect_dead_code(code)
        if dead_code:
            patterns_found['code_smells'].append({
                'name': 'Dead Code',
                'description': 'Unreachable or unused code sections',
                'locations': dead_code,
                'severity': 'low',
                'recommendation': 'Remove unused code to improve maintainability'
            })
        
        return patterns_found
    
    async def _analyze_dependencies(self, params: Dict[str, Any]) -> Dict[str, Any]:
        """Analyze program dependencies"""
        code = params.get('code', '')
        
        dependencies = {
            'programs': [],
            'copybooks': [],
            'files': [],
            'databases': [],
            'external_systems': []
        }
        
        # Called programs
        call_matches = self.patterns['call'].finditer(code)
        for match in call_matches:
            program_name = match.group(1)
            dependencies['programs'].append({
                'name': program_name,
                'type': 'static_call',
                'location': self._get_location(code, match.start())
            })
        
        # Copybooks
        copy_matches = self.patterns['copy'].finditer(code)
        for match in copy_matches:
            copybook_name = match.group(1)
            dependencies['copybooks'].append({
                'name': copybook_name,
                'location': self._get_location(code, match.start())
            })
        
        # Files
        file_matches = self.patterns['file_op'].finditer(code)
        files_seen = set()
        for match in file_matches:
            file_name = match.group(2)
            if file_name not in files_seen:
                files_seen.add(file_name)
                dependencies['files'].append({
                    'name': file_name,
                    'operations': [match.group(1)]
                })
        
        # Database operations
        sql_matches = self.patterns['sql'].finditer(code)
        for match in sql_matches:
            sql_statement = match.group(1).strip()
            table_names = self._extract_table_names(sql_statement)
            for table in table_names:
                dependencies['databases'].append({
                    'type': 'SQL',
                    'table': table,
                    'operation': self._get_sql_operation(sql_statement)
                })
        
        # CICS operations
        cics_matches = self.patterns['cics'].finditer(code)
        for match in cics_matches:
            cics_command = match.group(1).strip()
            dependencies['external_systems'].append({
                'type': 'CICS',
                'command': cics_command.split()[0] if cics_command else 'UNKNOWN'
            })
        
        return {
            'summary': {
                'total_dependencies': sum(len(deps) for deps in dependencies.values()),
                'programs_called': len(dependencies['programs']),
                'copybooks_used': len(dependencies['copybooks']),
                'files_accessed': len(dependencies['files']),
                'database_operations': len(dependencies['databases']),
                'external_systems': len(dependencies['external_systems'])
            },
            'dependencies': dependencies,
            'dependency_graph': self._build_dependency_graph(dependencies)
        }
    
    async def _assess_modernization_readiness(self, params: Dict[str, Any]) -> Dict[str, Any]:
        """Assess readiness for modernization"""
        analysis = params.get('analysis', {})
        
        readiness_factors = {
            'complexity': self._assess_complexity_factor(analysis.get('complexity', {})),
            'dependencies': self._assess_dependency_factor(analysis.get('dependencies', {})),
            'quality': self._assess_quality_factor(analysis.get('quality', {})),
            'patterns': self._assess_pattern_factor(analysis.get('patterns', {})),
            'business_logic': self._assess_business_logic_factor(analysis.get('business_logic', {}))
        }
        
        # Calculate overall readiness score
        weights = {
            'complexity': 0.25,
            'dependencies': 0.20,
            'quality': 0.20,
            'patterns': 0.15,
            'business_logic': 0.20
        }
        
        overall_score = sum(
            readiness_factors[factor]['score'] * weights[factor]
            for factor in readiness_factors
        )
        
        # Determine modernization approach
        if overall_score >= 80:
            approach = "automated_transformation"
            confidence = "high"
        elif overall_score >= 60:
            approach = "semi_automated_transformation"
            confidence = "medium"
        elif overall_score >= 40:
            approach = "assisted_rewrite"
            confidence = "medium"
        else:
            approach = "manual_rewrite"
            confidence = "low"
        
        return {
            'overall_score': round(overall_score, 2),
            'readiness_level': self._get_readiness_level(overall_score),
            'factors': readiness_factors,
            'recommended_approach': approach,
            'confidence': confidence,
            'challenges': self._identify_modernization_challenges(analysis),
            'prerequisites': self._identify_prerequisites(readiness_factors),
            'estimated_effort': self._estimate_modernization_effort(analysis, overall_score)
        }
    
    # Helper methods
    
    def _generate_cache_key(self, code: str) -> str:
        """Generate cache key for code analysis"""
        import hashlib
        return hashlib.md5(code.encode()).hexdigest()
    
    def _summarize_file_operations(self, operations: List[Dict[str, Any]]) -> Dict[str, Any]:
        """Summarize file operations"""
        summary = defaultdict(list)
        for op in operations:
            summary[op['file']].append(op['operation'])
        
        return {
            'files_accessed': len(summary),
            'operations': dict(summary),
            'total_operations': len(operations)
        }
    
    def _summarize_database_calls(self, calls: List[Dict[str, Any]]) -> Dict[str, Any]:
        """Summarize database calls"""
        summary = {
            'sql_calls': sum(1 for c in calls if c['type'] == 'SQL'),
            'cics_calls': sum(1 for c in calls if c['type'] == 'CICS'),
            'total_calls': len(calls)
        }
        return summary
    
    def _rate_complexity(self, metrics: ComplexityMetrics) -> str:
        """Rate overall complexity"""
        if metrics.cyclomatic_complexity < 10 and metrics.cognitive_complexity < 15:
            return "low"
        elif metrics.cyclomatic_complexity < 20 and metrics.cognitive_complexity < 30:
            return "medium"
        elif metrics.cyclomatic_complexity < 50 and metrics.cognitive_complexity < 75:
            return "high"
        else:
            return "very_high"
    
    def _get_location(self, code: str, position: int) -> Dict[str, Any]:
        """Get line number from string position"""
        line_number = code[:position].count('\n') + 1
        return {
            'line': line_number,
            'position': position
        }
    
    def _extract_variables(self, code_snippet: str) -> List[Dict[str, Any]]:
        """Extract variables from code snippet"""
        # Simplified variable extraction
        variables = []
        # This would need more sophisticated parsing in production
        words = re.findall(r'\b[A-Z][\w-]*\b', code_snippet)
        for word in words:
            if not word in ['IF', 'THEN', 'ELSE', 'END', 'COMPUTE', 'MOVE', 'TO']:
                variables.append({
                    'name': word,
                    'type': 'unknown'
                })
        return variables
    
    def _is_business_validation(self, condition: str) -> bool:
        """Check if condition represents business validation"""
        business_keywords = [
            'VALID', 'CHECK', 'VERIFY', 'AMOUNT', 'DATE', 'RATE',
            'BALANCE', 'TOTAL', 'LIMIT', 'RANGE', 'STATUS'
        ]
        return any(keyword in condition.upper() for keyword in business_keywords)
    
    def _describe_condition(self, condition: str) -> str:
        """Generate human-readable description of condition"""
        # Simplified - would need NLP in production
        return f"Validates that {condition.lower()}"
    
    def _extract_if_actions(self, code: str, start_pos: int) -> List[str]:
        """Extract actions from IF statement"""
        # Find the matching END-IF
        end_match = re.search(r'END-IF', code[start_pos:], re.IGNORECASE)
        if end_match:
            action_code = code[start_pos:start_pos + end_match.start()]
            # Extract individual statements
            return [line.strip() for line in action_code.split('\n') if line.strip() and not line.strip().startswith('*')]
        return []
    
    def _identify_workflows(self, perform_matches) -> List[Dict[str, Any]]:
        """Identify workflow patterns from PERFORM statements"""
        workflows = []
        # Group related PERFORMs
        # This is simplified - production would need more sophisticated analysis
        return workflows
    
    def _categorize_rules(self, rules: List[BusinessRule]) -> Dict[str, int]:
        """Categorize rules by type"""
        categories = defaultdict(int)
        for rule in rules:
            categories[rule.type] += 1
        return dict(categories)
    
    def _rule_to_dict(self, rule: BusinessRule) -> Dict[str, Any]:
        """Convert BusinessRule to dictionary"""
        return {
            'rule_id': rule.rule_id,
            'name': rule.name,
            'type': rule.type,
            'description': rule.description,
            'location': rule.location,
            'confidence': rule.confidence
        }
    
    def _calculate_business_complexity(self, rules: List[BusinessRule]) -> float:
        """Calculate business logic complexity"""
        if not rules:
            return 0.0
        
        complexity_weights = {
            'calculation': 1.0,
            'validation': 0.8,
            'workflow': 1.5
        }
        
        total_complexity = sum(
            complexity_weights.get(rule.type, 1.0) * rule.confidence
            for rule in rules
        )
        
        return round(total_complexity / len(rules), 2)
    
    def _check_security_issues(self, analysis: Dict[str, Any]) -> List[Dict[str, Any]]:
        """Check for security issues"""
        issues = []
        # Check for hardcoded values, SQL injection risks, etc.
        # Simplified for example
        return issues
    
    def _check_performance_issues(self, analysis: Dict[str, Any]) -> List[Dict[str, Any]]:
        """Check for performance issues"""
        issues = []
        
        # Check for nested loops
        complexity = analysis.get('complexity', {})
        if complexity.get('max_nesting_depth', 0) > 3:
            issues.append({
                'type': 'deep_nesting',
                'description': 'Deep nesting may impact performance',
                'severity': 'medium'
            })
        
        return issues
    
    def _get_maintainability_issues(self, complexity: Dict[str, Any]) -> List[Dict[str, Any]]:
        """Get maintainability issues from complexity metrics"""
        issues = []
        
        if complexity.get('cyclomatic_complexity', 0) > 20:
            issues.append({
                'type': 'high_complexity',
                'description': 'High cyclomatic complexity makes maintenance difficult',
                'severity': 'high'
            })
        
        if complexity.get('lines_of_code', 0) > 1000:
            issues.append({
                'type': 'large_program',
                'description': 'Large program size impacts maintainability',
                'severity': 'medium'
            })
        
        return issues
    
    def _generate_quality_recommendations(self, scores: Dict[str, float]) -> List[str]:
        """Generate quality improvement recommendations"""
        recommendations = []
        
        if scores['readability'] < 60:
            recommendations.append("Improve code readability by adding comments and reducing complexity")
        
        if scores['maintainability'] < 60:
            recommendations.append("Refactor complex sections into smaller, focused paragraphs")
        
        if scores['testability'] < 60:
            recommendations.append("Break down large paragraphs for better testability")
        
        if scores['security'] < 80:
            recommendations.append("Review and address security vulnerabilities")
        
        if scores['performance'] < 70:
            recommendations.append("Optimize performance-critical sections")
        
        return recommendations
    
    def _get_quality_rating(self, score: float) -> str:
        """Get quality rating from score"""
        if score >= 80:
            return "excellent"
        elif score >= 60:
            return "good"
        elif score >= 40:
            return "fair"
        else:
            return "poor"
    
    def _has_pattern(self, code: str, pattern_name: str) -> bool:
        """Check if code has specific pattern"""
        # Simplified pattern detection
        pattern_checks = {
            'modular_structure': lambda c: len(re.findall(r'PERFORM\s+\w+', c, re.IGNORECASE)) > 5,
            'error_handling': lambda c: 'ON ERROR' in c.upper() or 'ERROR-ROUTINE' in c.upper()
        }
        
        check = pattern_checks.get(pattern_name)
        return check(code) if check else False
    
    def _detect_duplicate_code(self, code: str) -> List[Dict[str, Any]]:
        """Detect duplicate code blocks"""
        # Simplified - production would use more sophisticated algorithms
        duplicates = []
        return duplicates
    
    def _detect_dead_code(self, code: str) -> List[Dict[str, Any]]:
        """Detect dead/unreachable code"""
        dead_code = []
        # Check for unreferenced paragraphs
        # Simplified for example
        return dead_code
    
    def _extract_table_names(self, sql: str) -> List[str]:
        """Extract table names from SQL"""
        # Simplified SQL parsing
        tables = []
        from_match = re.search(r'FROM\s+(\w+)', sql, re.IGNORECASE)
        if from_match:
            tables.append(from_match.group(1))
        return tables
    
    def _get_sql_operation(self, sql: str) -> str:
        """Get SQL operation type"""
        sql_upper = sql.upper()
        if 'SELECT' in sql_upper:
            return 'SELECT'
        elif 'INSERT' in sql_upper:
            return 'INSERT'
        elif 'UPDATE' in sql_upper:
            return 'UPDATE'
        elif 'DELETE' in sql_upper:
            return 'DELETE'
        else:
            return 'OTHER'
    
    def _build_dependency_graph(self, dependencies: Dict[str, List[Any]]) -> Dict[str, Any]:
        """Build dependency graph representation"""
        # Simplified graph structure
        nodes = []
        edges = []
        
        # Add program as central node
        nodes.append({
            'id': 'main',
            'type': 'program',
            'label': 'Main Program'
        })
        
        # Add dependency nodes
        for dep_type, items in dependencies.items():
            for item in items:
                node_id = f"{dep_type}_{item.get('name', 'unknown')}"
                nodes.append({
                    'id': node_id,
                    'type': dep_type,
                    'label': item.get('name', 'unknown')
                })
                edges.append({
                    'from': 'main',
                    'to': node_id
                })
        
        return {
            'nodes': nodes,
            'edges': edges
        }
    
    def _assess_complexity_factor(self, complexity: Dict[str, Any]) -> Dict[str, Any]:
        """Assess complexity factor for modernization"""
        score = 100
        
        if complexity.get('cyclomatic_complexity', 0) > 50:
            score -= 30
        elif complexity.get('cyclomatic_complexity', 0) > 20:
            score -= 15
        
        if complexity.get('lines_of_code', 0) > 2000:
            score -= 20
        elif complexity.get('lines_of_code', 0) > 1000:
            score -= 10
        
        return {
            'score': max(0, score),
            'description': 'Code complexity assessment',
            'issues': self._get_complexity_issues(complexity)
        }
    
    def _get_complexity_issues(self, complexity: Dict[str, Any]) -> List[str]:
        """Get complexity-related issues"""
        issues = []
        
        if complexity.get('cyclomatic_complexity', 0) > 20:
            issues.append("High cyclomatic complexity")
        
        if complexity.get('cognitive_complexity', 0) > 30:
            issues.append("High cognitive complexity")
        
        if complexity.get('max_nesting_depth', 0) > 4:
            issues.append("Deep nesting levels")
        
        return issues
    
    def _assess_dependency_factor(self, dependencies: Dict[str, Any]) -> Dict[str, Any]:
        """Assess dependency factor for modernization"""
        score = 100
        summary = dependencies.get('summary', {})
        
        # Many external dependencies reduce score
        total_deps = summary.get('total_dependencies', 0)
        if total_deps > 50:
            score -= 30
        elif total_deps > 20:
            score -= 15
        
        # CICS/IMS dependencies are complex
        if summary.get('external_systems', 0) > 0:
            score -= 20
        
        return {
            'score': max(0, score),
            'description': 'External dependency assessment',
            'issues': self._get_dependency_issues(dependencies)
        }
    
    def _get_dependency_issues(self, dependencies: Dict[str, Any]) -> List[str]:
        """Get dependency-related issues"""
        issues = []
        summary = dependencies.get('summary', {})
        
        if summary.get('programs_called', 0) > 10:
            issues.append("Many program dependencies")
        
        if summary.get('external_systems', 0) > 0:
            issues.append("External system dependencies (CICS/IMS)")
        
        return issues
    
    def _assess_quality_factor(self, quality: Dict[str, Any]) -> Dict[str, Any]:
        """Assess quality factor for modernization"""
        overall_score = quality.get('overall_score', 50)
        
        return {
            'score': overall_score,
            'description': 'Code quality assessment',
            'issues': self._get_quality_issues(quality)
        }
    
    def _get_quality_issues(self, quality: Dict[str, Any]) -> List[str]:
        """Get quality-related issues"""
        issues = []
        scores = quality.get('scores', {})
        
        for metric, score in scores.items():
            if score < 50:
                issues.append(f"Low {metric} score")
        
        return issues
    
    def _assess_pattern_factor(self, patterns: Dict[str, Any]) -> Dict[str, Any]:
        """Assess pattern factor for modernization"""
        score = 80  # Base score
        
        # Anti-patterns reduce score
        anti_patterns = patterns.get('anti_patterns', [])
        score -= len(anti_patterns) * 10
        
        # Code smells reduce score
        code_smells = patterns.get('code_smells', [])
        score -= len(code_smells) * 5
        
        # Good patterns increase score
        design_patterns = patterns.get('design_patterns', [])
        score += len(design_patterns) * 5
        
        return {
            'score': max(0, min(100, score)),
            'description': 'Code pattern assessment',
            'issues': self._get_pattern_issues(patterns)
        }
    
    def _get_pattern_issues(self, patterns: Dict[str, Any]) -> List[str]:
        """Get pattern-related issues"""
        issues = []
        
        for anti_pattern in patterns.get('anti_patterns', []):
            issues.append(f"Anti-pattern: {anti_pattern['name']}")
        
        for smell in patterns.get('code_smells', []):
            issues.append(f"Code smell: {smell['name']}")
        
        return issues
    
    def _assess_business_logic_factor(self, business_logic: Dict[str, Any]) -> Dict[str, Any]:
        """Assess business logic factor for modernization"""
        total_rules = business_logic.get('total_rules', 0)
        complexity = business_logic.get('business_complexity', 0)
        
        score = 100
        
        # Complex business logic reduces score
        if complexity > 2.0:
            score -= 30
        elif complexity > 1.5:
            score -= 15
        
        # Many rules can be good (well-documented) or bad (complex)
        if total_rules > 50 and complexity > 1.5:
            score -= 20
        
        return {
            'score': max(0, score),
            'description': 'Business logic clarity assessment',
            'issues': self._get_business_logic_issues(business_logic)
        }
    
    def _get_business_logic_issues(self, business_logic: Dict[str, Any]) -> List[str]:
        """Get business logic issues"""
        issues = []
        
        if business_logic.get('business_complexity', 0) > 1.5:
            issues.append("Complex business logic")
        
        rules_by_type = business_logic.get('rules_by_type', {})
        if rules_by_type.get('workflow', 0) > 10:
            issues.append("Complex workflows")
        
        return issues
    
    def _get_readiness_level(self, score: float) -> str:
        """Get readiness level from score"""
        if score >= 80:
            return "high"
        elif score >= 60:
            return "medium"
        elif score >= 40:
            return "low"
        else:
            return "very_low"
    
    def _identify_modernization_challenges(self, analysis: Dict[str, Any]) -> List[Dict[str, Any]]:
        """Identify specific modernization challenges"""
        challenges = []
        
        # Check complexity
        complexity = analysis.get('complexity', {})
        if complexity.get('cyclomatic_complexity', 0) > 50:
            challenges.append({
                'category': 'complexity',
                'description': 'Very high program complexity requires careful decomposition',
                'severity': 'high',
                'mitigation': 'Consider breaking into multiple services'
            })
        
        # Check dependencies
        dependencies = analysis.get('dependencies', {}).get('summary', {})
        if dependencies.get('external_systems', 0) > 0:
            challenges.append({
                'category': 'integration',
                'description': 'External system dependencies (CICS/IMS) require special handling',
                'severity': 'high',
                'mitigation': 'Plan for API gateway or adapter pattern'
            })
        
        # Check patterns
        patterns = analysis.get('patterns', {})
        if len(patterns.get('anti_patterns', [])) > 2:
            challenges.append({
                'category': 'code_quality',
                'description': 'Multiple anti-patterns detected',
                'severity': 'medium',
                'mitigation': 'Refactor before transformation'
            })
        
        return challenges
    
    def _identify_prerequisites(self, factors: Dict[str, Dict[str, Any]]) -> List[str]:
        """Identify prerequisites for modernization"""
        prerequisites = []
        
        for factor_name, factor_data in factors.items():
            if factor_data['score'] < 40:
                if factor_name == 'complexity':
                    prerequisites.append("Reduce code complexity through refactoring")
                elif factor_name == 'dependencies':
                    prerequisites.append("Document and analyze all external dependencies")
                elif factor_name == 'quality':
                    prerequisites.append("Improve code quality and add documentation")
                elif factor_name == 'patterns':
                    prerequisites.append("Address anti-patterns and code smells")
                elif factor_name == 'business_logic':
                    prerequisites.append("Document and validate business rules")
        
        return prerequisites
    
    def _estimate_modernization_effort(self, analysis: Dict[str, Any], readiness_score: float) -> Dict[str, Any]:
        """Estimate modernization effort"""
        complexity = analysis.get('complexity', {})
        loc = complexity.get('lines_of_code', 0)
        
        # Base effort calculation (person-days)
        base_effort = loc / 50  # Assume 50 LOC per day for transformation
        
        # Adjust for readiness
        if readiness_score >= 80:
            effort_multiplier = 1.0
        elif readiness_score >= 60:
            effort_multiplier = 1.5
        elif readiness_score >= 40:
            effort_multiplier = 2.5
        else:
            effort_multiplier = 4.0
        
        adjusted_effort = base_effort * effort_multiplier
        
        # Break down by phase
        phases = {
            'analysis': adjusted_effort * 0.15,
            'design': adjusted_effort * 0.20,
            'transformation': adjusted_effort * 0.35,
            'testing': adjusted_effort * 0.20,
            'deployment': adjusted_effort * 0.10
        }
        
        return {
            'total_effort_days': round(adjusted_effort, 1),
            'effort_range': {
                'min': round(adjusted_effort * 0.8, 1),
                'max': round(adjusted_effort * 1.3, 1)
            },
            'phases': {k: round(v, 1) for k, v in phases.items()},
            'confidence': 'high' if readiness_score >= 70 else 'medium' if readiness_score >= 50 else 'low'
        }


# Supporting classes

class ComplexityCalculator:
    """Helper class for complexity calculations"""
    pass


class BusinessLogicExtractor:
    """Helper class for business logic extraction"""
    pass 