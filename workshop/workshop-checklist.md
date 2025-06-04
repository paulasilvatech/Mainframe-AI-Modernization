# ✅ Workshop Checklist

Use this comprehensive checklist to ensure you're fully prepared for the Mainframe Modernization Workshop and to track your progress throughout the journey.

## 📋 Table of Contents

- [Pre-Workshop Preparation](#-pre-workshop-preparation)
- [Technical Setup](#-technical-setup)
- [Azure Configuration](#-azure-configuration)
- [Workshop Materials](#-workshop-materials)
- [During the Workshop](#-during-the-workshop)
- [Post-Workshop Actions](#-post-workshop-actions)
- [Troubleshooting Checklist](#-troubleshooting-checklist)

## 📝 Pre-Workshop Preparation

### Knowledge Prerequisites

- [ ] **Mainframe Basics**
  - [ ] Understanding of COBOL/Natural syntax
  - [ ] Familiarity with JCL concepts
  - [ ] Basic knowledge of mainframe file systems

- [ ] **Modern Development**
  - [ ] Basic programming concepts
  - [ ] Understanding of APIs and microservices
  - [ ] Familiarity with version control (Git)

- [ ] **Cloud Concepts**
  - [ ] Understanding of cloud vs on-premise
  - [ ] Basic knowledge of containers
  - [ ] Familiarity with CI/CD concepts

### Administrative Tasks

- [ ] **Registration**
  - [ ] Workshop registration completed
  - [ ] Calendar blocked for workshop dates
  - [ ] Manager approval (if required)
  - [ ] Team notified of absence

- [ ] **Accounts Setup**
  - [ ] GitHub account created/verified
  - [ ] Azure account activated
  - [ ] Slack workspace joined
  - [ ] Email notifications configured

## 💻 Technical Setup

### Hardware Requirements

- [ ] **System Specifications**
  - [ ] Minimum 8GB RAM available
  - [ ] 20GB free disk space
  - [ ] Stable internet connection tested
  - [ ] Webcam/microphone working (for live sessions)

### Software Installation

- [ ] **Core Tools**
  ```bash
  # Check versions after installation
  - [ ] Python 3.8+ installed
        python --version
  
  - [ ] Docker Desktop installed and running
        docker --version
        docker ps
  
  - [ ] Git installed
        git --version
  
  - [ ] VS Code or preferred IDE
        code --version
  ```

- [ ] **Command Line Tools**
  ```bash
  - [ ] Azure CLI
        az --version
  
  - [ ] GitHub CLI (optional)
        gh --version
  
  - [ ] curl/wget available
        curl --version
  ```

### Development Environment

- [ ] **Python Environment**
  ```bash
  - [ ] Virtual environment created
        python -m venv workshop-env
  
  - [ ] Virtual environment activated
        source workshop-env/bin/activate  # Linux/Mac
        .\workshop-env\Scripts\activate   # Windows
  
  - [ ] Dependencies installed
        pip install -r requirements.txt
  ```

- [ ] **IDE Configuration**
  - [ ] VS Code extensions installed
    - [ ] Python extension
    - [ ] Docker extension
    - [ ] COBOL extension
    - [ ] GitLens
  - [ ] IDE configured for Python development
  - [ ] Terminal/shell configured

### Repository Setup

- [ ] **Workshop Repository**
  ```bash
  - [ ] Repository cloned
        git clone https://github.com/mainframe-modernization/workshops.git
  
  - [ ] On correct branch
        cd workshops
        git checkout main
  
  - [ ] Latest updates pulled
        git pull origin main
  ```

## ☁️ Azure Configuration

### Azure Account

- [ ] **Subscription Setup**
  - [ ] Azure subscription active
  - [ ] Free credits available/payment method added
  - [ ] Subscription ID noted
  - [ ] Resource group created for workshop

### Service Principal

- [ ] **Authentication Setup**
  ```bash
  - [ ] Service principal created
        az ad sp create-for-rbac --name "workshop-sp"
  
  - [ ] Credentials saved securely
  - [ ] Environment variables configured
  - [ ] Connection tested
        az login --service-principal
  ```

### Required Services

- [ ] **Azure Services Enabled**
  - [ ] Azure OpenAI Service
    - [ ] Access requested/approved
    - [ ] Deployment created
    - [ ] API key obtained
  - [ ] Azure Container Registry
  - [ ] Azure Key Vault
  - [ ] Azure Monitor

### Cost Management

- [ ] **Budget Controls**
  - [ ] Cost alerts configured
  - [ ] Spending limit set
  - [ ] Resource tagging planned
  - [ ] Auto-shutdown configured for VMs

## 📚 Workshop Materials

### Documentation

- [ ] **Essential Reading**
  - [ ] [README.md](README.md) reviewed
  - [ ] [QUICK_START.md](QUICK_START.md) completed
  - [ ] [intro-mainframe-modernization.md](intro-mainframe-modernization.md) read
  - [ ] Workshop module READMEs reviewed

### Sample Data

- [ ] **Test Data Prepared**
  ```bash
  - [ ] Sample COBOL programs downloaded
        ./scripts/download-samples.sh
  
  - [ ] Test databases initialized
        docker-compose up -d test-db
  
  - [ ] Sample data loaded
        ./scripts/load-test-data.sh
  ```

### Tools Verification

- [ ] **Workshop Scripts**
  ```bash
  - [ ] Environment verification passed
        python scripts/verify_setup.py
  
  - [ ] Docker images pulled
        docker pull workshop/mainframe-env:latest
  
  - [ ] Agent framework tested
        python test_agent_framework.py
  ```

## 🎯 During the Workshop

### Daily Checklist

#### Day 1
- [ ] **Morning Session**
  - [ ] Introduction module completed
  - [ ] Environment double-checked
  - [ ] First lab started
  - [ ] Questions noted

- [ ] **Afternoon Session**
  - [ ] Lab 1 completed
  - [ ] Lab 2 started
  - [ ] Code committed to Git
  - [ ] Progress tracked

#### Day 2
- [ ] **Morning Session**
  - [ ] Previous day's work reviewed
  - [ ] Lab 2 completed
  - [ ] Lab 3 started
  - [ ] Challenges documented

- [ ] **Afternoon Session**
  - [ ] All labs completed
  - [ ] Deployment exercise done
  - [ ] Final project started
  - [ ] Feedback provided

### Best Practices

- [ ] **During Labs**
  - [ ] Read instructions completely first
  - [ ] Take notes on key concepts
  - [ ] Save work frequently
  - [ ] Ask questions when stuck
  - [ ] Help other participants

- [ ] **Code Management**
  - [ ] Commit code after each lab
  - [ ] Use meaningful commit messages
  - [ ] Create branches for experiments
  - [ ] Document any deviations

### Troubleshooting Quick Checks

- [ ] **If Something Fails**
  - [ ] Check error messages carefully
  - [ ] Verify environment variables
  - [ ] Ensure services are running
  - [ ] Consult [TROUBLESHOOTING.md](TROUBLESHOOTING.md)
  - [ ] Ask for help in Slack

## 📈 Progress Tracking

### Module Completion

- [ ] **Introduction Module** (4 hours)
  - [ ] Concepts understood
  - [ ] Quiz completed
  - [ ] Certificate generated

- [ ] **COBOL Workshop** (if taken)
  - [ ] Lab 1: Environment Setup ✓
  - [ ] Lab 2: Code Analysis ✓
  - [ ] Lab 3: Transformation ✓
  - [ ] Lab 4: Testing ✓
  - [ ] Lab 5: Deployment ✓

- [ ] **Natural Workshop** (if taken)
  - [ ] Lab 1: DDM Analysis ✓
  - [ ] Lab 2: Schema Design ✓
  - [ ] Lab 3: Data Migration ✓
  - [ ] Lab 4: Application Build ✓
  - [ ] Lab 5: Integration ✓

### Skills Acquired

- [ ] **Technical Skills**
  - [ ] AI agent configuration
  - [ ] Code transformation techniques
  - [ ] Testing strategies
  - [ ] Deployment automation
  - [ ] Monitoring setup

- [ ] **Soft Skills**
  - [ ] Modernization planning
  - [ ] Risk assessment
  - [ ] Stakeholder communication
  - [ ] Change management

## 🎉 Post-Workshop Actions

### Immediate Actions (Week 1)

- [ ] **Knowledge Consolidation**
  - [ ] Review all lab solutions
  - [ ] Complete any missed exercises
  - [ ] Document lessons learned
  - [ ] Share feedback via survey

- [ ] **Resource Cleanup**
  - [ ] Stop/delete Azure resources
  - [ ] Archive workshop materials
  - [ ] Save important code snippets
  - [ ] Update resume/LinkedIn

### Short-term Actions (Month 1)

- [ ] **Apply Learning**
  - [ ] Identify pilot project at work
  - [ ] Create modernization proposal
  - [ ] Share knowledge with team
  - [ ] Join community discussions

- [ ] **Continued Learning**
  - [ ] Read recommended resources
  - [ ] Watch supplementary videos
  - [ ] Attend office hours
  - [ ] Contribute to workshop improvements

### Long-term Actions (Quarter 1)

- [ ] **Project Implementation**
  - [ ] Start pilot modernization
  - [ ] Apply workshop patterns
  - [ ] Measure results
  - [ ] Share success stories

- [ ] **Community Engagement**
  - [ ] Present at user group
  - [ ] Write blog post
  - [ ] Mentor new participants
  - [ ] Contribute code examples

## 🔧 Troubleshooting Checklist

### Common Issues Quick Fix

- [ ] **Docker Issues**
  ```bash
  # Restart Docker
  - [ ] Docker Desktop restarted
  - [ ] Containers cleaned up
        docker system prune -a
  - [ ] Images re-pulled
  ```

- [ ] **Azure Connection**
  ```bash
  # Re-authenticate
  - [ ] Logged out and back in
        az logout
        az login
  - [ ] Service principal recreated
  - [ ] Firewall rules checked
  ```

- [ ] **Python Environment**
  ```bash
  # Rebuild environment
  - [ ] Virtual env deleted and recreated
  - [ ] All packages reinstalled
  - [ ] Python version verified
  ```

## 📊 Final Verification

### Workshop Readiness Score

Calculate your readiness (1 point per checked item):

| Category | Items | Checked | Score |
|----------|-------|---------|-------|
| Pre-Workshop | 10 | ___ | ___/10 |
| Technical Setup | 20 | ___ | ___/20 |
| Azure Config | 15 | ___ | ___/15 |
| Materials | 10 | ___ | ___/10 |
| **Total** | **55** | **___** | **___/55** |

**Readiness Level:**
- 50-55: Fully prepared! 🌟
- 40-49: Almost ready 👍
- 30-39: Some work needed ⚠️
- <30: Need significant preparation 🚨

## 💡 Pro Tips

1. **Start Early**: Begin setup at least 3 days before workshop
2. **Test Everything**: Run all verification scripts
3. **Have Backups**: Alternative internet, power bank, etc.
4. **Stay Organized**: Use provided folder structure
5. **Network**: Connect with other participants early

---

<div align="center">

**📋 Checklist Complete?**

[🚀 **Start Workshop**](README.md) | [❓ **Check FAQ**](FAQ.md) | [🔧 **Troubleshooting**](TROUBLESHOOTING.md)

*Remember: Preparation is key to success!*

</div> 