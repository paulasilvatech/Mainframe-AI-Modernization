# ❓ Frequently Asked Questions

Find answers to common questions about the Mainframe Modernization Workshop.

## 📋 Table of Contents

- [General Questions](#-general-questions)
- [Technical Requirements](#-technical-requirements)
- [Workshop Content](#-workshop-content)
- [Azure and Cloud](#-azure-and-cloud)
- [Agent Framework](#-agent-framework)
- [COBOL Modernization](#-cobol-modernization)
- [Natural/Adabas Migration](#-naturaladabas-migration)
- [Certification and Credits](#-certification-and-credits)
- [Support and Community](#-support-and-community)

## 🌟 General Questions

### Q: What is the Mainframe Modernization Workshop?

**A:** The Mainframe Modernization Workshop is a comprehensive hands-on training series that teaches you how to modernize legacy mainframe applications using AI agents, Azure AI Platform, and modern DevOps practices. It covers COBOL, Natural, and other mainframe languages, providing practical experience in transformation, testing, and deployment.

### Q: Who should attend this workshop?

**A:** This workshop is designed for:
- Mainframe developers and administrators
- DevOps engineers working on legacy systems
- Cloud architects planning modernization projects
- Technical leads managing transformation initiatives
- Anyone interested in AI-driven code modernization

### Q: How long does the workshop take?

**A:** The complete workshop series includes:
- **Introduction Module**: 4 hours
- **COBOL Workshop**: 2 days (16 hours)
- **Natural/Adabas Workshop**: 2 days (16 hours)
- **Total**: Approximately 36 hours of content

You can complete modules independently based on your needs.

### Q: Is this workshop free?

**A:** Yes, the workshop materials are free and open-source. However, you'll need:
- An Azure subscription (free tier available)
- GitHub account (free)
- Local development tools (mostly free/open-source)

### Q: Can I do the workshop self-paced?

**A:** Absolutely! All materials are designed for self-paced learning. You can:
- Start and stop anytime
- Skip modules not relevant to you
- Repeat exercises as needed
- Access materials indefinitely

## 💻 Technical Requirements

### Q: What are the minimum system requirements?

**A:** You'll need:
- **RAM**: Minimum 8GB (16GB recommended)
- **Storage**: 20GB free space
- **OS**: Windows 10/11, macOS 10.15+, or Linux (Ubuntu 20.04+)
- **Internet**: Stable broadband connection

### Q: Do I need a powerful GPU?

**A:** No, GPU is not required. All AI processing happens in the cloud through Azure AI services.

### Q: Can I use a cloud VM instead of my local machine?

**A:** Yes! We recommend:
- Azure VM (Standard D4s v3 or better)
- AWS EC2 (t3.xlarge or better)
- Google Cloud (n2-standard-4 or better)

See our [Cloud VM Setup Guide](resources/cloud-vm-setup.md) for details.

### Q: What programming languages do I need to know?

**A:** You should have:
- **Required**: Basic understanding of any programming language
- **Helpful**: COBOL or Natural knowledge (for respective workshops)
- **Beneficial**: Python basics (for agent framework)
- **Nice to have**: Java or C# (for modernized code)

## 📚 Workshop Content

### Q: Can I skip the introduction module?

**A:** If you're already familiar with:
- Mainframe modernization concepts
- AI and agent-based approaches
- Azure AI Platform basics

Then yes, you can jump directly to the hands-on workshops.

### Q: Are the workshops updated regularly?

**A:** Yes, we update workshops:
- **Monthly**: Bug fixes and minor improvements
- **Quarterly**: New features and tools
- **Annually**: Major content revisions

Follow our [changelog](CHANGELOG.md) for updates.

### Q: Can I contribute improvements?

**A:** Absolutely! We welcome contributions:
- Report issues on GitHub
- Submit pull requests
- Share your experiences
- Contribute new examples

See our [Contributing Guide](../CONTRIBUTING.md).

### Q: Are workshop recordings available?

**A:** Currently, we provide:
- Written tutorials and guides
- Code examples and solutions
- Architecture diagrams
- Community-contributed videos (unofficial)

Official video content is planned for the future.

## ☁️ Azure and Cloud

### Q: How much will Azure services cost?

**A:** Estimated costs for workshop completion:
- **Free tier eligible**: Most services
- **AI services**: ~$10-50 depending on usage
- **Storage**: <$5
- **Compute**: Can use free credits

Tips to minimize costs:
- Use Azure free tier
- Stop resources when not in use
- Use provided cost management scripts

### Q: Can I use AWS or Google Cloud instead?

**A:** The workshops are designed for Azure, but concepts apply to other clouds:
- **AWS**: Use Bedrock instead of Azure OpenAI
- **Google Cloud**: Use Vertex AI
- **Generic**: Use OpenAI API directly

You'll need to adapt the code examples.

### Q: Do I need an enterprise Azure account?

**A:** No, a personal Azure account works fine. You'll need:
- Valid credit card (for verification)
- Azure free credits ($200 for new accounts)
- No special enterprise features required

### Q: What Azure services are used?

**A:** Primary services include:
- Azure OpenAI Service
- Azure Container Instances
- Azure Storage
- Azure Key Vault
- Azure Monitor
- Azure DevOps (optional)

## 🤖 Agent Framework

### Q: What is the agent framework?

**A:** The agent framework is a production-ready system for:
- Orchestrating multiple AI agents
- Managing agent communication
- Handling state and memory
- Integrating with mainframe systems
- Providing observability and monitoring

### Q: Can I use different AI models?

**A:** Yes! The framework supports:
- **Azure OpenAI**: GPT-4, GPT-3.5
- **OpenAI API**: Direct integration
- **Anthropic**: Claude models
- **Open source**: LLaMA, Mistral (self-hosted)

### Q: How do agents communicate?

**A:** Agents communicate through:
- **MCP (Model Context Protocol)**: Primary protocol
- **REST APIs**: HTTP-based communication
- **Message queues**: Redis pub/sub
- **WebSockets**: Real-time updates

### Q: Is the agent framework production-ready?

**A:** Yes, it includes:
- Error handling and retry logic
- Monitoring and metrics (Prometheus)
- State management (Redis)
- Security best practices
- Scalability patterns

## 📘 COBOL Modernization

### Q: Which COBOL dialects are supported?

**A:** The workshop covers:
- **IBM Enterprise COBOL**
- **Micro Focus COBOL**
- **GnuCOBOL** (for testing)
- **Other dialects** with minor adjustments

### Q: What about CICS and JCL?

**A:** We provide:
- CICS command transformation patterns
- JCL to shell script conversion
- Batch processing modernization
- Transaction handling in modern frameworks

### Q: Can I keep some COBOL programs?

**A:** Yes! The hybrid approach allows:
- Gradual modernization
- COBOL-Java interoperability
- Selective transformation
- Risk mitigation

### Q: How accurate is the transformation?

**A:** Typically:
- **Syntax accuracy**: 95-99%
- **Business logic**: 90-95%
- **Performance**: May need optimization
- **Testing required**: Always!

## 🗄️ Natural/Adabas Migration

### Q: Is Adabas to SQL conversion automatic?

**A:** Partially automated:
- **DDM conversion**: 80% automated
- **Data migration**: Tool-assisted
- **MU/PE fields**: Require manual design
- **Performance tuning**: Manual optimization

### Q: How are superdescriptors handled?

**A:** We provide patterns for:
- Converting to indexes
- Creating materialized views
- Implementing in application layer
- Performance optimization strategies

### Q: What about Natural Construct?

**A:** Natural Construct applications need:
- Additional analysis phase
- Template pattern extraction
- Framework selection (Spring Boot recommended)
- Custom transformation rules

### Q: Can I migrate to NoSQL instead?

**A:** Yes, we cover:
- When NoSQL makes sense
- MongoDB migration patterns
- Document design from Adabas
- Hybrid SQL/NoSQL approaches

## 🎓 Certification and Credits

### Q: Is there a certification?

**A:** Currently:
- **Completion certificates**: Self-generated
- **Badges**: Digital badges for each module
- **LinkedIn**: Add to your profile
- **Official certification**: Coming in 2025

### Q: Can I get academic credit?

**A:** Some universities accept workshop completion:
- Check with your institution
- We provide completion records
- Equivalent to 3-4 credit hours
- Professional development credits available

### Q: Do employers recognize this training?

**A:** Yes! The workshop is recognized by:
- Major consulting firms
- Fortune 500 companies
- Government agencies
- Modernization vendors

## 🤝 Support and Community

### Q: How do I get help during the workshop?

**A:** Multiple support channels:
1. **Documentation**: Comprehensive guides
2. **Troubleshooting**: [TROUBLESHOOTING.md](TROUBLESHOOTING.md)
3. **Slack community**: Real-time help
4. **Office hours**: Weekly Q&A sessions
5. **GitHub discussions**: Async support

### Q: Is there a community?

**A:** Yes! Join our community:
- **Slack**: 2000+ members
- **Monthly meetups**: Virtual and in-person
- **Annual conference**: MainframeCon
- **Local user groups**: 15+ cities

### Q: Can I network with others?

**A:** Absolutely:
- **Buddy system**: Pair with another learner
- **Study groups**: Regional groups
- **LinkedIn group**: Professional networking
- **Job board**: Modernization opportunities

### Q: What if I find bugs or errors?

**A:** Please help us improve:
1. Check existing issues on GitHub
2. Report new issues with details
3. Submit fixes via pull request
4. Get recognized as contributor

## 🔮 Future and Updates

### Q: What's coming next?

**A:** Roadmap includes:
- **2024 Q4**: Video tutorials
- **2025 Q1**: Official certification
- **2025 Q2**: Advanced workshops
- **2025 Q3**: Enterprise features

### Q: Will there be advanced workshops?

**A:** Yes, planned topics:
- Performance optimization
- Security hardening
- Multi-cloud deployment
- Enterprise integration
- AI model fine-tuning

### Q: How do I stay updated?

**A:** Stay informed through:
- **Newsletter**: Monthly updates
- **Blog**: Technical articles
- **GitHub watch**: Repository updates
- **Slack announcements**: Real-time news

---

<div align="center">

**Still have questions?**

[💬 **Ask in Slack**](https://mainframe-modern.slack.com) | [📧 **Email Support**](mailto:workshop@mainframe-modernization.org) | [🔙 **Back to Workshop**](README.md)

</div> 