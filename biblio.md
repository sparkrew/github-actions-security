# Bibliography of GitHub Actions Security Research

## The Monsters in Your Build Cache – GitHub Actions Cache Poisoning
**[Read Article](https://adnanthekhan.com/2024/05/06/the-monsters-in-your-build-cache-github-actions-cache-poisoning/)**

### Problem Addressed
This blog post by **Adnan Khan** explores a critical security issue in **GitHub Actions caching** and how attackers can manipulate it to gain unauthorized access to CI/CD workflows. The core issue is **cache poisoning**, allowing malicious actors to replace legitimate cache entries, leading to privilege escalation and exposure of sensitive data.

### Key Takeaways
- Detailed analysis of **GitHub Actions caching mechanisms** and potential attack vectors.
- Step-by-step execution of cache poisoning attacks.
- Highlights the **real-world impact** on projects like **Angular, Mozilla MDN, and Hyperledger Besu**.
- Introduces the **Actions Cache Blasting attack** technique.

### Potential Improvements
- More focus on **prevention strategies** beyond attack demonstrations.
- Recommendations on **how GitHub could enhance cache security**.
- Provide **defensive tools** to help repository maintainers detect cache poisoning risks.

### Related Security Tools
- **Cacheract** ([GitHub Repo](https://github.com/adnanekhan/cacheract))

---

## How to Secure Your GitHub Actions Workflows with CodeQL
**[Read Article](https://github.blog/security/application-security/how-to-secure-your-github-actions-workflows-with-codeql/#results)**

### Problem Addressed
This article from GitHub's security blog discusses how **CodeQL**, GitHub’s static analysis tool, has been enhanced to **detect vulnerabilities in GitHub Actions workflows**. It addresses workflow misconfigurations and **untrusted data flow risks** through **taint tracking analysis**.

### Key Takeaways
- **Taint tracking for untrusted data sources**, improving security scans.
- **Introduces GitHub Actions as a structured language** in CodeQL.
- **Supports Bash script analysis**, allowing security checks within shell commands.
- **Detects cache poisoning vulnerabilities**, mitigating supply chain attacks.
- Already in use to **secure 75+ major open-source projects** and detect **90+ vulnerabilities**.

### Potential Improvements
- Expanding taint tracking for **complex, multi-repo workflows**.
- **Automated remediation suggestions** for detected issues.
- Better detection of **logic-based attacks** (e.g., **TOCTOU exploits**).

---

## Now I'll Think Twice Before Using GitHub Actions Again
**[Read Article](https://ninkovic.dev/blog/2025/think-twice-before-using-github-actions)**

### Problem Addressed
This blog post critiques **GitHub Actions' usability and scalability issues**, especially for **monorepos** and complex CI/CD pipelines. The author argues that GitHub Actions suffers from **inefficiencies, lack of local testing, and YAML complexity**, making alternative CI/CD solutions more attractive.

### Key Takeaways
- **Inefficient monorepo support**, causing merge conflicts.
- **YAML complexity** leads to bloated workflows with excessive logic.
- **No local testing support**, forcing developers to push untested changes.
- **GitHub is slow to address usability concerns**, frustrating users.
- Alternative CI/CD solutions like **GitLab CI/CD, Jenkins, TeamCity, and Dagger** provide better scalability.

### Potential Improvements
- **Dynamic required checks** for monorepos instead of workflow-based enforcement.
- Improve **YAML syntax** to reduce conditional statements.
- Enable **local execution of workflows** for debugging.
- **Better communication on GitHub's roadmap** to address long-standing issues.

### Related Security Tools
- **GitLab CI/CD**, **Jenkins**, **TeamCity**, **Dagger** (as alternative CI/CD solutions)

---

## Research Directions in Software Supply Chain Security
**[Read Article](https://dl.acm.org/doi/pdf/10.1145/3714464)**

### Problem Addressed
This research provides a broad overview of **software supply chain security risks**, categorizing **three major attack vectors**: **dependencies, build infrastructure, and human factors**.

### Key Takeaways
- **96% of software contains open-source dependencies**, making them a prime attack target.
- **Build infrastructure is a major attack vector**, as seen in the **SolarWinds** and **Log4j** incidents.
- **Social engineering targets developers**, leading to compromised credentials and injected backdoors.
- Tools like **SCA scanners, SBOMs, and in-toto** are essential for mitigating risks.

### Potential Improvements
- **Better vulnerability intelligence** for dependencies.
- **Enhanced analysis tools** for securing build environments.
- **Stronger protection against social engineering in open-source development.**
