# Bibliography of GitHub Actions Security Research

## The Monsters in Your Build Cache – GitHub Actions Cache Poisoning

[Read Article](https://adnanthekhan.com/2024/05/06/the-monsters-in-your-build-cache-github-actions-cache-poisoning/)

### Problem Addressed
This blog post by **Adnan Khan** explores a critical security issue in **GitHub Actions caching** and how attackers can manipulate it to gain unauthorized access to CI/CD workflows. The core issue is **cache poisoning**, allowing malicious actors to replace legitimate cache entries, leading to privilege escalation and exposure of sensitive data.

### Key Takeaways
- **Detailed analysis of GitHub Actions caching mechanisms** and potential attack vectors.
- **Step-by-step execution of cache poisoning attacks**.
- **Highlights the real-world impact** on projects like **Angular, Mozilla MDN, and Hyperledger Besu**.
- **Introduces the Actions Cache Blasting attack technique**.

### Potential Improvements
- **More focus on prevention strategies** beyond attack demonstrations.
- **Recommendations on how GitHub could enhance cache security**.
- **Provide defensive tools** to help repository maintainers detect cache poisoning risks.

### Related Security Tools
- **Cacheract** ([GitHub Repo](https://github.com/adnanekhan/cacheract))

---

## How to Secure Your GitHub Actions Workflows with CodeQL

[Read Article](https://github.blog/security/application-security/how-to-secure-your-github-actions-workflows-with-codeql/#results)

### Problem Addressed
This article from GitHub's security blog discusses how **CodeQL**, GitHub’s static analysis tool, has been enhanced to **detect vulnerabilities in GitHub Actions workflows**. It addresses workflow misconfigurations and **untrusted data flow risks** through **taint tracking analysis**.

### Key Takeaways
- **Taint tracking for untrusted data sources**, improving security scans.
- **Introduces GitHub Actions as a structured language** in CodeQL.
- **Supports Bash script analysis**, allowing security checks within shell commands.
- **Detects cache poisoning vulnerabilities**, mitigating supply chain attacks.
- **Already in use to secure 75+ major open-source projects** and detect **90+ vulnerabilities**.

### Potential Improvements
- **Expanding taint tracking for complex, multi-repo workflows**.
- **Automated remediation suggestions** for detected issues.
- **Better detection of logic-based attacks** (e.g., **TOCTOU exploits**).

### Related Security Tools
- **CodeQL** ([GitHub Repo](https://github.com/github/codeql?tab=readme-ov-file#codeql))

---

## I'll Think Twice Before Using GitHub Actions Again

[Read Article](https://ninkovic.dev/blog/2025/think-twice-before-using-github-actions)

### Problem Addressed
This blog post critiques **GitHub Actions' usability and scalability issues**, especially for **monorepos** and complex CI/CD pipelines. The author argues that GitHub Actions suffers from **inefficiencies, lack of local testing, and YAML complexity**, making alternative CI/CD solutions more attractive.

### Key Takeaways
- **Inefficient monorepo support**, causing merge conflicts.
- **YAML complexity** leads to bloated workflows with excessive logic.
- **No local testing support**, forcing developers to push untested changes.
- **GitHub is slow to address usability concerns**, frustrating users.
- **Alternative CI/CD solutions like GitLab CI/CD, Jenkins, TeamCity, and Dagger** provide better scalability.

### Potential Improvements
- **Dynamic required checks** for monorepos instead of workflow-based enforcement.
- Improve **YAML syntax** to reduce conditional statements.
- Enable **local execution of workflows** for debugging.
- **Better communication on GitHub's roadmap** to address long-standing issues.

---

## Exploring the Characteristics and Challenges in GitHub Actions and Its Ecosystem

[Read Article](https://gupea.ub.gu.se/bitstream/handle/2077/84480/CSE%2024-27%20KM%20SS.pdf?sequence=1&isAllowed=y)

### Problem Addressed
This research explores the challenges within the **GitHub Actions ecosystem**, analyzing **997 discussion threads** and **4.1K repositories** to identify common issues like **security vulnerabilities, dependency problems, and workflow obsolescence**.

### Key Takeaways
- **Security vulnerabilities** are the most prevalent issue in GitHub Actions workflows.
- **Dependency issues** and **breaking changes** create reliability concerns.
- **Marketplace actions are widely used**, but **locally maintained actions** are preferred in **high-complexity repositories**.
- **Developers struggle with obsolescence**, as outdated actions impact build stability.

### Potential Improvements
- **Stronger security enforcement** in marketplace actions.
- **Improved versioning practices** to prevent breaking changes.
- **Dependency monitoring tools** to mitigate obsolescence risks.

---

## On the Use of GitHub Actions in Software Development Repositories

[Read Article](https://orbi.umons.ac.be/bitstream/20.500.12907/43043/1/paper.pdf)

### Problem Addressed
This empirical study analyzes **68K GitHub repositories**, showing **how GitHub Actions is adopted, which workflows are automated, and the security risks involved**.

### Key Takeaways
- **43.9% of repositories use GitHub Actions**, making it the dominant CI/CD tool.
- **Reusable actions are heavily used**, but security concerns exist.
- **Most workflows focus on CI/CD tasks**, with **continuous integration and testing being top priorities**.
- **Security issues arise from unverified third-party actions**, exposing projects to potential threats.

### Potential Improvements
- **Enforce best practices** for referencing reusable actions (e.g., **pinning versions to commit SHAs**).
- **Improve GitHub's security scanning for workflow dependencies**.
- **Increase awareness of supply chain attacks** through better documentation and automation.

---

## Research Directions in Software Supply Chain Security

[Read Article](https://dl.acm.org/doi/pdf/10.1145/3714464)

### Problem Addressed
This research provides a broad overview of **software supply chain security risks**, categorizing **three major attack vectors**: **dependencies, build infrastructure, and human factors**.

### Key Takeaways
- **96% of software contains open-source dependencies**, making them a prime attack target.
- **Build infrastructure is a major attack vector**, as seen in the **SolarWinds** and **Log4j** incidents.
- **Social engineering targets developers**, leading to compromised credentials and injected backdoors.
- **Tools like SCA scanners, SBOMs, and in-toto** are essential for mitigating risks.

### Potential Improvements
- **Better vulnerability intelligence** for dependencies.
- **Enhanced analysis tools** for securing build environments.
- **Stronger protection against social engineering in open-source development.**

### Related Security Tools
- **SCA tools, SBOMs, OpenSSF Scorecard, SLSA, in-toto, TUF**

---

## GitHub’s Native Attestation Support

[Read Article](https://github.com/INRIA/spoon/attestations)

### Problem Addressed
Ensuring that software artifacts in GitHub Actions are verifiable and haven’t been tampered with.

### Key Takeaways
GitHub provides a web UI ("Attestations" tab) and CLI command (`gh attestation verify`) for verifying artifact provenance. This helps prevent supply chain attacks.

### Potential Improvements
It is unclear how many repositories use attestations and whether GitHub provides a public transparency log for attestations.

---

## GitHub Attestation API

[Read Article](https://docs.github.com/en/rest/users/attestations?apiVersion=2022-11-28)

### Problem Addressed
Enables developers to programmatically retrieve and verify artifact attestations.

### Key Takeaways
The API allows for automated verification of software artifacts, ensuring that only trusted artifacts are used in production.

### Potential Improvements
Fine-grained adoption statistics would help measure the real-world impact of this security feature.
