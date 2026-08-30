| Weakness ID | Weakness Name | Description |
|---|---|---|
| AIW | Artifact Integrity Weakness | The workflow downloads or reuses artifacts, caches, or produced binaries without validating integrity. |
| CFW | Control Flow Weakness | Workflow conditions are written in a way that causes jobs or steps to run unexpectedly, often because expressions are coerced into strings or always evaluate truthy. |
| EPW | Excessive Permission Weakness | Workflow or job runs with more permissions than necessary. |
| GRCW | GitHub Runner Compatibility Weakness | Workflow references actions, runtimes, contexts, or keys that do not behave correctly in the current GitHub Actions environment. |
| HGW | Hardening Gap Weakness | Workflow does not include baseline security hardening checks. |
| IW | Injection Weakness | Untrusted values are used in shell commands or scripts without quoting, validation, or safe handling. |
| KVCW | Known Vulnerable Component Weakness | Workflow uses an action or component version with a known published vulnerability. |
| PTW | Privileged Trigger Weakness | Workflow uses triggers where attacker-controlled events can run privileged code paths. |
| SEW | Secrets Exposure Weakness | Secrets are exposed beyond intended scope, passed into untrusted code paths, inherited broadly, or printed. |
| UDW | Unpinned Dependency Weakness | Workflow references actions or dependencies with mutable refs. |
