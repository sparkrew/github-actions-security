package main

import (
	"log"
	"os"

	"github.com/step-security/secure-repo/remediation/workflow"
)

func main() {
	path := "."
	if len(os.Args) > 1 {
		path = os.Args[1]
	}

	findings, err := workflow.AnalyzeWorkflow(path)
	if err != nil {
		log.Fatalf("Scan failed: %v", err)
	}
}
