#!/usr/bin/env python3

import standalone
import os

cwd = os.getcwd()
compile_commands = os.path.join(os.environ["CODEQL_EXTRACTOR_CPP_LOG_DIR"], 'compile_commands.json')

compiler = standalone.find_compiler()

if compiler is None:
    print('Error: no suitable C++ compiler was found. Check your PATH variable.')
    exit(1)

standalone.generate_compile_commands_json(cwd, compiler, compile_commands)
standalone.run_compile_commands_json(compiler, compile_commands, standalone.get_thread_count())
