#!/usr/bin/env python3
import platform
import sys
import time

import standalone
import os

import telemetry


def report_system_infos(compiler):
    """
    Produce telemetry information about the system used
    """
    system_info = {
        "system_name": platform.uname().system,
        "system_architecture": platform.uname().machine,
        "cpu_count": os.cpu_count(),
        "compiler_name": compiler.get_name() if compiler else "None"
    }

    telemetry_message = telemetry.telemetry_message("Platform information", system_info)
    telemetry.DiagnosticsWriter.instance.write(telemetry_message)


def extract():
    cwd = os.getcwd()
    compile_commands = os.path.join(os.environ["CODEQL_EXTRACTOR_CPP_LOG_DIR"],
                                    'compile_commands.json')
    extractor = os.path.join(os.environ['CODEQL_EXTRACTOR_CPP_ROOT'], 'tools', os.environ['CODEQL_PLATFORM'],
                             'extractor')

    os.environ['SEMMLE_CPP_MISSING_INCLUDES_NOT_FATAL'] = '1'
    os.environ['CODEQL_EXTRACTOR_CPP_OPTION_SCALE_TIMEOUTS'] = '10'

    compiler = standalone.find_compiler()

    report_system_infos(compiler)

    if compiler is None:
        print('Error: no suitable C++ compiler was found. Check your PATH variable.', file=sys.stderr)
        exit(1)

    standalone.generate_compile_commands_json(cwd, compiler, compile_commands)
    standalone.extract_compile_commands_json(extractor, compiler, compile_commands, standalone.get_thread_count())


def main():
    try:
        # Set up the telemetry environment
        telemetry.DiagnosticsWriter.initialize()
        # Report to the telemetry that the standalone extraction has started
        telemetry.DiagnosticsWriter.instance.write(
            telemetry.telemetry_message("Standalone extraction started", {}))

        start_time = time.time()

        # Run the standalone extraction
        extract()

        # Report to the telemetry that the standalone extraction has finished and the time taken
        telemetry.DiagnosticsWriter.instance.write(
            telemetry.telemetry_message("Standalone extraction completed",
                                        {"standalone_extraction_time_seconds": time.time() - start_time}))
    except Exception as e:
        # If an error occurs, report it to the user and to the telemetry
        print(f"Extraction error: {e}", file=sys.stderr)
        telemetry.DiagnosticsWriter.instance.write(telemetry.standalone_error_message("Standalone extraction error", e))
        exit(1)

    exit(0)


if __name__ == '__main__':
    main()
