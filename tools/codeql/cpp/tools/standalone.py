# Library for standalone extraction
# Not intended to be run directly

import os
import subprocess
import json
import sys
import tempfile
import multiprocessing
from concurrent.futures import Future, ThreadPoolExecutor, as_completed
import pathlib
import shutil
import shlex
import time

import dependency_resolver
import telemetry
from folder_scanner import FolderScanner

verbose_logging = False  # Whether to output launched processes
prefer_cl = True  # If cl and clang are both available, which do we use


def shlex_join(split_command):
    '''
    Implements shlex.join() for platforms that don't support it
    '''
    return ' '.join(shlex.quote(x) for x in split_command)


class CompileCommands:
    '''
    Class to construct the compile_commands.json file
    '''

    def __init__(self, repo_dir, compiler):
        self.compiler = compiler
        self.cpp_compiler = compiler.get_cpp_compiler_prefix()
        self.c_compiler = compiler.get_c_compiler_prefix()
        self.repo_dir = repo_dir
        self.folder_scanner = FolderScanner()

    def find_files(self):
        """ Initialize all files in the repo directory """
        self.folder_scanner.scan_dir(self.repo_dir)

    def get_project_stats(self):
        """
        Returns a dictionary with the statistics of the project being analyzed.
        """
        all_files_count = len(self.folder_scanner.all_files)
        source_files_count = len(self.folder_scanner.source_files)
        header_files_count = len(self.folder_scanner.header_files)
        return {"#all_files": all_files_count,
                "#source_files": source_files_count,
                "#header_files": header_files_count}

    def get_compile_commands(self):
        '''
        Returns the list of all compile commands.
        '''
        compile_commands = []

        # Dictionary to store telemetry information
        telemetry_information = dict()
        # Store the projects statistics such as the number of source files and header files
        telemetry_information["project_stats"] = self.get_project_stats()

        folder_indexing_start_time = time.time()
        for src in sorted(self.folder_scanner.source_files):
            cmd_line = []
            if src.lower().endswith('.c'):
                cmd_line += self.c_compiler
            else:
                cmd_line += self.cpp_compiler

            # Add include directives needed to satisfy include statements.
            for i in self.folder_scanner.get_include_dirs_from_source(src):
                cmd_line.append(self.compiler.include(i))

            out = src + ".o"
            compile_command = {}
            compile_command['directory'] = self.repo_dir
            compile_command['command'] = shlex_join(
                cmd_line + self.compiler.output(out) + self.compiler.source(src))
            compile_command['output'] = out
            compile_command['file'] = src
            compile_commands.append(compile_command)
        # Save to the telemetry the time taken for folder indexing and the number of missing includes
        telemetry_information["folder_indexing_time_seconds"] = time.time() - folder_indexing_start_time
        telemetry_information["missing_includes_before_system_scan"] = len(self.folder_scanner.missing_includes)
        if resolve_dependencies():
            system_include_start_time = time.time()
            print("Scanning system directories")
            extra_include_folders, still_missing = dependency_resolver.get_missing_system_includes(
                self.folder_scanner.missing_includes, self.compiler.clangpp,
                verbose=verbose_output())
            extra_include_arg = shlex_join(
                [self.compiler.include(i) for i in extra_include_folders])
            for compile_command in compile_commands:
                compile_command['command'] = '{} {}'.format(compile_command['command'],
                                                            extra_include_arg)
            system_folder_time = time.time() - system_include_start_time
            # Store to the telemetry the time taken for system folder include resolution, the number of include path
            # added and the number of missing includes after the system scan
            telemetry_information["system_folder_time_seconds"] = system_folder_time
            telemetry_information["system_folder_added"] = len(extra_include_folders)
            telemetry_information["missing_includes_after_system_scan"] = len(still_missing)
        scanning_message = telemetry.telemetry_message("Folder scan information", telemetry_information)
        telemetry.DiagnosticsWriter.instance.write(scanning_message)
        return compile_commands

    def write_compile_commands(self, compile_commands, output):
        '''
        Write compile_commands.json to the given file.
        '''
        encoder = json.JSONEncoder(indent=4)

        with open(output, "w") as text_file:
            text_file.write(encoder.encode(compile_commands))

        print('Written', output)


def generate_compile_commands_json(dir, compiler, json_file):
    '''
    Compute and write the compile_commands to the given json file.
    '''
    print('Generating compilation commands...')
    cc = CompileCommands(dir, compiler)
    cc.find_files()
    commands = cc.get_compile_commands()
    cc.write_compile_commands(commands, json_file)


def getVarDir(name):
    d = os.environ.get(name, None)
    if d is None:
        raise Exception(name + ' is not set')
    if not os.path.isdir(d):
        raise Exception(name + ' (' + d + ') is not a directory')
    return d


class CommandWithResponse:
    '''
    A command line split into the command and its arguments.
    The command must support response files - the ability to replace its arguments
    with an `@` file containing the arguments.
    '''

    def __init__(self, line, compiler):
        l = shlex.split(line)
        self.command = l[:1] if len(l) > 0 else []
        self.args = l[1:] if len(l) > 1 else []
        self.compiler = compiler

    def write_response_file(self, response_file):
        self.compiler.write_response_file(response_file, self.args)


class ProcessWithResponseFile:
    '''
    Helper to rewrite a command line to a temporary response file, then launch the process.
    '''

    def __init__(self, command: CommandWithResponse):
        self.command = command
        self.response_file = None
        self.logFile = None

    def __enter__(self):
        cmd = self.command.command
        scratchdir = getVarDir('CODEQL_EXTRACTOR_CPP_SCRATCH_DIR')
        argDir = os.path.join(scratchdir, 'args')
        pathlib.Path(argDir).mkdir(exist_ok=True)
        self.response_file = tempfile.NamedTemporaryFile(delete=False, dir=argDir, prefix='args')
        self.command.write_response_file(self.response_file)
        self.response_file.close()
        if verbose_logging:
            print('Running:', cmd, '@' + self.response_file.name)

        if verbose_logging:
            output = None
        else:
            logDir = getVarDir('CODEQL_EXTRACTOR_CPP_LOG_DIR')
            t = time.time_ns()
            p = os.getpid()
            n = 0
            while self.logFile is None:
                try:
                    n += 1
                    h = abs(hash((t, p, n)))
                    fn = f"%s/standalone/%02x/%02x/%05x.log" % (
                        logDir, h & 0x3f, (h >> 6) & 0x3f, h >> 12);
                    pathlib.Path(fn).parent.mkdir(parents=True, exist_ok=True)
                    self.logFile = open(fn, 'x')
                except FileExistsError:
                    # If the file already existed, then go round the loop
                    # again with the next `n`
                    pass
            output = self.logFile
        # Don't capture stdout or stderr as these will be written to the log directory of the extractor.
        self.process = subprocess.Popen(cmd + ['@' + self.response_file.name], shell=False,
                                        stdout=output, stderr=output)
        return self

    def wait(self):
        self.process.wait()
        self.returncode = self.process.returncode
        # print('Return code:', self.returncode)

    def __exit__(self, *exc):
        if self.logFile is not None:
            self.logFile.close()
        return False


def run_process_with_response_file(command):
    start_time = time.time()
    try:
        with ProcessWithResponseFile(command) as p:
            p.wait()
            return p.returncode, command.command, command.args, time.time() - start_time, None
    except Exception as ex:
        command_str = ' '.join(command.command)
        reason = f'Failed to launch {command_str} because {ex}'
        print(reason, file=sys.stderr)
        return -1, command.command, command.args, time.time() - start_time, reason


# A simple executor that runs tasks sequentially and deterministically.
class SimpleExecutor:
    def __init__(self):
        pass

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_value, traceback):
        pass

    def submit(self, fn, *args, **kwargs):
        result = Future()
        result.set_result(fn(*args, **kwargs))
        return result


# An executor that uses SimpleExecutor if max_workers is 1,
# and ThreadPoolExecutor otherwise.
class CodeQLExecutor:
    _executor = None

    def __init__(self, max_workers):
        self._max_workers = max_workers

    def __enter__(self):
        self._executor = SimpleExecutor() if self._max_workers == 1 else ThreadPoolExecutor(
            max_workers=self._max_workers)
        return self

    def __exit__(self, exc_type, exc_value, traceback):
        return self._executor.__exit__(exc_type, exc_value, traceback)

    def submit(self, fn, *args, **kwargs):
        if not self._executor:
            raise RuntimeError("Executor is not initialized. Use the wrapper within a 'with' statement.")
        return self._executor.submit(fn, *args, **kwargs)


def run_commands_in_parallel(commands, threads):
    print('Running', len(commands), 'commands in', threads, 'threads')
    errors = 0
    partial = 0
    successes = 0
    times_and_commands = []
    errors_and_commands = []
    start_time = time.time()
    with CodeQLExecutor(max_workers=threads) as executor:
        futures = {executor.submit(run_process_with_response_file, command): command for command in commands}
        for future in as_completed(futures):
            output = futures[future]
            try:
                future_result, future_command, future_args, future_time, failure_reason = future.result()
                times_and_commands.append((future_time, future_command, future_args))
                if future_result == 0:
                    successes = successes + 1
                elif future_result == -1:
                    errors = errors + 1
                    errors_and_commands.append((future_time, future_command, future_args, failure_reason))
                else:
                    # We had some errors, so the output wasn't clean.
                    # However, we still manage to extract something, so we warn accordingly
                    partial = partial + 1
            except Exception as e:
                print(f"{output}: {e}")
                errors = errors + 1
    end_time = time.time()
    print(f'Ran {len(commands)} commands [s={successes},p={partial},f={errors}]')
    send_extraction_telemetry(start_time, end_time, successes, partial, errors, times_and_commands, errors_and_commands)


def send_extraction_telemetry(extraction_start_time, extraction_end_time, extraction_successes_count,
                              extraction_partial_count, extraction_errors_count, extraction_times_and_commands,
                              extraction_errors_and_commands):
    """
    Report telemetry information about the extraction process.
    """
    slowest_time, slowest_command, slowest_args = max(extraction_times_and_commands, key=lambda x: x[0])
    telemetry_body = {"extraction_status": {"#success": extraction_successes_count,
                                            "#partial": extraction_partial_count,
                                            "#errors": extraction_errors_count},
                      "slowest_extraction_time_seconds": slowest_time,
                      "slowest_extraction_command": slowest_command,
                      "slowest_extraction_args": slowest_args,
                      "extraction_cpu_time_seconds": sum([tc[0] for tc in extraction_times_and_commands]),
                      "total_extraction_time_seconds": extraction_end_time - extraction_start_time}
    if len(extraction_errors_and_commands) > 0:
        telemetry_body["extraction_errors"] = []
        for error_time, error_command, error_args, failure_reason in extraction_errors_and_commands:
            telemetry_body["extraction_errors"].append(
                {"error_time_seconds": error_time,
                 "error_command": error_command,
                 "error_args": error_args,
                 "failure_reason": failure_reason})
    telemetry.DiagnosticsWriter.instance.write(telemetry.telemetry_message("Extraction information", telemetry_body))


def run_compile_commands_json(compiler, json_file, threads):
    '''
    Read a list of compile-commands from the given json file, then execute them in parallel.
    This should be run in a traced context.
    '''
    print('Running compile commands')
    with open(json_file, 'r') as f:
        data = f.read()
    compile_commands = json.loads(data)
    compiler_invocations = []
    for command in compile_commands:
        compiler_invocations.append(CommandWithResponse(command['command'], compiler))
    run_commands_in_parallel(list, threads)


def extract_compile_commands_json(extractor, compiler, json_file, threads):
    '''
    Read the list of compile commands and then launch the extractor directly
    for each command. This should not be run in a traced context.
    '''
    print('Extracting compile commands')
    with open(json_file, 'r') as f:
        data = f.read()
    commands = json.loads(data)
    list = []
    for command in commands:
        command = CommandWithResponse(command['command'], compiler)
        command.command = [extractor, '--mimic'] + command.command
        list.append(command)
    run_commands_in_parallel(list, threads)


class ClangCompiler:
    '''
    Constructs the command line for the clang compiler.
    '''

    def __init__(self, clang, clangpp):
        self.clang = clang
        self.clangpp = clangpp

    def get_name(self):
        return 'clang'

    def get_cpp_compiler_prefix(self):
        return [self.clangpp, '-std=c++20', '-fexceptions', '-c', '-fsyntax-only']

    def get_c_compiler_prefix(self):
        return [self.clang, '-c', '-fsyntax-only']

    def include(self, inc):
        return '-I' + str(inc)

    def output(self, out):
        return ['-o', out]

    def source(self, src):
        return [src]

    def write_response_file(self, response_file, args):
        response_file.write(shlex_join(args).encode('utf-8'))


class MsvcCompiler:
    '''
    Constructs the command line for the MSVC compiler.
    '''

    def __init__(self, cl):
        self.cl = cl

    def get_name(self):
        return 'msvc'

    def get_cpp_compiler_prefix(self):
        return [self.cl, '/std:c++latest', '/Zs', '/EHs', '/permissive']

    def get_c_compiler_prefix(self):
        return [self.cl, '/Zs']

    def include(self, inc):
        return '/I' + str(inc)

    def output(self, out):
        return ['/Fo' + out]

    def source(self, src):
        return [src]

    def write_response_file(self, response_file, args):
        for a in args:
            response_file.write((a + '\n').encode('utf-8'))


def find_compiler():
    '''
    Looks for the best available compiler on this platform.
    '''
    cl = shutil.which('cl')
    clang = shutil.which('clang')
    clangpp = shutil.which('clang++')
    if prefer_cl and cl is not None:
        return MsvcCompiler(cl)
    elif clang is not None and clangpp is not None:
        return ClangCompiler(clang, clangpp)
    elif cl is not None:
        return MsvcCompiler(cl)
    else:
        return None


def get_thread_count():
    try:
        n = int(os.environ['CODEQL_THREADS'])
        return n if n > 0 else multiprocessing.cpu_count()
    except:
        return multiprocessing.cpu_count()


def verbose_output():
    return os.environ.get('CODEQL_EXTRACTOR_CPP_BUILD_MODE_NONE_VERBOSE', 'false') == 'true'


def resolve_dependencies():
    return os.environ.get('CODEQL_EXTRACTOR_CPP_BUILD_MODE_NONE_DEPENDENCIES_FROM_SYSTEM_INCLUDES',
                          'true') == 'true'
