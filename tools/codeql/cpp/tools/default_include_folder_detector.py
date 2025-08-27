import os.path
import re
import shlex
import subprocess

"""
This file contains functions to get the default include paths of a given compiler (i.e. the include
 folders which are automatically used by the given compiler without the need ot specify -I or 
 -isystem).
"""


def split_and_normalize_paths(compiler_output):
    """
    Split the compiler output and normalize the paths.

    :param compiler_output: the compiler output
    :return: a list of non-empty normalized paths
    """
    result = []
    lines = compiler_output.splitlines()
    for line in lines:
        cleaned_line = line.strip()
        if len(cleaned_line) > 0:
            # Removing potential characters after the path
            cleaned_line = shlex.split(cleaned_line)[0]
            normalized = os.path.normpath(cleaned_line)
            if normalized not in result:
                result.append(normalized)

    return result


def parse_compiler_includes(output):
    """
    Parse the output of a compiler to get the system include paths.

    :param output: The output of the compiler
    :return: A list of system include paths
    """
    local_include_regex = r'^#include "..." search starts here:\n(( .*\n)*)(End of search list.)?'
    local_include_re_search = re.search(local_include_regex, output, re.MULTILINE)
    local_include_includes_capture = local_include_re_search.group(1) \
        if local_include_re_search is not None else ''

    global_include_regex = r'^#include <...> search starts here:\n(( .*\n)*)(End of search list.)?'
    global_include_re_search = re.search(global_include_regex, output, re.MULTILINE)
    global_include_includes_capture = global_include_re_search.group(1) \
        if global_include_re_search is not None else ''

    return (split_and_normalize_paths(local_include_includes_capture),
            split_and_normalize_paths(global_include_includes_capture))


def get_default_include_folder_for_compiler(compiler, cpp):
    """
    Get the normalized default include paths of the given compiler (limited to clang or gcc).

    :param compiler: The compiler to use
    :param cpp: Print paths for C++ (C if False)
    :return: A list of default include paths

    :note: This function returns a pair of empty lists if run on non-unix systems or if the given
            compiler cannot be invoked.
    """
    language = 'c++' if cpp else 'c'

    cmd = [compiler, '-x', language, '-v', '-E', '/dev/null', '-o', '/dev/null']
    try:
        r = subprocess.run(cmd, capture_output=True, check=True)
        return parse_compiler_includes(r.stderr.decode())
    except Exception as e:
        # If the command fails, return an empty set
        return [], []
