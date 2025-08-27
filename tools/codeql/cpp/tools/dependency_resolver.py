import default_include_folder_detector
from folder_scanner import FolderScanner


def print_if(verbose, msg):
    if verbose:
        print(msg)

def get_missing_system_includes(includes, compiler, verbose=False):
    """
    Get a list of missing system includes from a list of includes.

    :param includes: A list of includes
    :param compiler: The compiler to use
    :param verbose: Print debug messages
    :return: A list of system includes folder to add to the include list and the list of includes still missing
    """
    if len(includes) == 0:
        return [], []
    folder_scanner = FolderScanner(verbose=verbose)
    root_dir = "/usr/include"
    print_if(verbose, f"Resolving {len(includes)} missing include files")
    # We don't know if it is C or C++ code, so we try C++
    local_standard_library_folders, global_standard_library_folders = (
        default_include_folder_detector.get_default_include_folder_for_compiler(compiler, True))
    folder_scanner.scan_dir(root_dir, False,
                            local_standard_library_folders + global_standard_library_folders)
    folder_scanner.compute_include_dirs_from_include_list(includes, root_dir)
    print_if(verbose, f"Added {len(folder_scanner.include_dirs)} include folders")
    for folder in folder_scanner.include_dirs:
        print_if(verbose, f"  {folder}")
    print_if(verbose, f"{len(folder_scanner.missing_includes)} include files still missing")
    return folder_scanner.include_dirs, folder_scanner.missing_includes
