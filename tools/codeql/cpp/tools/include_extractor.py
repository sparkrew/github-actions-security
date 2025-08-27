import re


class IncludeExtractor:
    """Extracts all files referenced in include preprocessor statements"""

    # Regular expression for an include directive
    # Capture everything between the " and " or between the < and >
    include_re = re.compile(r'^\s*#\s*include\s*[<"]([^">]+)[">]')

    def __init__(self):
        # Include file cache
        self.include_files_cache = {}

    def extract_includes(self, file):
        """Get all files referenced in include preprocessor statements"""
        if self.include_files_cache.get(file) is not None:
            return self.include_files_cache.get(file)

        includes = []

        with open(file, 'r', errors='ignore', encoding='utf-8') as f:
            for line in f:
                m = self.include_re.match(line)
                if m:
                    include = m.group(1)
                    # Map '\' to '/' to be more platform-agnostic.
                    include = include.replace('\\', '/')
                    includes.append(include)

        self.include_files_cache[file] = includes
        return includes