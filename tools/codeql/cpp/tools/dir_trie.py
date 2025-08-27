'''
Create a trie of files for fast lookup of path suffixes
The datastructure adds elements based on path seperator in 'reverse':

'a/b/c.h' maps to 'c.h' -> 'b' -> 'a'

which allows fast lookups for names matching the end of filenames:
'c.h' -> ['a/b']
'b/c.h' -> ['a']

All results are prefixed with the root
(see set_root)

Note that this data structure is not meant to lookup files specified using absolute paths.
'''
import os
import re

class FileFilters:
    '''
    A predicate to test whether a given path should be included.
    Reads filters from the environment variable LGTM_INDEX_FILTERS if not explicitly provided.
    If no includes are specified, then all files in the directory are included.
    Exclusions take priority over inclusions, irrespective of declaration order.
    '''

    def __init__(self, root, filters = None):
        exclude = '(?!)' # Match nothing
        root = os.path.normpath(root)
        include = None
        if filters is None:
            filters = os.environ.get('LGTM_INDEX_FILTERS','')
        for line in filters.split('\n'):
            line = line.strip()
            if not line:
                continue
            if line.startswith('include:'):
                pattern = os.path.join(root, line[8:].strip(' /\\'))
                tail = glob_translate(pattern) + '|' + glob_translate(pattern + '/**')
                if include is None:
                    include = tail
                else:
                    include = include + '|' + tail
            elif line.startswith('exclude:'):
                pattern = os.path.join(root, line[8:].strip(' /\\'))
                exclude = exclude + '|' + glob_translate(pattern) + '|' + glob_translate(pattern + '/**')
            else:
                raise ValueError('Invalid filter: ' + line)
        self.exclude = re.compile(exclude)

        if include is None:
            include = glob_translate(root + '/**')

        self.include = re.compile(include)

    def includes(self, path):
        '''
        Matches an absolute path against the filters.
        Returns True is the path is not excluded.
        '''
        path = path.replace('\\', '/')
        return bool(self.include.match(path)) and not bool(self.exclude.match(path))


class DirTrie:

    def __init__(self):
        self.trie = {}

    def set_root(self, root):
        self.trie[root] = DirTrie()

    def insert(self, item, root='.'):
        '''Insert an element into the trie'''
        (path, last) = os.path.split(item)
        sub_trie = self.trie.get(last)
        if sub_trie is None:
            sub_trie = DirTrie()
            self.trie[last] = sub_trie

        if path not in ('', os.path.sep):
            sub_trie.insert(path, root)
        else:
            sub_trie.set_root(root)

    def add_files(self, items, root='.'):
        '''Add a set of files to the trie'''
        for item in items:
            self.insert(item, root)

    def of_dir(self, root_dir, follow_symlinks=False):
        '''
        Create a trie of all files in the given root_dir.
        Existing items will be discarded.
        '''

        root_dir=str(root_dir)

        filter = FileFilters(root_dir)

        self.trie={}

        root_dir_len=len(root_dir) + 1

        for root, _dirs, files in os.walk(root_dir, followlinks=follow_symlinks):
            for filename in files:
                fullname = os.path.join(root, filename)
                if filter.includes(fullname):
                    name = fullname[root_dir_len:]
                    self.insert(name, root_dir)


    def get_paths(self):
        '''Get all files in the trie'''
        result = []
        for key, value in self.trie.items():
            if len(value) == 0:
                # Leaf
                result.append([key])
            else:
                paths = value.get_paths()
                for path in paths:
                    path.append(key)
                    result.append(path)
        return result

    def lookup(self, item):
        '''
        Return all paths (trie) that has the given prefix 'item' in the tree
        item is a file-system path.

        Note that this function is not meant to lookup files specified using absolute paths.
        '''

        (path, last) = os.path.split(item)
        if last == '':
            if path != '':
                return []
            return self.get_paths()

        result = self.trie.get(last)
        if result is None:
            return []

        # Recursive descent
        r = result.lookup(path)
        return r

    def __len__(self):
        return len(self.trie)

# A light version of glob.translate() which is only available from Python 3.13
def glob_translate(pat):
    pat = os.path.normpath(pat).replace('\\','/')
    result = '(?s:'
    stars = 0

    def accept(c):
        if c == '?':
            return '[^/]'
        else:
            return re.escape(c)

    for c in pat:
        if c == '*':
            stars += 1
        elif stars>0:
            if stars == 1:
                result += '[^/]*'
                result += accept(c)
            else:
                result = result + '.*'
                if c != '/': # Allow ** to match no directories 
                    result += accept(c)
            stars = 0
        else:
            result += accept(c)
    if stars > 0:
        if stars == 1:
            result += '[^/]*'
        else:
            result += '.*'
    result += ')\\Z'
    return result
