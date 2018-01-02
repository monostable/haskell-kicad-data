import pcbnew
import sys
import os

pretties = []
for dirname, dirnames, filenames in os.walk(sys.argv[1]):
    # don't go into any .git directories.
    if '.git' in dirnames:
        dirnames.remove('.git')

    for filename in filenames:
        if (not os.path.isdir(filename)) and (os.path.splitext(filename)[-1] == '.kicad_mod'):
            pretties.append(os.path.realpath(dirname))
            break

src_plugin = pcbnew.IO_MGR.PluginFind(1)

for libpath in pretties:
    try:
        list_of_footprints = src_plugin.FootprintEnumerate(libpath)
    except UnicodeDecodeError:
        # pcbnew python modules (at least git-7d6230a and v4.x) have an issue
        # with loading unicode paths
        # https://bugs.launchpad.net/kicad/+bug/1740881
        pass
    except Exception as e:
        print(libpath)
        raise e
