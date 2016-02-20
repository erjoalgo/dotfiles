#!/usr/bin/python
import argparse, subprocess
from os.path import join, exists, islink, realpath
import os
import pdb

parser = argparse.ArgumentParser()
parser.add_argument("--home","-u",default=os.getenv("HOME"), type = os.path.expanduser)
parser.add_argument("--projects")
parser.add_argument("--init_files")
parser.add_argument("--repos")
parser.add_argument("--portable_root")
parser.add_argument("--no_confirm", "-y", action = "store_true")
globals().update(vars(parser.parse_args()))

myprofile = ".my_profile_nirvana.sh"
projects = projects or join(home, "projects")
portable_root = portable_root or join(home, "portable_root")
# init_files = init_files or join(projects, "init_files")
# repos = repos or join(portable_root, "repos")
repos = repos or join(home, "repos")

init_files = init_files or join(repos, "inits")

mybashrc = ".my_bashrc.sh"
myprofilerc = ".myprofile.rc"

links_tree = [
    (
        repos,
        [
            ("stumpwm",
             [
                 ".stumpwmrc",
                 ".sbclrc",
                 ".stumpwm_per_window_bindings.el",
                 ".xinitrc", 
                 myprofilerc,
                 ".my_startups.sh", 
                 ("keynavs", [".keynavrc"]), 
             ]),
            ("dotemacs",
             [
                 ".emacs",
                 ".emacs.d", 
                 ".gnus", 
                 ".authinfo"
             ]),
            ("bash", [
                ".pythonrc.py",
                ".bash_aliases",
                ".inputrc",
                mybashrc,
                ".roxterm.sourceforge.net"
            ]), 
            
            ("inits", 
             [
             ])
        ]
    ),
]

def parse_sublinks_tree ( tree, pwd = None):
    L = []
    for item in tree:
        if isinstance(item, tuple):
            subpwd, subtree = item
            # print ( "pwd: {}, subpwd: {}".format(pwd, subpwd) )
            
            L.extend(parse_sublinks_tree(subtree, join(pwd, subpwd)))
        else:
            L.append((item, home, pwd))
    return L

links = parse_sublinks_tree(links_tree)
# exit(1)

links.append(("bin", home, projects))
links.append(("Projects", home, portable_root))

# print ( "\n".join(map(str, links)) )

for (base, location_dir, target_dir) in links:
    
    location = join(location_dir, base)
    target = join(target_dir, base)
    if not exists(target):
        print "warning: target doesn't exist: %s. skipping..." % (target)
        print ( "" )
        continue
        
    cmd = ["ln", "-s", target, location]
    skip_confirm = False
    
    if islink(location):
            curr_target = realpath(location)
            real_target = realpath(target)
            if curr_target==real_target:
                # print "location already points to target"
                print ( "" )
                continue
            elif exists(location):
                print ( location )
                print ( "" )
                continue
            else:
                print ("link already exists pointing to different non-existing target:")
                print "%s ->\n%s (old target)" % (location, curr_target)
                print "%s (new target)" % (target)
                
                if False and raw_input("overwrite? "):
                    print ( "" )
                    continue
                    # do not overwrite
                else:
                    skip_confirm = True
            print "unlinking"
            subprocess.call(["unlink", location])
    elif exists(location):
        print ( "location exists but is a regular file!: {}".format(location) )
        mvcmd = ["mv", location, location+".old"]
        if not raw_input(" ".join(mvcmd)+" ? "):
            subprocess.call(mvcmd)
        else:
            print ( "" )
            continue
    if no_confirm  or skip_confirm or not raw_input("{}\nconfirm: ".format(" ".join(cmd))):
        print (" ".join(cmd))
        print ( "" )
        subprocess.call(cmd)

def add_source ( fn, source_directive ):
    with open( fn, "a") as fh:
        with open( fn, "r") as fhr:
            if not source_directive in fhr.read():
                print ("appending to {}\n{}").format(fn, source_directive)
                fh.write("\n"+source_directive)
            else:
                # print ("{} already contains source directive: {}".format(fn, source_directive))
                pass
                       
                       

bashrc = join(home, ".bashrc")
add_source(bashrc, "source %s\n" % (join(home, mybashrc)))

dotprofile = join(home, ".profile")
add_source(dotprofile, "source %s\n" % (join(home, myprofilerc)))

# print "done"

def maybe_mkdir( path ):
    if not exists(path):
        os.mkdir(path)

maybe_mkdir(join(home, "programs"))
maybe_mkdir(join(home, "logs", "button_images"))
maybe_mkdir(join(home, "logs/my_startups/"))
# subprocess.call(["apt-get", "install", "sudo"])

