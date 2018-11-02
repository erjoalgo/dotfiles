#!/usr/bin/python
from __future__ import print_function
import os, re, subprocess, sys
from os.path import join, exists, basename

def mvn_dep_list(directory=os.getcwd()):
    os.chdir(directory)
    out=subprocess.check_output(["mvn", "dependency:list"])
    group_artifact_version=re.findall("([a-z0-9.]+):([a-z0-9.-]+):jar:([a-zA-Z0-9.]+)",
                                  out)
    return group_artifact_version

def jar_classes(jar):
    out=subprocess.check_output(["jar", "tf", jar])
    # org/aspectj/weaver/tools/PointcutDesignatorHandler.class
    return out.split("\n")

M2_REPO=join(os.getenv("HOME"), ".m2", "repository")
def find_jar(group, artifact, version):
    base="{}-{}.jar".format(artifact, version)
    directory=join(M2_REPO, *group.split(".")+[artifact, version])
    filename=join(directory, base)
    if exists(filename):
        return filename
    else:
        print ("resorting to find: {}".format(filename), file=sys.stderr)
        out=subprocess.check_output(["find", M2_REPO, "-name", base]).strip()
        lines=len(filter(bool, out.split("\n")))
        if lines:
            return out
        else:
            print ("could not find: {}".format(filename), file=sys.stderr)
            return None

if __name__ == "__main__":
    d={}
    for (g, a, v) in mvn_dep_list():
        jar=find_jar(g, a, v)
        if jar==None:
            continue
        print (jar)
        classes=jar_classes(jar)
        d[jar]=jar_classes(jar)
        print ("\n".join(" -> ".join((basename(jar), clazz)) for clazz in classes))
