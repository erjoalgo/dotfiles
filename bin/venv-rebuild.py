#!/usr/bin/python3
"""Recreate a venv on the system python3 and reinstall its top-level packages.

Editables are reinstalled editable from the same path; the rest by name at
their latest version, since the old pins may not support the new python.
"""

import argparse
import glob
import json
import os
import subprocess
import sys

# the python the venv is rebuilt on, and whose pip lists the old packages;
# not PATH's python3, which may be the venv's own or conda's
SYSTEM_PYTHON = "/usr/bin/python3"
# packaging tools that a new venv brings its own of
EXCLUDE = ("pip", "setuptools", "wheel")


def list_top_level(venv):
    """Return the venv's top-level (packages, editables).

    The system pip reads the old site-packages' metadata without importing
    anything, so this works on a venv whose python is gone. --not-required
    keeps the top of the dependency tree; installing those pulls in the rest.
    """
    sites = sorted(glob.glob(os.path.join(venv, "lib", "python3*", "site-packages")))
    if not sites:
        return [], []
    cmd = [SYSTEM_PYTHON, "-m", "pip", "list", "--not-required", "--format=json"]
    for site in sites:
        cmd += ["--path", site]
    for name in EXCLUDE:
        cmd += ["--exclude", name]
    listed = json.loads(subprocess.run(cmd, check=True, stdout=subprocess.PIPE).stdout)
    packages = [p["name"] for p in listed if "editable_project_location" not in p]
    editables = [p["editable_project_location"] for p in listed
                 if "editable_project_location" in p]
    return packages, editables


def confirm(prompt):
    try:
        return input(prompt).strip().lower() == "y"
    except EOFError:
        return False


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("venv", nargs="?", default=os.path.expanduser("~/.venv"),
                        help="the venv to rebuild (default: ~/.venv)")
    venv = parser.parse_args().venv

    if not os.path.isfile(os.path.join(venv, "pyvenv.cfg")):
        sys.exit(f"{venv} is not a venv (no pyvenv.cfg)")

    packages, editables = list_top_level(venv)
    if not packages and not editables:
        print(f"found nothing to reinstall in {venv}; left it as is")
        return 0

    version = subprocess.run([SYSTEM_PYTHON, "--version"], check=True,
                             stdout=subprocess.PIPE, text=True).stdout.strip()
    print(f"will recreate {venv} on {version} and install:")
    for package in packages:
        print(f"    {package}")
    for editable in editables:
        print(f"    -e {editable}")
    if not confirm("go ahead? [y/N] "):
        print("nothing changed")
        return 1

    subprocess.run([SYSTEM_PYTHON, "-m", "venv", "--clear", venv], check=True)

    # one at a time, so a package that fails on the new python doesn't stop the rest
    pip = os.path.join(venv, "bin", "pip")
    failed = []
    for package in packages:
        if subprocess.run([pip, "install", package]).returncode != 0:
            failed.append(package)
    for editable in editables:
        if subprocess.run([pip, "install", "-e", editable]).returncode != 0:
            failed.append(f"-e {editable}")

    if failed:
        print("failed to install:", file=sys.stderr)
        for spec in failed:
            print(f"    {spec}", file=sys.stderr)
        return 1
    print(f"rebuilt {venv}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
