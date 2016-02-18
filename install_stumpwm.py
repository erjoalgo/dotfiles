#!/usr/bin/python
import sys, pexpect, subprocess, os
from os.path import join, exists
from os import chdir, mkdir

call = subprocess.call
# call(["sudo", "apt-get", "install", "-y", "sbcl", "cl-swank", "curl", "autoconf"])
call(["sudo", "apt-get", "install", "-y", "sbcl", "curl", "autoconf", "make"])
call("curl -O http://beta.quicklisp.org/quicklisp.lisp".split(" "))

home = os.getenv("HOME")
programs = join(home, "programs")
sbcl_lines = [
"(require :asdf)", 
"(require :asdf-install)", 
"(asdf-install:install :clx)", 
"(asdf-install:install :cl-ppcre)", 
# "(asdf-install:install :swank)"
]

    
sbcl_lines = [
    # '(quicklisp-quickstart:install :path "/home/ernesto/programs/quicklisp")',
    '(quicklisp-quickstart:install)',
    # "(ql:update-client)",
    '(ql:quickload "clx")', 
    '(ql:quickload "cl-ppcre")'
    '(ql:quickload "swank")'
    '(ql:quickload "quicklisp-slime-helper")'
    "(ql:add-to-init-file)\r\n\n\r",
    "(quit)"
]


# p = pexpect.spawn("sbcl")
# if not exists(join(programs, "quicklisp")):
# print ( "installing lisp packages via quicklisp" )
# call(["sbcl", "--load", "quicklisp.lisp", "--eval", "(progn {})".format(" ".join(sbcl_lines))])
call(["sbcl", "--load", "quicklisp.lisp"] + ["--eval" if i%2==0 else sbcl_lines[i/2]
                                             for i in xrange(len(sbcl_lines)*2)])
call("rm quicklisp.lisp".split(" "))
# p = pexpect.spawn("sbcl --load quicklisp.lisp")
# p.logfile = sys.stdout
# p.expect(".*")
# map(p.sendline, sbcl_lines)

if not exists(programs):
    mkdir(programs)
chdir(programs)

if not exists("stumpwm"):
    call("git clone https://github.com/stumpwm/stumpwm.git".split(" "))

chdir("stumpwm")
if not exists("stumpwm"):#the executable
    raw_input("confirm make in {}: ".format(os.getcwd()))
    call("./autogen.sh")
    call("./configure")
    call("make")
