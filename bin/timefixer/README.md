This service helps to fix newly created or modified files with timestamps in the future.
This can happen occassionally when downloading files with incorrect timestamps based on different time zones.
Fixing such cases makes it possible to reliably fetch the latest modified file from a list of directories, which helps my productivity.

To install:
$ pip install timefixer
$ timefixer --install-systemd