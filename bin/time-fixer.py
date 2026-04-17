#!/usr/bin/env python3

"""
Observe a directory and new fix files with timestamps in the future.
"""


import argparse
import logging
import os
import time
import sys

logger = logging

try:
    import watchdog.events
    import watchdog.observers
except Exception as ex:
    logger.warning("failed to import watchdog: %s", ex)
    watchdog = None

class FsObserver():
    """Invoke a callback on file changes in a directory."""
    def __init__(self, directory, on_change):
        self.directory = directory
        self.observer = watchdog.observers.Observer()
        self.on_change = on_change

    @staticmethod
    def __handler__(on_change):
        class Handler(watchdog.events.FileSystemEventHandler):
            """Invoke the user-provided callback."""
            def __init__(self, on_change):
                self.on_change = on_change

            def on_created(self, event):
                logger.debug(f'File created: {event.src_path}')
                self.on_change("created", event.src_path, event)

            def on_modified(self, event):
                logger.debug(f'File modified: {event.src_path}')
                self.on_change("modified", event.src_path, event)

            def on_moved(self, event):
                logger.debug(f'File moved: {event.src_path} => {event.dest_path}')
                self.on_change("moved", event.dest_path, event)

        return Handler(on_change)

    def start(self):
        event_handler = self.__handler__(self.on_change)
        self.observer.schedule(event_handler, self.directory, recursive=True)
        self.observer.start()


    def join(self):
        self.observer.join()


class TimestampFixer(object):
    def __init__(self, dirs):
        self.observers = [FsObserver(directory,
                                     self.onchange)
                          for directory in dirs]

    def start(self):
        for obs in self.observers:
            logging.info(f"starting observer for directory {obs.directory}")
            obs.start()
        for obs in self.observers:
            obs.join()

    @staticmethod
    def maybe_fix_time(filename):
        stat = os.stat(filename)

        secs_ago = round(time.time() - stat.st_mtime)
        logger.info("%s was modified %s seconds ago", filename, secs_ago)

        if secs_ago >= 0:
            return

        logger.info(
            "modified time stamp for %s file '%s' is %ss in the future. fixing...",
            change_type, filename, abs(secs_ago))
        os.utime(filename, None)

    @staticmethod
    def onchange(change_type, filename, event):
        time.sleep(2)
        TimestampFixer.maybe_fix_time(filename)
        if change_type == "moved":
            TimestampFixer.maybe_fix_time(event.dest_path)

def install(dirs):
    import subprocess
    # subprocess.call(["pip", "install", "watchdog"])
    subprocess.call(["sudo", "apt-get", "install", "-y", "python3-watchdog"])

    cmd = ["python3", "-u", __file__, "-q", "-d"] + dirs

    environment = "Environment=PYTHONUNBUFFERED=1"

    service_def = f"""
[Unit]
Description=Timestamp Fixer Service

[Service]
ExecStart={" ".join(cmd)}
{environment}

[Install]
WantedBy=default.target
"""

    subprocess.call(["/home/ealfonso/git/dotfiles/bin/install-systemd-service.sh",
                     "-d", service_def, "-u", "time-fixer"])

def main():
    parser = argparse.ArgumentParser()
    DEFAULT_DIRS = ["~/Downloads",
                    "~/pictures/auto-scrots",
                    "~/git/3d/",
                    "~/uploads/"]
    parser.add_argument("-d", "--dirs",
                        help="directory to observe",
                        nargs="+",
                        default=list(os.path.realpath(os.path.expanduser(dirname))
                                     for dirname in DEFAULT_DIRS))
    parser.add_argument("-q", "--quiet", help="quiet", action="store_true")
    parser.add_argument("-i", "--install", help="install", action="store_true")
    args = parser.parse_args()

    level = logging.INFO if args.quiet else logging.DEBUG
    logging.basicConfig(level=level)

    print(f"starting time-fixer with log-level {logging.getLevelName(level)}...")
    if args.install:
        install(args.dirs)
        return

    fixer = TimestampFixer(args.dirs)
    fixer.start()


if __name__ == "__main__":
    main()

# Local Variables:
# compile-command: "systemctl --user restart time-fixer"
# compile-command: "./time-fixer.py -i"
# End:
