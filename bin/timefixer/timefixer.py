#!/usr/bin/env python3


"""
Observe a directory and new fix files with timestamps in the future.
"""


import argparse
import logging
import os
import time
import subprocess

logger = logging

try:
    import watchdog.events
    import watchdog.observers
except ImportError as ex:
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
        """Start the FS Observer to listen and react to filesystem events."""
        event_handler = self.__handler__(self.on_change)
        self.observer.schedule(event_handler, self.directory, recursive=True)
        self.observer.start()


    def join(self):
        """Blockingly join the observer thread."""
        self.observer.join()


class TimestampFixer:
    """Fix created or modified files in the given directories with timestamps in the future."""
    def __init__(self, dirs):
        self.observers = [FsObserver(directory,
                                     self.onchange)
                          for directory in dirs]

    def start(self):
        """Start the timestamp fixer service."""
        for obs in self.observers:
            logging.info("starting observer for directory %s", obs.directory)
            obs.start()
        for obs in self.observers:
            obs.join()

    @staticmethod
    def maybe_fix_time(filename):
        """Fix filename modified timestamp: if it is in the future, reset it to now."""
        stat = os.stat(filename)

        secs_ago = round(time.time() - stat.st_mtime)
        logger.info("%s was modified %s seconds ago", filename, secs_ago)

        if secs_ago >= 0:
            return

        logger.info(
            "modified time stamp for file '%s' is %ss in the future. fixing...",
            filename, abs(secs_ago))
        os.utime(filename, None)

    @staticmethod
    def onchange(change_type, filename, event):
        """onchange callback for fileobserver"""
        time.sleep(2)
        TimestampFixer.maybe_fix_time(filename)
        if change_type == "moved":
            TimestampFixer.maybe_fix_time(event.dest_path)

def install_systemd(name, run_cmd, environment, as_user = True,
                    description = None):
    """Installs the given command as a systemd service with the given name."""
    if as_user:
        config = os.path.expanduser(f"~/.config/systemd/user/{name}.service")
    else:
        config = f"/etc/systemd/system/{name}.service"

    environment_str = ",".join(
        f"{k}={v}" for k, v in environment.items())

    description = description or f"{name} Service"
    contents = f"""
[Unit]
Description={description}

[Service]
ExecStart={" ".join(run_cmd)}
Environment={environment_str}

[Install]
WantedBy=default.target
"""
    with open(config, "w") as fh:
        fh.write(contents)

    unit = f"{name}.service"
    if as_user:
        start_commands = [["systemctl", "--user", "enable", unit],
                          ["systemctl", "--user", "daemon-reload"],
                          ["systemctl", "--user",  "restart", unit]]
    else:
        start_commands = [["sudo", "systemctl", "enable", unit],
                          ["sudo", "systemctl", "daemon-reload"],
                          ["sudo", "systemctl",  "restart", unit]]
    for cmd in start_commands:
        print("DDEBUG timefixer.py cftu: value of cmd: {}".format(cmd))
        subprocess.run(cmd, check = True)
    logging.info(f"succesfully installed {unit} with contents: \n{contents}")

def install_time_fixer_service(dirs, as_user = True):
    """Install the timefixer serrvice."""
    name = "timefixer"
    run_cmd = ["python3", "-u", __file__, "-q", "-d"] + dirs
    environment = {"PYTHONUNBUFFERED": "1"}
    if not as_user:
        subprocess.call(["sudo", "apt-get", "install", "-y", "python3-watchdog"])
    install_systemd(name=name, run_cmd=run_cmd, environment=environment,
                    as_user = as_user)

def main():
    """Main function."""
    parser = argparse.ArgumentParser()
    default_dirs = ["~/Downloads", "~/pictures/auto-scrots", "~/git/3d/", "~/uploads/"]
    parser.add_argument("-d", "--dirs",
                        help="directory to observe",
                        nargs="+",
                        default=list(os.path.realpath(os.path.expanduser(dirname))
                                     for dirname in default_dirs))
    parser.add_argument("-q", "--quiet", help="quiet", action="store_true")
    parser.add_argument("-i", "--install", help="install", action="store_true")
    args = parser.parse_args()

    level = logging.INFO if args.quiet else logging.DEBUG
    logging.basicConfig(level=level)

    print(f"starting timefixer with log-level {logging.getLevelName(level)}...")
    if args.install:
        install_time_fixer_service(args.dirs)
        return

    fixer = TimestampFixer(args.dirs)
    fixer.start()


if __name__ == "__main__":
    main()

# Local Variables:
# compile-command: "systemctl --user restart timefixer"
# compile-command: "./timefixer.py -i"
# End:
