#!/usr/bin/env python3


"""
Observe a directory and new fix files with timestamps in the future.
"""


import argparse
import logging
import os
import socket
import subprocess
import threading
import time

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


class UnixSocketServer:
    """Mostly AI-generated unix socket server."""

    def __init__(self, path: str):
        """Initializes the server configuration and socket path."""
        self.path = path
        self._socket = None
        self.last_modified = None

    def update_last_modified(self, filename):
        """Update the last modified filename."""
        self.last_modified = filename

    def start(self):
        """Prepares the socket file, binds to the path, and starts the listen loop."""
        # 1. Clean up any stale socket file left from previous crashes
        self._cleanup()

        # 2. Configure a Unix Stream socket
        self._socket = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)

        try:
            # 3. Bind and begin listening
            self._socket.bind(self.path)
            self._socket.listen(1)
            logging.info("Server started. Listening on %s...", self.path)

            # 4. Handle connections sequentially
            while True:
                logging.info("Waiting for a connection...")
                connection, _ = self._socket.accept()
                self._handle_client(connection)
        finally:
            self.stop()

    def _handle_client(self, connection: socket.socket):
        """Manages communication with an individual connected client."""
        with connection:
            logging.info("Client connected!")
            while True:
                data = connection.recv(1024)
                if not data:
                    logging.info("Client disconnected.")
                    break  # Client closed connection cleanly

                logging.info("Received data: %s", data.decode('utf-8'))
                body_bytes = (self.last_modified or "None").encode()
                http_response = (
                    "HTTP/1.1 200 OK\r\n"
                    "Content-Type: application/json; charset=utf-8\r\n"
                    f"Content-Length: {len(body_bytes)}\r\n"
                    "Connection: close\r\n"
                    "\r\n"
                ).encode() + body_bytes

                connection.sendall(http_response)  # Echo back data
                logging.info("Sent back data: %s", http_response)

    def stop(self):
        """Safely closes the socket descriptor and removes the socket file."""
        logging.info("Stopping server and cleaning up assets...")
        if self._socket:
            self._socket.close()
        self._cleanup()

    def _cleanup(self):
        if os.path.exists(self.path):
            os.remove(self.path)


class TimestampFixer:
    """Fix created or modified files in the given directories with timestamps in the future."""
    def __init__(self, dirs, update_fn = None):
        self.observers = [FsObserver(directory, self.onchange)
                          for directory in dirs]
        self.update_fn = update_fn

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

    def onchange(self, change_type, filename, event):
        """onchange callback for fileobserver"""
        time.sleep(2)
        new_filename = filename
        if change_type == "moved":
            new_filename = event.dest_path
        try:
            TimestampFixer.maybe_fix_time(new_filename)
        except Exception as ex:
            logging.error("failed to fix time on %s: %s", new_filename, ex)
            new_filename = None
        if new_filename and change_type != "modified":
            self.update_fn(new_filename)

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
        subprocess.run(cmd, check = True)
    logging.info("succesfully installed %s with contents: \n%s", unit, contents)

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
    parser.add_argument("-i", "--install-systemd", help="install", action="store_true")
    parser.add_argument("-p", "--socket-path",
                        default=os.path.join(
                            os.getenv("XDG_RUNTIME_DIR"),
                            "timestampfixer.sock"))

    args = parser.parse_args()

    level = logging.INFO if args.quiet else logging.DEBUG
    logging.basicConfig(level=level)

    print(f"starting timefixer with log-level {logging.getLevelName(level)}...")
    if args.install:
        install_time_fixer_service(args.dirs)
        return

    socket_server = UnixSocketServer(args.socket_path)
    fixer = TimestampFixer(args.dirs, update_fn=socket_server.update_last_modified)

    socket_server_thread = threading.Thread(target=socket_server.start,
                                            daemon=True)
    socket_server_thread.start()

    fixer.start()


if __name__ == "__main__":
    main()

# Local Variables:
# compile-command: "systemctl --user restart timefixer"
# compile-command: "./timefixer.py -i"
# End:
