This service helps to fix newly created or modified files with timestamps in the future.
This can happen occasionally when downloading files with incorrect timestamps based on different time zones.
Fixing such cases makes it possible to reliably fetch the latest modified file from a list of directories, which helps my productivity.

It runs on Linux, as a systemd user service.

To install:

`$ pip install timefixer`

Install and start the service, watching `~/Downloads`:

`$ timefixer --install-systemd`

Or watch other directories:

`$ timefixer --install-systemd -d ~/Downloads ~/Pictures`

The service also reports the most recently created or moved file over a unix socket,
in `$XDG_RUNTIME_DIR/timestampfixer.sock` (or `~/.cache/timestampfixer.sock` if `XDG_RUNTIME_DIR` isn't set):

`$ curl --unix-socket "$XDG_RUNTIME_DIR/timestampfixer.sock" http://localhost/`
