#!/usr/bin/env python3

"""
Display all images recursively contained in a directory on a web browser.
"""


from __future__ import absolute_import

from six.moves.urllib import parse

import argparse
import base64
import http.server
import logging
import math
import mimetypes
import os
import re
import shutil
import socket
import subprocess
import threading
import traceback
import urllib.parse

import watchdog.events
import watchdog.observers

# import queue

try:
    from .version import __version__
except Exception as ex:
    __version__ = "unknown"


class FsObserver():
    def __init__(self, directory, on_change):
        self.directory = directory
        self.observer = watchdog.observers.Observer()
        self.on_change = on_change

    @staticmethod
    def __handler__(on_change):
        class Handler(watchdog.events.FileSystemEventHandler):
            def __init__(self, on_change):
                self.on_change = on_change

            def on_created(self, event):
                print(f'File created: {event.src_path}')
                self.on_change("created", event.src_path, event)

            def on_modified(self, event):
                print(f'File modified: {event.src_path}')
                self.on_change("modified", event.src_path, event)

            def on_moved(self, event):
                print(f'File moved: {event.src_path} => {event.dest_path}')
                self.on_change("moved", event.src_path, event)

        return Handler(on_change)

    def start(self):
        event_handler = self.__handler__(self.on_change)
        self.observer.schedule(event_handler, self.directory, recursive=True)
        self.observer.start()


class ImageCrawler(object):
    def __init__(self, directory, image_regexp, skip_image_regexp, reverse):
        self.images = []
        self.directory = directory
        self.image_regexp = image_regexp
        self.skip_image_regexp = skip_image_regexp
        self.reverse = reverse

    def maybe_add_image(self, filename, verbose = False):
        if re.search(self.image_regexp, filename) and not (
                self.skip_image_regexp and
                re.search(self.skip_image_regexp, filename)):
            self.images.append(filename)
        elif verbose:
            logging.info("skipping filename %s")

    def start(self):
        for (root, dirs, files) in os.walk(self.directory):
            del dirs
            for base_filename in sorted(files, reverse=self.reverse):
                filename = os.path.join(root, base_filename)
                self.maybe_add_image(filename)

class HttpHandler(http.server.BaseHTTPRequestHandler):
    """
    Display all images recursively contained in a directory on a web browser.
    """

    def __init__(self, images, dimensions, image_size):
        self.images = images
        self.dimensions = dimensions
        self.image_size = image_size

    def __call__(self, *args, **kwargs):
        # https://stackoverflow.com/a/58909293/1941755
        super(HttpHandler, self).__init__(*args, **kwargs)


    def respond(self, status, body):
        """Sends an http response."""
        self.send_response(status)
        self.send_header("Content-type", "text/plain")
        self.wfile.write(body.encode())
        self.end_headers()

    FILE_PREFIX = "/file"

    def do_GET(self):
        """Handle GET requests."""
        try:
            m = re.match("/page/([0-9]+)$", self.path)
            if m:
                self.serve_page(int(m.group(1)))
            elif self.path in ("/", ""):
                self.serve_page(1)
            elif self.path.startswith(self.FILE_PREFIX):
                filename = urllib.parse.unquote(self.path[len(self.FILE_PREFIX):])
                self.serve_file(filename)
            elif self.path == "/version":
                self.respond(200, __version__)
            else:
                self.respond(400, "unknown route: {}".format(self.path))
        except (ConnectionResetError, BrokenPipeError):
            # ignore errors from typical browser clients
            pass

    @staticmethod
    def generate_pdf(files, output, quality=15):
        cmd = ["convert"] + files + [f"-quality {quality}", output]
        logging.warning("generating pdf: %s", cmd)
        subprocess.check_output(cmd, stderr=subprocess.PIPE)

    def do_POST(self):
        """Handle POST requests."""
        try:
            if self.path.startswith("/generate-pdf"):
                ctype = self.headers["Content-Type"]
                if ctype == 'multipart/form-data':
                    raise NotImplementedError("unimplemented")
                elif ctype == 'application/x-www-form-urlencoded':
                    length = int(self.headers["Content-Length"])
                    data = self.rfile.read(length)
                    postvars = parse.parse_qs(data, keep_blank_values=1)
                else:
                    postvars = {}
                files = postvars[b"files"][0].decode().split(",")
                output = "/tmp/generated.pdf" # TODO
                if not files:
                    self.send_response(400)
                    self.end_headers()
                    self.wfile.write("no files selected")
                    return
                try:
                    self.generate_pdf(files, output)
                except subprocess.CalledProcessError as exc:
                    traceback.print_exc()
                    self.send_response(500)
                    contents = str(exc).encode()
                    contents += "\n\noutput: ".encode() + exc.output
                    contents += "\n\nerror: ".encode() + (exc.stderr or b"")
                    self.end_headers()
                    self.wfile.write(contents)
                    return
                self.serve_file(output)
            else:
                self.respond(400, "unknown route: {}".format(self.path))
        except ConnectionResetError:
            # ignore errors from typical browser clients
            pass

    def serve_page(self, page_number):
        rows, cols = self.dimensions
        images_per_page = rows * cols
        page_images = self.images[
            (page_number-1)*images_per_page:
            (page_number)*images_per_page]
        total_pages = math.ceil(len(self.images)/images_per_page)
        table = """<table>"""
        for (i, filename) in enumerate(page_images):
            if i%cols == 0:
                table += "<tr>"
            filename_href = f"{self.FILE_PREFIX}{filename}"
            checkbox_id = base64.b64encode(filename.encode()).decode()
            table += (
                f"""<td>
                <a href="{filename_href}">
                <img src="{filename_href}" width="{self.image_size}" height="{self.image_size}">
                </a>
                <input type="checkbox" id="{checkbox_id}">
                </td>\n"""
            )
            if i % cols == cols - 1:
                table += "</tr>\n\n"
        table += """</table>"""
        prev_page_href = "/page/{}".format(max(1, page_number - 1))
        next_page_href = "/page/{}".format(min(page_number + 1, total_pages))

        javascript = f"""
            window.addEventListener("keydown", function (event) {{
  if (event.defaultPrevented) {{
    return; // Do nothing if the event was already processed
  }}

  console.log(event.key);
  switch (event.key) {{
    case "Left": // IE/Edge specific value
    case "ArrowLeft":
      // Do something for "left arrow" key press.
            window.location.href = "{prev_page_href}";
            break;
    case "Right": // IE/Edge specific value
    case "ArrowRight":
      // Do something for "right arrow" key press.
            window.location.href = "{next_page_href}";
            break;
    case "Enter":
      // Do something for "enter" or "return" key press.
      break;
    default:
      return; // Quit when this doesn't handle the key event.
  }}

  // Cancel the default action to avoid it being handled twice
  event.preventDefault();
}}, true);"""
        title = f"Page {page_number}/{total_pages}"
                # TODO " of {self.directory}"

        javascript += """
function fillInFilenames() {
        const boxes = [...document.querySelectorAll("input[type='checkbox']:checked")];
        const filenames = boxes.map(x => atob(x.id));
        console.log(filenames);
        const filesInput = document.getElementById("generate-pdf-filenames");
        filesInput.value = filenames.join(",");
        return true;
}

function deselectAll() {
   const boxes = [...document.querySelectorAll("input[type='checkbox']:checked")];
   for (var box of boxes) box.checked = false;
}
"""


        doc = f"""<!DOCTYPE html>
<html>
  <head>
    <meta charset="UTF-8">
    <title>{title}</title>
  </head>
  <body>

<table>
        <tr>
        <td>
<form id="generate-pdf" action="/generate-pdf" method="POST" onsubmit="fillInFilenames()">
   <input type="hidden" name="files" id="generate-pdf-filenames" value="" />
   <input type="submit" value="Generate PDF" />
</form>
        </td>
        <td>
<input type="button" value="Deselect All" onclick="deselectAll()"/>
        </td>
        </tr>
        </table>

<script>{javascript}</script>

 <a href={prev_page_href}>Prev</a> <a href="/page/1"> Home </a> <a href={next_page_href}>Next</a>

{table}
  </body>
</html>
"""
        self.send_response(200)
        self.send_header("Content-type", "text/html")
        self.end_headers()
        self.wfile.write(doc.encode())

    def serve_file(self, filename):
        with open(filename, "rb") as fh:
            self.send_response(200)
            ext = ".{}".format(filename.split(".")[-1].lower())
            content_type = mimetypes.types_map.get(ext)
            if content_type:
                self.send_header("Content-type", content_type)
            else:
                logging.warning("no content-type found for %s", filename)
            self.send_header("Cache-Control", "private")
            self.end_headers()
            shutil.copyfileobj(fh, self.wfile)

    def log_request(self, *args):
        """Reduce verbose logging."""
        pass

class ImageOverview(object):
    """ImageOverview provides a web server to display all images in a directory."""

    def __init__(self, directory, image_regexp, skip_image_regexp, reverse):
        self.crawler = ImageCrawler(directory, image_regexp, skip_image_regexp, reverse)

        def on_change(change_type, filename, event, crawler=self.crawler):
            print("DDEBUG imageoverview.py uxpm: value of change_type: {}".format(change_type))
            print("DDEBUG imageoverview.py qohz: value of filename: {}".format(filename))
            print("DDEBUG imageoverview.py e7sx: value of event: {}".format(event))
            to_add = None
            if change_type == "created":
                to_add = filename
            elif change_type == "moved":
                to_add = event.dest_path
            else:
                return
            if os.path.isfile(to_add):
                crawler.maybe_add_image(to_add)

        self.observer = FsObserver(directory, on_change)

        self.crawl_thread = None
        self.http_thread = None

    def start_observer(self):
        self.observer.start()

    def start_crawl(self):
        # TODO maybe stop existing
        self.crawl_thread = threading.Thread(target=self.crawler.start, args=())
        self.crawl_thread.start()
        return self.crawl_thread

    def start_http(self, port, dimensions, image_size, listen = ""):
        server_address = (listen, port)
        httpd = http.server.HTTPServer(server_address,
                                       HttpHandler(images = self.crawler.images,
                                                   dimensions=dimensions,
                                                   image_size=image_size))
        logging.info("starting http server on %s", server_address)

        # TODO maybe stop
        self.http_thread = threading.Thread(target=httpd.serve_forever, args=())
        self.http_thread.start()

        hostname = socket.gethostname()
        url = f"http://{hostname}.local:{port}"
        return url

def x_www_browse(url):
    subprocess.Popen(["x-www-browser", url])

def main():
    def parse_dimensions_spec(spec):
        m = re.match("([0-9]+)x([0-9]+)", spec)
        if not m:
            raise ValueError("invalid dimension spec: {}".format(spec))
        dimensions = (int(m.group(1)), int(m.group(2)))
        return dimensions

    parser = argparse.ArgumentParser()
    parser.add_argument("-p", "--port", help="port number", default=6969, type=int)
    parser.add_argument("-d", "--directory",
                        help="directory with images", default=os.getcwd())
    parser.add_argument("-D", "--dimensions",
                        help="image grid dimensions, in ROWSxCOLS format, e.g. 10x10",
                        default=(4, 10),
                        type=parse_dimensions_spec)
    parser.add_argument("-s", "--image_size", help="html image size", default=40)
    parser.add_argument("-x", "--image_regexp",
                        help="regexp used to filter image files",
                        default="(?i)[.](jpe?g|png|mp4)$")
    parser.add_argument("-X", "--skip_image_regexp",
                        help="negative regexp used to filter out image files")
    parser.add_argument("-r", "--reverse",
                        help="reverse sorting of filenames, show latest first",
                        action="store_true")
    parser.add_argument("-v", "--verbose", help="verbose", action="store_true")
    args = parser.parse_args()

    logging.getLogger(__name__).setLevel(logging.DEBUG if args.verbose
                                         else logging.INFO)

    io = ImageOverview(directory=args.directory,
                       image_regexp=args.image_regexp,
                       skip_image_regexp=args.skip_image_regexp,
                       reverse=args.reverse)
    io.start_observer()
    io.start_crawl()
    url = io.start_http(args.port, args.dimensions, args.image_size)
    x_www_browse(url)
    io.http_thread.join()

if __name__ == "__main__":
    main()

# Local Variables:
# compile-command: "./imageoverview.py -d ~/Downloads/ -v"
# End:
