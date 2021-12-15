#!/usr/bin/python3

"""
Display all images recursively contained in a directory on a web browser.
"""


import argparse
import http.server
import logging
import math
import mimetypes
import os
import queue
import re
import shutil
import subprocess
import threading
import urllib.parse

try:
    from .version import __version__
except Exception as ex:
    logging.warning("unable to determine version: %s", ex)
    __version__ = "unknown"

class ImageOverviewHandler(http.server.BaseHTTPRequestHandler):
    """
    Display all images recursively contained in a directory on a web browser.
    """

    def __init__(self, directory, dimensions, image_size):
        self.images = []
        self.directory = directory
        self.dimensions = dimensions
        self.image_size = image_size
        threading.Thread(target=self.crawl_images).start()

    def __call__(self, *args, **kwargs):
        # https://stackoverflow.com/a/58909293/1941755
        super(ImageOverviewHandler, self).__init__(*args, **kwargs)


    def crawl_images(self, image_regexp="(?i)[.](jpe?g|png|mp4)$"):
        for (root, dirs, files) in os.walk(self.directory):
            for filename in files:
                if re.search(image_regexp, filename):
                    self.images.append(os.path.join(root, filename))

    def respond(self, status, body):
        """Sends an http response."""
        self.send_response(status)
        self.send_header("Content-type", "text/plain")
        self.wfile.write(body.encode())
        self.end_headers()

    FILE_PREFIX = "/file"


    def do_GET(self):
        """Handle GET requests."""
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
            table += (
                """<td><a href="{filename_href}">
                <img src="{filename_href}" width="{image_size}" height="{image_size}">
                </a></td>\n""".format(
                    filename_href="{}{}".format(self.FILE_PREFIX, filename),
                    image_size=self.image_size)
            )
            if i % cols == cols - 1:
                table += "</tr>\n\n"

        prev_page_href = "/page/{}".format(page_number - 1)
        next_page_href = "/page/{}".format(page_number + 1)

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
        title = f"Page {page_number}/{total_pages} of {self.directory}"

        doc = f"""<!DOCTYPE html>
<html>
  <head>
    <meta charset="UTF-8">
    <title>{title}</title>
  </head>
  <body>
{table}
<script>{javascript}</script>
<br>
 <a href={prev_page_href}>Prev</a> <a href="/page/1"> Home </a> <a href={next_page_href}>Next</a>
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
                logging.warn("no content-type found for %s", filename)
            self.send_header("Cache-Control", "private")
            self.end_headers()
            shutil.copyfileobj(fh, self.wfile)

    def log_request(self, *args):
        """Reduce verbose logging."""
        pass

def main():
    def parse_dimensions_spec(spec):
        m = re.match("([0-9]+)x([0-9]+)", spec)
        if not m:
            raise ValueError("invalid dimension spec: {}".format(spec))
        dimensions = (int(m.group(1)), int(m.group(2)))
        return dimensions

    parser = argparse.ArgumentParser()
    parser.add_argument("--port", help="port number", default=6969)
    parser.add_argument("-d", "--images_directory",
                        help="directory with images", default=os.getcwd())
    parser.add_argument("-D", "--dimensions",
                        help="image grid dimensions, in ROWSxCOLS format, e.g. 10x10",
                        default=(4, 10),
                        type=parse_dimensions_spec)
    parser.add_argument("-s", "--image_size", help="html image size", default=40)
    parser.add_argument("-v", "--verbose", help="verbose", action="store_true")
    args = parser.parse_args()

    if args.verbose:
        log_level = logging.DEBUG
    else:
        log_level = logging.INFO
    logging.getLogger(__name__).setLevel(log_level)

    server_address = ('', args.port)
    httpd = http.server.HTTPServer(server_address,
                                   ImageOverviewHandler(directory=args.images_directory,
                                                        dimensions=args.dimensions,
                                                        image_size=args.image_size))
    logging.info("starting http server on %s", server_address)
    subprocess.Popen(["x-www-browser", f"http://localhost:{args.port}"])
    httpd.serve_forever()


if __name__ == "__main__":
    main()

# Local Variables:
# compile-command: "./imageoverview.py -d ~/Downloads/android/"
# End:
