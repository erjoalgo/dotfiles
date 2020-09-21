#!/usr/bin/python3

"""
Display all images recursively contained in a directory on a web browser.
"""


import argparse
import http.server
import logging
import mimetypes
import os
import queue
import re
import shutil
import threading
import traceback
import urllib.parse

try:
    from .version import __version__
except Exception as ex:
    logging.error("unable to determine version: %s", ex)
    __version__ = "unknown"

def crawl_images(directory, images_queue,
                 image_regexp="(?i)[.](jpe?g|png|mp4)$"):
    for (root, dirs, files) in os.walk(directory):
        for filename in files:
            if re.search(image_regexp, filename):
                images_queue.put(os.path.join(root, filename))

def MakeImageOverviewHandler(directory, dimensions, image_size):
    images_queue = queue.Queue()
    threading.Thread(target=crawl_images, kwargs=
                     {"directory":directory, "images_queue":images_queue}).start()

    class ImageOverviewHandler(http.server.BaseHTTPRequestHandler):
        """A web server to serve info about a chrome browser instance."""

        def __init__(self, *args, **kwargs):
            self.images = []
            self.images_queue = images_queue
            # self.dimensions = dimensions
            # self.image_size = image_size
            super(ImageOverviewHandler, self).__init__(*args, **kwargs)

        def flush_images_queue(self):
            if self.images_queue is not None:
                while True:
                    try:
                        image = self.images_queue.get(block=False)
                    except queue.Empty:
                        break
                    if image is None:
                        self.images_queue = None
                    else:
                        self.images.append(image)

        def respond(self, status, body):
            """Sends an http response."""
            self.send_response(status)
            self.send_header("Content-type", "text/plain")
            self.wfile.write(body.encode())
            self.end_headers()

        def do_GET(self):
            """Handle GET requests."""
            FILE_PREFIX = "/file"
            print ("DEBUG imageoverview 3mxp: value of self.path: {}".format(self.path))
            m = re.match("/page/([0-9]+)$", self.path)
            if m or self.path in ("/", ""):
                page_number = int(m.group(1)) if m else 1
                rows, cols = dimensions
                images_per_page = rows * cols
                self.flush_images_queue()
                page_images = self.images[
                    (page_number-1)*images_per_page:
                    (page_number)*images_per_page]
                table = """<table>"""
                for (i, filename) in enumerate(page_images):
                    if i%cols == 0:
                        table += "<tr>"
                    image_size = 40
                    table += (
                        """<td><a href="{filename_href}">
                        <img src="{filename_href}" width="{image_size}" height="{image_size}">
                        </a></td>\n""".format(
                            filename_href="{}{}".format(FILE_PREFIX, filename),
                            image_size=image_size)
                    )
                    if i % cols == cols - 1:
                        table += "</tr>\n\n"

                prev_page_href = "/page/{}".format(page_number - 1)
                next_page_href = "/page/{}".format(page_number + 1)

                javascript = """
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
    }}, true);""".format(next_page_href=next_page_href,
                         prev_page_href=prev_page_href)

                doc = """<!DOCTYPE html>
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
    """.format(title="Images in {}: page {}".format(
        "TODO",
        page_number),
               table=table,
               javascript = javascript,
               next_page_href=next_page_href,
               prev_page_href=prev_page_href
    )
                self.send_response(200)
                self.send_header("Content-type", "text/html")
                self.end_headers()
                self.wfile.write(doc.encode())
            elif self.path.startswith(FILE_PREFIX):
                try:
                    filename = urllib.parse.unquote(self.path[len(FILE_PREFIX):])
                    with open(filename, "rb") as fh:
                        self.send_response(200)
                        ext = filename.split(".")[-1]
                        content_type = mimetypes.types_map.get(ext)
                        if content_type:
                            self.send_header("Content-type", content_type)
                        self.send_header("Cache-Control", "private")
                        self.end_headers()
                        shutil.copyfileobj(fh, self.wfile)
                except Exception as ex:
                    traceback.print_exc()
                    self.send_response(500)
                    self.wfile.write(str(ex).encode())
            elif self.path == "/version":
                self.respond(200, __version__)
            else:
                self.respond(400, "unknown route: {}".format(self.path))
    return ImageOverviewHandler

def parse_dimensions_spec(spec):
    m = re.match("([0-9]+)x[0-9]+")
    if not m:
        raise ValueError("invalid dimension spec: {}".format(spec))
    return (int(m.group(1), m.group(2)))

def main():
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--port", help="port number", default=6969)
    parser.add_argument("-d", "--images_directory",
                        help="directory with images", default=os.getcwd())
    parser.add_argument("-D", "--dimensions", help="image grid dimensions", default=(4, 10))
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
                                   MakeImageOverviewHandler(directory=args.images_directory,
                                                            dimensions=args.dimensions,
                                                            image_size=args.image_size))
    logging.info("starting http server on %s", server_address)
    httpd.serve_forever()


if __name__ == "__main__":
    main()

# Local Variables:
# compile-command: "./imageoverview.py -d ~/Downloads/android/"
# End:
