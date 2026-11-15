"""Smoke tests for imageoverview."""

import functools
import http.client
import http.server
import json
import os
import shutil
import sys
import tempfile
import threading
import unittest
import urllib.parse
from unittest import mock

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import imageoverview  # noqa: E402

IMAGE_REGEXP = "(?i)[.](jpe?g|png|mp4|mpe?g)$"


def make_tree(directory, filenames):
    """Create the given files, relative to directory, each holding its name."""
    for name in filenames:
        path = os.path.join(directory, name)
        os.makedirs(os.path.dirname(path), exist_ok=True)
        with open(path, "w") as fh:
            fh.write(name)


class CrawlerTest(unittest.TestCase):

    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.directory = tmp.name
        make_tree(self.directory, ["b.jpg", "a.PNG", "notes.txt",
                                   "sub/c.mp4", "sub/thumb-d.jpg"])

    def crawl(self, skip_image_regexp=None, reverse=False):
        crawler = imageoverview.ImageCrawler(
            self.directory, IMAGE_REGEXP, skip_image_regexp, reverse)
        crawler.start()
        return [os.path.relpath(f, self.directory) for f in crawler.images]

    def test_finds_images_recursively_in_order(self):
        self.assertEqual(self.crawl(),
                         ["a.PNG", "b.jpg", "sub/c.mp4", "sub/thumb-d.jpg"])

    def test_skip_regexp(self):
        self.assertEqual(self.crawl(skip_image_regexp="thumb-"),
                         ["a.PNG", "b.jpg", "sub/c.mp4"])

    def test_reverse(self):
        self.assertEqual(self.crawl(reverse=True),
                         ["b.jpg", "a.PNG", "sub/thumb-d.jpg", "sub/c.mp4"])


class ServerTest(unittest.TestCase):
    """The HTTP server, on a free local port."""

    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.directory = tmp.name
        make_tree(self.directory, ["one.jpg", "two & <three>.png", "clip.mp4",
                                   "unlisted.jpg"])
        self.images = [os.path.join(self.directory, name)
                       for name in ("one.jpg", "two & <three>.png", "clip.mp4")]
        handler = functools.partial(imageoverview.HttpHandler,
                                    images=self.images, dimensions=(1, 2),
                                    image_size=40)
        self.httpd = http.server.ThreadingHTTPServer(("127.0.0.1", 0), handler)
        threading.Thread(target=self.httpd.serve_forever, daemon=True).start()
        self.addCleanup(self.httpd.server_close)
        self.addCleanup(self.httpd.shutdown)

    def request(self, method, path, body=None, headers=None):
        conn = http.client.HTTPConnection("127.0.0.1", self.httpd.server_port,
                                          timeout=5)
        self.addCleanup(conn.close)
        conn.request(method, path, body=body, headers=headers or {})
        resp = conn.getresponse()
        return resp.status, resp.getheader("Content-type"), resp.read()

    def file_path(self, filename):
        return "/file" + urllib.parse.quote(filename)

    def test_pages(self):
        status, ctype, body = self.request("GET", "/")
        self.assertEqual((status, ctype), (200, "text/html"))
        page = body.decode()
        self.assertIn("<title>Page 1/2</title>", page)
        self.assertIn(self.file_path(self.images[0]), page)
        # names are escaped in the page
        self.assertIn(self.file_path(self.images[1]), page)
        self.assertNotIn("<three>", page)
        self.assertNotIn(self.file_path(self.images[2]), page)

        _, _, body = self.request("GET", "/page/2")
        page = body.decode()
        self.assertIn("<title>Page 2/2</title>", page)
        self.assertIn("<video controls", page)
        self.assertIn(self.file_path(self.images[2]), page)

    def test_serves_listed_images(self):
        status, ctype, body = self.request("GET", self.file_path(self.images[1]))
        self.assertEqual((status, ctype, body),
                         (200, "image/png", b"two & <three>.png"))

    def test_refuses_files_that_are_not_listed_images(self):
        for filename in (os.path.join(self.directory, "unlisted.jpg"),
                         "/etc/passwd"):
            status, _, _ = self.request("GET", self.file_path(filename))
            self.assertEqual(status, 404, filename)

    def test_listed_image_deleted_since_the_crawl(self):
        os.remove(self.images[0])
        status, _, _ = self.request("GET", self.file_path(self.images[0]))
        self.assertEqual(status, 404)

    def test_version_and_favicon(self):
        status, _, body = self.request("GET", "/version")
        self.assertEqual((status, body.decode()),
                         (200, imageoverview.__version__))
        status, ctype, _ = self.request("GET", "/favicon.ico")
        self.assertEqual((status, ctype), (200, "image/x-icon"))

    def test_unknown_route(self):
        status, _, _ = self.request("GET", "/nope")
        self.assertEqual(status, 400)

    def post_pdf(self, fields):
        body = urllib.parse.urlencode(fields)
        return self.request(
            "POST", "/generate-pdf", body,
            {"Content-Type": "application/x-www-form-urlencoded"})

    def test_generate_pdf(self):
        def fake_generate_pdf(files, output, quality=15):
            with open(output, "wb") as fh:
                fh.write(b"%PDF " + json.dumps(files).encode())

        with mock.patch.object(imageoverview.HttpHandler, "generate_pdf",
                               staticmethod(fake_generate_pdf)):
            status, ctype, body = self.post_pdf(
                {"files": json.dumps(self.images[:2])})
        self.assertEqual((status, ctype), (200, "application/pdf"))
        self.assertEqual(body, b"%PDF " + json.dumps(self.images[:2]).encode())

    def test_generate_pdf_rejects_bad_requests(self):
        unlisted = os.path.join(self.directory, "unlisted.jpg")
        for fields, expected_status in [
                ({}, 400),
                ({"files": ""}, 400),
                ({"files": "[]"}, 400),
                ({"files": "not json"}, 400),
                ({"files": json.dumps({"a": 1})}, 400),
                ({"files": json.dumps([unlisted])}, 400),
                ({"files": json.dumps(["/etc/passwd"])}, 400),
        ]:
            status, _, _ = self.post_pdf(fields)
            self.assertEqual(status, expected_status, fields)

    def test_generate_pdf_with_a_deleted_image(self):
        os.remove(self.images[0])
        status, _, _ = self.post_pdf({"files": json.dumps(self.images[:1])})
        self.assertEqual(status, 404)

    @unittest.skipUnless(shutil.which("convert"), "needs ImageMagick")
    def test_generate_pdf_error(self):
        # the files aren't real images, so convert fails
        status, _, _ = self.post_pdf({"files": json.dumps(self.images[:1])})
        self.assertEqual(status, 500)


if __name__ == "__main__":
    unittest.main()
