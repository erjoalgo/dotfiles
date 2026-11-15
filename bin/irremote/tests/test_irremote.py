"""Smoke tests for irremote, with a fake Broadlink device."""

import http.client
import http.server
import os
import sys
import tempfile
import threading
import unittest

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import irremote  # noqa: E402


class FakeDevice:
    """Records the IR packets it's asked to send."""

    def __init__(self):
        self.sent = []

    def auth(self):
        pass

    def send_data(self, packet):
        self.sent.append(packet)


class ButtonsTest(unittest.TestCase):

    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.directory = tmp.name
        self.device = FakeDevice()

    def press(self, spec):
        irremote.press_button(self.device, spec, 0, [self.directory])

    def test_button_text_is_valid(self):
        for text in ("LG_POWER", "LG_POWER,LG_VOLUPx10", "TCL_BACKx2"):
            self.assertTrue(irremote.button_text_is_valid(text), text)
        for text in ("lg_power", "LG POWER", "../etc/passwd", ""):
            self.assertFalse(irremote.button_text_is_valid(text), text)

    def test_persist_and_load(self):
        irremote.persist_button("TV_POWER", b"\x26\x00\x01", self.directory)
        self.assertEqual(irremote.load_button("TV_POWER", self.directory),
                         b"\x26\x00\x01")
        self.assertIn(("TV_POWER", os.path.join(self.directory, "TV_POWER")),
                      list(irremote.list_buttons(self.directory)))

    def test_load_missing_button(self):
        with self.assertRaises(KeyError):
            irremote.load_button("NO_SUCH_BUTTON", self.directory)

    def test_press_builtin_button(self):
        self.press("VIZIO_MENU")
        self.assertEqual(self.device.sent, [irremote.BUTTONS["VIZIO_MENU"]])

    def test_press_sequence_and_repeats(self):
        irremote.persist_button("A", b"a", self.directory)
        irremote.persist_button("B", b"b", self.directory)
        self.press("A,Bx3")
        self.assertEqual(self.device.sent, [b"a", b"b", b"b", b"b"])

    def test_press_composite_button(self):
        """A button file can hold a sequence of other buttons."""
        irremote.persist_button("A", b"a", self.directory)
        irremote.persist_button("B", b"b", self.directory)
        irremote.persist_button("BOTH", b"A,Bx2\n", self.directory)
        self.press("BOTH")
        self.assertEqual(self.device.sent, [b"a", b"b", b"b"])

    def test_press_missing_button(self):
        with self.assertRaises(Exception):
            self.press("NO_SUCH_BUTTON")
        self.assertEqual(self.device.sent, [])


class ServiceTest(unittest.TestCase):
    """The HTTP API, on a free local port."""

    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        irremote.persist_button("A", b"a", tmp.name)
        self.device = FakeDevice()
        handler = irremote.IRService(device=self.device, delay=0,
                                     directory=tmp.name,
                                     local_directory=tmp.name)
        self.httpd = http.server.HTTPServer(("127.0.0.1", 0), handler)
        threading.Thread(target=self.httpd.serve_forever, daemon=True).start()
        self.addCleanup(self.httpd.server_close)
        self.addCleanup(self.httpd.shutdown)

    def get(self, path):
        conn = http.client.HTTPConnection("127.0.0.1", self.httpd.server_port,
                                          timeout=5)
        self.addCleanup(conn.close)
        conn.request("GET", path)
        resp = conn.getresponse()
        return resp.status, resp.read().decode()

    def test_press_button(self):
        self.assertEqual(self.get("/Ax2"), (200, "success!"))
        self.assertEqual(self.device.sent, [b"a", b"a"])

    def test_invalid_button_name(self):
        status, _ = self.get("/not-a-button")
        self.assertEqual(status, 400)
        self.assertEqual(self.device.sent, [])

    def test_missing_button(self):
        status, _ = self.get("/NO_SUCH_BUTTON")
        self.assertEqual(status, 500)


if __name__ == "__main__":
    unittest.main()
