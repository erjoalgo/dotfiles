#!/usr/bin/python3

"""Pack an unpacked extension directory into a signed CRX3 and force-install
it via the ExtensionSettings policy, with a file:// update URL. No Web
Store, no browser binary needed for packing."""

import argparse
import hashlib
import io
import json
import os
import struct
import subprocess
import sys
import urllib.request
import zipfile

KEY_DIR = os.path.expanduser("~/.config/chrome-local-extensions")
OUT_TOP = os.path.expanduser("~/.local/share/chrome-local-extensions")


def varint(n):
    b = bytearray()
    while True:
        b.append((n & 0x7f) | (0x80 if n > 0x7f else 0))
        n >>= 7
        if not n:
            return bytes(b)


def field(num, data):
    # protobuf length-delimited field
    return varint(num << 3 | 2) + varint(len(data)) + data


def ensure_key(name):
    os.makedirs(KEY_DIR, exist_ok=True)
    os.chmod(KEY_DIR, 0o700)
    key = os.path.join(KEY_DIR, name + ".pem")
    if not os.path.exists(key):
        old_umask = os.umask(0o077)
        try:
            subprocess.run(["openssl", "genpkey", "-quiet", "-algorithm",
                            "RSA", "-pkeyopt", "rsa_keygen_bits:2048",
                            "-out", key],
                           check=True)
        finally:
            os.umask(old_umask)
    return key


def build_zip(src, manifest):
    buf = io.BytesIO()
    with zipfile.ZipFile(buf, "w", zipfile.ZIP_DEFLATED) as z:
        for root, dirs, files in os.walk(src):
            dirs[:] = sorted(d for d in dirs if not d.startswith("."))
            for f in sorted(files):
                path = os.path.join(root, f)
                arc = os.path.relpath(path, src)
                if arc == "manifest.json":
                    z.writestr(arc, json.dumps(manifest, indent=2))
                elif arc != "vendor.txt" and not f.startswith("."):
                    z.write(path, arc)
        # vendor.txt: "URL SHA256" lines for third-party files to fetch into
        # the extension's top level instead of checking them in.
        vendor = os.path.join(src, "vendor.txt")
        for line in open(vendor) if os.path.exists(vendor) else []:
            if not line.strip() or line.startswith("#"):
                continue
            url, sha256 = line.split()
            data = urllib.request.urlopen(url).read()
            if hashlib.sha256(data).hexdigest() != sha256:
                sys.exit(f"sha256 mismatch for {url}")
            z.writestr(os.path.basename(url), data)
    return buf.getvalue()


def pack(src, key, out):
    pub = subprocess.run(["openssl", "pkey", "-in", key, "-pubout",
                          "-outform", "DER"],
                         check=True, capture_output=True).stdout
    crx_id = hashlib.sha256(pub).digest()[:16]
    ext_id = "".join("abcdefghijklmnop"[int(c, 16)] for c in crx_id.hex())

    manifest = json.load(open(os.path.join(src, "manifest.json")))
    version = manifest["version"]
    crx_path = os.path.join(out, "extension.crx")
    manifest["update_url"] = "file://" + os.path.join(out, "updates.xml")
    archive = build_zip(src, manifest)

    signed_data = field(1, crx_id)
    to_sign = (b"CRX3 SignedData\x00" + struct.pack("<I", len(signed_data))
               + signed_data + archive)
    sig = subprocess.run(["openssl", "dgst", "-sha256", "-sign", key],
                         input=to_sign, check=True,
                         capture_output=True).stdout

    header = (field(2, field(1, pub) + field(2, sig))
              + field(10000, signed_data))
    with open(crx_path, "wb") as fh:
        fh.write(b"Cr24" + struct.pack("<II", 3, len(header))
                 + header + archive)

    with open(os.path.join(out, "updates.xml"), "w") as fh:
        fh.write(f"""<?xml version='1.0' encoding='UTF-8'?>
<gupdate xmlns='http://www.google.com/update2/response' protocol='2.0'>
  <app appid='{ext_id}'>
    <updatecheck codebase='file://{crx_path}' version='{version}' />
  </app>
</gupdate>
""")
    with open(os.path.join(out, "id"), "w") as fh:
        fh.write(ext_id + "\n")
    print(f"packed {ext_id} v{version} into {crx_path}")


def install_policy():
    # One policy file for every local extension: separate files setting
    # ExtensionSettings would conflict rather than merge.
    settings = {}
    for name in sorted(os.listdir(OUT_TOP)):
        out = os.path.join(OUT_TOP, name)
        ext_id = open(os.path.join(out, "id")).read().strip()
        settings[ext_id] = {
            "installation_mode": "force_installed",
            "update_url": "file://" + os.path.join(out, "updates.xml"),
        }
    subprocess.run(["chrome-add-policy.sh", "-b", "local-extensions.json"],
                   input=json.dumps({"ExtensionSettings": settings},
                                    indent=2),
                   text=True, check=True)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("extension_dir")
    args = parser.parse_args()

    src = os.path.realpath(args.extension_dir)
    if not os.path.exists(os.path.join(src, "manifest.json")):
        sys.exit(f"no manifest.json in {src}")
    name = os.path.basename(src)
    out = os.path.join(OUT_TOP, name)
    os.makedirs(out, exist_ok=True)

    pack(src, ensure_key(name), out)
    install_policy()

    print("Chrome picks up a new version on its next update check;")
    print(f'bump "version" in {src}/manifest.json for it to see one.')


if __name__ == "__main__":
    main()

# Local Variables:
# compile-command: "./install-chrome-local-extension.py ../inits/userscripts"
# End:
