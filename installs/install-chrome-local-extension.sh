#!/bin/bash

set -euo pipefail

# Pack an unpacked extension directory into a signed CRX3 and force-install
# it via the ExtensionSettings policy, with a file:// update URL. No Web
# Store, no browser binary needed for packing.

usage() {
    echo "usage: $0 EXTENSION_DIR" >&2
    exit 1
}

SRC=$(realpath "${1:-}") || usage
test -f "${SRC}/manifest.json" || usage

NAME=$(basename "${SRC}")
KEY_DIR=${HOME}/.config/chrome-local-extensions
OUT_TOP=${HOME}/.local/share/chrome-local-extensions
OUT=${OUT_TOP}/${NAME}

mkdir -p "${KEY_DIR}" "${OUT}"
chmod 700 "${KEY_DIR}"

KEY=${KEY_DIR}/${NAME}.pem
if ! test -f "${KEY}"; then
    (umask 077 && openssl genpkey -quiet -algorithm RSA \
                          -pkeyopt rsa_keygen_bits:2048 -out "${KEY}")
fi

python3 -I - "${SRC}" "${KEY}" "${OUT}" <<'EOF'
import hashlib, io, json, os, struct, subprocess, sys, urllib.request, zipfile

src, key, out = sys.argv[1:]

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

pub = subprocess.run(["openssl", "pkey", "-in", key, "-pubout",
                      "-outform", "DER"],
                     check=True, capture_output=True).stdout
crx_id = hashlib.sha256(pub).digest()[:16]
ext_id = "".join("abcdefghijklmnop"[int(c, 16)] for c in crx_id.hex())

manifest = json.load(open(os.path.join(src, "manifest.json")))
version = manifest["version"]
updates_url = "file://" + os.path.join(out, "updates.xml")
crx_path = os.path.join(out, "extension.crx")
manifest["update_url"] = updates_url

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
    # vendor.txt: "URL SHA256" lines for third-party files to fetch into the
    # extension's top level instead of checking them in.
    vendor = os.path.join(src, "vendor.txt")
    for line in open(vendor) if os.path.exists(vendor) else []:
        if not line.strip() or line.startswith("#"):
            continue
        url, sha256 = line.split()
        data = urllib.request.urlopen(url).read()
        if hashlib.sha256(data).hexdigest() != sha256:
            sys.exit(f"sha256 mismatch for {url}")
        z.writestr(os.path.basename(url), data)
archive = buf.getvalue()

signed_data = field(1, crx_id)
to_sign = (b"CRX3 SignedData\x00" + struct.pack("<I", len(signed_data))
           + signed_data + archive)
sig = subprocess.run(["openssl", "dgst", "-sha256", "-sign", key],
                     input=to_sign, check=True, capture_output=True).stdout

header = field(2, field(1, pub) + field(2, sig)) + field(10000, signed_data)
with open(crx_path, "wb") as fh:
    fh.write(b"Cr24" + struct.pack("<II", 3, len(header)) + header + archive)

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
EOF

# One policy file for every local extension: separate files setting
# ExtensionSettings would conflict rather than merge.
POLICY=$(for DIR in "${OUT_TOP}"/*/; do
             printf '%s\t%s\n' "$(cat "${DIR}/id")" "file://${DIR}updates.xml"
         done | python3 -I -c '
import json, sys
settings = {}
for line in sys.stdin:
    ext_id, url = line.split()
    settings[ext_id] = {"installation_mode": "force_installed",
                        "update_url": url}
print(json.dumps({"ExtensionSettings": settings}, indent=2))
')

chrome-add-policy.sh -b local-extensions.json <<< "${POLICY}"

echo "Chrome picks up a new version on its next update check;"
echo "bump \"version\" in ${SRC}/manifest.json for it to see one."

# Local Variables:
# compile-command: "./install-chrome-local-extension.sh ../inits/userscripts"
# End:
