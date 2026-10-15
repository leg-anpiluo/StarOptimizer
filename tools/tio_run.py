#!/usr/bin/env python3
"""Replicate the StarOptimizer's tio.run request pipeline (from js/stars2.js)
to run Calculator2.wls on the public Mathematica kernel end-to-end."""
import base64, sys, zlib, urllib.request, uuid

def build_body(calculator_path, ft, stars, upgrade, unlocked="Automatic", mangle=False):
    txt = open(calculator_path, encoding="utf-8").read()
    i = txt.index("\n")
    header = txt[i + 1:] + "calcoptBLPWrapper[\n"
    footer = "]"  # both versions use the plain footer now (the old Chinese footer was invalid syntax)
    if upgrade in ("", None):
        upgrade = "Automatic"
    if unlocked in ("", None):
        unlocked = "Automatic"
    inner = header + f"{ft},{stars},{upgrade},{unlocked}" + footer
    if mangle:
        # emulate the JS byteStringToByteArray: charCodeAt -> Uint8Array (ToUint8)
        inner_bytes = bytes(ord(ch) % 256 for ch in inner)
    else:
        inner_bytes = inner.encode("utf-8")
    NUL = b"\x00"
    outer = (b"Vlang" + NUL + b"1" + NUL + b"mathematica" + NUL + b"F.code.tio" + NUL
             + str(len(inner_bytes)).encode() + NUL + inner_bytes
             + b"F.input.tio" + NUL + b"0" + NUL + b"Vargs" + NUL + b"0" + NUL + b"R")
    return zlib.compress(outer, 9)[2:-4]  # deflateRaw

def run(calculator_path, ft, stars, upgrade, unlocked="Automatic", mangle=False):
    body = build_body(calculator_path, ft, stars, upgrade, unlocked, mangle)
    token = uuid.uuid4().hex
    url = f"https://tio.run/cgi-bin/static/fb67788fd3d1ebf92e66b295525335af-run/{token}"
    req = urllib.request.Request(url, data=body, method="POST")
    req.add_header("Content-Type", "application/octet-stream")
    with urllib.request.urlopen(req, timeout=120) as r:
        resp = r.read()
    # response: 10-byte header + raw-deflate payload (JS uses pako.inflateRaw)
    raw = zlib.decompressobj(-15).decompress(resp[10:])
    text = raw.decode("utf-8", "replace")
    sep = text[:16]
    out = text[16:].split(sep)[0]
    return out

if __name__ == "__main__":
    calc = sys.argv[1] if len(sys.argv) > 1 else "Calculator2.wls"
    ft, stars, up = sys.argv[2], sys.argv[3], (sys.argv[4] if len(sys.argv) > 4 else "")
    unl = sys.argv[5] if len(sys.argv) > 5 else ""
    mangle = len(sys.argv) > 6 and sys.argv[6] == "mangle"
    out = run(calc, ft, stars, up, unl, mangle)
    outpath = "/tmp/tio_out.txt"
    open(outpath, "w", encoding="utf-8").write(out)
    print(f"saved {len(out)} chars to {outpath}")
    # extract the base64 PNG if present
    k = out.find("data:image/png;base64,")
    if k >= 0:
        b64 = out[k + len("data:image/png;base64,"):].strip()
        png = base64.b64decode(b64)
        open("/tmp/tio_result.png", "wb").write(png)
        print("PNG saved:", len(png), "bytes -> /tmp/tio_result.png")
    else:
        print(out[:2000])
