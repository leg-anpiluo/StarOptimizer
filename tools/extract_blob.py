#!/usr/bin/env python3
"""Extract strings and IEEE-754 double tables from the inflated CompiledFunction blob."""
import base64, re, sys, zlib, struct

path = sys.argv[1]
src = open(path, encoding="utf-8").read()
m = re.search(r'ImportString\[\s*"([^"]+)"\s*,\s*\{\s*"Base64",\s*"WDX"\s*\}', src, re.S)
raw = base64.b64decode("".join(m.group(1).split()))

out = b""
for i in range(len(raw) - 1):
    if raw[i] == 0x78 and raw[i+1] in (0x01, 0x5e, 0x9c, 0xda):
        try:
            out = zlib.decompressobj().decompress(raw[i:])
            if len(out) > 1000:
                break
        except zlib.error:
            pass

print(f"== {path}: blob {len(out)} bytes ==")

# ASCII strings >= 4 chars
strings = re.findall(rb"[\x20-\x7e]{4,}", out)
print("--- STRINGS ---")
seen = set()
for s in strings:
    t = s.decode()
    if t not in seen:
        seen.add(t)
        print(repr(t))

# doubles: keep only plausible magnitudes (not tiny denormals, not huge garbage)
print("--- DOUBLES (dedup, in order, offset) ---"
)
seen = set()
for off in range(0, len(out) - 8):
    v = struct.unpack_from("<d", out, off)[0]
    if v != 0 and abs(v) >= 1e-6 and abs(v) < 1e30 and v == v:
        key = round(v, 12)
        if key not in seen:
            seen.add(key)
            print(f"{off:6d}  {v!r}")
