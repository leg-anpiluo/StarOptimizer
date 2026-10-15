#!/usr/bin/env python3
"""Decode the WDX (compressed WXF) embedded in Calculator*.wls into a
readable Wolfram Language expression dump."""
import base64, re, sys, zlib

path = sys.argv[1] if len(sys.argv) > 1 else "Calculator.wls"
src = open(path, encoding="utf-8").read()

# 1. extract base64 payload
m = re.search(r'ImportString\[\s*"([^"]+)"\s*,\s*\{\s*"Base64",\s*"WDX"\s*\}', src, re.S)
b64 = "".join(m.group(1).split())
raw = base64.b64decode(b64)
print(f"[+] WDX container: {len(raw)} bytes, header={raw[:16].hex()}", file=sys.stderr)

# 2. find and decompress zlib stream(s)
out = b""
i = 0
while i < len(raw) - 1:
    if raw[i] == 0x78 and raw[i+1] in (0x01, 0x5e, 0x9c, 0xda):
        try:
            d = zlib.decompressobj()
            out = d.decompress(raw[i:])
            if len(out) > 16:
                print(f"[+] zlib stream at offset {i}: inflated to {len(out)} bytes, starts {out[:12]!r}", file=sys.stderr)
                break
        except zlib.error:
            pass
    i += 1
if not out:
    sys.exit("no zlib stream found")

# 3. parse WXF
data = out
pos = [0]
def u8():
    v = data[pos[0]]; pos[0] += 1; return v

def varint():
    # WXF varint: 7 bits per byte, little endian; high bit set => last byte
    result = 0; shift = 0
    while True:
        b = u8()
        result |= (b & 0x7F) << shift
        if b & 0x80:
            break
        shift += 7
    return result

def wstring(n=None):
    if n is None:
        n = varint()
    s = data[pos[0]:pos[0]+n]; pos[0] += n
    return s.decode("utf-8", "replace")

def parse():
    t = u8(); c = chr(t)
    if c == 'F':   # function: varint argc, head, args
        argc = varint()
        head = parse()
        args = [parse() for _ in range(argc)]
        return (head, args)
    if c == 'G':   # function long form: 64-bit argc
        argc = int.from_bytes(data[pos[0]:pos[0]+8], "little"); pos[0] += 8
        head = parse()
        args = [parse() for _ in range(argc)]
        return (head, args)
    if c == 'S':   # symbol: u8 len, name
        return ("Sym", wstring(u8()))
    if c == 's':   # string
        return wstring(varint())
    if c in 'CDEf':  # integer 8/16/32/64
        n = {'C':1,'D':2,'E':4,'f':8}[c]
        v = int.from_bytes(data[pos[0]:pos[0]+n], "little", signed=True); pos[0] += n
        return v
    if c == 'R':   # real64
        import struct
        v = struct.unpack("<d", data[pos[0]:pos[0]+8])[0]; pos[0] += 8
        return v
    if c == 'A':   # association: varint count, rules
        n = varint()
        return ("Assoc", [parse() for _ in range(n)])
    if c in 'Bb':  # numeric/packed array
        # type token, rank varint, dims varints..., (offsets?), data
        elem = chr(u8())
        rank = varint()
        dims = [varint() for _ in range(rank)]
        total = 1
        for d in dims: total *= d
        sizes = {'C':1,'D':2,'E':4,'f':8,'R':8,'I':2,'U':1,'N':4}
        if elem in 'iuIUbB':  # integer types
            n = {'i':2,'u':1,'I':4,'U':2,'B':1,'b':8}.get(elem, 8)
            fmt = {"i":"<h","u":"<B","I":"<i","U":"<H","B":"<b","b":"<q"}[elem]
            import struct
            vals = list(struct.unpack("<"+str(total)+fmt[-1], data[pos[0]:pos[0]+total*n]))
            pos[0] += total*n
        elif elem == 'R':
            import struct
            vals = list(struct.unpack("<"+str(total)+"d", data[pos[0]:pos[0]+total*8]))
            pos[0] += total*8
        elif elem == 'N':
            import struct
            vals = list(struct.unpack("<"+str(total)+"f", data[pos[0]:pos[0]+total*4]))
            pos[0] += total*4
        else:
            vals = [f"?{elem}"]*total
        return ("Array", dims, vals)
    raise ValueError(f"unknown WXF token {c!r} (0x{t:02x}) at offset {pos[0]-1}")

expr = parse()
print(f"[+] parse finished, consumed {pos[0]}/{len(data)} bytes", file=sys.stderr)

# 4. pretty print
def fmt(e, prec=0):
    if isinstance(e, tuple):
        if e and e[0] == "Sym":
            return e[1]
        if e and e[0] == "Array":
            dims, vals = e[1], e[2]
            if len(dims) == 1:
                return "{" + ", ".join(fmt(v) for v in vals) + "}"
            return f"Array[{dims}]"
        if e and e[0] == "Assoc":
            return "<|" + ", ".join(fmt(r) for r in e[1]) + "|>"
        head, args = e
        hs = fmt(head)
        if hs == "List":
            return "{" + ", ".join(fmt(a) for a in args) + "}"
        if hs == "Rule" or hs == "RuleDelayed":
            return f"{fmt(args[0])} {'=>:' if hs=='RuleDelayed' else '->'} {fmt(args[1])}"
        if not args:
            return hs
        argstr = ", ".join(fmt(a) for a in args)
        if len(argstr) > 60 and prec == 0:
            return f"{hs}[\n  " + ",\n  ".join(fmt(a) for a in args) + "]"
        return f"{hs}[{argstr}]"
    if isinstance(e, float):
        return repr(e)
    return str(e)

text = fmt(expr)
open(path.replace(".wls", "") + ".decoded.wl", "w", encoding="utf-8").write(text)
print(f"[+] wrote {path.replace('.wls','')+'.decoded.wl'} ({len(text)} chars)", file=sys.stderr)
