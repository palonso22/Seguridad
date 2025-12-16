# -----------------------------------------
# DES IMPLEMENTATION IN PURE PYTHON
# -----------------------------------------
# This is a pure Python implementation of the Data Encryption Standard (DES)
# cipher, which uses a 64-bit block size and a 56-bit key (stored as 64 bits
# with 8 parity bits). DES is a symmetric-key algorithm that was widely used
# but is now considered insecure due to its short key length.

# ---- PERMUTATION TABLES ----

# IP: Initial Permutation
# Applied to the 64-bit input block at the start of DES encryption.
# Rearranges the bits according to a fixed permutation pattern.
IP = [
    58,50,42,34,26,18,10,2,
    60,52,44,36,28,20,12,4,
    62,54,46,38,30,22,14,6,
    64,56,48,40,32,24,16,8,
    57,49,41,33,25,17,9,1,
    59,51,43,35,27,19,11,3,
    61,53,45,37,29,21,13,5,
    63,55,47,39,31,23,15,7
]

# IP_INV: Inverse Initial Permutation (Final Permutation)
# Applied at the end of DES to reverse the initial permutation.
# This is the mathematical inverse of IP.
IP_INV = [
    40,8,48,16,56,24,64,32,
    39,7,47,15,55,23,63,31,
    38,6,46,14,54,22,62,30,
    37,5,45,13,53,21,61,29,
    36,4,44,12,52,20,60,28,
    35,3,43,11,51,19,59,27,
    34,2,42,10,50,18,58,26,
    33,1,41,9,49,17,57,25
]

# E: Expansion Permutation
# Expands the 32-bit right half (R) to 48 bits in the Feistel function.
# Some bits are duplicated to match the 48-bit subkey size.
E = [
    32,1,2,3,4,5,4,5,
    6,7,8,9,8,9,10,11,
    12,13,12,13,14,15,16,17,
    16,17,18,19,20,21,20,21,
    22,23,24,25,24,25,26,27,
    28,29,28,29,30,31,32,1
]

# P: Permutation function
# Applied to the 32-bit output of the S-boxes in the Feistel function.
# Provides diffusion by spreading bits across the block.
P = [
    16,7,20,21,29,12,28,17,
    1,15,23,26,5,18,31,10,
    2,8,24,14,32,27,3,9,
    19,13,30,6,22,11,4,25
]

# PC1: Permuted Choice 1
# Selects 56 bits from the 64-bit key (discarding 8 parity bits)
# and permutes them for the key schedule.
PC1 = [
    57,49,41,33,25,17,9,
    1,58,50,42,34,26,18,
    10,2,59,51,43,35,27,
    19,11,3,60,52,44,36,
    63,55,47,39,31,23,15,
    7,62,54,46,38,30,22,
    14,6,61,53,45,37,29,
    21,13,5,28,20,12,4
]

# PC2: Permuted Choice 2
# Selects and permutes 48 bits from the 56-bit key halves
# to generate each round's subkey.
PC2 = [
    14,17,11,24,1,5,
    3,28,15,6,21,10,
    23,19,12,4,26,8,
    16,7,27,20,13,2,
    41,52,31,37,47,55,
    30,40,51,45,33,48,
    44,49,39,56,34,53,
    46,42,50,36,29,32
]

# SHIFT_AMOUNTS: Left shift schedule for key generation
# Specifies how many positions to rotate the key halves in each round.
# Most rounds use 2-bit shifts, while rounds 1, 2, 9, and 16 use 1-bit shifts.
SHIFT_AMOUNTS = [
    1,1,2,2,2,2,2,2,
    1,2,2,2,2,2,2,1
]

# SBOX: Substitution Boxes (S-boxes)
# Eight 4x16 lookup tables that provide non-linearity in DES.
# Each S-box takes 6 input bits and produces 4 output bits.
# The outer bits select the row, inner 4 bits select the column.
# This is the core source of confusion/security in DES.
SBOX = [
    # S1
    [
        [14,4,13,1,2,15,11,8,3,10,6,12,5,9,0,7],
        [0,15,7,4,14,2,13,1,10,6,12,11,9,5,3,8],
        [4,1,14,8,13,6,2,11,15,12,9,7,3,10,5,0],
        [15,12,8,2,4,9,1,7,5,11,3,14,10,0,6,13]
    ],
    # S2
    [
        [15,1,8,14,6,11,3,4,9,7,2,13,12,0,5,10],
        [3,13,4,7,15,2,8,14,12,0,1,10,6,9,11,5],
        [0,14,7,11,10,4,13,1,5,8,12,6,9,3,2,15],
        [13,8,10,1,3,15,4,2,11,6,7,12,0,5,14,9]
    ],
    # S3
    [
        [10,0,9,14,6,3,15,5,1,13,12,7,11,4,2,8],
        [13,7,0,9,3,4,6,10,2,8,5,14,12,11,15,1],
        [13,6,4,9,8,15,3,0,11,1,2,12,5,10,14,7],
        [1,10,13,0,6,9,8,7,4,15,14,3,11,5,2,12]
    ],
    # S4
    [
        [7,13,14,3,0,6,9,10,1,2,8,5,11,12,4,15],
        [13,8,11,5,6,15,0,3,4,7,2,12,1,10,14,9],
        [10,6,9,0,12,11,7,13,15,1,3,14,5,2,8,4],
        [3,15,0,6,10,1,13,8,9,4,5,11,12,7,2,14]
    ],
    # S5
    [
        [2,12,4,1,7,10,11,6,8,5,3,15,13,0,14,9],
        [14,11,2,12,4,7,13,1,5,0,15,10,3,9,8,6],
        [4,2,1,11,10,13,7,8,15,9,12,5,6,3,0,14],
        [11,8,12,7,1,14,2,13,6,15,0,9,10,4,5,3]
    ],
    # S6
    [
        [12,1,10,15,9,2,6,8,0,13,3,4,14,7,5,11],
        [10,15,4,2,7,12,9,5,6,1,13,14,0,11,3,8],
        [9,14,15,5,2,8,12,3,7,0,4,10,1,13,11,6],
        [4,3,2,12,9,5,15,10,11,14,1,7,6,0,8,13]
    ],
    # S7
    [
        [4,11,2,14,15,0,8,13,3,12,9,7,5,10,6,1],
        [13,0,11,7,4,9,1,10,14,3,5,12,2,15,8,6],
        [1,4,11,13,12,3,7,14,10,15,6,8,0,5,9,2],
        [6,11,13,8,1,4,10,7,9,5,0,15,14,2,3,12]
    ],
    # S8
    [
        [13,2,8,4,6,15,11,1,10,9,3,14,5,0,12,7],
        [1,15,13,8,10,3,7,4,12,5,6,11,0,14,9,2],
        [7,11,4,1,9,12,14,2,0,6,10,13,15,3,5,8],
        [2,1,14,7,4,10,8,13,15,12,9,0,3,5,6,11]
    ]
]


# ---- UTILITY FUNCTIONS ----

def permute(block, table):
    """
    Permute bits according to a permutation table.
    
    Args:
        block: List of bits (0s and 1s)
        table: Permutation table where each entry indicates which bit position to take
        
    Returns:
        List of permuted bits
    """
    return [block[i-1] for i in table]

def split_bits(bits):
    """
    Split a bit array into two equal halves.
    Used to divide blocks into left (L) and right (R) halves.
    
    Args:
        bits: List of bits to split
        
    Returns:
        Tuple of (left_half, right_half)
    """
    return bits[:len(bits)//2], bits[len(bits)//2:]

def xor(a, b):
    """
    Perform bitwise XOR on two bit arrays.
    
    Args:
        a, b: Two lists of bits of equal length
        
    Returns:
        List of XORed bits
    """
    return [i ^ j for i, j in zip(a, b)]

def left_shift(bits, n):
    """
    Circular left shift (rotation) of bits.
    Used in the key schedule to rotate key halves.
    
    Args:
        bits: List of bits to rotate
        n: Number of positions to rotate left
        
    Returns:
        Rotated list of bits
    """
    return bits[n:] + bits[:n]

def sbox_substitution(bits48):
    """
    Apply S-box substitution to 48 bits, producing 32 bits.
    
    The 48 input bits are divided into 8 groups of 6 bits.
    Each group is passed through its corresponding S-box:
    - Outer 2 bits (first and last) select the row (0-3)
    - Inner 4 bits select the column (0-15)
    - Output is a 4-bit value from the S-box table
    
    Args:
        bits48: List of 48 bits
        
    Returns:
        List of 32 bits after S-box substitution
    """
    output = []
    for i in range(8):
        chunk = bits48[i*6:(i+1)*6]
        row = (chunk[0] << 1) | chunk[5]  # First and last bit form row index
        col = (chunk[1] << 3) | (chunk[2] << 2) | (chunk[3] << 1) | chunk[4]  # Middle 4 bits form column
        val = SBOX[i][row][col]  # Lookup value in S-box
        output += [ (val >> (3-k)) & 1 for k in range(4) ]  # Convert 4-bit value to bit list
    return output


# ---- FEISTEL FUNCTION F ----

def feistel(R, subkey):
    """
    The Feistel function (f-function) used in each DES round.
    
    This is the core of DES encryption, combining expansion, XOR with subkey,
    S-box substitution, and permutation.
    
    Steps:
    1. Expand R from 32 bits to 48 bits using expansion permutation E
    2. XOR the expanded result with the 48-bit round subkey
    3. Apply S-box substitution to reduce 48 bits back to 32 bits
    4. Apply permutation P to the 32-bit result
    
    Args:
        R: 32-bit right half of the block
        subkey: 48-bit subkey for this round
        
    Returns:
        32-bit output after Feistel transformation
    """
    ER = permute(R, E)          # Expand 32 bits to 48 bits
    A = xor(ER, subkey)         # XOR with round subkey
    B = sbox_substitution(A)    # S-box substitution: 48 bits → 32 bits
    return permute(B, P)        # Final permutation


# ---- KEY SCHEDULE ----

def generate_subkeys(key64):
    """
    Generate 16 round subkeys from the 64-bit DES key.
    
    The key schedule process:
    1. Apply PC1 permutation to reduce 64-bit key to 56 bits (removes parity bits)
    2. Split into two 28-bit halves (C and D)
    3. For each of 16 rounds:
       - Rotate both halves left by 1 or 2 positions (according to SHIFT_AMOUNTS)
       - Combine halves and apply PC2 permutation to get 48-bit subkey
    
    Args:
        key64: 64-bit key as a list of bits
        
    Returns:
        List of 16 subkeys, each 48 bits
    """
    key56 = permute(key64, PC1)  # Apply PC1: 64 bits → 56 bits
    C, D = split_bits(key56)     # Split into two 28-bit halves
    subkeys = []
    for shift in SHIFT_AMOUNTS:
        C = left_shift(C, shift)  # Rotate left half
        D = left_shift(D, shift)  # Rotate right half
        subkeys.append(permute(C + D, PC2))  # Apply PC2: 56 bits → 48 bits
    return subkeys


# ---- DES MAIN ----

def des_encrypt_block(block64, key64):
    """
    Encrypt a single 64-bit block using DES.
    
    DES encryption structure:
    1. Apply initial permutation (IP)
    2. Split into left (L) and right (R) halves
    3. Perform 16 Feistel rounds:
       - new_R = L XOR f(R, subkey_i)
       - new_L = R
    4. Swap and combine: R || L (note the swap!)
    5. Apply final permutation (IP_INV)
    
    Args:
        block64: 64-bit plaintext block as list of bits
        key64: 64-bit key as list of bits
        
    Returns:
        64-bit ciphertext block as list of bits
    """
    subkeys = generate_subkeys(key64)

    block = permute(block64, IP)  # Initial permutation
    L, R = split_bits(block)      # Split into 32-bit halves

    # 16 Feistel rounds
    for i in range(16):
        new_R = xor(L, feistel(R, subkeys[i]))
        L = R
        R = new_R

    final = permute(R + L, IP_INV)  # Final permutation (note: R + L, not L + R)
    return final


def des_decrypt_block(block64, key64):
    """
    Decrypt a single 64-bit block using DES.
    
    DES decryption uses the same algorithm as encryption,
    but applies the subkeys in reverse order (from round 15 to 0).
    This is a property of the Feistel structure.
    
    Args:
        block64: 64-bit ciphertext block as list of bits
        key64: 64-bit key as list of bits
        
    Returns:
        64-bit plaintext block as list of bits
    """
    subkeys = generate_subkeys(key64)

    block = permute(block64, IP)  # Initial permutation
    L, R = split_bits(block)      # Split into 32-bit halves

    # 16 Feistel rounds with subkeys in reverse order
    for i in range(15, -1, -1):
        new_R = xor(L, feistel(R, subkeys[i]))
        L = R
        R = new_R

    final = permute(R + L, IP_INV)  # Final permutation
    return final


# ---- HELPER: convert bytes to bits ----

def bytes_to_bits(data):
    """
    Convert a byte string to a list of bits.
    
    Args:
        data: Bytes object
        
    Returns:
        List of bits (0s and 1s)
    """
    return [ (data[i] >> (7-j)) & 1 for i in range(len(data)) for j in range(8) ]

def bits_to_bytes(bits):
    """
    Convert a list of bits to a byte string.
    
    Args:
        bits: List of bits (0s and 1s)
        
    Returns:
        Bytes object
    """
    result = bytearray()
    for i in range(0, len(bits), 8):
        b = 0
        for j in range(8):
            b = (b << 1) | bits[i+j]
        result.append(b)
    return bytes(result)


# ---- HIGH-LEVEL API ----

def des_encrypt(plaintext8, key8):
    """
    Encrypt an 8-byte plaintext with an 8-byte key using DES.
    
    Args:
        plaintext8: 8-byte plaintext (bytes object)
        key8: 8-byte key (bytes object)
        
    Returns:
        8-byte ciphertext (bytes object)
    """
    bits = bytes_to_bits(plaintext8)
    keybits = bytes_to_bits(key8)
    out_bits = des_encrypt_block(bits, keybits)
    return bits_to_bytes(out_bits)


def des_decrypt(ciphertext8, key8):
    """
    Decrypt an 8-byte ciphertext with an 8-byte key using DES.
    
    Args:
        ciphertext8: 8-byte ciphertext (bytes object)
        key8: 8-byte key (bytes object)
        
    Returns:
        8-byte plaintext (bytes object)
    """
    bits = bytes_to_bits(ciphertext8)
    keybits = bytes_to_bits(key8)
    out_bits = des_decrypt_block(bits, keybits)
    return bits_to_bytes(out_bits)


# ---- TEST ----

if __name__ == "__main__":
    # Test vectors for DES
    # These are standard test values to verify correct implementation
    pt = b"\x01\x23\x45\x67\x89\xAB\xCD\xEF"   # Test plaintext
    key = b"\x13\x34\x57\x79\x9B\xBC\xDF\xF1"  # Test key

    print("Original plaintext:", pt.hex().upper())
    
    # Encrypt
    ct = des_encrypt(pt, key)
    print("Ciphertext:", ct.hex().upper())  # Should be: 85E813540F0AB405
    
    # Decrypt
    decrypted = des_decrypt(ct, key)
    print("Decrypted plaintext:", decrypted.hex().upper())
    
    # Verify decryption
    if pt == decrypted:
        print("✓ Decryption successful! Plaintext matches.")
    else:
        print("✗ Decryption failed! Plaintexts don't match.")