# Vendored verbatim from gwbtc/mnemonyms (libraries/python/mnemonyms.py).
# Reference BIP-39-style encoder used to render Groundwire IDs as mnemonyms.
# Do not edit; regenerate from upstream. Wordlist: mnemonyms-english.txt.

from __future__ import annotations

import hashlib


class ConfigurationError(Exception):
    """Raised when Mnemonym configuration is invalid."""


class Mnemonym:
    wordlist_length: int = 2048

    def __init__(self, tweaked: bool, strength: int, wordlist: list[str]) -> None:
        if len(wordlist) != self.wordlist_length:
            raise ConfigurationError(
                f"Wordlist must contain exactly {self.wordlist_length} words",
            )

        if strength < 32 or strength % 32 != 0:
            raise ConfigurationError(
                f"Strength must be a non-zero multiple of 32 bits, got {strength}"
            )

        self.tweaked = tweaked
        self.strength = strength
        self.wordlist = wordlist

    def to_nym(self, data: bytes) -> str:
        if len(data) != self.strength // 8:
            raise ValueError(
                f"Input must be {self.strength} bits, got {len(data) * 8} bits"
            )

        h: str = hashlib.sha256(data).hexdigest()
        b: str = (
            bin(int.from_bytes(data, byteorder="big"))[2:].zfill(len(data) * 8)
            + bin(int(h, 16))[2:].zfill(256)[: len(data) * 8 // 32]
        )
        indices: list[int] = [
            int(b[i * 11: (i + 1) * 11], 2) for i in range(len(b) // 11)
        ]

        while indices and indices[0] == 0:
            indices.pop(0)

        words: list[str] = [self.wordlist[idx] for idx in indices]
        prefix: str = "." if self.tweaked else ".."

        return prefix + ".".join(words)

    def to_eny(self, nym: str) -> str:
        if nym.startswith(".."):
            words_str: str = nym[2:]
        elif nym.startswith("."):
            words_str: str = nym[1:]
        else:
            raise ValueError("Nym must start with '.' or '..'")

        words: list[str] = words_str.split(".")
        total_words: int = (self.strength + self.strength // 32) // 11

        if len(words) > total_words:
            raise ValueError(
                f"Nym has {len(words)} words but {self.strength}-bit strength allows at most {total_words}"
            )

        leading_zeros: int = total_words - len(words)
        indices: list[int] = [0] * leading_zeros + [
            self.wordlist.index(w) for w in words
        ]
        b: str = "".join(bin(idx)[2:].zfill(11) for idx in indices)
        checksum_bits: int = len(b) // 33
        entropy_bits: int = len(b) - checksum_bits
        entropy: bytes = int(b[:entropy_bits], 2).to_bytes(
            entropy_bits // 8, byteorder="big"
        )
        h: str = hashlib.sha256(entropy).hexdigest()
        expected_cs: str = bin(int(h, 16))[2:].zfill(256)[:checksum_bits]

        if b[entropy_bits:] != expected_cs:
            raise ValueError("Failed to decode nym: checksum mismatch")

        return entropy.hex()

    def validate_nym(self, nym: str) -> bool:
        if nym.startswith(".."):
            words_str: str = nym[2:]
        elif nym.startswith("."):
            words_str: str = nym[1:]
        else:
            return False

        words: list[str] = words_str.split(".")

        for w in words:
            if w not in self.wordlist:
                return False

        total_words: int = (self.strength + self.strength // 32) // 11

        if len(words) > total_words:
            return False

        leading_zeros: int = total_words - len(words)
        indices: list[int] = [0] * leading_zeros + [
            self.wordlist.index(w) for w in words
        ]
        b: str = "".join(bin(idx)[2:].zfill(11) for idx in indices)
        checksum_bits: int = len(b) // 33
        entropy_bits: int = len(b) - checksum_bits
        entropy: bytes = int(b[:entropy_bits], 2).to_bytes(
            entropy_bits // 8, byteorder="big"
        )
        h: str = hashlib.sha256(entropy).hexdigest()
        expected_cs: str = bin(int(h, 16))[2:].zfill(256)[:checksum_bits]

        return b[entropy_bits:] == expected_cs

    def complete_word(self, nym: str) -> str | None:
        if nym.startswith(".."):
            rest: str = nym[2:]
        elif nym.startswith("."):
            rest: str = nym[1:]
        else:
            raise ValueError("Nym must start with '.' or '..'")

        fragment: str = rest.split(".")[-1]
        matches: list[str] = [w for w in self.wordlist if w.startswith(fragment)]

        return matches[0] if len(matches) == 1 else None

    def complete_nym(self, nym: str) -> str | None:
        if nym.startswith(".."):
            prefix: str = ".."
            rest: str = nym[2:]
        elif nym.startswith("."):
            prefix: str = "."
            rest: str = nym[1:]
        else:
            raise ValueError("Nym must start with '.' or '..'")

        parts: list[str] = rest.split(".")
        completed: str | None = self.complete_word(nym)

        if completed is None:
            return None

        parts[-1] = completed
        return prefix + ".".join(parts)
