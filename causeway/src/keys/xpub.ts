// Parse an xpub or BIP-380 output descriptor exported from a hardware wallet
// and derive BIP-86 taproot keys/addresses.
//
// Supported inputs (in order of preference):
//   - Output descriptor:  tr([fp/86h/0h/0h]xpub.../0/*)
//   - Bare xpub plus an origin spec:
//       { xpub: "xpub...", masterFingerprint: 0xa0b1c2d3, accountPath: [86+H, 0+H, 0+H] }
//
// The xpub is the *account-level* key (m/86'/0'/0'), and receive/change keys
// live one level below (0/i and 1/i).

import { HDKey } from "@scure/bip32";
import { p2tr, NETWORK, TEST_NETWORK } from "@scure/btc-signer";
import type { BTC_NETWORK } from "@scure/btc-signer/utils";
import type { KeyInfo } from "../signing/psbt.js";

export type BTCNetwork = "main" | "testnet";

export interface KeySource {
  account: HDKey;              // account-level xpub (m/86'/coin'/account')
  masterFingerprint: number;   // 4-byte big-endian (as number)
  accountPath: number[];       // e.g. [86+H, 0+H, 0+H]
  network: BTCNetwork;
}

const HARDENED = 0x80000000;

function networkBtc(n: BTCNetwork): BTC_NETWORK {
  return n === "main" ? NETWORK : TEST_NETWORK;
}

// Parse a uint32 path component like "86h" / "86'" / "86" into a number
// (hardened adds 2^31).
function parsePathComponent(s: string): number {
  const hardened = s.endsWith("'") || s.endsWith("h") || s.endsWith("H");
  const n = parseInt(hardened ? s.slice(0, -1) : s, 10);
  if (!Number.isFinite(n) || n < 0) throw new Error(`bad path component: ${s}`);
  return hardened ? n + HARDENED : n;
}

function parsePath(path: string): number[] {
  const clean = path.replace(/^m\//i, "").split("/").filter(Boolean);
  return clean.map(parsePathComponent);
}

// Parse a master fingerprint from hex.
function parseFingerprint(hex: string): number {
  if (hex.length !== 8) throw new Error(`fingerprint must be 8 hex chars: ${hex}`);
  return parseInt(hex, 16) >>> 0;
}

// Parse a BIP-380 output descriptor of the form:
//   tr([fingerprint/86h/coin'/account']xpub.../0/*)
// We ignore checksum (after `#`) if present.
export function parseDescriptor(desc: string, network: BTCNetwork): KeySource {
  const stripped = desc.split("#")[0]!.trim();
  const trMatch = stripped.match(/^tr\((.+)\)$/);
  if (!trMatch) throw new Error("expected descriptor of form tr(...)");
  const inner = trMatch[1]!;

  // [fp/path]xpub/.../... → capture fp, path, xpub
  // Example: [a0b1c2d3/86h/0h/0h]xpub6...ABC/0/*
  const m = inner.match(/^\[([0-9a-fA-F]{8})((?:\/[0-9]+['hH]?)+)\]([0-9a-zA-Z]+)(?:\/(.*))?$/);
  if (!m) throw new Error("descriptor must include [fp/path]xpub origin");
  const fpHex = m[1]!;
  const pathStr = m[2]!;
  const xpubStr = m[3]!;
  // m[4] is the optional derivation suffix after the xpub, e.g. "0/*" — we
  // ignore it here because we derive receive/change explicitly later.

  const accountPath = parsePath(pathStr);
  const account = HDKey.fromExtendedKey(xpubStr);
  return {
    account,
    masterFingerprint: parseFingerprint(fpHex),
    accountPath,
    network,
  };
}

export interface BareXpubInput {
  xpub: string;
  masterFingerprint: string | number;  // hex string or already-parsed
  accountPath: number[] | string;      // array or "m/86'/0'/0'"
  network: BTCNetwork;
}

export function fromBareXpub(input: BareXpubInput): KeySource {
  const account = HDKey.fromExtendedKey(input.xpub);
  const accountPath = typeof input.accountPath === "string"
    ? parsePath(input.accountPath)
    : input.accountPath;
  const masterFingerprint = typeof input.masterFingerprint === "string"
    ? parseFingerprint(input.masterFingerprint)
    : input.masterFingerprint;
  return { account, masterFingerprint, accountPath, network: input.network };
}

// SLIP-0132 version bytes for non-BIP-86 encodings — if the user pastes one
// of these we explain why we can't use it rather than silently accepting a
// key at the wrong derivation.
const NON_TAPROOT_XPUB_PREFIXES: Record<string, string> = {
  ypub: "BIP-49 (P2SH-wrapped segwit, 3... addresses)",
  Ypub: "BIP-49 multisig",
  zpub: "BIP-84 (native segwit v0, bc1q... addresses)",
  Zpub: "BIP-84 multisig",
  upub: "BIP-49 testnet",
  vpub: "BIP-84 testnet",
  Vpub: "BIP-84 testnet multisig",
  Upub: "BIP-49 testnet multisig",
};

// Try descriptor first, fall back to a raw xpub (user supplies fingerprint/path separately).
export function parseKeySource(
  input: string,
  fallback: { masterFingerprint: string; accountPath: string; network: BTCNetwork },
): KeySource {
  const trimmed = input.trim();
  if (trimmed.startsWith("tr(")) {
    return parseDescriptor(trimmed, fallback.network);
  }

  const prefix = trimmed.slice(0, 4);
  if (prefix in NON_TAPROOT_XPUB_PREFIXES) {
    throw new Error(
      `This is a ${prefix} key — ${NON_TAPROOT_XPUB_PREFIXES[prefix]}. ` +
      `Causeway requires a BIP-86 taproot key (xpub, starts with "xpub"). ` +
      `Your wallet needs a taproot account: not all wallets support it. ` +
      `Try Sparrow Wallet (desktop), BlueWallet (mobile, Taproot wallet type), ` +
      `or a hardware wallet (Keystone, Passport, Coldcard, SeedSigner). ` +
      `Muun, Phoenix, and most LN-first wallets don't expose taproot xpubs.`,
    );
  }

  if (/^(x|t)pub/.test(trimmed) || /^([XT])pub/.test(trimmed)) {
    return fromBareXpub({
      xpub: trimmed,
      masterFingerprint: fallback.masterFingerprint,
      accountPath: fallback.accountPath,
      network: fallback.network,
    });
  }
  throw new Error(
    `Unrecognized input. Expected either:\n` +
    `  • a taproot output descriptor, e.g. tr([a0b1c2d3/86'/0'/0']xpub6...)\n` +
    `  • a bare xpub (starts with "xpub" for mainnet, "tpub" for testnet)`,
  );
}

// Derive a child HDKey at (change, index) under the account key.
function childAt(src: KeySource, change: number, index: number): HDKey {
  return src.account.deriveChild(change).deriveChild(index);
}

// Derive the PSBT-ready KeyInfo for signing input at (change, index).
export function deriveKeyInfo(src: KeySource, change: number, index: number): KeyInfo {
  const node = childAt(src, change, index);
  const pub = node.publicKey;
  if (!pub) throw new Error("no public key at derivation");
  // 33-byte compressed → 32-byte xonly (drop parity prefix byte).
  const internalKey = pub.slice(1);
  return {
    internalKey,
    masterFingerprint: src.masterFingerprint,
    derivationPath: [...src.accountPath, change, index],
  };
}

// Derive the BIP-86 P2TR address at (change, index).
export function deriveP2TRAddress(src: KeySource, change: number, index: number): string {
  const info = deriveKeyInfo(src, change, index);
  return p2tr(info.internalKey, undefined, networkBtc(src.network)).address!;
}

// Convenience: default account path for a network.
export function defaultAccountPath(network: BTCNetwork): number[] {
  const coin = network === "main" ? 0 : 1;
  return [86 + HARDENED, coin + HARDENED, 0 + HARDENED];
}

export function formatAccountPath(path: number[]): string {
  return "m/" + path.map((c) => {
    const hardened = c >= HARDENED;
    const n = hardened ? c - HARDENED : c;
    return hardened ? `${n}'` : `${n}`;
  }).join("/");
}
