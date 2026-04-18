import { describe, expect, test } from "vitest";
import {
  parseDescriptor, fromBareXpub, deriveKeyInfo, deriveP2TRAddress,
  defaultAccountPath, formatAccountPath, parseKeySource,
} from "../src/keys/xpub.js";

// A well-known BIP-86 test vector from the spec:
//   mnemonic: "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about"
//   path: m/86'/0'/0' on mainnet
//   xpub: xpub6BgBgsespWvERF3LHQu6CnqdvfEvtMcQjYrcRzx53QJjSxarj2afYWcLteoGVky7D3UKDP9QyrLprQ3VCECoY49yfdDEHGCtMMj92pReUsQ
//   Address at 0/0: bc1p5cyxnuxmeuwuvkwfem96lqzszd02n6xdcjrs20cac6yqjjwudpxqkedrcr
const KNOWN_XPUB = "xpub6BgBgsespWvERF3LHQu6CnqdvfEvtMcQjYrcRzx53QJjSxarj2afYWcLteoGVky7D3UKDP9QyrLprQ3VCECoY49yfdDEHGCtMMj92pReUsQ";
const KNOWN_FP = "73c5da0a";  // master fingerprint for "abandon x12" seed
const KNOWN_ADDR_0_0 = "bc1p5cyxnuxmeuwuvkwfem96lqzszd02n6xdcjrs20cac6yqjjwudpxqkedrcr";
const KNOWN_ADDR_0_1 = "bc1p4qhjn9zdvkux4e44uhx8tc55attvtyu358kutcqkudyccelu0was9fqzwh";
const KNOWN_ADDR_1_0 = "bc1p3qkhfews2uk44qtvauqyr2ttdsw7svhkl9nkm9s9c3x4ax5h60wqwruhk7";

describe("xpub / descriptor parsing", () => {
  test("parseDescriptor reads origin + xpub", () => {
    const desc = `tr([${KNOWN_FP}/86'/0'/0']${KNOWN_XPUB}/0/*)`;
    const src = parseDescriptor(desc, "main");
    expect(src.masterFingerprint).toBe(parseInt(KNOWN_FP, 16));
    expect(src.accountPath).toEqual([86 + 0x80000000, 0 + 0x80000000, 0 + 0x80000000]);
    expect(src.network).toBe("main");
  });

  test("parseDescriptor handles 'h' hardened marker", () => {
    const desc = `tr([${KNOWN_FP}/86h/0h/0h]${KNOWN_XPUB}/0/*)`;
    const src = parseDescriptor(desc, "main");
    expect(src.accountPath.length).toBe(3);
  });

  test("parseDescriptor accepts checksum tail", () => {
    const desc = `tr([${KNOWN_FP}/86'/0'/0']${KNOWN_XPUB}/0/*)#deadbeef`;
    const src = parseDescriptor(desc, "main");
    expect(src.masterFingerprint).toBe(parseInt(KNOWN_FP, 16));
  });

  test("fromBareXpub + deriveP2TRAddress matches BIP-86 test vector (0/0)", () => {
    const src = fromBareXpub({
      xpub: KNOWN_XPUB,
      masterFingerprint: KNOWN_FP,
      accountPath: defaultAccountPath("main"),
      network: "main",
    });
    expect(deriveP2TRAddress(src, 0, 0)).toBe(KNOWN_ADDR_0_0);
  });

  test("derives 0/1 and 1/0 correctly (BIP-86 vectors)", () => {
    const src = fromBareXpub({
      xpub: KNOWN_XPUB,
      masterFingerprint: KNOWN_FP,
      accountPath: defaultAccountPath("main"),
      network: "main",
    });
    expect(deriveP2TRAddress(src, 0, 1)).toBe(KNOWN_ADDR_0_1);
    expect(deriveP2TRAddress(src, 1, 0)).toBe(KNOWN_ADDR_1_0);
  });

  test("deriveKeyInfo produces 32-byte xonly", () => {
    const src = fromBareXpub({
      xpub: KNOWN_XPUB,
      masterFingerprint: KNOWN_FP,
      accountPath: defaultAccountPath("main"),
      network: "main",
    });
    const ki = deriveKeyInfo(src, 0, 0);
    expect(ki.internalKey.length).toBe(32);
    expect(ki.masterFingerprint).toBe(parseInt(KNOWN_FP, 16));
    expect(ki.derivationPath).toEqual([86 + 0x80000000, 0 + 0x80000000, 0 + 0x80000000, 0, 0]);
  });

  test("formatAccountPath round-trips", () => {
    expect(formatAccountPath(defaultAccountPath("main"))).toBe("m/86'/0'/0'");
    expect(formatAccountPath(defaultAccountPath("testnet"))).toBe("m/86'/1'/0'");
  });

  test("parseKeySource prefers descriptor", () => {
    const desc = `tr([${KNOWN_FP}/86'/0'/0']${KNOWN_XPUB})`;
    const src = parseKeySource(desc, {
      masterFingerprint: "00000000",
      accountPath: "m/86'/0'/0'",
      network: "main",
    });
    expect(src.masterFingerprint).toBe(parseInt(KNOWN_FP, 16));
  });

  test("parseKeySource falls back to bare xpub", () => {
    const src = parseKeySource(KNOWN_XPUB, {
      masterFingerprint: KNOWN_FP,
      accountPath: "m/86'/0'/0'",
      network: "main",
    });
    expect(src.masterFingerprint).toBe(parseInt(KNOWN_FP, 16));
    expect(deriveP2TRAddress(src, 0, 0)).toBe(KNOWN_ADDR_0_0);
  });
});
