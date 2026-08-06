// The fief rides the $snapshot into the on-chain state commitment.
//
// state-key is taken over the JAMMED snapshot (see +state-commit / +state-leaf
// / +state-key in groundwire/lib/gw-btc-pass.hoon), so an encoder that drops or
// mangles a fief silently produces a DIFFERENT taproot output key: the rekey's
// input merkle root no longer matches the UTXO it spends (unsignable PSBT) and
// the committed state-key matches nothing a verifier computes.
//
// The authority is the desktop tool, causeway/desktop/causeway.py — it is what
// minted and re-keyed the live mainnet comets, so its +fief_noun /
// +snapshot_noun / +state_commit are what the chain agrees with. The vectors
// below were generated from it:
//
//   python3 -c "import causeway as cw; \
//     n = cw.snapshot_dict_to_noun(SNAP); \
//     print(cw.jam_bytes(n).hex(), cw.state_commit(SNAP).hex())"
//
// `if_c3` is the real fief of the live comet C3 — `[%if .64.227.13.22 34.343]`
// — which is exactly the case the web app used to encode as `fief: null`.

import { describe, it, expect } from "vitest";
import {
  fiefToNoun, snapshotToNoun, stateCommit, FIEF_TAG, type Snapshot,
} from "../src/spawn/snapshot.js";
import type { Fief } from "../src/protocol/types.js";
import { jam } from "../src/protocol/jam.js";
import { bytesToHex } from "../src/protocol/tagged-hash.js";

const jamHex = (n: Parameters<typeof jam>[0]): string => bytesToHex(jam(n));

// .64.227.13.22
const C3_IP = (64 << 24) | (227 << 16) | (13 << 8) | 22;
const C3_FIEF: Fief = { type: "if", ip: C3_IP >>> 0, port: 34343 };

// @t atoms are the LE byte encoding of the cord: 'com', 'example', 'net'.
const cord = (s: string): bigint => {
  let a = 0n;
  for (let i = s.length - 1; i >= 0; i--) a = (a << 8n) | BigInt(s.charCodeAt(i));
  return a;
};

interface Vec { name: string; snap: Snapshot; jam: string; c: string }

const VECTORS: Vec[] = [
  {
    name: "no fief (regression guard — must still match)",
    snap: { life: 1, rift: 0, key: 0xabcdn, sponsor: null, fief: null },
    jam: "710684e6d514",
    c: "c31d0c5ce2b35a9479ceaf7b284afac8e955324c0b4059f6bdc8b18222aaba75",
  },
  {
    name: "%if — the live C3 fief",
    snap: { life: 2, rift: 0, key: 0xabcdn, sponsor: null, fief: C3_FIEF },
    jam: "21332034af66063ecd1cf02d1ac68140383104",
    c: "2d0d2e9559d19d6be4eeb48552be0bb7c096cb0b167671cca40327009b268e60",
  },
  {
    name: "%if with a sponsor (~zod) and a rift",
    snap: {
      life: 3, rift: 1, key: 0x1122334455667788n, sponsor: 0n, fief: C3_FIEF,
    },
    jam: "a1e3003be29d5915d18c482ccdc0a79903be45c33810082786",
    c: "5c7dc0921681e6153a9936b71fb814ebad304017e17969734c5ff4422eb72400",
  },
  {
    name: "%is (128-bit address)",
    snap: {
      life: 1, rift: 0, key: 0xdeadbeefn, sponsor: null,
      fief: { type: "is", ip: 0x20010db8000000000000000000000001n, port: 443 },
    },
    jam: "710608de7d5bbd33039f360fa01f000000000000000000000080db100082d90d",
    c: "83b41af01d10a4074d52703185e87af0ecf13b17464365731ac975df003030d6",
  },
  {
    name: "%turf — ~[~['com' 'example'] ~['net']] on port 31.337",
    snap: {
      life: 4, rift: 2, key: 0x99n, sponsor: null,
      fief: {
        type: "turf",
        domains: [[cord("com"), cord("example")], [cord("net")]],
        port: 31337,
      },
    },
    jam: "61860c446666c0a7ab933357c01b7b6b0778cbf0c2dae0d8ca16f0dccae80a3e4d0f",
    c: "703a109cfee3f31e6b43efc6d47d739b1e4ee2921afb145a25bff9cc13f15ee7",
  },
];

describe("snapshot fief encoding matches the desktop tool byte for byte", () => {
  for (const v of VECTORS) {
    it(`jam(snapshot) — ${v.name}`, () => {
      expect(jamHex(snapshotToNoun(v.snap))).toBe(v.jam);
    });
    it(`state-commit c — ${v.name}`, () => {
      expect(bytesToHex(stateCommit(v.snap))).toBe(v.c);
    });
  }

  it("a fief actually changes the state commitment", () => {
    const withFief: Snapshot = {
      life: 2, rift: 0, key: 0xabcdn, sponsor: null, fief: C3_FIEF,
    };
    const without: Snapshot = { ...withFief, fief: null };
    expect(bytesToHex(stateCommit(withFief)))
      .not.toBe(bytesToHex(stateCommit(without)));
  });
});

describe("fiefToNoun", () => {
  it("tags are the Hoon cord atoms", () => {
    expect(FIEF_TAG.if).toBe(0x6669n);
    expect(FIEF_TAG.is).toBe(0x7369n);
    expect(FIEF_TAG.turf).toBe(0x66727574n);
  });

  it("%if is [tag [ip port]]", () => {
    expect(fiefToNoun(C3_FIEF)).toEqual([0x6669n, [1088621846n, 34343n]]);
  });

  it("%turf p is a list of turfs, each a null-terminated list of @t", () => {
    const n = fiefToNoun({ type: "turf", domains: [[cord("net")]], port: 1 });
    expect(n).toEqual([0x66727574n, [[[cord("net"), 0n], 0n], 1n]]);
  });

  it("an empty turf list is ~ (0), not undefined", () => {
    expect(fiefToNoun({ type: "turf", domains: [], port: 0 }))
      .toEqual([0x66727574n, [0n, 0n]]);
  });

  it("rejects a negative port rather than encoding garbage", () => {
    expect(() => fiefToNoun({ type: "if", ip: 1, port: -1 })).toThrow(/negative/);
  });
});
