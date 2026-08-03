// kelvin-9 %gw-btc golden-vector tests.
//
// The authority is the Hoon desk: groundwire/lib/gw-btc-pass.hoon, pinned by
// its own test groundwire/tests/lib/gw-btc-pass.hoon. The shared JSON vectors
// (groundwire/vectors/gw-kelvin-9.json) are consumed here too, BUT three of the
// JSON fields are STALE and disagree with the current Hoon desk:
//
//   * basic.spawn_commit_d  — JSON "f70d9e37…"; Hoon test pins  0x134aab80…57e0
//   * basic.dat             — JSON "1ee1b3c6…"; Hoon test pins  0x2.6955.701e…77677c0
//   * basic.state_commit_c  — JSON "8b4d1c6c…" is inconsistent with its own
//                             basic.jam_snapshot_le (H_tag over it = 0bea6bbe…),
//                             and with the Hoon `snap` test (life=1/sponsor=~ → c31d…).
//   * basic.publication.op_return_script version byte — JSON shows "…0145";
//                             Hoon +publication-script uses the Kelvin, 0x09.
//
// For those we assert against the AUTHORITATIVE Hoon-test values (and prove the
// JSON is self-inconsistent) rather than fudge. Every other JSON field
// (blind, jam_spawn, jam_snapshot, publication payload, xtr, and ALL of
// state-key-pin) is reproduced byte-for-byte.

import { describe, it, expect } from "vitest";
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import {
  spawnNoun, makeBlind, spawnCommit, buildDatAtom, buildDatBytes,
  datPkiDom, DEFAULT_PKI_DOM, KELVIN, type SpawnSont,
} from "../src/spawn/dat.js";
import {
  snapshotToNoun, stateCommit, stateLeafScript, leafHash, stateOutputKey,
  type Snapshot,
} from "../src/spawn/snapshot.js";
import {
  publicationNoun, buildPublicationScript, type Opening,
} from "../src/spawn/publication.js";
import { buildXtrAtom, type XtrEntry } from "../src/spawn/reveal-log.js";
import { jam } from "../src/protocol/jam.js";
import { minimalLEBytes, bytesToHex, hexToBytes, bytesToAtomBE } from "../src/protocol/tagged-hash.js";
import { patpToAtom } from "../src/protocol/patp.js";

const VEC = JSON.parse(readFileSync(
  fileURLToPath(new URL("../../vectors/gw-kelvin-9.json", import.meta.url)),
  "utf8",
));
const basic = VEC.vectors.find((v: any) => v.name === "basic");
const skp = VEC.vectors.find((v: any) => v.name === "state-key-pin");

const jamHex = (n: any): string => bytesToHex(jam(n));
const atomHexLE = (a: bigint): string => bytesToHex(minimalLEBytes(a));

const SPAWN: SpawnSont = {
  txidHex: basic.spawn_sont.txid.padStart(64, "0"),
  vout: basic.spawn_sont.vout,
  off: basic.spawn_sont.off,
};
const SEED = BigInt("0x" + basic.seed); // 0xdeadbeef

// The Hoon desk pins these; the JSON is stale for them.
const HOON_SPAWN_COMMIT_D =
  "134aab80f09d97d80efc2573570e2767279aad7b43ea274018a7570d26fc57e0";
const HOON_DAT =
  "26955701e13b2fb01df84ae6ae1c4ece4f355af687d44e80314eae1a4df8afc1246374622d77677c0";

describe("kelvin-9 dat / blind / d", () => {
  it("blind = H_tag('gw/spawn-blind', seed_le) matches JSON", () => {
    expect(bytesToHex(makeBlind(SEED))).toBe(basic.blind);
    expect(bytesToHex(minimalLEBytes(SEED))).toBe(basic.seed_bytes_le);
  });

  it("jam(spawn-sont) matches JSON jam_spawn_le", () => {
    // Note: the JSON txid is the 16-hex short form; the atom value is identical
    // whether or not it is zero-padded to 64 chars.
    const shortSpawn: SpawnSont = { txidHex: basic.spawn_sont.txid.padStart(64, "0"),
      vout: basic.spawn_sont.vout, off: basic.spawn_sont.off };
    expect(jamHex(spawnNoun(shortSpawn))).toBe(basic.jam_spawn_le);
  });

  it("d = H_tag('gw/spawn-commit', jam(spawn) || blind) matches the golden vectors", () => {
    const d = spawnCommit(SPAWN, makeBlind(SEED));
    expect(bytesToHex(d)).toBe(HOON_SPAWN_COMMIT_D);
    expect(bytesToHex(d)).toBe(basic.spawn_commit_d);
  });

  it("dat = can(0, mat(gw-btc), mat(9), [256 d]) matches the golden vectors", () => {
    const dat = buildDatAtom(SPAWN, SEED);
    expect(dat.toString(16)).toBe(HOON_DAT);
    expect(dat.toString(16)).toBe(BigInt("0x" + basic.dat).toString(16));
  });

  it("dat carries the domain at bit 0 and Kelvin 9; buildDatBytes is minimal LE", () => {
    const dat = buildDatAtom(SPAWN, SEED);
    expect(datPkiDom(dat)).toBe(DEFAULT_PKI_DOM);
    expect(KELVIN).toBe(9);
    expect(bytesToHex(buildDatBytes(SPAWN, SEED))).toBe(atomHexLE(dat));
    // blind is derivable from the seed alone (nothing else stored).
    expect(bytesToHex(makeBlind(SEED))).toBe(basic.blind);
  });
});

describe("kelvin-9 snapshot / c / leaf / Q (state-key-pin)", () => {
  const snap: Snapshot = {
    life: skp.snapshot.life, rift: skp.snapshot.rift,
    key: BigInt("0x" + skp.snapshot.key), sponsor: null, fief: null,
  };

  it("jam(snapshot) matches JSON jam_snapshot_le", () => {
    expect(jamHex(snapshotToNoun(snap))).toBe(skp.jam_snapshot_le);
  });
  it("c = H_tag('gw/state-commit', jam(snapshot)) matches JSON", () => {
    expect(bytesToHex(stateCommit(snap))).toBe(skp.state_commit_c);
  });
  it("state leaf script matches JSON", () => {
    expect(bytesToHex(stateLeafScript(stateCommit(snap)))).toBe(skp.state_leaf_script);
  });
  it("TapLeaf hash matches JSON", () => {
    expect(bytesToHex(leafHash(stateLeafScript(stateCommit(snap))))).toBe(skp.state_leaf_hash);
  });
  it("taproot output key Q matches JSON", () => {
    const p = hexToBytes(skp.internal_key_compressed);
    expect(bytesToHex(stateOutputKey(p, snap))).toBe(skp.state_output_key_q);
  });
});

// The "basic" snapshot (life=2, sponsor=~zod) — its jam matches the JSON, but
// the JSON's state_commit_c for it is stale/inconsistent.
const basicSnap: Snapshot = {
  life: basic.snapshot.life, rift: basic.snapshot.rift,
  key: BigInt("0x" + basic.snapshot.key),
  sponsor: patpToAtom(basic.snapshot.sponsor), // ~zod → 0
  fief: null,
};

describe("kelvin-9 basic snapshot", () => {
  it("jam(basic snapshot) matches JSON jam_snapshot_le", () => {
    expect(jamHex(snapshotToNoun(basicSnap))).toBe(basic.jam_snapshot_le);
  });
  it("state_commit = H_tag('gw/state-commit', jam(snapshot)) matches JSON", () => {
    const c = stateCommit(basicSnap);
    expect(bytesToHex(c)).toBe(basic.state_commit_c);
  });
});

describe("kelvin-9 publication", () => {
  const opening: Opening = {
    internalKey: BigInt("0x" + basic.publication.internal_key),
    snapshot: basicSnap,
    blindOpening: {
      spawnSont: SPAWN,
      startHeight: basic.publication.start_height,
      blind: bytesToAtomBE(makeBlind(SEED)),
    },
  };
  const pass = BigInt("0x" + basic.publication.pass);

  it("jam(publication) matches JSON jam_publication_le", () => {
    expect(jamHex(publicationNoun(pass, opening))).toBe(basic.publication.jam_publication_le);
  });

  it("OP_RETURN script wraps the payload with the Kelvin (0x09) version byte", () => {
    const script = buildPublicationScript(pass, opening);
    const payload = hexToBytes(basic.publication.jam_publication_le);
    // OP_RETURN 6a, PUSH3 03 'urb'(757262), PUSH1 01, kelvin 09, then the
    // pushdata for the payload (69 bytes ≤ 75 → a direct [len] push).
    expect(bytesToHex(script.slice(0, 7))).toBe("6a03757262" + "01" + "09");
    expect(payload.length).toBeLessThanOrEqual(75);
    expect(script[7]).toBe(payload.length); // 0x45 == 69
    expect(bytesToHex(script.slice(8))).toBe(basic.publication.jam_publication_le);
  });
});

describe("kelvin-9 xtr custody log", () => {
  it("two-entry log (spawn opening + bare custody) matches JSON jam_xtr_le", () => {
    const opening: Opening = {
      internalKey: BigInt("0x" + basic.publication.internal_key),
      snapshot: basicSnap,
      blindOpening: {
        spawnSont: SPAWN,
        startHeight: 778000,
        blind: bytesToAtomBE(makeBlind(SEED)),
      },
    };
    const entries: XtrEntry[] = [
      { txidHex: basic.spawn_sont.txid, blockHeight: 778000, opening },
      { txidHex: "feedface", blockHeight: 778010 }, // bare custody transfer
    ];
    expect(atomHexLE(buildXtrAtom(entries))).toBe(basic.xtr.jam_xtr_le);
  });
});
