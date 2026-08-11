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
//
// (basic.publication.op_return_script used to be a prose placeholder; it now
// holds the real bytes, cross-checked against Hoon +make-publication, and is
// asserted below.)
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
  publicationNoun, buildPublicationScript, pushData, MAX_PUBLICATION,
  type Opening,
} from "../src/spawn/publication.js";
import { buildXtrAtom, type XtrEntry } from "../src/spawn/reveal-log.js";
import { passWithXtr, messagingKeyFromPass } from "../src/spawn/mine-c.js";
import { jam } from "../src/protocol/jam.js";
import { minimalLEBytes, bytesToHex, hexToBytes, bytesToAtomBE } from "../src/protocol/tagged-hash.js";
import { patpToAtom } from "../src/protocol/patp.js";

const VEC = JSON.parse(readFileSync(
  fileURLToPath(new URL("../../vectors/gw-kelvin-9.json", import.meta.url)),
  "utf8",
));
const basic = VEC.vectors.find((v: any) => v.name === "basic");
const skp = VEC.vectors.find((v: any) => v.name === "state-key-pin");
const pd2 = VEC.vectors.find((v: any) => v.name === "pushdata2-fief");
const full = VEC.vectors.find((v: any) => v.name === "full-packet");

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
    // ... and the whole script is pinned in the shared vector
    expect(bytesToHex(script)).toBe(basic.publication.op_return_script);
    expect(script.length).toBe(basic.publication.op_return_script_bytes);
    expect(bytesToHex(script.slice(7, 8))).toBe(basic.publication.pushdata);
  });
});

// OP_PUSHDATA2 — the publication that does not fit in one length byte.
//
// PUSHDATA1 (0x4c) carries a SINGLE length byte and so stops at 255, but
// MAX_PUBLICATION is 1024 and every fief-carrying publication measures 265–269.
// Uint8Array.of(0x4c, n) silently takes n mod 256, so this used to emit a
// corrupt script here with no error at all. The vector is the cross-language
// pin: the Hoon desk (+make-publication) and the Python desktop tool
// (make_publication_script) produce these exact bytes for these exact inputs.
describe("kelvin-9 publication — OP_PUSHDATA2 (pushdata2-fief vector)", () => {
  const snap: Snapshot = {
    life: pd2.snapshot.life,
    rift: pd2.snapshot.rift,
    key: BigInt("0x" + pd2.snapshot.key),
    sponsor: null,
    fief: { type: "if", ip: pd2.snapshot.fief.ip_atom, port: pd2.snapshot.fief.port },
  };
  const spawn: SpawnSont = {
    txidHex: pd2.spawn_sont.txid,
    vout: pd2.spawn_sont.vout,
    off: pd2.spawn_sont.off,
  };
  const opening: Opening = {
    internalKey: BigInt("0x" + pd2.publication.internal_key),
    snapshot: snap,
    blindOpening: {
      spawnSont: spawn,
      startHeight: pd2.publication.start_height,
      blind: BigInt("0x" + pd2.blind),
    },
  };
  const pass = BigInt("0x" + pd2.publication.pass);

  it("the fief-carrying snapshot reproduces the vector's jam, c and Q", () => {
    expect(jamHex(snapshotToNoun(snap))).toBe(pd2.jam_snapshot_le);
    expect(bytesToHex(stateCommit(snap))).toBe(pd2.state_commit_c);
    expect(bytesToHex(stateLeafScript(stateCommit(snap)))).toBe(pd2.state_leaf_script);
    expect(bytesToHex(leafHash(stateLeafScript(stateCommit(snap))))).toBe(pd2.state_leaf_hash);
    expect(bytesToHex(stateOutputKey(hexToBytes(pd2.internal_key_compressed), snap)))
      .toBe(pd2.state_output_key_q);
  });

  // The snapshot's `key` is cry.pub, the 32-byte messaging half — NOT the
  // 108-byte pass. This vector pins both, so it settles the question: the
  // rekey page reads the current snapshot off an oracle $point, whose
  // `net.pass` is the WHOLE pass, and must split it. Feeding the pass in
  // whole yields a different jam, a different c, and a Q that no verifier —
  // Hoon, Python or JS — ever reconstructs, which is an unsignable PSBT and
  // an unmatchable state-key. Same class of bug as a dropped fief.
  it("the vector's snapshot key IS cry of its 108-byte pass", () => {
    expect(pass & 0xffn).toBe(0x63n);                       // suite-C
    expect((pass.toString(2).length + 7) >> 3).toBe(108);    // a real pass
    expect(messagingKeyFromPass(pass)).toBe(snap.key);
    expect(snap.key.toString(16).padStart(64, "0")).toBe(pd2.snapshot.key);
  });

  it("a snapshot built from a $point's net.pass reproduces the vector's Q", () => {
    // Exactly what ui/pages/op.ts does with `point.net.pass` on the rekey path.
    const fromPoint: Snapshot = { ...snap, key: messagingKeyFromPass(pass) };
    expect(bytesToHex(stateOutputKey(hexToBytes(pd2.internal_key_compressed), fromPoint)))
      .toBe(pd2.state_output_key_q);

    // …and the regression it guards: the un-split pass commits a different Q.
    const unsplit: Snapshot = { ...snap, key: pass };
    expect(bytesToHex(stateOutputKey(hexToBytes(pd2.internal_key_compressed), unsplit)))
      .not.toBe(pd2.state_output_key_q);
  });

  it("messagingKeyFromPass refuses a non-suite-C atom", () => {
    expect(() => messagingKeyFromPass(0x62n)).toThrow(/not a suite-C pass/);
  });

  it("blind and d match the vector", () => {
    expect(bytesToHex(makeBlind(BigInt("0x" + pd2.seed)))).toBe(pd2.blind);
    expect(bytesToHex(spawnCommit(spawn, hexToBytes(pd2.blind)))).toBe(pd2.spawn_commit_d);
    expect(jamHex(spawnNoun(spawn))).toBe(pd2.jam_spawn_le);
  });

  it("jam(publication) is 269 bytes and matches the vector", () => {
    const payload = jam(publicationNoun(pass, opening));
    expect(bytesToHex(payload)).toBe(pd2.publication.jam_publication_le);
    expect(payload.length).toBe(pd2.publication.payload_bytes);
    expect(payload.length).toBe(269);
    expect(payload.length).toBeGreaterThan(255); // PUSHDATA1 cannot express this
  });

  it("the script uses OP_PUSHDATA2 with a two-byte LITTLE-ENDIAN length", () => {
    const script = buildPublicationScript(pass, opening);
    expect(bytesToHex(script)).toBe(pd2.publication.op_return_script);
    expect(script.length).toBe(pd2.publication.op_return_script_bytes);
    expect(bytesToHex(script.slice(0, 7))).toBe("6a03757262" + "01" + "09");
    expect(bytesToHex(script.slice(7, 10))).toBe(pd2.publication.pushdata);
    expect(bytesToHex(script.slice(7, 10))).toBe("4d0d01");
    expect(script[8]! | (script[9]! << 8)).toBe(269); // little-endian, lo then hi
    expect(bytesToHex(script.slice(10))).toBe(pd2.publication.jam_publication_le);
  });
});

describe("pushData boundaries", () => {
  const cases: [number, string][] = [
    [3, "03"], [75, "4b"],                          // direct push: opcode IS length
    [76, "4c4c"], [254, "4cfe"], [255, "4cff"],     // PUSHDATA1, one length byte
    [256, "4d0001"], [269, "4d0d01"], [512, "4d0002"], // PUSHDATA2, LE length
    [588, "4d4c02"], [1024, "4d0004"],
  ];
  for (const [n, head] of cases) {
    it(`${n} bytes -> ${head}`, () => {
      expect(bytesToHex(pushData(new Uint8Array(n).fill(0xab)))).toBe(head);
    });
  }
  it("refuses what OP_PUSHDATA2 cannot express", () => {
    expect(pushData(new Uint8Array(0xffff)).length).toBe(3);
    expect(() => pushData(new Uint8Array(0x10000))).toThrow(/OP_PUSHDATA2/);
  });
  it("MAX_PUBLICATION is inside PUSHDATA2's range", () => {
    expect(MAX_PUBLICATION).toBe(1024);
    expect(MAX_PUBLICATION).toBe(full.max_publication);
    expect(MAX_PUBLICATION).toBeLessThanOrEqual(0xffff);
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

// ---------------------------------------------------------------------------
// THE FULL-PACKET PUBLICATION (full-packet vector).
//
// A kelvin-9 publication is the comet's whole attestation packet: the pass a
// peer receives over ames, custody log and all, plus the opening for the hop
// the carrying transaction performs. Two surfaces have to agree across the
// three implementations for that to work — the xtr re-encoding (passWithXtr,
// +with-xtr:gw-btc-pass, pass_with_xtr) and the script — and a realistic
// seven-hop packet lands at 588 payload bytes, which is exactly why the
// 512-byte cap could not survive the change.
// ---------------------------------------------------------------------------
function fullSnap(s: any): Snapshot {
  return {
    life: s.life,
    rift: s.rift,
    key: BigInt("0x" + s.key),
    sponsor: s.sponsor == null ? null : patpToAtom("~" + s.sponsor),
    fief: s.fief == null ? null : { type: s.fief.type, ip: s.fief.ip_atom, port: s.fief.port },
  };
}

function fullOpening(o: any): Opening {
  const bo = o.blind_opening;
  return {
    internalKey: BigInt("0x" + o.internal_key),
    snapshot: fullSnap(o.snapshot),
    blindOpening: bo == null ? null : {
      spawnSont: { txidHex: bo.spawn_sont.txid, vout: bo.spawn_sont.vout, off: bo.spawn_sont.off },
      startHeight: bo.start_height,
      blind: BigInt("0x" + bo.blind),
    },
  };
}

describe("kelvin-9 full-packet publication", () => {
  const entries: XtrEntry[] = full.custody_log.map((e: any) => ({
    txidHex: e.txid,
    blockHeight: e.height,
    opening: e.opening == null ? null : fullOpening(e.opening),
  }));
  const passEmpty = BigInt("0x" + full.pass_empty);
  const terminal = fullOpening(full.terminal_opening);

  it("the six-hop custody log jams to the vector's xtr", () => {
    expect(atomHexLE(buildXtrAtom(entries))).toBe(full.jam_xtr_le);
    expect(minimalLEBytes(buildXtrAtom(entries)).length).toBe(full.xtr_bytes);
  });

  it("passWithXtr reproduces the full pass and never changes the name", () => {
    const passFull = passWithXtr(passEmpty, buildXtrAtom(entries));
    expect(passFull.toString(16).padStart(full.pass_full_bytes * 2, "0")).toBe(full.pass_full);
    expect(minimalLEBytes(passFull).length).toBe(full.pass_full_bytes);
    // ugn, cry and dat are copied verbatim: xtr is outside the key tweak
    const head = (1n << 520n) - 1n;
    expect(passFull & head).toBe(passEmpty & head);
  });

  it("the publication payload is 588 bytes — over the OLD 512 cap", () => {
    const passFull = BigInt("0x" + full.pass_full);
    const payload = jam(publicationNoun(passFull, terminal));
    expect(bytesToHex(payload)).toBe(full.jam_publication_le);
    expect(payload.length).toBe(full.payload_bytes);
    expect(payload.length).toBe(588);
    expect(payload.length).toBeGreaterThan(512);
    expect(payload.length).toBeLessThanOrEqual(MAX_PUBLICATION);
  });

  it("the script is byte-identical to Hoon and Python", () => {
    const passFull = BigInt("0x" + full.pass_full);
    const script = buildPublicationScript(passFull, terminal);
    expect(bytesToHex(script)).toBe(full.op_return_script);
    expect(script.length).toBe(full.op_return_script_bytes);
    expect(bytesToHex(script.slice(7, 10))).toBe(full.pushdata);
    expect(script[8]! | (script[9]! << 8)).toBe(588);
  });

  it("stays LOUD over the cap: it throws, it does not emit a script", () => {
    const huge: Opening = {
      internalKey: (1n << 4000n) - 1n,
      snapshot: { life: 1, rift: 0, key: (1n << 4000n) - 3n, sponsor: null, fief: null },
      blindOpening: null,
    };
    const bigPass = (1n << 4000n) - 5n;
    expect(jam(publicationNoun(bigPass, huge)).length).toBeGreaterThan(MAX_PUBLICATION);
    expect(() => buildPublicationScript(bigPass, huge)).toThrow(/payload/);
  });
});
