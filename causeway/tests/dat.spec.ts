// cc-draft-2 dat/xtr golden vectors, generated with `urbit eval` against
// the cyc/cc-draft-2 kernel:
//   dat: `@ux`(can 0 (mat %<dom>) [256 txid] (mat vout) (mat off) ~)
//   xtr: `@ux`(jam ~[[txid block internal-key leaf-version wid script] ...])
// The Python twin (desktop/tests/test_causeway.py) pins the same vectors.

import { describe, it, expect } from "vitest";
import { buildDatAtom, buildDatBytes, datPkiDom, cordToAtom, DEFAULT_PKI_DOM } from "../src/spawn/dat.js";
import { buildXtrAtom } from "../src/spawn/reveal-log.js";
import { buildPassAtom, buildRingAtomBytes, appendXtrToRing } from "../src/spawn/mine-c.js";
import { bytesToAtomLE } from "../src/protocol/bitwriter.js";
import { mat, rub } from "../src/protocol/mat.js";

const TXID = "ab12f00d9c330000111122223333444455556666777788889999aaaabbbbcccc";

function ux(s: string): bigint {
  return BigInt("0x" + s.replace(/\./g, ""));
}

describe("cc-draft-2 dat", () => {
  it("matches the %groundwire vout=1 golden vector", () => {
    const exp = ux(
      "1.d562.5e01.b386.6000.0222.2444.4666.6888.8aaa.accc.ceee" +
      ".f111.1333.3555.5777.7999.995c.9a5d.d91b.9d5b.dc99.cf80",
    );
    expect(buildDatAtom({ txidHex: TXID, vout: 1 })).toBe(exp);
    // spec §8 size estimate: ≈45 bytes for a domain tag + satpoint
    expect(buildDatBytes({ txidHex: TXID, vout: 1 }).length).toBe(45);
  });

  it("matches the %groundwire vout=0 golden vector", () => {
    const exp = ux(
      "7562.5e01.b386.6000.0222.2444.4666.6888.8aaa.accc.ceee" +
      ".f111.1333.3555.5777.7999.995c.9a5d.d91b.9d5b.dc99.cf80",
    );
    expect(buildDatAtom({ txidHex: TXID, vout: 0 })).toBe(exp);
  });

  it("matches the %urb-watcher offset golden vector", () => {
    const exp = ux(
      "1112.8680.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000" +
      ".0000.0000.0000.0000.0000.0efc.995a.18dd.185d.cb58.9c9d.5780",
    );
    expect(buildDatAtom({
      txidHex: "77".padStart(64, "0"), vout: 3, off: 546, dom: "urb-watcher",
    })).toBe(exp);
  });

  it("keeps the domain rub-extractable at bit 0 (+pass-pki-dom)", () => {
    const dat = buildDatAtom({ txidHex: TXID, vout: 7 });
    expect(datPkiDom(dat)).toBe(DEFAULT_PKI_DOM);
  });

  it("mat(%groundwire) matches hoon", () => {
    const { p, q } = mat(cordToAtom("groundwire"));
    expect(p).toBe(93);
    expect(q).toBe(7849075396568004131013906304n);
  });
});

describe("cc-draft-2 xtr reveal log", () => {
  const e1 = {
    txidHex: "ab12", blockHashHex: "bb44", internalKeyHex: "cc55",
    leafVersion: 0xc0, leafScriptHex: "dd66",
  };
  const e2 = {
    txidHex: "ab13", blockHashHex: "bb45", internalKeyHex: "cc56",
    leafVersion: 0xc0, leafScriptHex: "1234",
  };

  it("matches the single-entry jam vector", () => {
    expect(buildXtrAtom([e1])).toBe(ux("2.dd66.0812.1c01.0398.aa10.1bb4.4080.d589.0405"));
  });

  it("matches the two-entry jam vector", () => {
    expect(buildXtrAtom([e1, e2])).toBe(ux(
      "523.4b04.86df.1b98.ac10.1bb4.5080.d589.8405" +
      ".dd66.0812.1c01.0398.aa10.1bb4.4080.d589.0405",
    ));
  });

  it("appends to ring and pass without disturbing dat", () => {
    const dat = buildDatAtom({ txidHex: TXID, vout: 0 });
    const seedMaterial = new Uint8Array(64).fill(7);
    const ring0 = buildRingAtomBytes(seedMaterial, dat);
    const xtr = buildXtrAtom([e1]);
    const ring1 = appendXtrToRing(ring0, xtr);

    const bod = bytesToAtomLE(ring1) >> 8n;
    const { p, q } = rub(512, bod);
    expect(q).toBe(dat);
    expect(bod >> BigInt(512 + p)).toBe(xtr);

    // pass with xtr: identical prefix (tag|ugn|cry|mat(dat)), xtr tail added
    const sPub = new Uint8Array(32).fill(1);
    const cPub = new Uint8Array(32).fill(2);
    const p0 = buildPassAtom(sPub, cPub, dat);
    const p1 = buildPassAtom(sPub, cPub, dat, xtr);
    const prefixBits = BigInt(8 + 256 + 256 + mat(dat).p);
    expect(p1 & ((1n << prefixBits) - 1n)).toBe(p0);
    expect(p1 >> prefixBits).toBe(xtr);
  });
});
