// Causeway refuses to MINT an unroutable comet.
//
// A snapshot with neither `sponsor` nor `fief` is a one-way identity:
// +urb-point-to-jael projects an absent sponsor to SELF, so nothing can route
// to it, and once a peer drops its state it can never be re-contacted. It is
// legal protocol (decisions-addendum §2 "Fief scope") — the verifier must
// never reject it — but the mint tools refuse by default.

import { describe, expect, test } from "vitest";
import { isRoutable, assertRoutable, type Snapshot } from "../src/spawn/snapshot.js";
import { buildStateUpdate } from "../src/ops/_common.js";
import { rekeyOp } from "../src/ops/rekey.js";
import type { StateUpdateCtx } from "../src/ops/types.js";

function snap(over: Partial<Snapshot> = {}): Snapshot {
  return { life: 1, rift: 0, key: 0x11n, sponsor: null, fief: null, ...over } as Snapshot;
}

describe("routability policy", () => {
  test("no sponsor and no fief is unroutable", () => {
    expect(isRoutable(snap())).toBe(false);
    expect(() => assertRoutable(snap())).toThrow(/neither a sponsor nor a fief/);
  });

  test("a sponsor makes it routable", () => {
    expect(isRoutable(snap({ sponsor: 0x9999n }))).toBe(true);
    expect(() => assertRoutable(snap({ sponsor: 0x9999n }))).not.toThrow();
  });

  test("the opt-out is explicit and deliberate", () => {
    expect(() => assertRoutable(snap(), true)).not.toThrow();
  });
});

// A state update is refused on the same rule — a rekey that drops the last
// sponsor strands the comet from that life onward.
function ctx(current: Snapshot, noRoute = false): StateUpdateCtx {
  // A valid x-only key: secp256k1's generator x coordinate.
  const gx = "79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798";
  const xonly = new Uint8Array(32);
  for (let i = 0; i < 32; i++) xonly[i] = parseInt(gx.slice(i * 2, i * 2 + 2), 16);
  const internalKey33 = new Uint8Array(33);
  internalKey33[0] = 0x02;
  internalKey33.set(xonly, 1);
  return {
    current: {
      txid: new Uint8Array(32).fill(7),
      vout: 0,
      value: 10_000n,
      scriptPubKey: new Uint8Array([0x51, 0x20, ...xonly]),
    },
    ownerKey: {
      internalKey: xonly,
      masterFingerprint: 0xdeadbeef,
      derivationPath: [0x80000056, 0x80000000, 0x80000000, 0, 0],
    },
    internalKey33,
    currentSnapshot: current,
    feeRate: 2,
    mp: null as any,
    noRoute,
  };
}

describe("state updates refuse to strand a comet", () => {
  test("a rekey that leaves no sponsor and no fief is refused", () => {
    expect(() => buildStateUpdate(snap({ life: 2 }), ctx(snap())))
      .toThrow(/neither a sponsor nor a fief/);
  });

  test("--no-route lets it through deliberately", () => {
    expect(() => buildStateUpdate(snap({ life: 2 }), ctx(snap(), true)))
      .not.toThrow(/neither a sponsor nor a fief/);
  });

  test("rekey carries the sponsor forward, so a sponsored comet stays routable", () => {
    const current = snap({ sponsor: 0x9999n });
    const built = rekeyOp.build({ newKey: 0x22n, breach: false }, ctx(current));
    expect(built.newSnapshot.sponsor).toBe(0x9999n);
    expect(built.newSnapshot.life).toBe(2);
  });

  test("rekey can set a new sponsor explicitly", () => {
    const built = rekeyOp.build(
      { newKey: 0x22n, breach: false, sponsor: 0x4242n }, ctx(snap({ sponsor: 0x9999n })),
    );
    expect(built.newSnapshot.sponsor).toBe(0x4242n);
  });

  test("clearing the sponsor without --no-route is refused", () => {
    expect(() => rekeyOp.build(
      { newKey: 0x22n, breach: false, sponsor: null }, ctx(snap({ sponsor: 0x9999n })),
    )).toThrow(/neither a sponsor nor a fief/);
  });
});
