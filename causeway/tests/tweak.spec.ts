import { describe, expect, test } from "vitest";
import { buildTweakBytes } from "../src/spawn/tweak.js";

function hex(b: Uint8Array): string {
  return Array.from(b, (x) => x.toString(16).padStart(2, "0")).join("");
}

// Golden vectors from gw-onboard.py:build_tweak_bytes, verified by hand.
describe("spawn tweak bytes", () => {
  test("matches Python reference for simple txid", () => {
    // Python reference output for:
    //   txid = "00" * 31 + "01"  (display hex → atom byte [01, 00, 00, …, 00])
    //   vout = 0, off = 0
    // Expected layout:
    //   09 99 "urb-watcher" "btc" "gw" 09 <32-byte-atom> <no vout> <no off>
    const result = buildTweakBytes({
      txidHex: "00".repeat(31) + "01",
      vout: 0,
      off: 0,
    });
    const expected =
      "09" +
      "99" +
      Buffer.from("urb-watcher").toString("hex") +
      Buffer.from("btc").toString("hex") +
      Buffer.from("gw").toString("hex") +
      "09" +
      "01" + "00".repeat(31);
    expect(hex(result)).toBe(expected);
  });

  test("omits vout and off when zero", () => {
    const withZeros = buildTweakBytes({ txidHex: "00".repeat(32), vout: 0, off: 0 });
    // Header(6) + "urb-watcher"(11) + "btc"(3) + "gw"(2) + 09(1) + txid(32) = 55 bytes
    expect(withZeros.length).toBe(1 + 1 + 11 + 3 + 2 + 1 + 32);
  });

  test("appends minimal-byte vout and off", () => {
    const r = buildTweakBytes({ txidHex: "00".repeat(32), vout: 258, off: 1 });
    // 258 = 0x0102 → LE bytes [02, 01]; 1 → [01]
    // Should have 3 extra bytes (2 for vout, 1 for off) vs the zero case.
    const base = buildTweakBytes({ txidHex: "00".repeat(32), vout: 0, off: 0 });
    expect(r.length).toBe(base.length + 2 + 1);
    const tail = hex(r.subarray(base.length));
    expect(tail).toBe("020101");
  });

  test("txid hex is reversed to LE atom bytes", () => {
    // Display 0123...ef → atom bytes ef..23 01 (reversed)
    const r = buildTweakBytes({
      txidHex: "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef",
      vout: 0, off: 0,
    });
    // The txid portion starts after the fixed prefix (22 bytes = 6+11+3+2 header-ish).
    // Prefix: 09 99 "urb-watcher"(11) "btc"(3) "gw"(2) 09 = 1+1+11+3+2+1 = 19 bytes
    const txidBytes = r.subarray(19, 19 + 32);
    expect(hex(txidBytes)).toBe("efcdab8967452301efcdab8967452301efcdab8967452301efcdab8967452301");
  });

  test("rejects short/long hex", () => {
    expect(() => buildTweakBytes({ txidHex: "0123", vout: 0 })).toThrow();
    expect(() => buildTweakBytes({ txidHex: "00".repeat(33), vout: 0 })).toThrow();
  });

  // Vectors generated directly from gw-onboard.py's build_tweak_bytes.
  test("golden: txid=0..1, vout=0, off=0", () => {
    const r = buildTweakBytes({ txidHex: "00".repeat(31) + "01", vout: 0, off: 0 });
    expect(hex(r)).toBe(
      "09997572622d776174636865726274636777090100000000000000000000000000000000000000000000000000000000000000",
    );
  });

  test("golden: txid=0123..cdef*4, vout=258, off=1", () => {
    const r = buildTweakBytes({
      txidHex: "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef",
      vout: 258, off: 1,
    });
    expect(hex(r)).toBe(
      "09997572622d77617463686572627463677709efcdab8967452301efcdab8967452301efcdab8967452301efcdab8967452301020101",
    );
  });

  test("golden: txid=0xff*32, vout=5, off=1000", () => {
    const r = buildTweakBytes({ txidHex: "ff".repeat(32), vout: 5, off: 1000 });
    expect(hex(r)).toBe(
      "09997572622d77617463686572627463677709ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff05e803",
    );
  });
});
