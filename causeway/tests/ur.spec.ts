import { describe, expect, test } from "vitest";
import { encodePsbtUR, decodePsbtURFrames, URPsbtDecoder } from "../src/signing/qr-ur.js";

const SAMPLE_PSBT = (() => {
  // 1 KB of pseudo-PSBT bytes for round-trip testing.
  const out = new Uint8Array(1024);
  for (let i = 0; i < out.length; i++) out[i] = (i * 37 + 11) & 0xff;
  return out;
})();

describe("UR PSBT codec", () => {
  test("round-trips a 1KB blob", () => {
    const stream = encodePsbtUR(SAMPLE_PSBT, 200);
    const back = decodePsbtURFrames(stream.frames);
    expect(back.length).toBe(SAMPLE_PSBT.length);
    expect(back).toEqual(SAMPLE_PSBT);
  });

  test("multi-frame output", () => {
    const stream = encodePsbtUR(SAMPLE_PSBT, 200);
    expect(stream.totalFragments).toBeGreaterThan(1);
    expect(stream.frames.every((f) => f.startsWith("ur:crypto-psbt/"))).toBe(true);
  });

  test("streaming decoder", () => {
    const stream = encodePsbtUR(SAMPLE_PSBT, 200);
    const d = new URPsbtDecoder();
    let result: Uint8Array | null = null;
    for (const f of stream.frames) {
      result = d.receive(f);
      if (result) break;
    }
    expect(result).not.toBeNull();
    expect(result!).toEqual(SAMPLE_PSBT);
  });

  test("small PSBT produces at least one frame", () => {
    const tiny = new Uint8Array([1, 2, 3, 4, 5]);
    const s = encodePsbtUR(tiny, 300);
    expect(s.frames.length).toBeGreaterThanOrEqual(1);
    const back = decodePsbtURFrames(s.frames);
    expect(back).toEqual(tiny);
  });
});
