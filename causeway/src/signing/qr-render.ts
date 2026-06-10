// Animated QR rendering.
//
// Renders a sequence of UR frames as a rotating QR code. The user's HW wallet
// scanner collects frames until it has enough for fountain-decoding.

import QRCode from "qrcode";
import type { URStream } from "./qr-ur.js";

export interface QrAnimator {
  stop(): void;
  setSpeed(fps: number): void;
}

// Render an animated QR inside `container`. Cycles through pre-generated
// frames at `fps` (default 4). On each tick, asks the stream for the next
// fragment (so fountain-coded streams can continue indefinitely).
export function animateUR(
  container: HTMLElement,
  stream: URStream,
  opts: { fps?: number; size?: number } = {},
): QrAnimator {
  const fps = opts.fps ?? 4;
  const size = opts.size ?? 360;

  const img = document.createElement("canvas");
  img.width = size;
  img.height = size;
  img.style.cssText = `display:block;background:#fff;width:${size}px;height:${size}px;border-radius:8px;`;
  container.innerHTML = "";
  container.appendChild(img);

  const meta = document.createElement("div");
  meta.className = "qr-meta";
  meta.style.cssText = "margin-top:8px;color:var(--muted);font:12px 'Fira Code',monospace;text-align:center;";
  container.appendChild(meta);

  let frameIdx = 0;
  let running = true;
  let intervalMs = Math.max(1, Math.round(1000 / fps));

  const drawFrame = async (): Promise<void> => {
    // Pull next fragment from the stream (so animated QR can continue past
    // the initial round with redundancy frames).
    const next = stream.nextFrame();
    try {
      await QRCode.toCanvas(img, next, {
        errorCorrectionLevel: "M",
        margin: 1,
        width: size,
        color: { dark: "#050505", light: "#f7f7f7" },
      });
    } catch (err) {
      meta.textContent = `QR error: ${String(err)}`;
      return;
    }
    meta.textContent = `frame ${(frameIdx++ % stream.totalFragments) + 1} / ${stream.totalFragments}`;
  };

  drawFrame();
  let timer = setInterval(drawFrame, intervalMs);

  return {
    stop() {
      running = false;
      clearInterval(timer);
    },
    setSpeed(newFps: number) {
      if (!running) return;
      clearInterval(timer);
      intervalMs = Math.max(1, Math.round(1000 / newFps));
      timer = setInterval(drawFrame, intervalMs);
    },
  };
}

// Render a single static QR (used for very small payloads or non-UR text).
export async function renderStaticQR(
  container: HTMLElement,
  text: string,
  size = 360,
): Promise<void> {
  const canvas = document.createElement("canvas");
  canvas.width = size;
  canvas.height = size;
  canvas.style.cssText = `display:block;background:#fff;width:${size}px;height:${size}px;border-radius:8px;`;
  container.innerHTML = "";
  container.appendChild(canvas);
  await QRCode.toCanvas(canvas, text, {
    errorCorrectionLevel: "M",
    margin: 1,
    width: size,
    color: { dark: "#050505", light: "#f7f7f7" },
  });
}
