// Camera-based QR scanner wrapper.
//
// Uses `qr-scanner` (lightweight, no zbar/wasm). Accumulates UR frames until
// a PSBT is fully decoded, then resolves.

import QrScanner from "qr-scanner";
import { URPsbtDecoder } from "./qr-ur.js";

export interface ScanHandle {
  stop(): void;
  video: HTMLVideoElement;
}

export interface ScanProgress {
  received: number;
  expected: number;
  percent: number;
}

export async function scanPsbtFromCamera(
  container: HTMLElement,
  onProgress: (p: ScanProgress) => void,
): Promise<{ handle: ScanHandle; psbt: Promise<Uint8Array> }> {
  const video = document.createElement("video");
  video.style.cssText = "width:100%;max-width:360px;border-radius:8px;background:#000;";
  video.setAttribute("playsinline", "true");
  container.innerHTML = "";
  container.appendChild(video);

  const decoder = new URPsbtDecoder();

  let resolve!: (b: Uint8Array) => void;
  let reject!: (e: unknown) => void;
  const psbt = new Promise<Uint8Array>((res, rej) => { resolve = res; reject = rej; });

  const scanner = new QrScanner(
    video,
    (result) => {
      try {
        const bytes = decoder.receive(result.data);
        onProgress({
          received: decoder.receivedFragments().length,
          expected: decoder.expectedFragments() || 1,
          percent: decoder.progress(),
        });
        if (bytes) {
          scanner.stop();
          resolve(bytes);
        }
      } catch (err) {
        scanner.stop();
        reject(err);
      }
    },
    { highlightScanRegion: true, returnDetailedScanResult: true },
  );

  try {
    await scanner.start();
  } catch (err) {
    reject(err);
  }

  return {
    handle: { stop: () => scanner.stop(), video },
    psbt,
  };
}
