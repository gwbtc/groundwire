// Camera-scan an xpub / output descriptor from a hardware wallet.
//
// Supports:
//   - Plain-text QR containing a descriptor string or xpub
//   - ur:crypto-hdkey  (Keystone, Passport — single hdkey export)
//   - ur:crypto-account (Keystone, Passport — wallet-wide account export)
//
// Returns a descriptor-ish string that `parseKeySource` accepts (either a
// `tr(...)` descriptor if origin+xpub are known, or a bare xpub).

import QrScanner from "qr-scanner";
import { URDecoder } from "@ngraveio/bc-ur";
import { CryptoHDKey, CryptoAccount } from "@keystonehq/bc-ur-registry";
import { Buffer } from "buffer";

export interface ScannedKey {
  kind: "descriptor" | "xpub";
  text: string;
  // When we got structured data, we can populate these too:
  masterFingerprint?: string;   // 8 hex chars
  accountPath?: string;         // e.g. "m/86'/0'/0'"
}

export interface XpubScanProgress {
  received: number;      // UR frames received (1 for plain-text)
  expected: number;      // frames required (1 for plain-text / single-frame UR)
  percent: number;
  decodeErrors?: number; // frames scanned that didn't contain a QR (heartbeat)
  lastRaw?: string;      // the most-recent raw QR content (may be non-UR)
}

export interface XpubScanHandle {
  stop(): void;
  result: Promise<ScannedKey>;
}

function hdKeyToScanned(hd: CryptoHDKey): ScannedKey {
  const xpub = hd.getBip32Key();
  const origin = hd.getOrigin();
  const path = origin?.getPath?.();
  const fpBuf = origin?.getSourceFingerprint?.();
  const fp = fpBuf ? bufferToHex(fpBuf) : undefined;
  if (path && fp) {
    // Caller-assembled descriptor: we don't know the script type from an
    // hdkey alone (it's just "the key"). Default to `tr` since Causeway is
    // BIP-86-only.
    return {
      kind: "descriptor",
      text: `tr([${fp}/${path}]${xpub})`,
      masterFingerprint: fp,
      accountPath: `m/${path}`,
    };
  }
  return { kind: "xpub", text: xpub };
}

function firstTaprootFromAccount(acc: CryptoAccount): ScannedKey | null {
  const fp = bufferToHex(acc.getMasterFingerprint());
  for (const out of acc.getOutputDescriptors()) {
    const descText = out.toString();
    // toString() returns the full descriptor e.g. tr([...]xpub.../0/*) or
    // wpkh(...)/pkh(...). We only accept tr(...) for Causeway.
    if (descText.startsWith("tr(")) {
      return { kind: "descriptor", text: descText, masterFingerprint: fp };
    }
  }
  return null;
}

function bufferToHex(b: Buffer | Uint8Array): string {
  return Array.from(b, (x: number) => x.toString(16).padStart(2, "0")).join("");
}

// Try to interpret a single QR payload as a key export. Returns null if the
// payload doesn't look like one on its own (e.g., the first frame of a
// multi-frame UR).
function tryParseSingle(payload: string): ScannedKey | null {
  const p = payload.trim();
  // Output descriptor or bare xpub
  if (/^tr\(/.test(p) || /^[xyztuvXYZTUV]pub[0-9A-HJ-NP-Za-km-z]+/.test(p)) {
    return { kind: p.startsWith("tr(") ? "descriptor" : "xpub", text: p };
  }
  return null;
}

export async function scanXpubFromCamera(
  container: HTMLElement,
  onProgress: (p: XpubScanProgress) => void,
): Promise<XpubScanHandle> {
  const video = document.createElement("video");
  video.style.cssText = "width:100%;max-width:360px;border-radius:8px;background:#000;";
  video.setAttribute("playsinline", "true");
  container.innerHTML = "";
  container.appendChild(video);

  let resolve!: (k: ScannedKey) => void;
  let reject!: (e: unknown) => void;
  const result = new Promise<ScannedKey>((res, rej) => { resolve = res; reject = rej; });

  const urDecoder = new URDecoder();
  let urMode = false;

  const scanner = new QrScanner(
    video,
    (r) => {
      const data = r.data;
      try {
        // UR path: any `ur:...` fragment triggers multi-frame mode.
        if (data.toLowerCase().startsWith("ur:")) {
          urMode = true;
          urDecoder.receivePart(data);
          if (urDecoder.isError()) throw new Error(urDecoder.resultError());
          onProgress({
            received: urDecoder.receivedPartIndexes().length,
            expected: urDecoder.expectedPartCount() || 1,
            percent: urDecoder.estimatedPercentComplete(),
          });
          if (!urDecoder.isComplete()) return;
          const ur = urDecoder.resultUR();
          const cbor = Buffer.from(ur.cbor);
          let scanned: ScannedKey;
          if (ur.type === "crypto-hdkey") {
            scanned = hdKeyToScanned(CryptoHDKey.fromCBOR(cbor));
          } else if (ur.type === "crypto-account") {
            const hit = firstTaprootFromAccount(CryptoAccount.fromCBOR(cbor));
            if (!hit) throw new Error("no tr(...) descriptor in account");
            scanned = hit;
          } else {
            throw new Error(`unsupported UR type: ${ur.type}`);
          }
          scanner.stop();
          resolve(scanned);
          return;
        }

        // Plain-text path: single QR must parse as descriptor/xpub.
        if (urMode) return; // don't mix streams
        const single = tryParseSingle(data);
        if (single) {
          scanner.stop();
          resolve(single);
          return;
        }
        // Otherwise, keep scanning — it may be a non-key QR frame.
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

  return { stop: () => scanner.stop(), result };
}
