import { cue } from "./cue.js";
import { decodeState } from "./state.js";
import type { UrbState } from "./state.js";

// In dev, the Vite proxy at /_proxy/urbwatcher forwards to 143.198.70.9:8081
// to work around CORS. In production, the hosting env must provide an
// equivalent reverse proxy (or urb-watcher must send ACAO headers).
export const DEFAULT_SNAPSHOT_URL: string = defaultUrl();

function defaultUrl(): string {
  if (typeof window !== "undefined" && window.location) {
    return `${window.location.origin}/_proxy/urbwatcher/apps/urb-watcher/snapshot`;
  }
  return "http://143.198.70.9:8081/apps/urb-watcher/snapshot";
}

export async function fetchSnapshotBytes(url = DEFAULT_SNAPSHOT_URL): Promise<Uint8Array> {
  const res = await fetch(url, { headers: { accept: "application/x-urb-jam" } });
  if (!res.ok) throw new Error(`snapshot: ${res.status} ${res.statusText}`);
  const buf = await res.arrayBuffer();
  return new Uint8Array(buf);
}

// Jam atoms are stored little-endian on the wire.
export function bytesToAtomLE(b: Uint8Array): bigint {
  let x = 0n;
  for (let i = b.length - 1; i >= 0; i--) {
    x = (x << 8n) | BigInt(b[i]!);
  }
  return x;
}

export async function fetchSnapshot(url = DEFAULT_SNAPSHOT_URL): Promise<UrbState> {
  const bytes = await fetchSnapshotBytes(url);
  const noun = cue(bytesToAtomLE(bytes));
  return decodeState(noun);
}
