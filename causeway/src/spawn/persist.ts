// localStorage-backed persistence of an in-progress spawn, so a mid-flow page
// refresh (or accidental tab close) doesn't strand the user with a
// broadcasted commit and no way to reach the reveal step.
//
// SECURITY: only NON-SECRET data is persisted:
//   - xpub descriptor text (the user already handed it to Causeway voluntarily)
//   - picked UTXO metadata (on-chain public info)
//   - mined {comet, pass, tries} — the @p and public networking key
//   - assembled PSBTs (public — will be broadcast)
//   - sponsor sig + height (public)
//   - phase marker
//
// The `feed` is DELIBERATELY NOT persisted: it embeds the comet's networking
// SEED (its private key). Writing that to localStorage would expose it to any
// XSS or a shared machine. The feed lives only in memory during the active
// flow (so the boot command renders normally); if the tab is refreshed before
// the boot step, the user re-supplies their downloaded feed file to resume —
// which is why the spawn page forces a feed download at mine time.

const STORAGE_KEY = "causeway:pending-spawn";

// Current schema version. Bumped when the shape below changes incompatibly.
// v2 dropped feedHex (a secret) from PersistedMined — a stored v1 record may
// contain a private seed, so loadPendingSpawn discards it on version mismatch.
const SCHEMA_VERSION = 2;

export type SpawnPhase =
  | "assembled"            // PSBTs built, nothing broadcast yet
  | "commit-broadcast"     // commit seen in mempool
  | "commit-confirmed"     // commit at >= 1 conf
  | "reveal-broadcast"     // reveal seen in mempool
  | "reveal-confirmed";    // reveal at >= 2 confs (terminal — we clear after this)

export interface PersistedUtxo {
  txidHex: string;       // display-hex (mempool.space style, big-endian)
  vout: number;
  value: string;         // bigint as decimal string
  scriptPubKeyHex: string;
  change: 0 | 1;
  index: number;
  address: string;
}

export interface PersistedSponsorSig {
  sig: string;           // bigint decimal
  height: number;        // block height at which the signer issued the sig
}

export interface PersistedMined {
  comet: string;         // bigint decimal
  pass: string;          // bigint decimal (public networking key)
  tries: number;
  // NB: no feedHex — the feed carries the private seed and must not be
  // persisted. It is held in memory and re-supplied from the user's download
  // when resuming after a refresh.
}

export interface PersistedSpawn {
  version: typeof SCHEMA_VERSION;
  createdAt: number;         // ms epoch
  network: "main" | "testnet";
  descriptor: string;        // xpub descriptor or raw xpub; what the user pasted
  accountPath: string;       // e.g. "m/86'/0'/0'"
  masterFingerprint: string; // 8 hex chars
  picked: PersistedUtxo;
  mined: PersistedMined;
  sponsor: PersistedSponsorSig;
  commitPsbtB64: string;
  revealPsbtB64: string;
  commitTxidHex: string;     // display hex
  revealTxidHex: string;
  phase: SpawnPhase;
}

function b64Encode(b: Uint8Array): string {
  let s = "";
  for (const x of b) s += String.fromCharCode(x);
  return btoa(s);
}

function b64Decode(s: string): Uint8Array {
  const bin = atob(s);
  const out = new Uint8Array(bin.length);
  for (let i = 0; i < bin.length; i++) out[i] = bin.charCodeAt(i);
  return out;
}

export function bytesToHex(b: Uint8Array): string {
  return Array.from(b, (x) => x.toString(16).padStart(2, "0")).join("");
}

export function hexToBytes(h: string): Uint8Array {
  const out = new Uint8Array(h.length / 2);
  for (let i = 0; i < out.length; i++) out[i] = parseInt(h.slice(i * 2, i * 2 + 2), 16);
  return out;
}

// Serialize and save.
export function savePendingSpawn(data: PersistedSpawn): void {
  try {
    localStorage.setItem(STORAGE_KEY, JSON.stringify(data));
  } catch (err) {
    console.warn("causeway: could not persist spawn state:", err);
  }
}

// Load and validate. Returns null if nothing saved or schema mismatch.
export function loadPendingSpawn(): PersistedSpawn | null {
  try {
    const raw = localStorage.getItem(STORAGE_KEY);
    if (!raw) return null;
    const data = JSON.parse(raw) as PersistedSpawn;
    if (data.version !== SCHEMA_VERSION) {
      console.warn("causeway: dropping pending spawn, schema mismatch");
      clearPendingSpawn();
      return null;
    }
    return data;
  } catch (err) {
    console.warn("causeway: could not read pending spawn:", err);
    return null;
  }
}

export function clearPendingSpawn(): void {
  try { localStorage.removeItem(STORAGE_KEY); } catch { /* ignore */ }
}

export function updatePhase(phase: SpawnPhase): void {
  const current = loadPendingSpawn();
  if (!current) return;
  savePendingSpawn({ ...current, phase });
}

// How many blocks old is the sponsor sig vs the given current tip? The
// sponsor-signer's sig has a ±10-block validity window enforced by urb-core.
export function sponsorSigAgeBlocks(
  saved: PersistedSpawn, currentTipHeight: number,
): number {
  return Math.max(0, currentTipHeight - saved.sponsor.height);
}

export { b64Encode, b64Decode };
