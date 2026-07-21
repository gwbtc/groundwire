// localStorage-backed persistence of an in-progress CONFIDENTIAL spawn, so a
// mid-flow page refresh (or accidental tab close) doesn't strand the user with
// a broadcast commit and no way to finish.
//
// SECURITY: only NON-SECRET data is persisted:
//   - xpub descriptor text (the user already handed it to Causeway voluntarily)
//   - picked UTXO metadata (on-chain public info)
//   - mined {comet, pass, tries} — the @p and public networking key
//   - the commit PSBT (public — will be broadcast)
//   - the xtr inputs (internal key + leaf) — public commitment data
//   - phase marker
//
// The `feed` is DELIBERATELY NOT persisted: it embeds the comet's networking
// SEED (its private key). Writing that to localStorage would expose it to any
// XSS or a shared machine. The feed lives only in memory during the active
// flow; if the tab is refreshed before the boot step, the user re-supplies
// their downloaded feed file to resume — which is why the spawn page forces a
// feed download at mine time.

const STORAGE_KEY = "causeway:pending-spawn";

// Current schema version. Bumped when the shape below changes incompatibly.
// v3 (cc-draft-2 confidential): dropped the reveal PSBT + sponsor sig, added
// the xtr inputs (internalKeyHex / leafScriptHex / leafVersion). A stored v1/v2
// record describes the retired public flow, so loadPendingSpawn discards it.
const SCHEMA_VERSION = 3;

export type SpawnPhase =
  | "assembled"            // commit PSBT built, nothing broadcast yet
  | "commit-broadcast"     // commit seen in mempool
  | "commit-confirmed";    // commit at >= 1 conf (terminal — we clear after this)

export interface PersistedUtxo {
  txidHex: string;       // display-hex (mempool.space style, big-endian)
  vout: number;
  value: string;         // bigint as decimal string
  scriptPubKeyHex: string;
  change: 0 | 1;
  index: number;
  address: string;
}

export interface PersistedMined {
  comet: string;         // bigint decimal
  pass: string;          // bigint decimal (public networking key)
  tries: number;
  // NB: no feedHex — the feed carries the private seed and must not be
  // persisted. It is held in memory and re-supplied from the user's download
  // when resuming after a refresh.
}

// The public commitment data the xtr reveal log needs once the commit confirms
// (block height comes from mempool.space at that point). Mirrors the subset of
// desktop proof.json a `causeway finalize` reads.
export interface PersistedXtrInputs {
  internalKeyHex: string;   // 32-byte funding xonly; xtr uses 02||xonly
  leafScriptHex: string;    // NUMS-wrapped %spawn attestation leaf
  leafVersion: number;      // 0xc0
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
  commitPsbtB64: string;
  commitTxidHex: string;     // display hex
  commitValue: string;       // bigint decimal — the commit output's sats
  xtr: PersistedXtrInputs;
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

export { b64Encode, b64Decode };
