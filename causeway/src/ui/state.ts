// Session state shared across UI pages. Kept in-memory only — nothing is
// persisted. Keys never touch localStorage/IndexedDB.

import type { UrbState } from "../oracle/state.js";
import type { AuthPubkey } from "../oracle/point.js";
import type { KeySource } from "../keys/xpub.js";
import { Mempool } from "../chain/mempool.js";
import type { Discovery } from "../chain/discover.js";
import { fetchSnapshot } from "../oracle/snapshot.js";

// A session can be "manage-existing" (patpAtom + auth are set) or "spawn-new"
// (no @p yet, just an xpub and a snapshot for sont-map filtering).
export interface Session {
  snapshot: UrbState;
  mp: Mempool;
  patpAtom?: bigint;
  auth?: AuthPubkey;
  keys?: KeySource;
  discovery?: Discovery;
}

let session: Session | null = null;

export function getSession(): Session | null { return session; }
export function setSession(s: Session): void { session = s; }
export function requireSession(): Session {
  if (!session) throw new Error("no session — return to landing");
  return session;
}
export function clearSession(): void { session = null; }

// Drop the @p-specific bits (patpAtom + auth) without tearing down the
// snapshot/mempool client. Used when transitioning from a manage-existing
// view into a fresh spawn flow.
export function clearManageSession(): void {
  if (!session) return;
  delete session.patpAtom;
  delete session.auth;
}

// Lazily set up a minimal session for the spawn flow (user has no @p yet).
// Fetches the snapshot if one isn't already in the session so we can filter
// inscription UTXOs. Idempotent — if a session exists, return it.
export async function ensureSession(): Promise<Session> {
  if (session) return session;
  const snapshot = await fetchSnapshot();
  const mp = new Mempool();
  session = { snapshot, mp };
  return session;
}

export function go(hash: string): void {
  window.location.hash = hash;
}
