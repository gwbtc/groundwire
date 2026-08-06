export type Ship = bigint;

// $fief (sur/urb.hoon):
//
//     +$  turf  (list @t)                  ::  domain, tld first
//     +$  fief  $%  [%turf p=(list turf) q=@udE]
//                   [%if   p=@ifF         q=@udE]
//                   [%is   p=@isH         q=@udE]
//               ==
//
// The fief rides the $snapshot, so it is part of the on-chain state
// commitment (state-key is taken over the JAMMED snapshot): this
// representation MUST be lossless, or re-committing a carried-forward fief
// silently produces a different taproot output key. `domains` is therefore the
// (list turf) verbatim — an array of turfs, each an array of its @t segments
// as atoms, tld first — and NOT a flattened/decoded string form.
export type Fief =
  | { type: "if"; ip: number; port: number }
  | { type: "is"; ip: bigint; port: number }
  | { type: "turf"; domains: bigint[][]; port: number };

export type Sont = { txid: Uint8Array; vout: bigint; off: bigint };

export type Mang =
  | { type: "sont"; sont: Sont }
  | { type: "pass"; pass: bigint };

export type SpawnTo = {
  spkh: Uint8Array;
  vout: bigint | null;
  off: bigint;
  tej: bigint;
};

export type Single =
  | { op: "spawn"; pass: bigint; fief: Fief | null; to: SpawnTo }
  | { op: "keys"; pass: bigint; breach: boolean }
  | { op: "escape"; parent: Ship; sig: bigint | null }
  | { op: "cancel-escape"; parent: Ship }
  | { op: "adopt"; ship: Ship }
  | { op: "reject"; ship: Ship }
  | { op: "detach"; ship: Ship }
  | { op: "fief"; fief: Fief | null }
  | { op: "set-mang"; mang: Mang | null };

export type SkimSotx = Single | { op: "batch"; items: Single[] };

export type Sotx = { ship: Ship; sig: bigint | null; skim: SkimSotx };
