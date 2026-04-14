export type Ship = bigint;

export type Fief =
  | { type: "if"; ip: number; port: number }
  | { type: "is"; ip: bigint; port: number }
  | { type: "turf"; domains: Uint8Array[]; port: number };

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
