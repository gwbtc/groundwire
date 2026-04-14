export type Atom = bigint;
export type Noun = Atom | Cell;
export interface Cell extends Array<Noun> {
  0: Noun;
  1: Noun;
  length: 2;
}

export function isAtom(n: Noun): n is Atom {
  return typeof n === "bigint";
}

export function isCell(n: Noun): n is Cell {
  return Array.isArray(n);
}

export function asAtom(n: Noun): Atom {
  if (!isAtom(n)) throw new Error("noun: expected atom");
  return n;
}

export function asCell(n: Noun): Cell {
  if (!isCell(n)) throw new Error("noun: expected cell");
  return n;
}

export function head(n: Noun): Noun { return asCell(n)[0]; }
export function tail(n: Noun): Noun { return asCell(n)[1]; }

// Hoon unit: ~ = 0, or [~ v] = [0 v]
export function asUnit<T>(n: Noun, pick: (v: Noun) => T): T | null {
  if (isAtom(n)) {
    if (n === 0n) return null;
    throw new Error("noun: bad unit (atom != 0)");
  }
  const h = head(n);
  if (!isAtom(h) || h !== 0n) throw new Error("noun: bad unit head");
  return pick(tail(n));
}

// Hoon list: ~ or [head rest]
export function asList<T>(n: Noun, pick: (v: Noun) => T): T[] {
  const out: T[] = [];
  let cur: Noun = n;
  while (isCell(cur)) {
    out.push(pick(head(cur)));
    cur = tail(cur);
  }
  if (cur !== 0n) throw new Error("noun: malformed list");
  return out;
}

// Hoon map (treap): ~ or [[key value] [left right]]
export function asMap<K, V>(
  n: Noun,
  keyFn: (n: Noun) => K,
  valFn: (n: Noun) => V,
): Map<K, V> {
  const out = new Map<K, V>();
  const walk = (node: Noun): void => {
    if (isAtom(node)) {
      if (node !== 0n) throw new Error("noun: bad map");
      return;
    }
    const kv = head(node);
    const lr = tail(node);
    out.set(keyFn(head(kv)), valFn(tail(kv)));
    walk(head(lr));
    walk(tail(lr));
  };
  walk(n);
  return out;
}

// Destructure a right-nested tuple [a b c d ...] = [a [b [c [d ...]]]]
export function asT2(n: Noun): [Noun, Noun] {
  return [head(n), tail(n)];
}
export function asT3(n: Noun): [Noun, Noun, Noun] {
  const t = tail(n);
  return [head(n), head(t), tail(t)];
}
export function asT4(n: Noun): [Noun, Noun, Noun, Noun] {
  const [a, rest] = asT2(n);
  const [b, c, d] = asT3(rest);
  return [a, b, c, d];
}
export function asT6(n: Noun): [Noun, Noun, Noun, Noun, Noun, Noun] {
  const [a, r1] = asT2(n);
  const [b, r2] = asT2(r1);
  const [c, r3] = asT2(r2);
  const [d, e, f] = asT3(r3);
  return [a, b, c, d, e, f];
}
