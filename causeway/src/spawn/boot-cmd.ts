// Produce the shell one-liner the user runs after their reveal tx has
// confirmed. Downloads the Groundwire runtime via our minimal boot script
// (public/boot.sh → /causeway/boot.sh) and launches vere with the provided
// comet + feed.

const UW_CHARS = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-~";

// Encode a bigint as Urbit @uw (base-64, "0v" prefix, dots every 5 chars).
export function atomToUw(a: bigint): string {
  if (a === 0n) return "0v0";
  let digits = "";
  let x = a;
  while (x > 0n) {
    digits = UW_CHARS[Number(x & 63n)] + digits;
    x >>= 6n;
  }
  const grouped: string[] = [];
  for (let i = digits.length; i > 0; i -= 5) {
    grouped.unshift(digits.slice(Math.max(0, i - 5), i));
  }
  return "0v" + grouped.join(".");
}

export function bytesToAtomLE(b: Uint8Array): bigint {
  let x = 0n;
  for (let i = b.length - 1; i >= 0; i--) x = (x << 8n) | BigInt(b[i]!);
  return x;
}

export interface BootCmdOpts {
  comet: string;                  // @p including leading ~
  feed: Uint8Array;               // raw jam bytes
  bootScriptUrl?: string;         // default https://groundwire.io/causeway/boot.sh
  port?: number;                  // default 8080
}

function defaultBootScriptUrl(): string {
  // When we're running in the browser, assume the boot script is served from
  // the same origin + same path as Causeway (good for local dev and staging).
  if (typeof window !== "undefined" && window.location) {
    return `${window.location.origin}/causeway/boot.sh`;
  }
  return "https://groundwire.io/causeway/boot.sh";
}

// The copy-paste one-liner. Uses `bash -s --` so the flags are piped into
// the downloaded script cleanly.
export function formatBootCommand(opts: BootCmdOpts): string {
  const url = opts.bootScriptUrl ?? defaultBootScriptUrl();
  const feedUw = atomToUw(bytesToAtomLE(opts.feed));
  const portArg = opts.port && opts.port !== 8080 ? ` --port ${opts.port}` : "";
  return `curl -fsSL ${url} | bash -s -- --comet ${opts.comet} --feed ${feedUw}${portArg}`;
}
