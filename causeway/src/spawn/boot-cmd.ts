// Produce the shell one-liner the user runs after their reveal tx has
// confirmed. Downloads the Groundwire runtime via our minimal boot script
// (public/boot.sh → /causeway/boot.sh) and launches vere with the provided
// comet + feed.

const UW_CHARS = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-~";

// Encode a bigint as a canonical Urbit @uw literal (base-64, "0w" prefix,
// dots every 5 chars). vere's -G parses the feed with (slaw %uw ...), which
// REJECTS any other prefix — the previous "0v" prefix here produced boot
// commands vere refused with "dawn: invalid private keys".
export function atomToUw(a: bigint): string {
  if (a === 0n) return "0w0";
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
  return "0w" + grouped.join(".");
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
  return formatBootCommandFromUw({
    comet: opts.comet,
    feedUw: atomToUw(bytesToAtomLE(opts.feed)),
    ...(opts.bootScriptUrl ? { bootScriptUrl: opts.bootScriptUrl } : {}),
    ...(opts.port ? { port: opts.port } : {}),
  });
}

export interface BootCmdUwOpts {
  comet: string;
  feedUw: string;                 // the 0w… feed atom (vere -G parses via slaw %uw)
  bootScriptUrl?: string;
  port?: number;
}

// Same command from an already-@uw feed — used on a resumed spawn where the
// feed (a secret) was never persisted and the user re-supplies their saved copy.
export function formatBootCommandFromUw(opts: BootCmdUwOpts): string {
  const url = opts.bootScriptUrl ?? defaultBootScriptUrl();
  const portArg = opts.port && opts.port !== 8080 ? ` --port ${opts.port}` : "";
  return `curl -fsSL ${url} | bash -s -- --comet ${opts.comet} --feed ${opts.feedUw}${portArg}`;
}
