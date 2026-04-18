// Groundwire sponsor-signer client. The sponsor-signer service at
// 143.198.70.9:8081 signs off on a new comet's %escape to the Groundwire
// networking planet on its behalf — without this signature, a freshly
// spawned comet has no sponsor and can't route.
//
// API (from gw-onboard.py:1003-1016):
//   POST /apps/sponsor-signer/sign
//   body: {"ship": "~sampel-palnet"}
//   response: {"sig": "0x1234.5678...", "height": 945000}
//
// The sig is valid only around `height` (urb-core enforces a ±10 block
// window in urb-core.hoon:500-506), so the reveal tx must confirm promptly.

function defaultSponsorUrl(): string {
  if (typeof window !== "undefined" && window.location) {
    return `${window.location.origin}/_proxy/sponsor`;
  }
  return "http://143.198.70.9:8081";
}

// The hard-coded star under which all Groundwire comets currently spawn.
export const SPONSOR_STAR = "~daplyd";

// The planet that provides networking — the escape target every fresh
// comet immediately escapes to. Mirrors gw-onboard.py:ESCAPE_SPONSOR.
export const ESCAPE_SPONSOR =
  "~linluc-palnus-barpub-dalweg--miptyp-molfer-pitren-daplyd";

export interface SponsorSignature {
  sig: bigint;     // 512-bit Hoon @ux atom
  height: number;  // Bitcoin block height the sig was issued at
}

function hoonUxToBig(hex: string): bigint {
  const clean = hex.replace(/^0x/, "").replace(/\./g, "").trim();
  return clean ? BigInt("0x" + clean) : 0n;
}

export async function requestEscapeSig(
  cometPatp: string,
  sponsorUrl: string = defaultSponsorUrl(),
): Promise<SponsorSignature> {
  const res = await fetch(`${sponsorUrl}/apps/sponsor-signer/sign`, {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify({ ship: cometPatp }),
  });
  if (!res.ok) {
    const text = await res.text().catch(() => "");
    throw new Error(`sponsor-signer: ${res.status} ${text}`);
  }
  const data = (await res.json()) as { sig: string; height: number | string };
  return {
    sig: hoonUxToBig(data.sig),
    height: typeof data.height === "string" ? parseInt(data.height, 10) : data.height,
  };
}

export const DEFAULT_SPONSOR_URL = defaultSponsorUrl();
