// Sponsor signer HTTP client. Used for %escape on fresh spawns.
//
// The production server is http://143.198.70.9:8081. Expect CORS issues
// when calling from the browser — may need a same-origin proxy.

export const DEFAULT_SPONSOR_URL = "http://143.198.70.9:8081";

export interface SponsorSignature {
  sponsorShip: bigint;
  sig: bigint;
}

export async function requestEscapeSig(
  comet: bigint,
  sponsorUrl = DEFAULT_SPONSOR_URL,
): Promise<SponsorSignature> {
  const r = await fetch(`${sponsorUrl}/apps/sponsor-signer/sign`, {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify({ comet: comet.toString() }),
  });
  if (!r.ok) throw new Error(`sponsor: ${r.status} ${await r.text()}`);
  const body = (await r.json()) as { sponsor: string; sig: string };
  return { sponsorShip: BigInt(body.sponsor), sig: BigInt(body.sig) };
}
