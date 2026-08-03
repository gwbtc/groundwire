// Groundwire Bitcoin RPC client (subset).

export interface RpcAuth {
  url: string;
  user: string;
  pass: string;
}

export const ALPHA_RPC: RpcAuth = {
  url: "https://alpha.groundwire.dev/rpc",
  user: "mainnetrpcuser",
  // Credentials are checked into gw-onboard.py; not a real secret.
  pass: "fc3d36ce83e15484e75a658b2a9a8a90a66f4cb017ace74c8631fe082b93adbf",
};

export class Rpc {
  constructor(readonly auth: RpcAuth = ALPHA_RPC) {}

  private authHeader(): string {
    const token = btoa(`${this.auth.user}:${this.auth.pass}`);
    return `Basic ${token}`;
  }

  async call<T = unknown>(method: string, params: unknown[] = []): Promise<T> {
    const r = await fetch(this.auth.url, {
      method: "POST",
      headers: {
        "content-type": "application/json",
        authorization: this.authHeader(),
      },
      body: JSON.stringify({ jsonrpc: "1.0", id: "causeway", method, params }),
    });
    if (!r.ok) throw new Error(`rpc ${method}: ${r.status}`);
    const body = (await r.json()) as { result: T; error: { message: string } | null };
    if (body.error) throw new Error(`rpc ${method}: ${body.error.message}`);
    return body.result;
  }

  getRawTransaction(txid: string, verbose = true): Promise<unknown> {
    return this.call("getrawtransaction", [txid, verbose]);
  }

  sendRawTransaction(rawHex: string): Promise<string> {
    return this.call("sendrawtransaction", [rawHex]);
  }

  estimateSmartFee(blocks: number): Promise<{ feerate?: number; blocks?: number }> {
    return this.call("estimatesmartfee", [blocks]);
  }
}
