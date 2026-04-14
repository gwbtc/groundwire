// Minimal mempool.space REST client for the oracle and tx broadcast.

export interface MempoolTxOutput {
  scriptpubkey: string;
  scriptpubkey_type: string;
  value: number;
}

export interface MempoolTx {
  txid: string;
  version: number;
  locktime: number;
  vin: Array<{
    txid: string;
    vout: number;
    prevout: MempoolTxOutput | null;
    witness?: string[];
  }>;
  vout: MempoolTxOutput[];
  status: { confirmed: boolean; block_height?: number };
}

export interface MempoolOutspend {
  spent: boolean;
  txid?: string;
  vin?: number;
  status?: { confirmed: boolean; block_height?: number };
}

export class Mempool {
  constructor(readonly baseUrl = "https://mempool.space/api") {}

  async tx(txid: string): Promise<MempoolTx> {
    const r = await fetch(`${this.baseUrl}/tx/${txid}`);
    if (!r.ok) throw new Error(`mempool tx ${txid}: ${r.status}`);
    return r.json();
  }

  async outspends(txid: string): Promise<MempoolOutspend[]> {
    const r = await fetch(`${this.baseUrl}/tx/${txid}/outspends`);
    if (!r.ok) throw new Error(`mempool outspends ${txid}: ${r.status}`);
    return r.json();
  }

  async tipHeight(): Promise<number> {
    const r = await fetch(`${this.baseUrl}/blocks/tip/height`);
    if (!r.ok) throw new Error(`mempool tip: ${r.status}`);
    return Number(await r.text());
  }

  async tipHash(): Promise<string> {
    const r = await fetch(`${this.baseUrl}/blocks/tip/hash`);
    if (!r.ok) throw new Error(`mempool tip hash: ${r.status}`);
    return (await r.text()).trim();
  }

  async utxo(address: string): Promise<Array<{ txid: string; vout: number; value: number; status: { confirmed: boolean } }>> {
    const r = await fetch(`${this.baseUrl}/address/${address}/utxo`);
    if (!r.ok) throw new Error(`mempool utxo ${address}: ${r.status}`);
    return r.json();
  }

  async broadcast(rawHex: string): Promise<string> {
    const r = await fetch(`${this.baseUrl}/tx`, {
      method: "POST",
      headers: { "content-type": "text/plain" },
      body: rawHex,
    });
    if (!r.ok) throw new Error(`mempool broadcast: ${r.status} ${await r.text()}`);
    return (await r.text()).trim();
  }
}
