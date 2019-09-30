import {
  Readable,
} from "stream";
import {
  IncomingHttpHeaders,
  OutgoingHttpHeaders,
} from "http";

export type Metadata = { [k: string]: string };


export function readAllAsUtf8String(s: Readable): Promise<string> {
  return new Promise<string>((resolve, reject) => {
    const chunks: Array<Uint8Array> = [];
    s.setEncoding("utf8");
    s.on("error", reject);
    s.on("end", () => {
      resolve(chunks.join(""));
    });
    s.on("data", chunk => {
      chunks.push(chunk);
    });
  });
}

export function headersToMeta(headers: IncomingHttpHeaders): Metadata {
  const rv: Metadata = {};
  Object.keys(headers).forEach(k => {
    const v = headers[k];
      if (typeof v === "string") {
        rv[k] = v;
      }
  });
  return rv;
}


export function metaToHeaders(meta: Metadata): OutgoingHttpHeaders {
  const rv: OutgoingHttpHeaders = {};
  Object.entries(meta).forEach(([k, v]) => {
    rv[k] = v;
  });
  return rv;
}
