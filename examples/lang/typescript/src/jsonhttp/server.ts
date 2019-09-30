import * as http from "http";
import {
  IncomingMessage,
  ServerResponse,
} from "http";
import {
  QualName,
  WithMeta,
  ServerCodecHandler,
} from "../../build/generated/gugugu/gugugu/transport";
import {
  FoldRequest,
  HelloServer,
} from "../../build/generated/gugugu/guguguexamples/definitions/hello";
import {
  JsonCodecImpl,
  JsonRepr,
} from "../codec/json-codec";
import {
  guguguExampleHost,
  guguguExamplePort,
} from "../utils";
import {
  Metadata,
  readAllAsUtf8String,
  headersToMeta,
  metaToHeaders,
} from "./common";


async function main(): Promise<void> {
  const jsonCodecImpl = new JsonCodecImpl();
  const impl = new HelloServerImpl();
  const serverTransport = HelloServer.toTransport(impl, jsonCodecImpl, jsonCodecImpl);

  const server = http.createServer(async (req: IncomingMessage, res: ServerResponse) => {
    console.log(`Got request ${req.url}`);
    console.log(req.headers)
    let url = req.url;
    if (url === undefined) {
      res.end();
      return;
    }
    if (url.startsWith("/")) {
      url = url.substr(1);
    }
    const ps = url.split("/");
    const psLen = ps.length;

    if (psLen < 2) {
      res.writeHead(404)
      res.end();
      return;
    }

    const qualName: QualName = {
      namespace: ps.slice(0, psLen - 1),
      name: ps[psLen - 1],
    }

    const handler = serverTransport.ask(qualName, async (fr, decoder, encoder, k) => {
      const a = decoder(fr.data);
      const gb = await k({
        data: a,
        meta: fr.meta,
      });
      const r = encoder(gb.data)
      const gr = {
        meta: gb.meta,
        data: r,
      };
      return gr;
    });

    if (handler === null) {
      res.writeHead(404)
      res.end();
      return;
    }

    const body = await readAllAsUtf8String(req);
    const json = JSON.parse(body);
    const reqMeta = headersToMeta(req.headers);
    const gr = await handler({
      meta: reqMeta,
      data: json,
    });

    let resMeta = gr.meta;
    if (resMeta === undefined) {
      resMeta = {};
    }
    const resContent = JSON.stringify(gr.data);
    const resHeaders = metaToHeaders(resMeta);
    res.writeHead(200, resHeaders);
    res.write(resContent);
    res.end();

  });

  server.listen(guguguExamplePort, guguguExampleHost);

  console.log(`Server running at http://${guguguExampleHost}:${guguguExamplePort}`);
  console.log(`Press Ctrl-D to shut down the server`);
  await new Promise(resolve => {
    process.stdin.on("data", _ => {});
    process.stdin.on("end", resolve);
  });
  console.log(`Shutting down the server`);
  server.close();

}


class HelloServerImpl implements HelloServer<Metadata, Metadata> {

  public fold(a: FoldRequest, meta: Metadata): Promise<WithMeta<Metadata, number>> {
    return this.withMeta(a, meta, async req => {
      return req.values.reduce((b, c) => b + c, req.initial);
    });
  }

  private async withMeta<a, b>(a: a, meta: Metadata, k: (a: a) => Promise<b>): Promise<WithMeta<Metadata, b>> {
    const begin = (new Date()).getTime();
    Object.entries(meta).forEach(([k, v]) => {
      console.log(`Metadata: ${k} = ${v}`);
    });
    const b = await k(a);
    const end   = (new Date()).getTime();
    return {
      meta: {
        "X-Process-Time": `${begin - end}ms`,
      },
      data: b,
    };
  }

}


main().catch(reason => {
  console.error("Failed!");
  console.error(reason);
  process.exit(1);
});
