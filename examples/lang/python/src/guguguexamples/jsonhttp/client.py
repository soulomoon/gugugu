import logging
from datetime import datetime

from requests import Session

from gugugu.lang.python.runtime.transport import QualName, ClientTransport
from guguguexamples.codec.json import JsonCodecImpl
from guguguexamples.config import HOST, PORT
from guguguexamples.jsonhttp import WithMeta
from guguguexamples.definitions.hello import (
    HelloModule,
    FoldRequest,
)
from guguguexamples.definitions.hellotypes import (
    Operation,
)

logger = logging.getLogger(__name__)


class HttpClientTransport(ClientTransport):

    def __init__(self, prefix: str):
        self._prefix = prefix
        self._session = Session()

    def send(self, name: QualName[str], encoder, decoder, fa):
        path = "/" + "/".join(name.namespace + [name.name])
        url = self._prefix + path
        encoded = encoder(fa.data)
        resp = self._session.post(
            url=url,
            headers=fa.meta,
            json=encoded,
        )
        try:
            body = resp.json()
            decoded = decoder(body)
            r_meta = dict()
            for k, v in resp.headers.items():
                r_meta[k] = v
            return WithMeta(r_meta, decoded)
        finally:
            resp.close()

    def close(self):
        self._session.close()


def do_request(req, f):
    meta = {
        "X-Some-Meta": "233",
    }
    fa = WithMeta(meta, req)
    fb = f(fa)
    for k, v in fb.meta.items():
        logger.info(f"Metadata: {k} = {v}")
    logger.info(f"Got response: {fb.data}")


def main():
    logging.basicConfig(
        level=logging.INFO,
    )
    transport = HttpClientTransport(f"http://{HOST}:{PORT}")
    codec_impl = JsonCodecImpl()
    try:
        client = HelloModule.from_transport(transport, codec_impl, codec_impl)
        do_request(FoldRequest(
            values=[1, 3, 4],
            initial=2,
            op=Operation.ADD,
        ), client.fold)
        do_request(10, client.calculate_fibs)
        now = datetime.now()
        do_request(now, client.incr_one_day)
    finally:
        transport.close()


if __name__ == "__main__":
    main()
