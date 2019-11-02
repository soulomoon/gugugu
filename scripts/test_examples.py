import os
import sys
import time
import socket
import subprocess
from argparse import ArgumentParser
from contextlib import ExitStack, contextmanager
from pathlib import Path
from typing import Tuple, Sequence, Mapping, ContextManager
from unittest import TestCase, TestSuite, TestResult, TextTestRunner

import yaml

from gugugu_defs import PROJECT_ROOT


def main():
    parser = ArgumentParser()
    parser.add_argument("--verbose", "-v", action="count", default=0,
                        help="be more verbose")
    args = parser.parse_args()
    verbosity = args.verbose
    suite = TestSuite()

    for protocol, (servers, clients) in load_config().items():
        protocol_suite = TestSuite()
        for server in servers:
            server_suite = TestSuiteWithServer(server)
            for client in clients:
                test_case = GuguguExampleTestCase(protocol, client, server)
                server_suite.addTest(test_case)
            protocol_suite.addTest(server_suite)
        suite.addTest(protocol_suite)

    runner = TextTestRunner(verbosity=verbosity)
    result = runner.run(suite)
    if result.wasSuccessful():
        return
    sys.exit(1)


class ProcessHelper:

    def __init__(self, name: str, protocol: str,
                 config: dict):
        self.name: str = name
        self.protocol: str = protocol
        self._command: str = config["command"]
        self._cwd: Path = PROJECT_ROOT / config.get("cwd")
        self.output_mode: str = "wb"

    def output_path(self, server: "Server") -> Path:
        raise NotImplementedError

    @contextmanager
    def with_process_running(self, server: "Server",
                             ) -> ContextManager[subprocess.Popen]:
        output_path = self.output_path(server)
        os.makedirs(output_path.parent, exist_ok=True)
        with ExitStack() as stack:
            stdout = stack.enter_context(
                open(output_path.with_suffix(".out"), self.output_mode))
            stderr = stack.enter_context(
                open(output_path.with_suffix(".err"), self.output_mode))
            env = dict(os.environ)
            env.update({
                "GUGUGU_EXAMPLE_HOST": server.host,
                "GUGUGU_EXAMPLE_PORT": str(server.port),
            })
            p: subprocess.Popen = stack.enter_context(
                subprocess.Popen(
                    args=self._command,
                    stdin=subprocess.PIPE,
                    stdout=stdout,
                    stderr=stderr,
                    cwd=self._cwd,
                    shell=True,
                    env=env,
                )
            )
            yield p


class Server(ProcessHelper):

    def __init__(self, name: str, protocol: str,
                 host: str, port: int, config: dict):
        super().__init__(
            name=name,
            protocol=protocol,
            config=config,
        )
        self.host: str = host
        self.port: str = port

    def output_path(self, server: "Server") -> Path:
        dir_path = PROJECT_ROOT / "tmp/test/logs/servers"
        return dir_path / f"{server.protocol}-{server.name}"

    @contextmanager
    def with_running(self) -> ContextManager[None]:
        with self.with_process_running(self) as p:
            try:
                self._wait_until_server_ready(p)
                yield
                p.stdin.close()
                p.wait(timeout=5)
            except Exception:
                p.terminate()
                raise

    def _wait_until_server_ready(self, p: subprocess.Popen):
        addr = self.host, self.port
        while True:
            s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            try:
                s.connect(addr)
            except ConnectionRefusedError:
                time.sleep(1)
                continue
            finally:
                s.close()
                rv = p.poll()
                if rv is not None:
                    raise RuntimeError(
                        f"Server {self.protocol}:{self.name}"
                        f" exited with code {rv}")
            return


class Client(ProcessHelper):

    def output_path(self, server: "Server") -> Path:
        dir_path = PROJECT_ROOT / "tmp/test/logs/clients"
        return dir_path / f"{self.protocol}-{self.name}-{server.name}"

    def run(self, server: Server):
        with self.with_process_running(server) as p:
            p.stdin.close()
            rv = p.wait()
            if rv != 0:
                raise RuntimeError(
                    f"Client {self.protocol}:{self.name} exited with"
                    f" non-zero exit status {rv}")


def load_config() -> Mapping[str, Tuple[Sequence[Server], Sequence[Client]]]:
    rv = dict()
    with open(PROJECT_ROOT / "scripts/test-configs.yaml") as h:
        config = yaml.safe_load(h)

    host = "127.0.0.1"
    next_port = 49152

    for protocol, protocol_entry in config.items():
        servers = []
        clients = []
        rv[protocol] = servers, clients

        for name, entry in protocol_entry.items():
            client_config = entry.get("client")
            if client_config is not None:
                client = Client(
                    name=name,
                    protocol=protocol,
                    config=client_config,
                )
                clients.append(client)
            server_config = entry.get("server")
            if server_config is not None:
                server = Server(
                    name=name,
                    protocol=protocol,
                    host=host,
                    port=next_port,
                    config=server_config,
                )
                servers.append(server)
                next_port += 1
    return rv


class GuguguExampleTestCase(TestCase):

    def __init__(self, protocol: str, client: Client, server: Server):
        super().__init__()
        self._protocol = protocol
        self._client = client
        self._server = server

    def runTest(self):
        client = self._client
        server = self._server
        client.run(server)

    def __str__(self):
        return (
            f"{self._protocol:<10} : "
            f"{self._server.name:<20} <- {self._client.name:<20}"
        )


class TestSuiteWithServer(TestSuite):

    def __init__(self, server: Server, tests=()):
        self._server = server
        super().__init__(tests)

    def run(self, result: TestResult, debug=False):
        name = f"{self._server.protocol:<10} : {self._server.name:<20} server"
        server_test = DummyTestForServer(
            self._server, f"{name} starting        ")
        result.startTest(server_test)
        try:
            with self._server.with_running():
                result.addSuccess(server_test)
                super().run(result, debug=debug)
                server_test = DummyTestForServer(
                    self._server, f"{name} stopping        ")
                result.startTest(server_test)
            result.addSuccess(server_test)
        except Exception as e:
            exc_info = sys.exc_info()
            result.addError(server_test, exc_info)


# Copied from unittest.suite._ErrorHolder
class DummyTestForServer(TestCase):

    failureException = None

    def __init__(self, server: Server, description):
        super().__init__()
        self._server = server
        self.description = description

    def id(self):
        return self.description

    def shortDescription(self):
        return None

    def __repr__(self):
        return f"<DummyTest " \
               f"server={self._server} description={self.description!r}>"

    def __str__(self):
        return self.id()

    def run(self, result):
        pass

    def __call__(self, result):
        return self.run(result)

    def countTestCases(self):
        return 0


if __name__ == "__main__":
    main()
