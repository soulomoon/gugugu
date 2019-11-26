import os


HOST = os.environ.get("GUGUGU_EXAMPLE_HOST", "127.0.0.1")

_PORT = os.environ.get("GUGUGU_EXAMPLE_PORT")

if _PORT is None:
    PORT = 8080
else:
    PORT = int(_PORT)
