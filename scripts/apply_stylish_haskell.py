import subprocess
from pathlib import Path

import yaml

from gugugu_defs import PROJECT_ROOT


def main():
    with open(PROJECT_ROOT / "stack.yaml") as h:
        stack_yaml = yaml.load(h, Loader=yaml.SafeLoader)
    for pkg in stack_yaml["packages"]:
        pkg_dir = PROJECT_ROOT / pkg
        for d in ["src", "app"]:
            src_dir = pkg_dir / d
            for p in src_dir.glob("**/*.hs"):
                apply_stylish_haskell(p)


def apply_stylish_haskell(path: Path):
    subprocess.check_call([
        "stylish-haskell",
        "-i",
        str(path),
    ])


if __name__ == "__main__":
    main()
