#!/usr/bin/env python3
import sys
import os
import inspect
import kaitaistruct
from kaitaistruct import KaitaiStream

import scls_file  # your generated module


def force_eval(obj, seen=None, path="root"):
    """
    Recursively forces evaluation of:
      - normal parsed fields (already in __dict__)
      - nested KaitaiStruct objects
      - lists/tuples of such objects
      - *properties* (Kaitai 'instances') which are lazy in Python
    """
    if seen is None:
        seen = set()

    # Avoid cycles
    oid = id(obj)
    if oid in seen:
        return
    seen.add(oid)

    # If it's a Kaitai struct, evaluate its instance properties
    if isinstance(obj, kaitaistruct.KaitaiStruct):
        cls = obj.__class__
        # Trigger all @property (Kaitai instances)
        for name, member in inspect.getmembers(cls, lambda x: isinstance(x, property)):
            if name.startswith("_"):
                continue
            try:
                value = getattr(obj, name)
            except Exception as e:
                raise RuntimeError(f"Instance/property failed at {path}.{name}: {e}") from e
            force_eval(value, seen, f"{path}.{name}")

        # Walk regular parsed fields
        for name, value in obj.__dict__.items():
            if name.startswith("_"):
                continue
            force_eval(value, seen, f"{path}.{name}")

        return

    # Containers
    if isinstance(obj, dict):
        for k, v in obj.items():
            force_eval(v, seen, f"{path}[{k!r}]")
        return

    if isinstance(obj, (list, tuple)):
        for i, v in enumerate(obj):
            force_eval(v, seen, f"{path}[{i}]")
        return

    # Scalars: nothing to do
    return


def verify_file(filename: str) -> None:
    try:
        with open(filename, 'rb') as f:
            _io = KaitaiStream(f);
            parsed = scls_file.SclsFile(_io)
            force_eval(parsed._read())
            _io.close()
    except Exception as e:
        print(f"[FAIL] {filename}: {e}", file=sys.stderr)
        raise


def main():
    argv = sys.argv
    if len(argv) < 2:
        print(f"Usage: {os.path.basename(argv[0])} FILE [FILE...]", file=sys.stderr)
        return 2

    failed = False
    for fn in argv[1:]:
        try:
            verify_file(fn)
            print(f"[OK]   {fn}")
        except Exception:
            failed = True

    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
