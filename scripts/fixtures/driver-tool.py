#!/usr/bin/python3
"""Local llc/clang stand-ins used only by check-driver.py on Linux."""

import json
import os
from pathlib import Path
import signal
import subprocess
import sys


tool = Path(sys.argv[0]).name
arguments = sys.argv[1:]
with open(os.environ["DTC_TEST_TRACE"], "a") as trace:
    trace.write(json.dumps({"tool": tool, "args": arguments}) + "\n")
mode = os.environ.get(f"DTC_TEST_{tool.upper()}_MODE", "real")
real = f"/usr/lib/llvm-18/bin/{tool}"
if mode == "real":
    os.execv(real, [real, *arguments])
if mode == "crash":
    os.kill(os.getpid(), signal.SIGTERM)
if mode == "fail":
    sys.exit(23 if tool == "llc" else 29)
if mode == "no_output":
    sys.exit(0)

output = Path(arguments[arguments.index("-o") + 1])
if mode == "empty":
    output.touch()
elif mode == "directory":
    output.mkdir()
elif mode == "symlink":
    output.symlink_to(os.environ["DTC_TEST_SOURCE"])
elif mode in ("partial_failure", "nonexecutable"):
    output.write_bytes(b"incomplete tool output\n")
    output.chmod(0o600)
    if mode == "partial_failure":
        sys.exit(23 if tool == "llc" else 29)
elif mode == "publish_failure":
    result = subprocess.run([real, *arguments], check=False)
    if result.returncode:
        sys.exit(result.returncode)
    # Make the final destination unreplaceable after compilation has started.
    target = Path(os.environ["DTC_TEST_TARGET"])
    target.mkdir()
    (target / "keep").write_text("destination blocker\n")
else:
    raise RuntimeError(f"unknown test tool mode: {mode}")
