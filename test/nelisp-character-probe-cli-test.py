#!/usr/bin/env python3
"""Verify probe option handling without starting either runtime."""
import contextlib
import importlib.util
import io
from pathlib import Path
from unittest.mock import patch

spec = importlib.util.spec_from_file_location(
    "character_probe", Path(__file__).with_name("nelisp-character-storage-regression.py"))
probe = importlib.util.module_from_spec(spec)
spec.loader.exec_module(probe)

for arguments, expected in ((["--help"], 0), (["--invalid-option"], 2),
                            (["--preload"], 2)):
    with patch("sys.argv", ["character-probe", *arguments]), \
            patch.object(probe.subprocess, "run",
                         side_effect=AssertionError("Options started a runtime")) as launch, \
            contextlib.redirect_stdout(io.StringIO()), \
            contextlib.redirect_stderr(io.StringIO()):
        try:
            probe.main()
        except SystemExit as condition:
            assert condition.code == expected, (arguments, condition.code)
        else:
            raise AssertionError(f"Options did not exit: {arguments}")
        assert launch.call_count == 0
print("character-probe-cli: help/invalid/missing argument PASS; runtime-startups=0")
