#!/usr/bin/env python3
"""NLC Test Check

Verifies the output of NLC tests against expected results.
"""

import os
import logging
import filecmp
import subprocess
from nlc_test_config_parser import CheckType

LOGGER_NAME = "nlc_test_check"

def check_nlc_test(test_dir, baseline_dir, check_files, logger=logging.getLogger(LOGGER_NAME)):
    """Check the output of an NLC test against expected results."""

    exists_checks = []
    cmp_checks = []
    nccmp_checks = []
    for check in check_files:
        if check[0] == CheckType.EXISTS:
            exists_checks.append(check[1])
        elif check[0] == CheckType.CMP:
            cmp_checks.append(check[1])
        elif check[0] == CheckType.NCCMP:
            nccmp_checks.append(tuple(check[1:]))
        else:
            logger.error("Invalid check type: %s", check[0])
            raise ValueError(f"Invalid check type: {check[0]}")

    for file in exists_checks:
        filepath = os.path.join(test_dir, file)
        if not os.path.exists(filepath):
            logger.error("Check failed, file does not exist: %s", filepath)
            raise FileNotFoundError(f"Check failed, file does not exist: {filepath}")
        else:
            logger.info("Check passed, file exists: %s", filepath)

    for file in cmp_checks:
        filepath = os.path.join(test_dir, file)
        if not os.path.exists(filepath):
            logger.error("Check failed, file does not exist: %s", filepath)
            raise FileNotFoundError(f"Check failed, file does not exist: {filepath}")
        baseline = os.path.join(baseline_dir, file)
        if not os.path.exists(baseline):
            logger.error("Check failed, baseline file does not exist: %s", baseline)
            raise FileNotFoundError(f"Check failed, baseline file does not exist: {baseline}")
        if not filecmp.cmp(filepath, baseline, shallow=False):
            logger.error("Check failed, files differ: %s", file)
            raise ValueError(f"Check failed, files differ: {file}")
        else:
            logger.info("Check passed, files match: %s", file)

    for file in nccmp_checks:
        filepath = os.path.join(test_dir, file[0])
        if not os.path.exists(filepath):
            logger.error("Check failed, file does not exist: %s", filepath)
            raise FileNotFoundError(f"Check failed, file does not exist: {filepath}")
        baseline = os.path.join(baseline_dir, file[0])
        if not os.path.exists(baseline):
            logger.error("Check failed, baseline file does not exist: %s", baseline)
            raise FileNotFoundError(f"Check failed, baseline file does not exist: {baseline}")
        args = ["-fdqS"]
        if len(file) > 1:
            if file[1] is not None:
                args.extend(file[1])
        try:
            result = subprocess.run(
                ["nccmp"] + args + [filepath, baseline],
                capture_output=True,
                text=True,
            )
            if result.returncode != 0:
                logger.error(f"Check failed, files differ ({' '.join(args)}): {filepath}")
                raise ValueError(f"Check failed, files differ ({' '.join(args)}): {filepath}")
            else:
                logger.info(f"Check passed, files match ({' '.join(args)}): {filepath}")
        except FileNotFoundError:
            logger.error("nccmp is not installed, cannot perform nccmp check for file: %s", filepath)
            raise ImportError("nccmp is not installed, cannot perform nccmp check")

