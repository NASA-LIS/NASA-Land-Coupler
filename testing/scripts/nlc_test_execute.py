#!/usr/bin/env python3
"""
NLC Test Runner

Orchestrates test execution in three phases:
1. Prepares test input data
2. Runs the test executable
3. Verifies output against baseline
"""

import argparse
import subprocess
import sys
import os
import shutil
import logging
from nlc_test_prepare_input import cache_nlc_input, prepare_nlc_input
from nlc_test_config_parser import read_tests, filter_tests, InputType
from nlc_test_check import check_nlc_test

LOGGER_NAME = "nlc_test_execute"

DANGEROUS_DIRS = {
    "/",
    "/bin",
    "/boot",
    "/dev",
    "/etc",
    "/lib",
    "/lib64",
    "/proc",
    "/root",
    "/sbin",
    "/sys",
    "/usr",
    "/var",
}

def clean_output_dir(output_dir, logger=logging.getLogger(LOGGER_NAME)):
    """Remove the output directory if it exists."""
    abs_output_dir = os.path.abspath(output_dir)

    # Check if running as root (only on Unix-like systems)
    if hasattr(os, 'geteuid') and os.geteuid() == 0:
        logger.error("Refusing to clean output directory as root: %s", output_dir)
        raise PermissionError(f"Refusing to clean output directory as root: {output_dir}")

    # Safety checks to prevent deleting important directories
    if abs_output_dir in DANGEROUS_DIRS:
        logger.error("Refusing to clean system directory: %s", abs_output_dir)
        raise ValueError(f"Refusing to clean system directory: {abs_output_dir}")
    abs_parent = os.path.dirname(abs_output_dir)
    if abs_parent == "/":
        logger.error("Refusing to clean directory at root level: %s", abs_output_dir)
        raise ValueError(f"Refusing to clean directory at root level: {abs_output_dir}")

    if os.path.exists(abs_output_dir):
        logger.info("Cleaning output directory: %s", abs_output_dir)
        shutil.rmtree(abs_output_dir)


def semicolon_list(value):
    """Parse a semicolon-separated list of values."""
    return [item.strip() for item in value.split(';')]


def parse_arguments():
    """Parse command-line arguments."""
    parser = argparse.ArgumentParser(
        description="NLC Test Runner"
    )
    parser.add_argument(
        "test_dir",
        help="Root directory for all tests"
    )
    parser.add_argument(
        "executable",
        type=os.path.abspath,
        help="Path to the test executable"
    )
    parser.add_argument(
        "--num-procs",
        type=int,
        default=1,
        help="Number of MPI processes (default: 1)"
    )
    parser.add_argument(
        "config_file",
        help="Path to the test configuration file (TOML)"
    )
    parser.add_argument(
        "--tests", "-t",
        action="extend",
        type=semicolon_list,
        dest="test_names",
        help="Semicolon-separated list of test names to filter the output",
    )
    parser.add_argument(
        "--mpi-exec",
        default="mpirun",
        help="MPI execution command (default: mpirun)"
    )
    parser.add_argument(
        "--mpi-numproc-flag",
        default="-n",
        help="MPI number of processes flag (default: -n)"
    )
    parser.add_argument(
        "--clean-run",
        action="store_true",
        help="Clean the output directory before staging inputs"
    )
    parser.add_argument(
        "--cache-only",
        action="store_true",
        help="Only cache remote input files and exit"
    )
    parser.add_argument(
        "--verbose","-v",
        action="store_true",
        help="Enable verbose logging"
    )
    return parser.parse_args()


def main():
    args = parse_arguments()
    logging.basicConfig(
        level=logging.DEBUG if args.verbose else logging.INFO,
        format="[%(levelname)s] %(message)s",
    )
    logger = logging.getLogger(LOGGER_NAME)

    tests = read_tests(args.config_file, args.test_names, logger)
    if args.test_names is not None:
        tests = filter_tests(tests, args.test_names, logger)

    # Cache remote input files
    failed_tests = []
    cache_dir = os.path.join(args.test_dir, ".cache_nlc_test")
    for test in tests:
        copy_files = [f for f in test["input"] if f[0] == InputType.COPY]
        try:
            cache_nlc_input(
                copy_files,
                cache_dir,
                logger,
            )
        except Exception as e:
            logger.error("cache_nlc_input failed for test %s", test["name"])
            failed_tests.append(test["name"])
    if args.cache_only:
        if failed_tests:
            logger.error("Caching failed for tests: %s", ", ".join(failed_tests))
            sys.exit(1)
        else:
            logger.info("Caching completed successfully for all tests.")
            sys.exit(0)

    # Prepare input data
    for test in tests:
        if test["name"] in failed_tests:
            continue
        logger.info("Preparing test: %s", test["name"])
        test_dir = os.path.join(args.test_dir, test["name"])
        input_files = test["input"]
        try:
            if args.clean_run:
                clean_output_dir(test_dir, logger)
            prepare_nlc_input(
                input_files,
                cache_dir,
                test_dir,
                logger,
            )
        except Exception as e:
            logger.error("Test preparation failed for test %s", test["name"])
            failed_tests.append(test["name"])

    # Run the test
    for test in tests:
        if test["name"] in failed_tests:
            continue
        logger.info("Executing test: %s", test["name"])
        test_dir = os.path.join(args.test_dir, test["name"])
        test_log = os.path.join(test_dir, test["name"] + ".log")
        mpi = test.get("mpi", False)
        num_procs = test.get("processors", 1)
        if mpi:
            if args.mpi_exec is None or args.mpi_numproc_flag is None:
                logger.error("MPI execution requested but mpi_exec or mpi_numproc_flag not provided.")
                sys.exit(1)
            run_command = [
                args.mpi_exec,
                args.mpi_numproc_flag,
                str(num_procs),
                args.executable,
            ]
        else:
            run_command = [args.executable]
        try:
            subprocess.run(
                run_command,
                stdout=open(test_log, "w"),
                stderr=subprocess.STDOUT,
                cwd=test_dir,
                check=True
            )
        except Exception as e:
            logger.error("Test execution failed for test %s", test["name"])
            failed_tests.append(test["name"])

    # Step 3: Output verification
    for test in tests:
        if test["name"] in failed_tests:
            continue
        logger.info("Verifying output for test: %s", test["name"])
        test_dir = os.path.join(args.test_dir, test["name"])
        baseline_dir = os.path.join(test_dir, test["baseline_dir"])
        check_files = test.get("checks", [])
        try:
            check_nlc_test(
                test_dir,
                baseline_dir,
                check_files,
                logger,
            )
        except Exception as e:
            logger.error("Output verification failed for test %s", test["name"])
            failed_tests.append(test["name"])

    if failed_tests:
        logger.error("The following tests failed: %s", ", ".join(failed_tests))
        sys.exit(1)
    else:
        logger.info("All tests completed successfully.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
