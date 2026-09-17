#!/usr/bin/env python3
"""NLC Test Config Parser

Parses TOML test data configuration and outputs CMake variable definitions.
"""

import sys
import os
import argparse
import logging
import tomllib
from enum import StrEnum

REQUIRED_VERSION = (3, 11)
LOGGER_NAME = "nlc_test_config_parser"

class ConfigKey(StrEnum):
    NAME = "name"
    MPI = "mpi"
    PROCESSORS = "processors"
    COMPONENTS = "components"
    INPUT = "input"
    CHECKS = "checks"
    BASELINE_DIR = "baseline_dir"
class InputKey(StrEnum):
    TYPE = "type"
    FILE = "file"
class InputType(StrEnum):
    COPY = "copy"
    SYMLINK = "symlink"
class CheckKey(StrEnum):
    TYPE = "type"
    FILE = "file"
    ARGS = "args"
class CheckType(StrEnum):
    EXISTS = "exists"
    CMP = "cmp"
    NCCMP = "nccmp"

VALID_TEST_KEYS = {item.value for item in ConfigKey}
VALID_INPUT_KEYS = {item.value for item in InputKey}
VALID_CHECK_KEYS = {item.value for item in CheckKey}
VALID_INPUT_TYPES = {item.value for item in InputType}
VALID_CHECK_TYPES = {item.value for item in CheckType}

if sys.version_info < REQUIRED_VERSION:
    print(f"Error: {__file__} requires Python {REQUIRED_VERSION[0]}.{REQUIRED_VERSION[1]}+.", file=sys.stderr)
    raise RuntimeError(f"{__file__} requires Python {REQUIRED_VERSION[0]}.{REQUIRED_VERSION[1]}+.")

def filter_tests(tests, test_names=None, logger=logging.getLogger(LOGGER_NAME)):
    """Filter the list of tests by name."""

    if test_names is None:
        return tests
    return [test for test in tests if test["name"] in test_names]


def parse_components(components):
    """Convert components value to components list."""

    component_list = []

    if components is None:
        return component_list
    elif isinstance(components, str):
        component_list.append(components)
    elif isinstance(components, list):
        for item in components:
            component_list.extend(parse_components(item))
    else:
        raise ValueError(f"Invalid components type: {type(components)}")

    return component_list


def parse_input(input):
    """Convert input value input lists."""

    input_list = []

    if input is None:
        return input_list
    elif isinstance(input, str):
        input_list.append((InputType.COPY, input))
    elif isinstance(input, list):
        for item in input:
            input_list.extend(parse_input(item))
    elif isinstance(input, dict):
        if not set(input.keys()).issubset(VALID_INPUT_KEYS):
            raise ValueError(f"Invalid keys in input dictionary: {input.keys()}")
        if InputKey.TYPE in input:
            if isinstance(input[InputKey.TYPE], str):
                if input[InputKey.TYPE] not in VALID_INPUT_TYPES:
                    raise ValueError(f"Invalid input type value: {input[InputKey.TYPE]}")
                if input[InputKey.TYPE] == InputType.COPY:
                    if InputKey.FILE not in input:
                        raise ValueError(f"Missing 'file' field for copy input type: {input}")
                    input_list.append((InputType.COPY, input[InputKey.FILE]))
                elif input[InputKey.TYPE] == InputType.SYMLINK:
                    if InputKey.FILE not in input:
                        raise ValueError(f"Missing 'file' field for symlink input type: {input}")
                    input_list.append((InputType.SYMLINK, input[InputKey.FILE]))
            else:
                raise ValueError(f"Invalid input type value: {input[InputKey.TYPE]}")
        else:
            if InputKey.FILE not in input:
                raise ValueError(f"Missing 'file' field for input dictionary: {input}")
            input_list.append((InputType.COPY, input[InputKey.FILE]))
    else:
        raise ValueError(f"Invalid input type: {type(input)}")

    return input_list


def parse_checks(checks):
    """Convert checks value to checks lists."""

    checks_list = []

    if checks is None:
        return checks_list
    elif isinstance(checks, str):
        checks_list.append((CheckType.EXISTS, checks))
    elif isinstance(checks, list):
        for item in checks:
            checks_list.extend(parse_checks(item))
    elif isinstance(checks, dict):
        if not set(checks.keys()).issubset(VALID_CHECK_KEYS):
            raise ValueError(f"Invalid keys in check dictionary: {checks.keys()}")
        if CheckKey.TYPE in checks:
            if isinstance(checks[CheckKey.TYPE], str):
                if checks[CheckKey.TYPE] not in VALID_CHECK_TYPES:
                    raise ValueError(f"Invalid check type value: {checks[CheckKey.TYPE]}")
                if checks[CheckKey.TYPE] == CheckType.EXISTS:
                    if CheckKey.FILE not in checks:
                        raise ValueError(f"Missing 'file' field for exists check type: {checks}")
                    checks_list.append((CheckType.EXISTS, checks[CheckKey.FILE]))
                elif checks[CheckKey.TYPE] == CheckType.CMP:
                    if CheckKey.FILE not in checks:
                        raise ValueError(f"Missing 'file' field for cmp check type: {checks}")
                    checks_list.append((CheckType.CMP, checks[CheckKey.FILE]))
                elif checks[CheckKey.TYPE] == CheckType.NCCMP:
                    if CheckKey.FILE not in checks:
                        raise ValueError(f"Missing 'file' field for nccmp check type: {checks}")
                    if CheckKey.ARGS in checks:
                        if isinstance(checks[CheckKey.ARGS], list):
                            args = checks[CheckKey.ARGS]
                        elif isinstance(checks[CheckKey.ARGS], str):
                            args = checks[CheckKey.ARGS].split()
                        else:
                            raise ValueError(f"Invalid 'args' field for nccmp check type: {checks[CheckKey.ARGS]}")
                    else:
                        args = []
                    checks_list.append((CheckType.NCCMP, checks[CheckKey.FILE], args))
            else:
                raise ValueError(f"Invalid check type value: {checks[CheckKey.TYPE]}")
        else:
            if CheckKey.FILE not in checks:
                raise ValueError(f"Missing 'file' field for check dictionary: {checks}")
            checks_list.append((CheckType.EXISTS, checks[CheckKey.FILE]))
    else:
        raise ValueError(f"Invalid check type: {type(checks)}")

    return checks_list


def read_tests(config_file, test_names=None, logger=logging.getLogger(LOGGER_NAME)):
    """Read the TOML configuration file and return a list of test dictionaries."""

    if not os.path.exists(config_file):
        logger.error(f"Config file not found: {config_file}")
        raise FileNotFoundError(f"Config file not found: {config_file}")

    with open(config_file, "rb") as f:
        try:
            config = tomllib.load(f)
        except Exception as e:
            logger.error(f"Error parsing TOML: {e}")
            raise ValueError(f"Error parsing TOML: {e}")
    if "tests" not in config:
        logger.error("Error: Invalid config structure")
        raise ValueError("Error: Invalid config structure")
    if test_names is not None:
        tests = [test for test in config["tests"] if test["name"] in test_names]
    else:
        tests = config["tests"]

    # Validate configuration for each test
    for test in tests:
        if not isinstance(test, dict):
            logger.error("Error: Invalid test entry format")
            raise ValueError("Error: Invalid test entry format")
        if not set(test.keys()).issubset(VALID_TEST_KEYS):
            invalid_keys = set(test.keys()) - VALID_TEST_KEYS
            logger.error(f"Invalid keys for test '{test[ConfigKey.NAME]}': {invalid_keys}")
            raise ValueError(f"Invalid keys for test '{test[ConfigKey.NAME]}': {invalid_keys}")
        if ConfigKey.NAME not in test:
            logger.error(f"Error: Test entry missing '{ConfigKey.NAME}'")
            raise ValueError(f"Error: Test entry missing '{ConfigKey.NAME}'")
        if ConfigKey.INPUT not in test:
            test["input"] = []
        else:
            test["input"] = parse_input(test[ConfigKey.INPUT])
        if ConfigKey.BASELINE_DIR not in test:
            test["baseline_dir"] = "baseline"
        if ConfigKey.CHECKS not in test:
            test["checks"] = []
        else:
            test["checks"] = parse_checks(test[ConfigKey.CHECKS])
        if ConfigKey.MPI not in test:
            test["mpi"] = False
        if ConfigKey.PROCESSORS not in test:
            test["processors"] = 1
        else:
            test["processors"] = test[ConfigKey.PROCESSORS]
        if ConfigKey.COMPONENTS not in test:
            test["components"] = []
        else:
            test["components"] = parse_components(test[ConfigKey.COMPONENTS])
    return tests


def generate_markdown_names(tests, logger=logging.getLogger(LOGGER_NAME)):
    """Generate a list of test names from the list of test dictionaries."""

    test_list = [f"| {'Test Name ':20} |"]
    test_list.append(f"| {'-' * 20} |")
    for test in tests:
        if ConfigKey.NAME not in test:
            logger.error(f"Error: Test entry missing '{ConfigKey.NAME}'")
            raise ValueError(f"Error: Test entry missing '{ConfigKey.NAME}'")
        test_list.append(f"| {test['name']:20} |")
    return test_list

def generate_markdown_summary(tests, logger=logging.getLogger(LOGGER_NAME)):
    """Print a markdown table of test configurations."""

    test_config = [f"| {'Test Name':<20} | {'Procs':<8} | {'Components':<20} | {'Input':<10} | {'Checks':<10} |"]
    test_config.append(f"| {'-' * 20} | {'-' * 8} | {'-' * 20} | {'-' * 10} | {'-' * 10} |")
    for test in tests:
        if ConfigKey.NAME not in test:
            logger.error(f"Error: Test entry missing '{ConfigKey.NAME}'")
            raise ValueError(f"Error: Test entry missing '{ConfigKey.NAME}'")
        name = test[ConfigKey.NAME]
        processors = test.get(ConfigKey.PROCESSORS, "N/A")
        components = ";".join(test.get("components", [])) if isinstance(test.get("components"), list) else test.get("components", "N/A")
        input_count = len(test.get("input", []))
        check_count = len(test.get("checks", []))
        test_config.append(f"| {name[:20]:<20} | {processors:<8} | {components[:20]:<20} | {input_count:<10} | {check_count:<10} |")
    return test_config


def generate_cmake_names(tests, logger=logging.getLogger(LOGGER_NAME)):
    """Generate a CMake set() command for the list of test names."""

    test_list = []

    for test in tests:
        test_list.append(test["name"])

    return [f'set(NLC_TESTS {";".join(test_list)})']


def generate_cmake_config(tests, logger=logging.getLogger(LOGGER_NAME)):
    """Generate CMake set() commands for test URLs."""

    test_config = []

    # Generate CMake variables for each test
    for test in tests:
        name = test["name"]
        mpi = "ON" if test["mpi"] else "OFF"
        procs = test["processors"]
        comps = ";".join(test["components"])
        test_config.append(f'set(NLC_TEST_{name}_MPI {mpi})')
        test_config.append(f'set(NLC_TEST_{name}_PROCS {procs})')
        test_config.append(f'set(NLC_TEST_{name}_COMPS {comps})')

    return test_config


def semicolon_list(value):
    return [item.strip() for item in value.split(';')]


def parse_args():
    """Parse command-line arguments."""

    parser = argparse.ArgumentParser(
        description="Parse TOML test data configuration and output CMake variable definitions."
    )
    parser.add_argument(
        "config_file",
        help="Path to test_data.toml",
    )
    parser.add_argument(
        "--list", "-l",
        action="store_true",
        help="List names of tests in the config file",
    )
    parser.add_argument("--tests", "-t",
        action="extend",
        type=semicolon_list,
        dest="test_names",
        help="Semicolon-separated list of test names to filter the output",
    )
    parser.add_argument("--cmake", "-c",
        action="store_true",
        help="Output CMake code for the tests",
    )
    parser.add_argument("--verbose", "-v",
        action="store_true",
        help="Enable verbose logging",
    )
    return parser.parse_args()


def main():

    args = parse_args()
    logging.basicConfig(
        level=logging.DEBUG if args.verbose else logging.INFO,
        format="[%(levelname)s] %(message)s",
    )
    logger = logging.getLogger(LOGGER_NAME)

    tests = read_tests(args.config_file, args.test_names, logger)
    if args.test_names is not None:
        tests = filter_tests(tests, args.test_names, logger)

    if args.cmake:
        output_list = generate_cmake_names(tests, logger)
        if not args.list:
            output_list.extend(generate_cmake_config(tests, logger))
    else:
        if args.list:
            output_list = generate_markdown_names(tests, logger)
        else:
            output_list = generate_markdown_summary(tests, logger)

    for command in output_list:
        print(command)

if __name__ == "__main__":
    main()
