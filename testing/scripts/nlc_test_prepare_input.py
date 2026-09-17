#!/usr/bin/env python3
"""NLC Test Prepare Input

Stage input files into an output directory. Supports:
  - Archives (.zip, .tar, .tar.gz, .tgz, .tar.bz2, .tar.xz, etc.) -> extracted
  - Copied Files -> copied into the output directory
  - Copied Directories -> copied or merged into the output directory
  - Symlinks -> recreated as symlinks in the output directory
  - Remote inputs (http://, https://, ftp://) -> downloaded first, then processed
"""

import argparse
import os
import sys
import hashlib
import shutil
import tarfile
import zipfile
import tempfile
import urllib.request
import urllib.parse
import logging
from nlc_test_config_parser import InputType


ARCHIVE_SUFFIXES = (
    ".zip",
    ".tar",
    ".tar.gz",
    ".tgz",
    ".tar.bz2",
    ".tbz2",
    ".tar.xz",
    ".txz",
)
SEPARATOR_SUFFIXES = (
    "*",
    os.sep,
    *( [os.altsep] if os.altsep else [] ),
)
URL_SCHEMES = (
    "http",
    "https",
    "ftp",
)
LOGGER_NAME = "nlc_test_prepare_input"

def is_remote(path):
    parsed = urllib.parse.urlparse(path)
    return parsed.scheme in URL_SCHEMES


def is_archive(path):
    lower = path.lower()
    return any(lower.endswith(suffix) for suffix in ARCHIVE_SUFFIXES)


def is_merge(path):
    """If path ends with separator or star, it should be merged."""
    return any(path.endswith(suffix) for suffix in SEPARATOR_SUFFIXES)


def url_hash(file_url):
    """Convert url to directory and filename for caching."""
    hash = hashlib.sha256(file_url.encode("utf-8")).hexdigest()
    return hash[:4], hash[4:]


def download_remote_file(url, dest_file, logger=logging.getLogger(LOGGER_NAME)):
    """Download a remote file into dest_dir and return the local path."""
    parsed = urllib.parse.urlparse(url)
    filename = os.path.basename(parsed.path)
    if not filename:
        logger.error("URL does not contain a valid filename: %s", url)
        raise ValueError(f"URL does not contain a valid filename: {url}")

    logger.info("Downloading %s", url)
    with urllib.request.urlopen(url) as response, open(dest_file, "wb") as out_file:
        shutil.copyfileobj(response, out_file)


def download_remote_archive(url, dest_dir, logger=logging.getLogger(LOGGER_NAME)):
    """Download a remote archive as tempfile then extract it into dest_dir."""
    with tempfile.NamedTemporaryFile(delete=False) as tmp_file:
        tmp_path = tmp_file.name
    logger.info("Downloading remote archive %s -> %s", url, tmp_path)
    with urllib.request.urlopen(url) as response, open(tmp_path, "wb") as out_file:
        shutil.copyfileobj(response, out_file)

    extract_archive(tmp_path, dest_dir)
    os.remove(tmp_path)


def extract_archive(archive_path, output_dir, strip_top_level=True, logger=logging.getLogger(LOGGER_NAME)):
    """Extract an archive (zip or tar variant) into output_dir.

    If strip_top_level=True and the archive contains a single top-level directory,
    its contents are extracted directly to output_dir instead.
    """
    logger.info("Extracting archive %s -> %s", archive_path, output_dir)

    os.makedirs(output_dir, exist_ok=True)

    if strip_top_level:
        # Extract to temp directory first to check for top-level wrapper
        with tempfile.TemporaryDirectory() as temp_dir:
            if archive_path.lower().endswith(".zip"):
                with zipfile.ZipFile(archive_path, "r") as zf:
                    zf.extractall(temp_dir)
            else:
                with tarfile.open(archive_path, "r:*") as tf:
                    tf.extractall(temp_dir)

            items = os.listdir(temp_dir)
            if len(items) == 1:
                item_path = os.path.join(temp_dir, items[0])
                if os.path.isdir(item_path):
                    logger.info("Stripping top-level directory: %s", items[0])
                    copy_directory_contents(item_path, output_dir)
                else:
                    copy_single_file(item_path, output_dir)
            else:
                copy_directory_contents(temp_dir, output_dir)
    else:
        # Extract directly without stripping
        if archive_path.lower().endswith(".zip"):
            with zipfile.ZipFile(archive_path, "r") as zf:
                zf.extractall(output_dir)
        else:
            with tarfile.open(archive_path, "r:*") as tf:
                tf.extractall(output_dir)


def copy_entire_directory(src_dir, output_dir, logger=logging.getLogger(LOGGER_NAME)):
    """Copy an entire directory into output_dir."""
    logger.info("Copying directory %s -> %s", src_dir, output_dir)
    shutil.copytree(src_dir, output_dir, dirs_exist_ok=True, symlinks=True)


def copy_directory_contents(src_dir, output_dir, logger=logging.getLogger(LOGGER_NAME)):
    """Copy contents of directory into output_dir."""
    for item in os.listdir(src_dir):
        src_item = os.path.join(src_dir, item)
        dst_item = os.path.join(output_dir, item)
        if os.path.islink(src_item):
            link_target = os.readlink(src_item)
            if os.path.lexists(dst_item):
                os.remove(dst_item)
            os.symlink(link_target, dst_item)
        elif os.path.isdir(src_item):
            shutil.copytree(src_item, dst_item, dirs_exist_ok=True, symlinks=True)
        else:
            shutil.copy2(src_item, dst_item)


def symlink_file(src_path, output_dir, logger=logging.getLogger(LOGGER_NAME)):
    """Create a symlink inside output_dir, pointing at the src_path."""
    link_target = os.path.abspath(src_path)
    dst_path = os.path.join(output_dir, os.path.basename(src_path))
    logger.info("Creating symlink %s -> %s", dst_path, link_target)
    if os.path.lexists(dst_path):
        os.remove(dst_path)
    os.symlink(link_target, dst_path)


def copy_single_file(src_path, dest_file, logger=logging.getLogger(LOGGER_NAME)):
    """Copy a single regular file into output_dir."""
    logger.info("Copying file %s -> %s", src_path, dest_file)
    shutil.copy2(src_path, dest_file)

def cache_nlc_input(input_files, cache_dir, logger=logging.getLogger(LOGGER_NAME)):
    """Cache all inputs into cache_dir"""

    cache_manifest = os.path.join(cache_dir, "cache_manifest.txt")

    copy_inputs = []
    symlink_inputs = []

    for input in input_files:
        if input[0] == InputType.COPY:
            copy_inputs.append(input[1])
        elif input[0] == InputType.SYMLINK:
            symlink_inputs.append(input[1])
        else:
            logger.error("Invalid input type: %s", input[0])
            raise ValueError(f"Invalid input type: {input[0]}")

    os.makedirs(cache_dir, exist_ok=True)
    with open(cache_manifest, "a") as f:
        for input_path in copy_inputs:
            if is_remote(input_path):
                cache_hash = url_hash(input_path)
                cache_file = os.path.join(cache_dir, cache_hash[0], cache_hash[1])
                if not os.path.exists(cache_file):
                    os.makedirs(os.path.dirname(cache_file), exist_ok=True)
                    download_remote_file(input_path, cache_file, logger)
                    f.write(f"{input_path} -> {cache_file}\n")


def prepare_nlc_input(input_files, cache_dir, output_dir, logger=logging.getLogger(LOGGER_NAME)):
    """Process all inputs and stage them into output_dir."""
    os.makedirs(output_dir, exist_ok=True)

    copy_inputs = []
    symlink_inputs = []

    for input in input_files:
        if input[0] == InputType.COPY:
            copy_inputs.append(input[1])
        elif input[0] == InputType.SYMLINK:
            symlink_inputs.append(input[1])
        else:
            logger.error("Invalid input type: %s", input[0])
            raise ValueError(f"Invalid input type: {input[0]}")


    for input_path in copy_inputs:
        if is_remote(input_path):
            cache_hash = url_hash(input_path)
            cache_file = os.path.join(cache_dir, cache_hash[0], cache_hash[1])
            if os.path.exists(cache_file):
                logger.info("Using cached file for %s", input_path)
                if is_archive(input_path):
                    extract_archive(cache_file, output_dir, logger)
                else:
                    copy_single_file(cache_file, os.path.join(output_dir, os.path.basename(input_path)), logger)
            else:
                if is_archive(input_path):
                    download_remote_archive(input_path, output_dir, logger)
                else:
                    download_remote_file(input_path, os.path.join(output_dir, os.path.basename(input_path)), logger)
        else:
            if os.path.isdir(input_path):
                if is_merge(input_path):
                    copy_directory_contents(input_path, output_dir, logger)
                else:
                    copy_entire_directory(input_path, output_dir, logger)
            elif os.path.isfile(input_path):
                if is_archive(input_path):
                    extract_archive(input_path, output_dir, logger)
                else:
                    copy_single_file(input_path, os.path.join(output_dir, os.path.basename(input_path)), logger)
            elif os.path.islink(input_path):
                symlink_file(input_path, output_dir)
            else:
                logger.error("Input path does not exist: %s", input_path)
                raise ValueError(f"Input path does not exist: {input_path}")

    for input_path in symlink_inputs:
        if is_remote(input_path):
            logger.error("Cannot symlink remote input: %s", input_path)
            raise ValueError(f"Cannot symlink remote input: {input_path}")
        else:
            symlink_file(input_path, output_dir)


def parse_args(logger=logging.getLogger(LOGGER_NAME)):
    """Parse command-line arguments."""

    parser = argparse.ArgumentParser(
        description="Stage test input files into an output directory."
    )
    parser.add_argument(
        "output_dir",
        type=os.path.abspath,
        help="Directory to stage the processed inputs into.",
    )
    parser.add_argument(
        "--copy",
        action="append",
        dest="copy_files",
        metavar="FILE",
        help="Input file or archive to copy (can be specified multiple times).",
    )
    parser.add_argument(
        "--symlink",
        action="append",
        dest="symlink_files",
        metavar="FILE",
        help="Input file to symlink (can be specified multiple times).",
    )
    parser.add_argument(
        "--cache-dir",
        type=os.path.abspath,
        default=os.path.join(os.getcwd(), ".cache_nlc_test"),
        help="Directory to cache remote files (default: ./.cache_nlc_test).",
    )
    parser.add_argument(
        "-v", "--verbose",
        action="store_true",
        help="Enable verbose logging.",
    )
    return parser.parse_args()


def main():
    args = parse_args()
    logging.basicConfig(
        level=logging.DEBUG if args.verbose else logging.INFO,
        format="[%(levelname)s] %(message)s",
    )
    logger = logging.getLogger(LOGGER_NAME)

    input_files = []
    if args.copy_files:
        for file in args.copy_files:
            input_files.append((InputType.COPY, file))
    if args.symlink_files:
        for file in args.symlink_files:
            input_files.append((InputType.SYMLINK, file))

    try:
        cache_nlc_input(
            input_files,
            args.cache_dir,
            logger,
        )
        prepare_nlc_input(
            input_files,
            args.cache_dir,
            args.output_dir,
            logger,
        )
    except Exception as exc:
        logger.error("prepare_nlc_input failed: %s", exc)
        sys.exit(1)


if __name__ == "__main__":
    main()
