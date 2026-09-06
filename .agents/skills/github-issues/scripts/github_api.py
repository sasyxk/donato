#!/usr/bin/env python3
"""Read GitHub tracking resources or preview/apply a reviewed JSON payload via gh."""

import argparse
import json
from pathlib import Path
import re
import subprocess
from urllib.parse import parse_qsl, urlencode, urlsplit, urlunsplit


def gh_json(endpoint, method="GET", payload=None):
    command = ["gh", "api", endpoint, "--hostname", "github.com", "--method", method]
    if payload is not None:
        command.extend(["--input", str(payload)])
    result = subprocess.run(command, capture_output=True, text=True,
                            encoding="utf-8", timeout=60)
    if result.returncode:
        raise RuntimeError(result.stderr.strip() or "GitHub request failed")
    return json.loads(result.stdout)


def read_pages(endpoint):
    parts = urlsplit(endpoint)
    query = dict(parse_qsl(parts.query))
    query["per_page"] = "100"
    items = []
    page = 1
    while True:
        query["page"] = str(page)
        batch = gh_json(urlunsplit(parts._replace(query=urlencode(query))))
        if not isinstance(batch, list):
            raise RuntimeError("--paginate requires an endpoint returning a JSON array")
        items.extend(batch)
        if len(batch) < 100:
            return items
        page += 1


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("resource", help="Repository-relative endpoint, e.g. milestones/1")
    parser.add_argument("--repo", default="sasyxk/donato")
    parser.add_argument("--expected-login", default="sasyxk")
    parser.add_argument("--method", choices=("GET", "POST", "PATCH"), default="GET")
    parser.add_argument("--input", type=Path, help="Reviewed UTF-8 JSON object for a write")
    parser.add_argument("--paginate", action="store_true", help="Combine all GET list pages")
    parser.add_argument("--apply", action="store_true", help="Send an authorized write")
    args = parser.parse_args()
    if not re.fullmatch(r"[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+", args.repo):
        parser.error("--repo must have owner/name form")
    parts = urlsplit(args.resource)
    if parts.scheme or parts.netloc or parts.fragment or not re.fullmatch(
            r"(?:issues|milestones|labels)(?:/[1-9][0-9]*)?", parts.path):
        parser.error("Use an issues, milestones or labels endpoint relative to the repository")
    write = args.method != "GET"
    if write and (args.input is None or args.paginate or parts.query):
        parser.error("Writes require --input and do not accept --paginate or a query string")
    if not write and (args.input is not None or args.apply):
        parser.error("GET does not accept --input or --apply")
    endpoint = f"repos/{args.repo}/{args.resource}"
    if write:
        payload = json.loads(args.input.read_text(encoding="utf-8-sig"))
        if not isinstance(payload, dict):
            parser.error("The JSON payload must be an object")
        if not args.apply:
            print(json.dumps(dict(method=args.method, endpoint=endpoint,
                                  payload=payload, applied=False), indent=2,
                             ensure_ascii=False))
            return
        profile = gh_json("user")
        if profile.get("login", "").lower() != args.expected_login.lower():
            raise RuntimeError("Unexpected GitHub account; verify gh authentication before writing")
        repo = gh_json(f"repos/{args.repo}")
        if not repo.get("permissions", {}).get("push"):
            raise RuntimeError("Repository write permission is required for this tracking helper")
        # Forward JSON from the reviewed file; credentials stay entirely inside gh.
        result = gh_json(endpoint, args.method, args.input.resolve())
    else:
        result = read_pages(endpoint) if args.paginate else gh_json(endpoint)
    print(json.dumps(result, indent=2, ensure_ascii=False))


if __name__ == "__main__":
    try:
        main()
    except (RuntimeError, OSError, ValueError, subprocess.TimeoutExpired) as error:
        raise SystemExit(str(error))
