#!/usr/bin/env python3
"""Print the heading outline of Markdown files, for the heading-scope check
in CLAUDE.md (see "Document verification").

Handles both ATX headings (`## Foo`) and Setext headings (a text line
underlined with `===` for level 1 or `---` for level 2), and ignores
`#`/`===`/`---` lines inside fenced code blocks, so shell comments and
horizontal rules are not mistaken for headings. Headings print as
"<line>: <#-prefix> <text>", indented by level, so a mis-levelled or
orphaned section shows up at a glance. Then read the body under each
heading and confirm it is actually about that heading, at that level.

Usage: python3 tools/heading-outline.py FILE [FILE ...]

Non-vacuity (per CLAUDE.md's "prove a checker non-vacuous"): run it on this
repo's docs and confirm it lists the ATX headings of CLAUDE.md *without* the
`# one-time setup...` comment inside the Build code fence, and lists the
Setext headings of README.md / GameDefinition/PLAYING.md (which have no `#`
at all). If either fails, the fence tracking or the Setext branch is broken.
"""
import re
import sys

ATX = re.compile(r'^(#{1,6}) +(.*?)\s*#*\s*$')
RULE_EQ = re.compile(r'^=+\s*$')
RULE_DASH = re.compile(r'^-+\s*$')
FENCE = re.compile(r'^\s*(```|~~~)')


def outline(path):
    lines = open(path, encoding='utf-8').read().splitlines()
    headings = []
    in_fence = False
    prev = ''
    for i, line in enumerate(lines):
        if FENCE.match(line):
            in_fence = not in_fence
            prev = line
            continue
        if in_fence:
            prev = line
            continue
        atx = ATX.match(line)
        setext_ok = prev.strip() and not prev.lstrip().startswith('#')
        if atx:
            headings.append((i + 1, len(atx.group(1)), atx.group(2)))
        elif setext_ok and RULE_EQ.match(line):
            headings.append((i, 1, prev.strip()))
        elif setext_ok and RULE_DASH.match(line):
            headings.append((i, 2, prev.strip()))
        prev = line
    return headings


def main(argv):
    if not argv:
        print('usage: heading-outline.py FILE [FILE ...]', file=sys.stderr)
        return 2
    for path in argv:
        print(f'=== {path} ===')
        headings = outline(path)
        if not headings:
            print('  (no headings found)')
        for line_no, level, text in headings:
            print(f'{line_no:>5}: {"  " * (level - 1)}{"#" * level} {text}')
        print()
    return 0


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
