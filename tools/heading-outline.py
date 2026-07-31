#!/usr/bin/env python3
"""Print the heading outline of Markdown files, for the heading-scope check
in the `doc-verification` skill, which CLAUDE.md's standing checks name.

Handles both ATX headings (`## Foo`) and Setext headings (a text line
underlined with `===` for level 1 or `---` for level 2), and ignores
`#`/`===`/`---` lines inside fenced code blocks, so shell comments and
horizontal rules are not mistaken for headings. Headings print as
"<line>: <#-prefix> <text>", indented by level, so a mis-levelled or
orphaned section shows up at a glance. Then read the body under each
heading and confirm it is actually about that heading, at that level.

Usage: python3 tools/heading-outline.py FILE [FILE ...]

Scope limits, deliberate: this shows where a block sits, never whether a
heading's words describe what sits under it. A section passes while its
title fits only its first paragraph — CLAUDE.md's "What this is" did
exactly that, holding twenty-six further lines of pointers to other
documents until they were split off under a heading that names them. No
outline can catch that; it belongs to the reading step above, which is
a human's.

Non-vacuity (per CLAUDE.md's "prove a checker non-vacuous"): run it on this
repo's docs and confirm it lists the ATX headings of CLAUDE.md *without* the
`# one-time setup...` comment inside the Build code fence, and lists the
Setext headings of README.md / GameDefinition/PLAYING.md (which have no `#`
at all). If either fails, the fence tracking or the Setext branch is broken.

Both live controls are this repo's alone: the horde-ad copy has neither a
Setext heading nor a `#` inside a fence, so it proves those two branches
on a scratch file instead. Keep them here.

Keeping them is not something to leave to memory, and that copy is the
cautionary tale: its Setext control named README.md's headings, was true
when written and false two commits later the same day, that file having
been restyled to ATX, and it went five days unnoticed -- a checker's own
proof drifting exactly as the documents it checks do, and reporting
nothing, because a recipe nobody re-runs cannot fail. The controls here
are the same shape and one restyle away from the same end, so the
fallback is written down rather than reconstructed under pressure: a
scratch file holding an ATX heading, a `===`-underlined line, a
`---`-underlined line, a fenced block containing a `#` line and a `===`
line, and one more ATX heading after it; confirm four headings, the
Setext pair among them at levels 1 and 2, and nothing from inside the
fence.

The frontmatter skip has a live control in both repos: run it on any
`SKILL.md` and confirm the only heading reported is the `#` title, not a
`## description: …` section. Before the skip, every skill file in reach
reported one -- including the `doc-verification` skill that prescribes
this very pass, since a closing `---` under a `description:` line is
indistinguishable from a Setext heading except by knowing it closes a
block.

Indented code blocks need no such guard: the ATX pattern is anchored at
column 0, so a `#` comment inside one cannot be read as a heading. That
matters here, where several documents indent blocks rather than fencing
them.
"""
import os
import re
import sys

ATX = re.compile(r'^(#{1,6}) +(.*?)\s*#*\s*$')
RULE_EQ = re.compile(r'^=+\s*$')
RULE_DASH = re.compile(r'^-+\s*$')
FENCE = re.compile(r'^\s*(```|~~~)')


def frontmatter_end(lines):
    """Index just past a YAML frontmatter block, or 0 if there is none.

    A skill file opens with `---`, a few `key: value` lines and a closing
    `---`. That closing delimiter is preceded by a non-blank line, which
    is exactly the shape of a Setext level-2 heading, so without this the
    outline reports the `description:` line as a section. Nothing in the
    line itself distinguishes the two -- both are `---` under text -- so
    the block has to be recognised and skipped.
    """
    if not lines or lines[0].strip() != '---':
        return 0
    for i in range(1, len(lines)):
        if lines[i].strip() == '---':
            return i + 1
    return 0


def outline(path):
    lines = open(path, encoding='utf-8').read().splitlines()
    headings = []
    in_fence = False
    prev = ''
    body = frontmatter_end(lines)
    for i, line in enumerate(lines):
        if i < body:
            continue
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


def require_readable(paths):
    """Exit cleanly on a mistyped name rather than with a traceback.

    Exit 2 means the run did not happen, as distinct from 1, which means
    it ran and found something.
    """
    for p in paths:
        if not os.path.isfile(p):
            print(f'no such document: {p}', file=sys.stderr)
            sys.exit(2)


def main(argv):
    if not argv:
        print('usage: heading-outline.py FILE [FILE ...]', file=sys.stderr)
        return 2
    require_readable(argv)
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
