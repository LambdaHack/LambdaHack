#!/usr/bin/env python3
"""Cross-check the leader-conversion inventory against the source tree.

Temporary, like the document it serves: delete this script together with
``docs/leader-desync-migration.md`` when the live-read migration lands, since
after it there is no threaded pointman left to census.

The inventory in that document's section 03 buckets every UI function that
holds the pointman.  It was built by hand from a grep proxy, and the proxy has
failed twice: once because it sees a wait only in a function's own body
(``closeDirHuman``, whose wait is inside the ``pickPoint`` it calls), and once
because it looked only at the *first* bound parameter (``runDefSkills`` and
``runDefInventory``, which bind ``leader`` third).  Both were caught by reading
rather than by anything re-runnable.  This script is the re-runnable part.

What it checks, in both directions:

* every top-level function under ``engine-src/Game/LambdaHack/Client/UI`` that
  binds a parameter named ``leader...`` -- at *any* position, which is the fix
  for the second failure -- appears in one of section 03's buckets;
* every function name the buckets list still exists in the tree.

What it cannot check, and prints for hand-classification instead: point-free
definitions (they bind no parameter, so the census rule cannot see them --
``pickLeaderWithPointerHuman`` is the standing instance), and whether a
bucketed function sits in the *right* bucket, which is a semantic question
about waits and about choosing versus confirming.

It also prints the counts section 03 states, so a drifted number there is
visible rather than inferred.

Prove it non-vacuous (both directions; restore the document afterwards):

    # tree -> doc: a function that is in no bucket must be reported
    $ sed -i 's/^| `runDefSkills` |/| `runDefSkillsXX` |/' \\
          docs/leader-desync-migration.md
    $ python3 tools/leader-census.py  # FAIL ... runDefSkills -- in no bucket,
                                      # plus runDefSkillsXX -- not in the tree

    # doc -> tree: a bucketed name that does not exist must be reported
    $ sed -i 's/`partyAfterLeader`/`partyAfterLeaderXX`/' \\
          docs/leader-desync-migration.md
    $ python3 tools/leader-census.py  # FAIL ... partyAfterLeaderXX --
                                      # not in the tree, plus the real
                                      # function, now in no bucket

    # and the case that used to be a false positive: a state field named in
    # a bucket's prose must NOT fail, being a token of the tree even though
    # no top-level signature under Client/UI declares it
    $ sed -i 's/read the pointman themselves/read `sleader` themselves/' \\
          docs/leader-desync-migration.md
    $ python3 tools/leader-census.py                            # 0 failed

Both failing cases were run when this script was written, on 2026-07-30,
and all three after the doc -> tree check was widened from signatures to
tokens later the same day; each reported exactly the lines above.
"""

import os
import re
import sys

TREE = 'engine-src/Game/LambdaHack/Client/UI'
DOC = 'docs/leader-desync-migration.md'

# Section 03's buckets, in document order.  Each is introduced by a bold lead
# and runs to the next one; the last runs to the end of the section.
BUCKET_LEADS = [
    ('read-live', '**Read live**'),
    ('keep', '**Keep**'),
    ('tail', '**The mechanical tail**'),
    ('convert', '**Convert**'),
    ('keep-param', '**Keep the parameter**'),
]

SIG = re.compile(r"^([a-z][A-Za-z0-9_']*)\s*::")
IDENT = re.compile(r'`([a-z][A-Za-z0-9_\']*)`')

# Every lowercase token in the sources, for the doc -> tree direction; see
# scan_tree for why that check resolves against tokens rather than
# signatures.
TOKEN = re.compile(r'\b([a-z][A-Za-z0-9_\']*)\b')

# Backticked in the buckets' prose as *parameter* names ("bound as `aid`"),
# never as functions, so their absence from the tree means nothing.
PROSE_TOKENS = {'aid', 'source', 'target', 'born', 'leader'}


def strip_parens(s):
    """Drop parenthesised groups, so a higher-order argument's ActorId does
    not count as the function taking one."""
    out, depth = [], 0
    for ch in s:
        if ch == '(':
            depth += 1
        elif ch == ')':
            depth = max(0, depth - 1)
        elif depth == 0:
            out.append(ch)
    return ''.join(out)


def scan_tree(root):
    """Return (by_name, point_free, all_names): every top-level function
    taking a bare ActorId parameter, mapped to (module, [bound parameter
    names]), plus every lowercase token occurring anywhere in the tree,
    which is what tells a stale bucket entry from a name the census simply
    does not cover.

    That second set is deliberately every *token*, not every top-level
    signature.  The check it feeds says "not in the tree", and a renamed or
    deleted function leaves the tree entirely, so tokens catch it; whereas
    signatures alone would also flag the record fields, local bindings and
    accessors the buckets' prose legitimately names -- @sleader@ in a
    sentence about what a Keep function reads is not a claim that a
    top-level @sleader@ lives under Client/UI, and failing on it forces the
    documentation to work around the checker."""
    files = []
    for d, _, fs in os.walk(root):
        files.extend(os.path.join(d, f) for f in fs if f.endswith('.hs'))
    files.sort()
    by_name, point_free, all_names = {}, [], set()
    for path in files:
        module = os.path.basename(path)[:-3]
        lines = open(path).read().split('\n')
        all_names.update(TOKEN.findall('\n'.join(lines)))
        i = 0
        while i < len(lines):
            m = SIG.match(lines[i])
            if not m:
                i += 1
                continue
            name = m.group(1)
            all_names.add(name)
            sig, j = [lines[i]], i + 1
            while j < len(lines) and lines[j][:1] in (' ', '\t') \
                    and lines[j].strip():
                sig.append(lines[j])
                j += 1
            text = ' '.join(s.strip() for s in sig)
            i = j
            if 'ActorId' not in text:
                continue
            # A bare ActorId parameter, ignoring haddock comments between the
            # arrows and ignoring ActorId inside a higher-order argument.
            body = strip_parens(re.sub(r'--[^-]*?(?=->|$)', ' ',
                                       text.split('::', 1)[1]))
            if not re.search(r'(^|->|=>)\s*ActorId\s*->', body):
                continue
            params = find_params(lines, name, j)
            if params is None:
                continue
            if not params:
                point_free.append((module, name))
            by_name[name] = (module, params)
    return by_name, point_free, all_names


def find_params(lines, name, start):
    """Bound parameter names of the first defining equation of `name`."""
    pat = re.compile(r'^' + re.escape(name) + r'\b')
    for k in range(start, len(lines)):
        if not pat.match(lines[k]):
            continue
        rest = lines[k][len(name):]
        eq = re.search(r'(?<![=<>/!+\-*])=(?![=>])', rest)
        return (rest[:eq.start()] if eq else rest).split()
    return None


def scan_doc(path):
    """Return {bucket: set(names)} from section 03's buckets.  The read-live
    bucket is a table with one function per row, in the first column; only
    that column counts, the later ones naming waits and callees that are not
    bucket members.  The rest are prose or bullet lists, where every
    backticked lowercase name counts."""
    text = open(path).read()
    start = text.index(BUCKET_LEADS[0][1])
    end = text.index('\n## ', start)
    region = text[start:end]
    cuts = [(key, region.index(lead)) for key, lead in BUCKET_LEADS]
    buckets = {}
    for n, (key, pos) in enumerate(cuts):
        stop = cuts[n + 1][1] if n + 1 < len(cuts) else len(region)
        chunk = region[pos:stop]
        names = set()
        for line in chunk.split('\n'):
            if key == 'read-live' and line.startswith('|'):
                cells = line.split('|')
                if len(cells) > 1 and '---' not in cells[1]:
                    names.update(IDENT.findall(cells[1]))
            elif key != 'read-live':
                names.update(IDENT.findall(line))
        buckets[key] = names
    return buckets


def main():
    doc = sys.argv[1] if len(sys.argv) > 1 else DOC
    by_name, point_free, all_names = scan_tree(TREE)
    buckets = scan_doc(doc)
    bucketed = set().union(*buckets.values())

    holds_leader = {n: v for n, v in by_name.items()
                    if any(p.startswith('leader') for p in v[1])}
    leader_first = {n: v for n, v in holds_leader.items()
                    if v[1][0].startswith('leader')}
    leader_later = {n: v for n, v in holds_leader.items()
                    if not v[1][0].startswith('leader')}

    fails = []
    for name, (module, _) in sorted(holds_leader.items()):
        if name not in bucketed:
            fails.append(f'FAIL {module}.{name} -- in no bucket of {doc}')
    for key, names in buckets.items():
        for name in sorted(names - PROSE_TOKENS):
            if name not in all_names:
                fails.append(f'FAIL {key}: {name} -- not in the tree')

    print(f'tree: {len(by_name)} functions under {TREE} take an ActorId')
    print(f'      {len(holds_leader)} bind a parameter named leader...'
          f' ({len(leader_first)} first, {len(leader_later)} later)')
    for module in sorted({v[0] for v in holds_leader.values()}):
        n = sum(1 for v in holds_leader.values() if v[0] == module)
        print(f'        {module}: {n}')
    if leader_later:
        print('      bound later in the head (the third blind spot):')
        for name, (module, params) in sorted(leader_later.items()):
            pos = next(i for i, p in enumerate(params)
                       if p.startswith('leader'))
            print(f'        {module}.{name} (parameter {pos + 1})')
    if point_free:
        print('      point-free, so no parameter to read -- hand-classify:')
        for module, name in sorted(point_free):
            print(f'        {module}.{name}')

    print(f'doc:  how the {len(holds_leader)} are bucketed in {doc}')
    for key, _ in BUCKET_LEADS:
        own = sorted(n for n in buckets[key] if n in holds_leader)
        extra = len(buckets[key]) - len(own)
        print(f'        {key}: {len(own)}'
              + (f' (+{extra} named but outside the census)' if extra else ''))
    twice = sorted(n for n in holds_leader
                   if sum(n in b for b in buckets.values()) > 1)
    for name in twice:
        fails.append(f'FAIL {name} -- in more than one bucket')

    print()
    for line in fails:
        print(line)
    print(f'{len(fails)} failed'
          + ('' if fails else ' -- bucket membership is not bucket'
                              ' correctness; the reading still decides that'))
    return 1 if fails else 0


if __name__ == '__main__':
    sys.exit(main())
