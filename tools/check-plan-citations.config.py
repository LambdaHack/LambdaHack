"""Settings for ~/slop/tools/check-plan-citations.py in this repository, read by its
common.repo_config: the block that stood at the head of the checker while it
lived in this repository's tools/, and which says what each setting does.
"""

# The directories a cited file is looked for under, the root included.
SEARCH_ROOTS = ["engine-src", "definition-src", "GameDefinition", "ts-src",
                "test", "tools", "docs", ".github", ".claude", "."]
