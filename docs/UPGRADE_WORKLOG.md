# MAniR scalable-analysis upgrade worklog

Base revision: `0c149e8e082a04d9aee565ecf038855fd925a974` (main, May 2023).
Work branch: `upgrade/scalable-analysis`.

## Scope
Implement the technical upgrade and research features from the audit, excluding
arXiv manuscript drafting. Do not change main until the upgrade is reviewed.
Preserve the historical original in Git and retain compatibility where practical.

## Scientific invariants
- Numeric matrix entries must not change when reordering, plotting or exporting.
- Keep row and column sample identifiers aligned; detect duplicate and missing IDs.
- Respect the independent measurement scales of matrices.
- Similarity, correlation and distance matrices require distinct interpretations.
- Diagonal and mirrored pairs are not independent observations.
- Any sampled comparison must disclose the sample size and sampling method.
- Permutation tests must permute isolate labels, never independent matrix cells.
- Reference results on the 2023 sample workbooks must match original input values.

## Work units
1. Input, validation and matrix matching; numeric/unit tests.
2. Shared clustering, scale handling and pairwise statistics.
3. Small and large plot engines; lazy loading, caching and navigation.
4. Shiny UI/server integration, metadata, exports and compatibility.
5. Benchmark harness, CI, dependency documentation, tests and README.

## Verification
The execution environment running this edit does not include R. GitHub Actions
must execute the R test suite and benchmark smoke tests on the pull request.
Record actual timing and hardware before reporting performance improvements;
the benchmark script is infrastructure, not evidence of a speedup.

## Release criteria
- All CI tests green.
- No change to existing pairwise numeric inputs during import/export.
- Proven matching ordering across all comparative views.
- Tested failure handling for bad/missing IDs, invalid scales and missing data.
- Accurate documentation of large-dataset preview sampling limits.
- Measured, repeated performance comparison before a release speed claim.
