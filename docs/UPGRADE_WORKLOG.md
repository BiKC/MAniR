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


## Implementation progress (September 28, 2026)
Completed in the upgrade branch:
- Reworked upload parsing and numeric validation, including preservation of
  leading-zero sample identifiers, row/column alignment, strict/intersection
  matching, missingness checks and blockwise symmetry validation.
- Split calculation and rendering into reusable R modules; replaced repeated
  clustering with a single selected sample order. Historical complete linkage
  is the default; alternative linkage settings are available.
- Added lazy Plotly views for up to 300 isolates and a region-zoomable,
  representative raster viewer for larger datasets. Large combined plots
  slice the viewport before assembling normalized matrices.
- Added metadata color tracks, pairwise agreement summaries, a two-sided
  isolate-permutation Mantel test, ARI and adjusted Wallace, and descriptive
  within-group/between-group comparisons.
- Added CSV/TSV/gzip/RDS imports, exact original matrix exports, PNG,
  PDF/SVG and HTML preview exports, settings manifest, tests, Linux/Windows CI,
  configurable upload size, and staged benchmarking scripts.
- Opened draft PR #2 to keep the original main branch intact.

Verification still outstanding:
- No R interpreter is installed in the environment that authored these
  changes, so the R test and Shiny integration suites cannot be reported
  as passing. The workflow is committed but no completed GitHub Actions
  job has been observed yet.
- Run the bundled XLSX regression, both CI platforms, and at least three
  repeated old/new benchmark comparisons on specified hardware.
- Run large real datasets locally, record memory/time and inspect exact
  values and visual ordering before release.
- Record dependency versions from the validated environment, and produce
  a lockfile with renv after a successful R run.

Do not merge or tag this development branch as a validated release until
these checks pass. The arXiv manuscript is outside this work scope.
