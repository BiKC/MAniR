# Reading MAniR results

MAniR compares **pairwise measurements for the same isolates**. It does not
calculate ANI from genomes or align raw MALDI-TOF peaks. Start with two
precomputed, named, square, symmetric matrices. For example, one could contain
genomic ANI percentages and another MALDI-TOF spectral similarities. Each row
and column refers to an isolate, and the same cell in both matrices must refer
to the same pair. The bundled teaching data are fictional.

## Choose the right input type

- **Similarity:** larger values indicate greater similarity (for example, ANI
  or a MALDI spectral similarity score). The absolute range depends on the
  method, so 99% ANI and 0.99 MALDI similarity are not interchangeable.
- **Correlation:** values lie between -1 and +1. A more positive value
  indicates a more positive relationship; -1 is strong inverse correlation.
- **Distance:** smaller values mean more similar samples. MAniR reverses
  distance ordering internally when constructing a clustering dissimilarity or
  comparing ranks, but does **not** change the original uploaded values.

"Unspecified" treats larger values as more similar for clustering. Supply the
correct type if you want the scientific meaning of the ordering and rank
comparison to be accurate. Neither the palette nor a logarithmic
transformation changes the input type.

## Reading single heatmaps

Each cell is one row-isolate versus column-isolate comparison. A symmetric
matrix repeats the same number on both sides of the diagonal. Clustering
reorders the rows and columns **together**, placing similar patterns adjacent.
A cluster-looking block in the picture is a visual clue, not a validated taxon.

The color legend is **relative display color (0–1)**, not the input units.
MAniR maps the range of each displayed matrix to the palette. With the
default diverging palette, red represents the low end and blue the high end
of that display range. Hover over a cell for its exact input value. For a
distance matrix, remember that low (red) values indicate closer samples,
the opposite of the biological interpretation of a similarity matrix.

If you choose a categorical metadata field, colored rectangles appear in
a narrow track above the interactive heatmap. A separate legend above the
plot explains which group has each color; the colors are stable when you
switch the matrix ordering. Metadata are additional isolate attributes,
not measurements of pairwise agreement. Gray indicates missing metadata.
Large raster views show a narrow side track rather than putting a legend
over the plot.

## The two combined views

A split matrix uses **matrix 1 above the diagonal** and **matrix 2 below
the diagonal**, with the same isolates along both axes. Combined (order 1)
uses clustering from matrix 1; Combined (order 2) puts matrix 2 above the
diagonal and clusters using matrix 2 instead. Compare the apparent blocks
and which samples move when you change the ordering.

MAniR normalizes the **two triangles separately**. The same blue shade
can therefore represent ANI of 99% and MALDI similarity of 0.9; this
is not equal biological similarity. The neutral diagonal in a split view
is a visual separator, not a measured 0.5. Hover to see which matrix
contributed a cell and its original value.

## Pairwise comparison and rank gaps

Each scatterplot dot shows one unique unordered pair of isolates.
For n isolates, there are n(n-1)/2 pairs, excluding the diagonal and the
mirrored half. With the 12-isolate example, that is 66 pairs. Matrix 1
supplies the X value and matrix 2 the Y value. An upward pattern suggests
positive association between the two types of pairwise measurements.

Two useful summaries are Pearson's r for a linear relationship and
Spearman's rho for the consistency of ranks. Values lie between -1 and +1.
They do not establish that two typing methods have equal resolution,
accuracy or biological validity. Since each isolate is reused in many
pairs, those dots are **not independent observations**.

The rank-gap table is useful when units differ. It ranks pairs by each
measurement, placing more similar samples higher in the ranking. For
distance matrices, the direction is reversed before ranking. The absolute
difference between the two percentile ranks is shown as a **rank gap**:
a larger gap means the pair occupies a more different relative position
in the two matrices. Orange outlines in the plot mark up to five of the
largest rank gaps. This is an exploratory list, **not** an outlier test or
a measure of statistical significance.

The bundled example intentionally includes two unusual pairs:
ISO_02 / ISO_03 have fictional ANI 99.655 and MALDI similarity 0.52;
ISO_04 / ISO_05 have fictional ANI 97.22 and MALDI similarity 0.88.
They illustrate the need to inspect individual discordant pairs.
Do not interpret those numbers as measured bacterial biology.

The app evaluates up to the selected maximum number of distinct pairs.
For larger datasets, it uses a reproducible sample and reports the
number of evaluated pairs. The rank gaps and displayed summary then
describe **that sample**, not necessarily every pair in the input.

## Cluster agreement

Choose the number of clusters k in the sidebar. MAniR independently
clusters each matrix using its specified type, then cuts both trees into
k groups. The linkage method and choice of k may change the result.

Cluster labels such as "1" and "2" are arbitrary. Two labels with the
same number are not necessarily the same biological group. Use the
cluster-overlap table to see which groups contain the same isolates.
The adjusted Rand index (ARI) is invariant to renaming clusters:
1 indicates the same partition, around 0 corresponds to chance-level
agreement under its adjustment model, and negative values can indicate
less agreement than that model expects.

Adjusted Wallace is directional. Matrix 1 → matrix 2 asks how well
pairs grouped together by the first matrix remain grouped by the second,
after correcting for the expected chance grouping. Reversing the
direction changes which matrix supplies the original pair groupings;
the values can differ. An adjusted Wallace value is **not the probability
that the first typing method is biologically correct**.

Both metrics compare the selected cluster assignments, not the full
distribution of matrix values or independently confirmed biological labels.

## Optional Mantel test

The Mantel test assesses matrix association while retaining the
dependence between cells by permuting **whole isolate labels** in one
matrix. Its permutation p-value is not obtained by treating every dot in
the scatterplot as an independent observation. The app uses a two-sided
permutation test and requires complete matrices with no more than 400
shared isolates. A small p-value is evidence against the chosen
no-association permutation null, **not** evidence that the methods are
equivalent, diagnostic or causal. Save the number of permutations and
random seed with the results.

## Difference plots: only directly comparable measurements

A difference is original matrix 1 minus original matrix 2 for the same
pair. Use this only when both matrices measure the **same quantity**,
in the **same units**, with compatible preprocessing. Comparing ANI
from two sequencing runs can qualify; ANI percentages versus MALDI
spectral scores do not. The example deliberately disables this feature
even if the comparable-measurements box is checked.

In a valid difference heatmap, zero maps to the neutral midpoint and
positive/negative differences map to opposite sides of the diverging
palette. The colors are symmetric around zero but rescaled for display:
hover or export the exact difference matrix to interpret magnitudes.
A difference by itself does not establish which method is correct.

## Metadata and reproducibility

Metadata categories (group, source, batch, etc.) are descriptive.
Within-group versus between-group pair summaries may suggest something
to investigate, but they are not independent-replicate statistical tests
and do not account for experimental confounding. If apparent biological
groups follow batches, consider an appropriate follow-up study.

CSV and RDS exports contain original numerical values. Figures may use
normalized colors and representative downsampling for very large
matrices. Keep the original matrices and metadata, the settings manifest,
the result tables and the actual package versions with published figures.

[Back to README](../README.md)
