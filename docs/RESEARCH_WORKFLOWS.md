# Analyze a research question with MAniR

MAniR accepts **precomputed, symmetric, named pairwise matrices**. It does not
process raw MALDI-TOF spectra or calculate ANI from genome assemblies. A matrix
has one row and one column per isolate. The number at an intersection
describes that pair. All comparisons require the **same isolate IDs** or a
deliberately selected intersection.

The application now starts with a question rather than a list of statistical
tools. You can work through a whole analysis without needing to know which
statistic to choose in advance.

## Begin with your own data

1. Upload your first matrix. CSV and RDS are preferable for very large
   datasets; Excel workbooks can contain two matrices and a metadata sheet.
2. Provide short names such as "ANI from WGS" and "MALDI similarity" and
   identify each matrix as similarity, correlation or distance. These choices
   matter for clustering and the direction of rank comparisons.
3. Upload a second matrix if comparing methods, and a metadata table if
   examining known biological groups, specimen sources or experimental batches.
   Choose whether the datasets must contain exactly the same isolates or
   whether only shared isolates should be compared.
4. Click **Load and analyze uploaded files**. The start page shows how many
   samples are available, which questions you can answer and whether matching
   removed any isolates. You can always return to this page via **All questions**
   or the **Start here** tab.

To try the workflow without data, click **Load example**. This loads 12 fictional
isolates, a percentage-like ANI matrix, a 0–1 MALDI-like similarity matrix and
group/source/batch metadata. These numbers are synthetic and must not be used
as measured biological evidence.

## Question 1: Do the methods describe similar relationships?

Click **Compare the methods**. Start with Spearman's rank correlation:
it summarizes whether isolate pairs that rank high in one method tend to
rank high in the other. Pearson's r describes a linear relationship.
For distance matrices, MAniR reverses their direction in these association
summaries so that higher oriented values always mean greater similarity.

Use **Inspect the pairs behind these numbers** to see the original scatterplot.
An overall correlation can coexist with individual pairs that rank very
differently, so review them before writing a conclusion. The optional Mantel
test uses whole-isolate label permutations for complete datasets of at most
400 isolates. It tests matrix association, not equivalent typing accuracy.

## Question 2: Which isolate pairs deserve follow-up?

Click **Find unusual pairs**. MAniR lists pairs with the largest **rank gaps**:
pairs occupying noticeably different positions in the two ranked lists.
Choose a pair from that list, or type any two shared isolate IDs in the
search boxes. MAniR shows their exact original values and, when selected,
their metadata group labels. The corresponding point is outlined in the
scatterplot, even if the pair was absent from a sampled overview.

In the fictional example, try **ISO_02 and ISO_03**. They have ANI-like
similarity 99.655 and MALDI-like similarity 0.52. A difference in rankings
is a prompt to inspect the spectra, genome quality and metadata, **not**
evidence of an error or a statistically significant outlier.

For very large datasets, the scatterplot and ranked list may use up to
the chosen maximum number of sampled pairs. The direct isolate-pair
inspector always looks up the original source matrices, not a sampled
or normalized preview.

## Question 3: Do the methods group isolates similarly?

Click **Compare clusters**. Pick the number of clusters (k) in the toolbar.
Each matrix is clustered independently using your selected linkage and then
cut into k groups. The overlap table shows exactly how memberships
correspond between methods.

The adjusted Rand index compares the two partitions independent of
their arbitrary numeric cluster labels. The adjusted Wallace coefficients
ask directional questions and need not be equal. Compare their values with
the cluster-overlap table before deciding which isolates need inspection.
Neither measure establishes biological typing accuracy.

Clustering is limited to 2,000 shared isolates in the current implementation.
For larger data, start by exploring a justified subset or import a
precomputed sample order.

## Question 4: Are known groups, replicates or batches reflected in the data?

Click **Explore my groups**. Select a group, replicate identifier or batch
column from **Group / annotation** in the results toolbar. MAniR shows
within-group and between-group pair counts, mean/median values and separate
distribution plots for each available matrix, each in its **own original units**.

A difference may indicate a feature worth investigating, but the isolate
pairs in these plots are **not independent replicates**. Biological groups
and experimental batches can also be confounded. The summaries are
descriptive and are not a formal test of batch effects or discrimination.

## Question 5: What does a particular cell mean?

Click **Explore heatmaps** and hover over the cell to see the original
value. The two combined views put one matrix above the diagonal and the
other below, reordering both by either method. Each triangle uses its
own display normalization. Equal colors in different matrices do **not**
mean equal biological similarity. On large data, zoom into a smaller
region to inspect it without sending the entire matrix to the browser.

The Difference view is available only after explicitly confirming
equivalent measurements and units. It is deliberately blocked for the
mixed-unit teaching example. If ANI is expressed in percentages and
MALDI similarity is on a 0–1 scale, use the scatterplot and rank-gap
inspection instead of subtracting the matrices.

## Record your observations

Click **Open exports** or visit **Export** when you have explored the
question. Enter your research question and write your observations and
planned follow-up. **Download research notes (.md)** creates an editable
Markdown record containing the dataset names, analysis settings, pair
association statistics, and any selected metadata group summaries.
Cluster statistics are included only if you explicitly enable their
calculation in Export. This does not automatically run the Mantel test.
The text includes cautions about pair dependence and synthetic data.

Save the original CSV/RDS matrices alongside the notes and figures.
Static and HTML plots may show normalized or downsampled previews,
while CSV and RDS exports retain original values.

For details of each statistical quantity, see
[Reading MAniR results](INTERPRETING_RESULTS.md).
