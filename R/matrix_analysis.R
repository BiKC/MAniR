# Reusable matrix calculations; no Shiny dependencies.

matrix_order <- function(m, kind = c("auto", "similarity", "correlation", "distance"),
                         cluster = TRUE, max_cluster = 2000L, linkage = "complete") {
  kind <- match.arg(kind)
  ids <- colnames(m)
  if (!cluster || nrow(m) > max_cluster)
    return(list(ids = ids, tree = NULL, skipped = nrow(m) > max_cluster))
  x <- m
  if (kind == "distance") {
    if (any(x < 0, na.rm = TRUE)) stop("Negative dissimilarities are invalid.")
  } else if (kind == "correlation") {
    x <- 1 - x
  } else {
    # Includes auto: larger entries mean more similar, regardless of scale.
    x <- max(x, na.rm = TRUE) - x
  }
  if (anyNA(x)) {
    replacement <- max(x, na.rm = TRUE)
    x[is.na(x)] <- replacement
  }
  diag(x) <- 0
  d <- stats::as.dist(x)
  tree <- if (requireNamespace("fastcluster", quietly = TRUE))
    fastcluster::hclust(d, method = linkage)
  else stats::hclust(d, method = linkage)
  list(ids = ids[tree$order], tree = tree, skipped = FALSE)
}

# Return indices of the strictly upper triangle without creating an n*n mask
# for sampled large matrices. Each pair is returned exactly once.
upper_pair_indices <- function(n, max_pairs = 100000L, seed = 1L) {
  n <- as.integer(n)
  total <- as.double(n) * (n - 1) / 2
  if (n < 2L) stop("At least two isolates are required.")
  max_pairs <- max(1L, as.integer(max_pairs))
  if (total <= max_pairs) {
    j <- rep.int(seq.int(2L, n), seq.int(1L, n - 1L))
    i <- sequence(seq.int(1L, n - 1L))
    return(list(i = i, j = j, total = total, sampled = FALSE))
  }
  # Local RNG restoration prevents an analysis from modifying the session RNG.
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit({
    if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
      rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(seed)
  k <- sort(sample.int(total, max_pairs))
  j <- ceiling((1 + sqrt(1 + 8 * k)) / 2)
  i <- k - (j - 1) * (j - 2) / 2
  list(i = as.integer(i), j = as.integer(j), total = total, sampled = TRUE)
}

paired_values <- function(a, b, max_pairs = 100000L, seed = 1L) {
  if (!identical(dim(a), dim(b)) ||
      !identical(rownames(a), rownames(b)) ||
      !identical(colnames(a), colnames(b)))
    stop("The matrices must have the same sample order.")
  ix <- upper_pair_indices(nrow(a), max_pairs = max_pairs, seed = seed)
  x <- a[cbind(ix$i, ix$j)]
  y <- b[cbind(ix$i, ix$j)]
  good <- is.finite(x) & is.finite(y)
  list(data = data.frame(sample_1 = rownames(a)[ix$i[good]],
                         sample_2 = colnames(a)[ix$j[good]],
                         first = x[good], second = y[good],
                         difference = x[good] - y[good]),
       total_pairs = ix$total, evaluated_pairs = length(x),
       valid_pairs = sum(good), sampled = ix$sampled, seed = seed)
}

matrix_agreement <- function(pairs) {
  d <- pairs$data
  if (nrow(d) < 3L) return(list(pearson = NA_real_, spearman = NA_real_,
                               mae = NA_real_, rmse = NA_real_, n = nrow(d)))
  list(
    pearson = if (stats::sd(d$first) > 0 && stats::sd(d$second) > 0)
      stats::cor(d$first, d$second) else NA_real_,
    spearman = if (length(unique(d$first)) > 1L && length(unique(d$second)) > 1L)
      suppressWarnings(stats::cor(d$first, d$second, method = "spearman"))
      else NA_real_,
    # Differences are meaningful only for matrices on comparable scales.
    mae = mean(abs(d$difference)), rmse = sqrt(mean(d$difference^2)),
    n = nrow(d)
  )
}

# A Mantel-style label permutation; not an ordinary p-value from treating
# all pairwise edges as independent. Intended for moderate complete matrices.
matrix_mantel <- function(a, b, permutations = 999L, seed = 1L,
                          method = c("pearson", "spearman"), max_n = 400L) {
  method <- match.arg(method)
  if (nrow(a) != nrow(b) ||
      !identical(rownames(a), rownames(b)) ||
      !identical(colnames(a), colnames(b)))
    stop("Match matrices before calculating the Mantel test.")
  n <- nrow(a)
  if (n > max_n) stop("Mantel permutation exceeds the configured isolate limit.")
  if (anyNA(a) || anyNA(b)) stop("Mantel permutation needs complete matrices.")
  idx <- upper.tri(a)
  x <- a[idx]
  y <- b[idx]
  if (stats::sd(x) == 0 || stats::sd(y) == 0)
    stop("A constant matrix cannot be tested.")
  obs <- suppressWarnings(stats::cor(x, y, method = method))
  permutations <- as.integer(permutations)
  if (permutations < 1L) stop("Request at least one permutation.")
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit({
    if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
      rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(seed)
  null <- replicate(permutations, {
    perm <- sample.int(n)
    suppressWarnings(stats::cor(x, b[perm, perm, drop = FALSE][idx],
                                method = method))
  })
  list(observed = obs, p_two_sided = (1 + sum(abs(null) >= abs(obs))) /
         (permutations + 1), permutations = permutations, seed = seed,
       method = method)
}

adjusted_rand <- function(labels_a, labels_b) {
  if (length(labels_a) != length(labels_b) || length(labels_a) < 2L)
    stop("Both cluster assignments must include the same isolates.")
  tab <- table(labels_a, labels_b)
  choose2 <- function(z) sum(z * (z - 1) / 2)
  nij <- choose2(tab)
  ai <- choose2(rowSums(tab))
  bj <- choose2(colSums(tab))
  all_pairs <- choose(length(labels_a), 2)
  expected <- ai * bj / all_pairs
  maximum <- (ai + bj) / 2
  if (maximum == expected) return(if (nij == maximum) 1 else NA_real_)
  (nij - expected) / (maximum - expected)
}


adjusted_wallace <- function(labels_a, labels_b) {
  if (length(labels_a) != length(labels_b) || length(labels_a) < 2L)
    stop("Cluster assignments must refer to the same isolates.")
  tab <- table(labels_a, labels_b)
  pairs <- function(x) sum(x * (x - 1) / 2)
  same_in_both <- pairs(tab)
  same_in_first <- pairs(rowSums(tab))
  expected_b <- pairs(colSums(tab)) / choose(length(labels_a), 2)
  if (same_in_first == 0 || expected_b >= 1) return(NA_real_)
  (same_in_both / same_in_first - expected_b) / (1 - expected_b)
}

cluster_concordance <- function(a, b, kind_a = "similarity",
                                kind_b = "similarity", k = 3L,
                                max_n = 2000L, linkage = "complete") {
  if (!identical(rownames(a), rownames(b))) stop("Align matrices first.")
  if (k < 2L || k >= nrow(a)) stop("Choose between 2 and n-1 clusters.")
  ca <- matrix_order(a, kind = kind_a, max_cluster = max_n,
                     linkage = linkage)
  cb <- matrix_order(b, kind = kind_b, max_cluster = max_n,
                     linkage = linkage)
  if (is.null(ca$tree) || is.null(cb$tree))
    stop("Cluster concordance exceeds the configured clustering limit.")
  first <- stats::cutree(ca$tree, k = k)
  second <- stats::cutree(cb$tree, k = k)
  list(ari = adjusted_rand(first, second),
       adjusted_wallace_1_to_2 = adjusted_wallace(first, second),
       adjusted_wallace_2_to_1 = adjusted_wallace(second, first),
       assignments = data.frame(sample_id = rownames(a),
                                first = unname(first[rownames(a)]),
                                second = unname(second[rownames(a)])),
       contingency = table(first, second))
}

replicate_summary <- function(m, metadata, group_column, max_pairs = 100000L) {
  if (is.null(metadata) || !group_column %in% names(metadata))
    stop("Select a valid metadata column identifying replicates or groups.")
  ids <- rownames(m)
  meta <- align_metadata(metadata, ids)
  groups <- as.character(meta[[group_column]])
  pp <- upper_pair_indices(nrow(m), max_pairs = max_pairs)
  valid <- is.finite(m[cbind(pp$i, pp$j)]) &
    !is.na(groups[pp$i]) & !is.na(groups[pp$j])
  values <- m[cbind(pp$i[valid], pp$j[valid])]
  within <- groups[pp$i[valid]] == groups[pp$j[valid]]
  data.frame(type = c("Within group", "Between groups"),
             pairs = c(sum(within), sum(!within)),
             mean = c(if (any(within)) mean(values[within]) else NA_real_,
                      if (any(!within)) mean(values[!within]) else NA_real_),
             median = c(if (any(within)) stats::median(values[within]) else NA_real_,
                        if (any(!within)) stats::median(values[!within]) else NA_real_),
             sampled = pp$sampled)
}

# Exploratory comparison of the order of isolate pairs when measurements have
# different units. A gap is the difference between empirical percentile ranks,
# after making smaller distances mean greater similarity. It is not a p-value
# or a biological cutoff, and it is based on the sampled pairs when sampling.
pair_rank_gaps <- function(pairs, kind1 = "similarity",
                           kind2 = "similarity", top = 10L) {
  d <- pairs$data
  if (nrow(d) < 3L) return(data.frame())
  if (!kind1 %in% c("auto", "similarity", "correlation", "distance") ||
      !kind2 %in% c("auto", "similarity", "correlation", "distance"))
    stop("Unrecognized matrix type.")
  score1 <- if (kind1 == "distance") -d$first else d$first
  score2 <- if (kind2 == "distance") -d$second else d$second
  n <- nrow(d)
  d$rank_gap <- abs((rank(score1, ties.method = "average") -
                     rank(score2, ties.method = "average")) / n)
  d <- d[order(d$rank_gap, decreasing = TRUE), , drop = FALSE]
  rownames(d) <- NULL
  utils::head(d[c("sample_1", "sample_2", "first", "second", "rank_gap")],
              as.integer(top))
}
