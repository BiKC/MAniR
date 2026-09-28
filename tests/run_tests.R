# Run with: Rscript tests/run_tests.R
source("R/matrix_io.R")
source("R/matrix_analysis.R")
source("R/matrix_plot.R")

expect_error <- function(expr, pattern = NULL) {
  e <- tryCatch({force(expr); NULL}, error = function(e) conditionMessage(e))
  if (is.null(e)) stop("Expected an error, but none was raised.", call. = FALSE)
  if (!is.null(pattern) && !grepl(pattern, e, ignore.case = TRUE))
    stop("Expected error matching: ", pattern, "; got: ", e, call. = FALSE)
  invisible(e)
}
test <- function(label, expression) {
  force(expression)
  cat("PASS:", label, "\n")
}

ids <- c("Isolate B", "Isolate A", "Isolate C", "Isolate D")
m <- matrix(c(
  100, 80, 40, 30,
   80,100, 45, 35,
   40, 45,100, 95,
   30, 35, 95,100
), 4, byrow = TRUE, dimnames = list(ids, ids))
test("matrix import reorders named rows to named columns", {
  scrambled <- m[c(3, 1, 4, 2), , drop = FALSE]
  imported <- validate_matrix(scrambled, "similarity")
  stopifnot(identical(dimnames(imported), dimnames(m)),
            isTRUE(all.equal(unname(imported), unname(m),
                             check.attributes = FALSE)))
})
test("numeric data frames take the exact-value matrix import path", {
  data <- as.data.frame(m, check.names = FALSE)
  result <- validate_matrix(data, "similarity")
  stopifnot(isTRUE(all.equal(unname(result), unname(m),
                            check.attributes = FALSE)))
})
test("matrix identifiers are trimmed without changing values", {
  dirty <- m
  rownames(dirty) <- paste0(" ", rownames(dirty), " ")
  colnames(dirty) <- paste0(" ", colnames(dirty), " ")
  cleaned <- validate_matrix(dirty)
  stopifnot(identical(rownames(cleaned), ids),
            identical(colnames(cleaned), ids))
})
test("duplicate, inconsistent, nonnumeric, negative and asymmetric data fail", {
  x <- m; rownames(x)[2] <- rownames(x)[1]
  expect_error(validate_matrix(x), "Duplicate")
  x <- m; colnames(x)[2] <- "Wrong ID"
  expect_error(validate_matrix(x), "differ")
  x <- as.data.frame(m); x[1, 3] <- "not a number"
  expect_error(validate_matrix(x), "nonnumeric")
  x <- m; x[1, 2] <- 70
  expect_error(validate_matrix(x), "symmetric")
  x <- m; x[1, 2] <- -1; x[2, 1] <- -1
  expect_error(validate_matrix(x, "similarity"), "negative")
  x <- m; x[1, 2] <- 2; x[2, 1] <- 2
  expect_error(validate_matrix(x, "correlation"), "between -1 and 1")
})
test("missing values must occur in mirrored pairs", {
  x <- m; x[1, 2] <- NA_real_
  expect_error(validate_matrix(x), "mirrored")
  x[2, 1] <- NA_real_
  stopifnot(is.na(validate_matrix(x)[1, 2]))
})
test("matched matrices preserve exact values", {
  reversed <- m[rev(ids), rev(ids), drop = FALSE] / 100
  mm <- match_matrices(m, reversed)
  stopifnot(identical(mm$ids, ids),
            isTRUE(all.equal(mm$second, m / 100,
                             check.attributes = FALSE)))
})
test("intersection reports excluded samples", {
  x <- m[1:3, 1:3]
  y <- m[2:4, 2:4]
  res <- match_matrices(x, y, "intersection")
  stopifnot(length(res$ids) == 2L,
            identical(res$only_first, ids[1]),
            identical(res$only_second, ids[4]))
  expect_error(match_matrices(x, y, "strict"), "differ")
})
test("pair indices cover the strictly upper triangle exactly once", {
  p <- upper_pair_indices(4, max_pairs = 100)
  stopifnot(identical(p$i, c(1L, 1L, 2L, 1L, 2L, 3L)),
            identical(p$j, c(2L, 3L, 3L, 4L, 4L, 4L)),
            p$total == 6, !p$sampled)
})
test("sampling is reproducible and does not alter the session RNG", {
  set.seed(1234)
  before <- .Random.seed
  p1 <- upper_pair_indices(500, 1000, seed = 41)
  stopifnot(identical(before, .Random.seed))
  p2 <- upper_pair_indices(500, 1000, seed = 41)
  stopifnot(identical(p1, p2), p1$sampled,
            all(p1$i < p1$j), !anyDuplicated(paste(p1$i, p1$j)))
})
test("matrix pair comparison excludes mirrored and diagonal pairs", {
  pp <- paired_values(m, m * 2)
  stopifnot(nrow(pp$data) == 6L, !pp$sampled)
  stats <- matrix_agreement(pp)
  stopifnot(abs(stats$pearson - 1) < 1e-12,
            abs(stats$spearman - 1) < 1e-12)
})
test("scale handling preserves source matrices", {
  before <- m
  combined <- ma_combined(m, m / 100)
  stopifnot(identical(m, before),
            identical(combined$original[1, 2], m[1, 2]),
            identical(combined$original[2, 1], m[2, 1] / 100),
            all(combined$values >= 0 & combined$values <= 1))
  constant <- matrix(4, 2, 2)
  stopifnot(all(ma_scale(constant) == 0.5))
  centered <- matrix(c(-3, -1, 0, 3), nrow = 2)
  stopifnot(isTRUE(all.equal(as.vector(ma_scale(centered,
             center_zero = TRUE)), c(0, 1/3, 0.5, 1))))
  stopifnot(all(is.finite(ma_scale(matrix(c(0, 1, 2, 3), 2), TRUE))))
  expect_error(ma_scale(matrix(c(-1, 0, 1, 2), 2), TRUE), "nonnegative")
})
test("cluster ordering and adjusted Rand index are reproducible", {
  c1 <- matrix_order(m)
  c2 <- matrix_order(m)
  stopifnot(identical(c1$ids, c2$ids),
            identical(sort(c1$ids), sort(ids)),
            adjusted_rand(c(1, 1, 2, 2), c(2, 2, 1, 1)) == 1)
  stopifnot(abs(adjusted_wallace(c(1, 1, 2, 2),
                                 c(2, 2, 1, 1)) - 1) < 1e-12)
  result <- cluster_concordance(m, m, k = 2)
  stopifnot(abs(result$ari - 1) < 1e-12,
            abs(result$adjusted_wallace_1_to_2 - 1) < 1e-12,
            abs(result$adjusted_wallace_2_to_1 - 1) < 1e-12,
            nrow(result$assignments) == 4L)
  skip <- matrix_order(m, max_cluster = 2L)
  stopifnot(isTRUE(skip$skipped), is.null(skip$tree))
})
test("Mantel permutation uses label permutations and restores RNG", {
  set.seed(48)
  before <- .Random.seed
  res <- matrix_mantel(m, m, permutations = 99, seed = 44)
  stopifnot(abs(res$observed - 1) < 1e-12,
            res$p_two_sided > 0 && res$p_two_sided <= 1,
            identical(before, .Random.seed))
})
test("metadata alignment and replicate summaries work", {
  meta <- data.frame(group = c("G1", "G1", "G2", "G2"),
                     row.names = rev(ids))
  aligned <- align_metadata(meta, ids)
  stopifnot(identical(rownames(aligned), ids))
  summary <- replicate_summary(m, meta, "group")
  stopifnot(nrow(summary) == 2L, sum(summary$pairs) == 6L)
})
test("CSV export and reimport retain original values", {
  path <- tempfile(fileext = ".csv")
  write_matrix_csv(m, path)
  imported <- read_matrix_input(path)
  stopifnot(isTRUE(all.equal(m, imported, check.attributes = FALSE)))
})
test("compressed TSV input keeps names and numeric precision", {
  path <- tempfile(fileext = ".tsv.gz")
  con <- gzfile(path, "wt")
  utils::write.table(data.frame(sample_id = ids, m, check.names = FALSE),
                     file = con, sep = "\t", row.names = FALSE, quote = TRUE)
  close(con)
  imported <- read_matrix_input(path)
  stopifnot(isTRUE(all.equal(m, imported, check.attributes = FALSE)))
})
test("RDS input supports numeric matrices", {
  path <- tempfile(fileext = ".rds")
  saveRDS(m, path)
  imported <- read_matrix_input(path)
  stopifnot(isTRUE(all.equal(m, imported, check.attributes = FALSE)))
})
test("large matrix preview is bounded and retains exact sampled values", {
  big <- matrix(1, 900, 900)
  diag(big) <- 100
  dimnames(big) <- list(paste0("I", seq_len(900)),
                        paste0("I", seq_len(900)))
  preview <- ma_preview(big, max_side = 128)
  stopifnot(identical(dim(preview$matrix), c(128L, 128L)),
            identical(preview$matrix,
                      big[preview$indices, preview$indices, drop = FALSE]))
  png_file <- tempfile(fileext = ".png")
  ma_save_png(big, png_file, max_side = 128)
  stopifnot(file.exists(png_file), file.info(png_file)$size > 0)
  pdf_file <- tempfile(fileext = ".pdf")
  ma_save_pdf(big[seq_len(20), seq_len(20)], pdf_file)
  stopifnot(file.exists(pdf_file), file.info(pdf_file)$size > 0)
})


test("bundled XLSX example imports and preserves matrix pairing", {
  file <- "www/moc_data.xlsx"
  stopifnot(file.exists(file))
  sheets <- openxlsx::getSheetNames(file)
  stopifnot(length(sheets) >= 2L)
  wb <- openxlsx::loadWorkbook(file)
  a <- read_matrix_input(file, sheet = sheets[1L], workbook = wb)
  b <- read_matrix_input(file, sheet = sheets[2L], workbook = wb)
  matched <- match_matrices(a, b)
  mixed <- ma_combined(matched$first, matched$second)
  stopifnot(nrow(a) > 1L, nrow(b) > 1L,
            identical(rownames(mixed$original), matched$ids),
            isTRUE(all.equal(mixed$original[upper.tri(mixed$original)],
                             matched$first[upper.tri(matched$first)])),
            isTRUE(all.equal(mixed$original[lower.tri(mixed$original)],
                             matched$second[lower.tri(matched$second)])))
})


test("CSV import preserves leading zeroes in sample IDs", {
  id <- c("001", "002", "003")
  x <- matrix(c(1, .8, .5, .8, 1, .6, .5, .6, 1),
              nrow = 3L, dimnames = list(id, id))
  file <- tempfile(fileext = ".csv")
  write_matrix_csv(x, file)
  y <- read_matrix_input(file)
  stopifnot(identical(rownames(y), id), identical(colnames(y), id))
})

cat("All MAniR regression tests passed.\n")
