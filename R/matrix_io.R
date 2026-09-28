# Input and validation shared by the Shiny application and command-line tools.

matrix_sheet_names <- function(path, format = path) {
  ext <- tolower(tools::file_ext(format))
  if (ext %in% c("xlsx", "xlsm")) return(openxlsx::getSheetNames(path))
  character()
}

read_matrix_input <- function(path, sheet = NULL, workbook = NULL, format = path) {
  ext <- tolower(tools::file_ext(format))
  if (ext %in% c("xlsx", "xlsm")) {
    if (is.null(sheet) || identical(sheet, "")) stop("Select a matrix worksheet.")
    if (is.null(workbook)) workbook <- openxlsx::loadWorkbook(path)
    x <- openxlsx::readWorkbook(workbook, sheet = sheet, colNames = TRUE,
                                rowNames = TRUE)
  } else if (ext == "rds") {
    x <- readRDS(path)
  } else if (ext %in% c("csv", "tsv", "txt", "gz")) {
    sep <- if (ext == "tsv" || grepl("\\.tsv\\.gz$", format, ignore.case = TRUE)) "\t" else ","
    if (ext != "gz" && requireNamespace("data.table", quietly = TRUE)) {
      x <- data.table::fread(path, sep = sep, data.table = FALSE,
                             check.names = FALSE, showProgress = FALSE)
      if (ncol(x) < 2L) stop("The file needs an ID column and matrix columns.")
      ids <- x[[1L]]
      x <- x[-1L]
      rownames(x) <- as.character(ids)
    } else {
      x <- utils::read.table(path, sep = sep, header = TRUE, row.names = 1L,
                             check.names = FALSE, comment.char = "",
                             stringsAsFactors = FALSE)
    }
  } else {
    stop("Unsupported matrix format. Use XLSX, CSV, TSV, CSV.GZ, TSV.GZ or RDS.")
  }
  validate_matrix(x)
}

validate_matrix <- function(x, kind = c("auto", "similarity", "correlation", "distance"),
                            symmetry_tolerance = 1e-8, allow_missing = TRUE) {
  kind <- match.arg(kind)
  if (!is.matrix(x) && !is.data.frame(x)) stop("Input must be a matrix or table.")
  if (nrow(x) != ncol(x) || nrow(x) < 2L)
    stop("The matrix must be square and contain at least two isolates.")
  row_ids <- rownames(x)
  col_ids <- colnames(x)
  if (is.null(row_ids) || is.null(col_ids))
    stop("Both row and column sample IDs are required.")
  row_ids <- trimws(row_ids)
  col_ids <- trimws(col_ids)
  if (anyNA(row_ids) || anyNA(col_ids) || any(!nzchar(row_ids)) ||
      any(!nzchar(col_ids))) stop("Sample IDs must not be empty.")
  if (anyDuplicated(row_ids) || anyDuplicated(col_ids))
    stop("Duplicate sample IDs were found after trimming whitespace.")
  if (!setequal(row_ids, col_ids)) {
    only_rows <- setdiff(row_ids, col_ids)
    only_cols <- setdiff(col_ids, row_ids)
    stop(sprintf("Row and column IDs differ. Only in rows: %s. Only in columns: %s.",
                 paste(head(only_rows, 10L), collapse = ", "),
                 paste(head(only_cols, 10L), collapse = ", ")))
  }
  original <- as.matrix(x)
  converted <- suppressWarnings(as.numeric(original))
  empty <- is.na(original) | trimws(as.character(original)) == ""
  if (any(is.na(converted) & !empty))
    stop("The matrix has nonnumeric values. Check the data worksheets.")
  m <- matrix(converted, nrow = nrow(x), ncol = ncol(x),
              dimnames = list(row_ids, col_ids))
  m <- m[col_ids, col_ids, drop = FALSE]
  if (any(is.infinite(m))) stop("Infinite matrix values are not supported.")
  if (!allow_missing && anyNA(m)) stop("Missing matrix values are not allowed.")
  if (anyNA(diag(m))) stop("Every diagonal entry must be present.")
  if (!identical(is.na(m), is.na(t(m))))
    stop("Missing values must occur in mirrored pairs for a symmetric matrix.")
  numeric_diff <- abs(m - t(m))
  if (any(numeric_diff > symmetry_tolerance, na.rm = TRUE))
    stop("The matrix must be symmetric; mirrored cells disagree.")
  if (kind == "correlation" && any(m < -1 - symmetry_tolerance |
                                     m > 1 + symmetry_tolerance, na.rm = TRUE))
    stop("Correlation values must lie between -1 and 1.")
  if (kind %in% c("similarity", "distance") &&
      any(m < -symmetry_tolerance, na.rm = TRUE))
    stop("Similarity and distance matrices cannot contain negative values.")
  attr(m, "matrix_kind") <- kind
  m
}

read_metadata_input <- function(path, sheet = NULL, workbook = NULL, format = path) {
  ext <- tolower(tools::file_ext(format))
  if (ext %in% c("xlsx", "xlsm")) {
    if (is.null(sheet) || sheet == "None") return(NULL)
    if (is.null(workbook)) workbook <- openxlsx::loadWorkbook(path)
    x <- openxlsx::readWorkbook(workbook, sheet = sheet,
                                colNames = TRUE, rowNames = TRUE)
  } else if (ext == "rds") {
    x <- readRDS(path)
  } else {
    sep <- if (ext == "tsv" || grepl("\\.tsv\\.gz$", format, ignore.case = TRUE)) "\t" else ","
    x <- utils::read.table(path, sep = sep, header = TRUE, row.names = 1L,
                           check.names = FALSE, stringsAsFactors = FALSE)
  }
  if (!is.data.frame(x)) x <- as.data.frame(x, stringsAsFactors = FALSE)
  ids <- trimws(rownames(x))
  if (any(!nzchar(ids)) || anyDuplicated(ids))
    stop("Metadata must have unique, nonempty sample IDs.")
  rownames(x) <- ids
  x
}

align_metadata <- function(metadata, ids) {
  if (is.null(metadata)) return(NULL)
  if (!all(ids %in% rownames(metadata)))
    warning(sprintf("%d isolates have no metadata; their annotations are missing.",
                    sum(!ids %in% rownames(metadata))))
  metadata[match(ids, rownames(metadata)), , drop = FALSE]
}

match_matrices <- function(first, second, mode = c("strict", "intersection")) {
  mode <- match.arg(mode)
  ids1 <- colnames(first)
  ids2 <- colnames(second)
  if (mode == "strict" && !setequal(ids1, ids2))
    stop(sprintf("The matrices differ: %d IDs only in matrix 1 and %d only in matrix 2. Select 'Shared isolates' to compare the intersection.",
                 length(setdiff(ids1, ids2)), length(setdiff(ids2, ids1))))
  ids <- if (mode == "strict") ids1 else intersect(ids1, ids2)
  if (length(ids) < 2L) stop("At least two shared sample IDs are required.")
  list(first = first[ids, ids, drop = FALSE],
       second = second[ids, ids, drop = FALSE],
       ids = ids,
       only_first = setdiff(ids1, ids2),
       only_second = setdiff(ids2, ids1))
}

write_matrix_csv <- function(m, file) {
  utils::write.csv(data.frame(sample_id = rownames(m), m, check.names = FALSE),
                   file = file, row.names = FALSE, na = "")
}
