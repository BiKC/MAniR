# Built-in teaching example. All values are synthetic, not measured isolates.
manir_example_files <- function(root = ".") {
  c(ANI = file.path(root, "examples", "ani.csv"),
    MALDI = file.path(root, "examples", "maldi.csv"),
    metadata = file.path(root, "examples", "metadata.csv"))
}

manir_load_example <- function(root = ".") {
  paths <- manir_example_files(root)
  missing <- paths[!file.exists(paths)]
  if (length(missing)) {
    stop("Built-in example files are missing: ",
         paste(basename(missing), collapse = ", "))
  }
  ani <- read_matrix_input(paths[["ANI"]], kind = "similarity")
  maldi <- read_matrix_input(paths[["MALDI"]], kind = "similarity")
  matched <- match_matrices(ani, maldi)
  metadata <- read_metadata_input(paths[["metadata"]])
  if (!setequal(rownames(metadata), matched$ids)) {
    stop("Built-in example metadata does not match the matrix identifiers.")
  }
  list(first = ani, second = maldi,
       metadata = align_metadata(metadata, matched$ids))
}

manir_write_example_workbook <- function(destination, root = ".") {
  example <- manir_load_example(root)
  wb <- openxlsx::createWorkbook()
  for (name in c("ANI", "MALDI")) {
    m <- if (name == "ANI") example$first else example$second
    openxlsx::addWorksheet(wb, name)
    openxlsx::writeData(wb, name,
      data.frame(sample_id = rownames(m), m, check.names = FALSE))
    openxlsx::freezePane(wb, name, firstRow = TRUE, firstCol = TRUE)
  }
  openxlsx::addWorksheet(wb, "metadata")
  openxlsx::writeData(wb, "metadata",
    data.frame(sample_id = rownames(example$metadata),
               example$metadata, check.names = FALSE))
  openxlsx::freezePane(wb, "metadata", firstRow = TRUE, firstCol = TRUE)
  openxlsx::addWorksheet(wb, "README")
  openxlsx::writeData(wb, "README", data.frame(
    Notes = c(
      "MAniR built-in example. All data are SYNTHETIC and intended for demonstration only.",
      "ANI: fictional percentage-like genomic similarity, diagonal = 100.",
      "MALDI: fictional spectral similarity on a 0-to-1 scale, diagonal = 1.",
      "The two matrices use different scales. Do not enable comparable-units metrics.",
      "Metadata includes three fictional groups, specimen source and measurement batch.",
      "Two pairs deliberately have discordant MALDI/ANI relationships for exploration.",
      "Select ANI as matrix 1, MALDI as matrix 2, and metadata as the annotation sheet."
    ), check.names = FALSE))
  openxlsx::setColWidths(wb, "README", cols = 1, widths = 100)
  openxlsx::saveWorkbook(wb, destination, overwrite = TRUE)
  invisible(destination)
}
