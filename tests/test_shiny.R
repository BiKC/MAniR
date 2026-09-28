# Minimal server integration test. Requires Shiny and the declared runtime packages.
source("R/matrix_io.R")
source("R/matrix_analysis.R")
source("R/matrix_plot.R")
source("server.R")

ids <- c("001", "002", "003", "004")
m <- matrix(c(1, .7, .4, .3,
              .7, 1, .8, .5,
              .4, .8, 1, .9,
              .3, .5, .9, 1), 4, byrow = TRUE,
            dimnames = list(ids, ids))
path <- tempfile(fileext = ".csv")
write_matrix_csv(m, path)
file_info <- data.frame(name = "example.csv",
                        size = as.numeric(file.info(path)$size),
                        type = "text/csv", datapath = path,
                        stringsAsFactors = FALSE)

shiny::testServer(server, {
  session$setInputs(
    first_file = file_info,
    kind1 = "similarity", kind2 = "similarity",
    cluster = FALSE, linkage = "complete",
    match_mode = "strict", visualize = 1
  )
  loaded_data <- loaded()
  stopifnot(!is.null(loaded_data), nrow(loaded_data$first) == 4L,
            identical(colnames(loaded_data$first), ids),
            is.null(loaded_data$second))
  # A palette change should affect only rendering, not the loaded matrix.
  session$setInputs(palette = "Viridis")
  stopifnot(identical(loaded()$first, loaded_data$first))
})
cat("Shiny upload and reactive smoke test passed.\n")
