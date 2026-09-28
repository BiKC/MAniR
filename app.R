# MAniR Shiny entry point. Dependencies are documented in README and renv.
required <- c("shiny", "plotly", "htmltools", "htmlwidgets",
              "openxlsx", "RColorBrewer")
missing <- setdiff(required, rownames(installed.packages()))
if (length(missing))
  stop("Install required packages before launching: ",
       paste(missing, collapse = ", "),
       ". See README.md for installation instructions.")
max_upload_mb <- as.numeric(Sys.getenv("MANIR_MAX_UPLOAD_MB", "1024"))
if (!is.finite(max_upload_mb) || max_upload_mb < 1)
  stop("MANIR_MAX_UPLOAD_MB must be a positive number.")
options(shiny.maxRequestSize = max_upload_mb * 1024^2)
source("R/matrix_io.R", local = TRUE)
source("R/matrix_analysis.R", local = TRUE)
source("R/matrix_plot.R", local = TRUE)
source("R/example_data.R", local = TRUE)
source("ui.R", local = TRUE)
source("server.R", local = TRUE)
shiny::shinyApp(ui = ui, server = server)
