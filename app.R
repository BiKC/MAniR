# MAniR Shiny entry point. Dependencies are documented in README and renv.
required <- c("shiny", "plotly", "openxlsx", "RColorBrewer")
missing <- setdiff(required, rownames(installed.packages()))
if (length(missing))
  stop("Install required packages before launching: ",
       paste(missing, collapse = ", "),
       ". See README.md for installation instructions.")
source("R/matrix_io.R", local = TRUE)
source("R/matrix_analysis.R", local = TRUE)
source("R/matrix_plot.R", local = TRUE)
source("ui.R", local = TRUE)
source("server.R", local = TRUE)
shiny::shinyApp(ui = ui, server = server)
