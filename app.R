#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# For local usage
# packages <- c(
#   "shiny", "shinyjs", "shinyBS", "shinyWidgets", "shinycssloaders",
#   "openxlsx", "RColorBrewer", "corrplot", "periscope", "heatmaply"
# )
# new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
# if (length(new_packages)) install.packages(new.packages)
# lapply(packages, require, character.only = TRUE)


# For shinyapps.io
library("shiny")
library("shinyjs")
library("shinyBS")
library("shinyWidgets")
library("openxlsx")
library("RColorBrewer")
library("corrplot")
library("periscope")
library("heatmaply")
library("plotly")

# import UI
source("ui.R")
# import server
source("server.R")

# Run the application
shinyApp(ui = ui, server = server)
