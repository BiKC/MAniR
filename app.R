#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# For local usage
#packages <- c("shiny", "shinyjs", "shinyBS", "shinyWidgets", "shinycssloaders", "openxlsx", "RColorBrewer", "corrplot")
#new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
#if(length(new.packages)) install.packages(new.packages)
#lapply(packages, require, character.only = TRUE)

# For shinyapps.io
library("shiny")
library("shinyjs")
library("shinyBS")
library("shinyWidgets")
library("shinycssloaders")
library("openxlsx")
library("RColorBrewer")
library("corrplot")

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    useShinyjs(),
    tags$head(tags$style(HTML("body {
                                  background-color:#FCF6F5FF;
                                  color:#2F3C7E;
                                  }
                              .well {
                                  background-color:#2F3C7E;
                                  color:#FCF6F5FF;
                              }
                              .sw-dropdown-in {
                                  color: #2F3C7E;
                              }"))),
    titlePanel("MALDI ANI analysis"),
    
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        
        sidebarPanel(
            fileInput("input_excel", "Excel containing the ANI MALDI"),
            uiOutput("tab1"),
            uiOutput("tab2"),
            actionButton('visualize', 'Visualize the dataset'),
            br(),
            br(),
            br(),
            dropdown(
                list(
                    sliderInput("dec", "Decimal places", 0, 5, 3, 1),
                    sliderInput("ncex", "Size of numbers in the correlation plot", 0.1, 2, 0.8, 0.1),
                    sliderInput("clcex", "Size of colorscale numbers", 0.1, 2, 0.8, 0.1),
                    sliderInput("scale", "Scale of the plot (default 1=1000x1000)", 0.1, 5, 1, 0.1),
                    sliderInput("labelcex", "Size of the ANI/MALDI labels", 1, 5, 3, 0.5)
                ),
                selectInput("cmode", "Color mode", c("sequential", "diverging"), selected = "diverging"),
                conditionalPanel(condition = "input.cmode=='sequential'",
                                 
                                     
                                     selectInput(
                                         inputId = "sequentialcolor",
                                         label = "Colorscale",
                                         rev(c(
                                             "Oranges",
                                             "Purples",
                                             "Reds",
                                             "Blues",
                                             "Greens",
                                             "Greys",
                                             "OrRd",
                                             "YlOrRd",
                                             "YlOrBr",
                                             "YlGn"
                                         )),
                                         selected = "YlOrRd"
                                     ),
                                     bsTooltip("sequentialcolor",title = '<img src="Sequential.png">', placement = "top")
                                 ),
                conditionalPanel(
                    condition = "input.cmode=='diverging'",
                    selectInput(
                        inputId = "divergentcolor",
                        label = "Colorscale",
                        rev(c("RdBu", "BrBG", "PiYG", "PRGn", "PuOr", "RdYlBu")),
                        selected = "RdBu"
                    ),
                    bsTooltip("divergentcolor",title = '<img src="Diverging.png">', placement = "top")
                    
                ),
                
                label = "Advanced settings"
            ),
            br(),
            br(),
            br(),
            br(),
            tags$div(tags$strong("Source code:")),
            tags$a(img(src = "bikc_github.png", width = "100px"), href =
                       "https://github.com/BiKC")
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(tabsetPanel(
            tabPanel(title = "ANI-plot",
                     withSpinner(
                         imageOutput("ANI", width = "100%", height = "85vh")
                     )),
            tabPanel(title = "MALDI-plot",
                     withSpinner(
                         imageOutput("MALDI", width = "100%", height = "85vh")
                     )),
            tabPanel(title = "ANI-MALDI-plot",
                     withSpinner(
                         imageOutput("ANIMALDI", width = "100%", height = "85vh")
                     )),
            tabPanel(title = "MALDI-ANI-plot",
                     withSpinner(
                         imageOutput("MALDIANI", width = "100%", height = "85vh")
                     ))
        ))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    runjs(
        "$('.load-container').each(function(){$(this).addClass('shiny-spinner-hidden')})"
    )
    
    getcolorScale <- function() {
        if (input$cmode == "sequential") {
            return(COL1(input$sequentialcolor, 200))
        }
        else {
            return(COL2(input$divergentcolor, 200))
        }
    }
    
    
    wb <- reactive({
        if (is.null(input$input_excel)) {
            return(NULL)
        }
        loadWorkbook(input$input_excel$datapath)
    })
    
    
    
    
    output$tab1 <- renderUI({
        if (is.null(input$input_excel) == F) {
            sheets <- sheets(wb())
            selectInput(
                "sheet1",
                "Worksheet with ANI values",
                choices = c(sheets,"None"),
                selected = sheets[1]
            )
        }
        
    })
    
    output$tab2 <- renderUI({
        if (is.null(input$input_excel) == F) {
            sheets <- sheets(wb())
            selectInput(
                "sheet2",
                "Worksheet with MALDI values",
                choices = c(sheets,"None"),
                selected = sheets[min(2,length(sheets))]
            )
        }
        
    })
    
    observeEvent(input$visualize, {
        
        if (is.null(wb())) {
            return()
        }
        runjs(
            "$('.load-container').each(function(){$(this).removeClass('shiny-spinner-hidden')})"
        )
        
        disable(id = "visualize")
        workbook <- wb()
        if (input$sheet1!="None"){
        ws <-
            readWorkbook(workbook,
                         colNames = T,
                         rowNames = T,
                         input$sheet1)
        rownames(ws) <- trimws(rownames(ws))
        colnames(ws) <- trimws(colnames(ws))
        
        # adjust format with correct rownames and make matrix
        ANI <- as.matrix(ws)
        ANI_PLOT <- corrplot.mixed(
            ANI,
            lower = 'color',
            upper = 'number',
            is.corr = FALSE,
            order = "hclust",
            diag = "n",
            tl.col = "black",
            number.cex = input$ncex,
            tl.pos = "lt"
        )
        
        output$ANI <- renderImage({
            outfile <- tempfile(fileext = '.png')
            png(
                outfile,
                width = 1000 * input$scale,
                height = 1000 * input$scale,
                res = 72
            )
            
            par(
                mar = c(input$labelcex + 1, 4.1, 4.1, 2.1),
                oma = c(3 * input$labelcex, 0, 0, 3 * input$labelcex),
                xpd = T
            )
            # ani plot
            corrplot.mixed(
                ANI,
                lower = 'color',
                lower.col = getcolorScale(),
                upper = 'number',
                upper.col = getcolorScale(),
                is.corr = FALSE,
                order = "hclust",
                diag = "n",
                tl.col = "black",
                number.cex = input$ncex,
                number.digits = input$dec,
                tl.pos = "lt",
                cl.cex = input$clcex
            )
            dev.off()
            # Return a list containing the filename
            list(src = outfile,
                 contentType = 'image/png',
                 alt = "ANI plot")
        }, deleteFile = T)
        }
        if (input$sheet2!="None"){
        ws <-
            readWorkbook(
                workbook,
                colNames = T,
                rowNames = T,
                sheet = input$sheet2
            )
        rownames(ws) <- trimws(rownames(ws))
        colnames(ws) <- trimws(colnames(ws))
        # adjust format with correct rownames and make matrix
        MALDI <- as.matrix(ws)
        MALDI_PLOT <- corrplot.mixed(
            MALDI,
            lower = 'color',
            upper = 'number',
            is.corr = FALSE,
            order = "hclust",
            diag = "n",
            tl.col = "black",
            number.cex = input$ncex,
            tl.pos = "lt"
        )
        
        output$MALDI <- renderImage({
            outfile <- tempfile(fileext = '.png')
            png(
                outfile,
                width = 1000 * input$scale,
                height = 1000 * input$scale,
                res = 72
            )
            
            par(
                mar = c(input$labelcex + 1, 4.1, 4.1, 2.1),
                oma = c(3 * input$labelcex, 0, 0, 3 * input$labelcex),
                xpd = T
            )
            # MALDI plot
            corrplot.mixed(
                MALDI,
                lower = 'color',
                lower.col = getcolorScale(),
                upper = 'number',
                upper.col = getcolorScale(),
                is.corr = FALSE,
                order = "hclust",
                diag = "n",
                tl.col = "black",
                number.cex = input$ncex,
                number.digits = input$dec,
                tl.pos = "lt",
                cl.cex = input$clcex
            )
            
            dev.off()
            # Return a list containing the filename
            list(src = outfile,
                 contentType = 'image/png',
                 alt = "ANI plot")
        }, deleteFile = T)
        }
        ## mixed plots
        if (input$sheet1!="None" && input$sheet2!="None"){
        # New MALDI matrix with order of columns from ANI matrix
        
        colANI <- colnames(ANI_PLOT$corr)
        ani_maldi <- MALDI[colANI, colANI]
        
        # Adjust margins for the legend (and lower axis title)
        output$ANIMALDI <- renderImage({
            outfile <- tempfile(fileext = '.png')
            png(
                outfile,
                width = 1000 * input$scale,
                height = 1000 * input$scale,
                res = 72
            )
            
            par(
                mar = c(input$labelcex + 1, 4.1, 4.1, 2.1),
                oma = c(3 * input$labelcex, 0, 0, 3 * input$labelcex),
                xpd = T
            )
            corrplot(
                ANI,
                is.corr = FALSE,
                type = "upper",
                order = "hclust",
                tl.col = "black",
                method = "color",
                col = getcolorScale(),
                addgrid.col = "grey",
                addCoef.col = 'black',
                number.cex = input$ncex,
                number.digits = input$dec,
                diag = TRUE,
                tl.pos = "lt",
                tl.cex = input$ncex,
                cl.cex = input$clcex
                
            )
            corrplot(
                ani_maldi,
                is.corr = FALSE,
                type = "lower",
                order = "original",
                tl.col = "black",
                method = "color",
                col = getcolorScale(),
                addgrid.col = "grey",
                addCoef.col = 'black',
                number.cex = input$ncex,
                number.digits = input$dec,
                diag = FALSE,
                tl.cex = input$ncex,
                tl.pos = "n",
                cl.pos = "b",
                cl.cex = input$clcex,
                add = TRUE
            )
            
            # Add axis titles
            mtext(
                "ANI",
                side = 4,
                outer = T,
                line = input$labelcex,
                cex = input$labelcex
            )
            mtext(
                "MALDI",
                side = 1,
                outer = T,
                line = input$labelcex,
                cex = input$labelcex
            )
            dev.off()
            # Return a list containing the filename
            list(src = outfile,
                 contentType = 'image/png',
                 alt = "ANIMALDI plot")
        }, deleteFile = TRUE)
        
        
        # New ANI matrix with order of columns from maldi matrix
        colMALDI <- colnames(MALDI_PLOT$corr)
        MALDI_ANI <- ANI[colMALDI, colMALDI]
        
        output$MALDIANI <- renderImage({
            outfile <- tempfile(fileext = '.png')
            png(
                outfile,
                width = 1000 * input$scale,
                height = 1000 * input$scale,
                res = 72
            )
            
            par(
                mar = c(input$labelcex + 1, 4.1, 4.1, 2.1),
                oma = c(3 * input$labelcex, 0, 0, 3 * input$labelcex),
                xpd = T
            )
            
            corrplot(
                MALDI_ANI,
                is.corr = FALSE,
                type = "upper",
                order = "original",
                tl.col = "black",
                method = "color",
                col = getcolorScale(),
                addgrid.col = "grey",
                addCoef.col = 'black',
                number.cex = input$ncex,
                number.digits = input$dec,
                diag = TRUE,
                tl.pos = "lt",
                tl.cex = input$ncex,
                cl.cex = input$clcex
            )
            
            corrplot(
                MALDI,
                type = 'lower',
                is.corr = FALSE,
                order = "hclust",
                tl.col = "black",
                method = "color",
                col = getcolorScale(),
                addgrid.col = "grey",
                addCoef.col = 'black',
                number.cex = input$ncex,
                number.digits = input$dec,
                tl.cex = input$ncex,
                diag = FALSE,
                tl.pos = "n",
                cl.pos = "b",
                cl.cex = input$clcex,
                add = TRUE
            )
            # Add axis titles
            mtext(
                "ANI",
                side = 4,
                outer = T,
                line = input$labelcex,
                cex = input$labelcex
            )
            mtext(
                "MALDI",
                side = 1,
                outer = T,
                line = input$labelcex,
                cex = input$labelcex
            )
            dev.off()
            # Return a list containing the filename
            list(src = outfile,
                 contentType = 'image/png',
                 alt = "ANI plot")
        }, deleteFile = T)
        }
        enable(id = "visualize")
        runjs(
            "$('.load-container').each(function(){$(this).addClass('shiny-spinner-hidden')})"
        )
    })
}

# Run the application
shinyApp(ui = ui, server = server)
