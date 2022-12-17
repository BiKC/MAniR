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
library("openxlsx")
library("RColorBrewer")
library("corrplot")
library("periscope")

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  useShinyjs(),
  tags$head(tags$style(HTML("body {
                                  background-color:#fffff9;
                                  color:#2F3C7E;
                                  }
                              .well {
                                  background-color:#2F3C7E;
                                  color:#fffff9;
                              }
                              .sw-dropdown-in {
                                  color: #2F3C7E;
                              }
                              .tab-content {
                                padding:10px;
                                background-color:#fffff9;
                                color: #2F3C7E;
                                border-radius:4px;
                              }
                              #outputPanel>li>a,
                              li[role='presentation']>a {
                               background-color:#DCD6D5FF;
                              }
                              #outputPanel>li.active>a{
                               background-color:#FFF;
                              }
                              table, th, td {
                                border: 1px solid black;
                                padding:5px
                              }
                              "
  ))),
  titlePanel("Correlation analysis"),
  
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    
    sidebarPanel(
      tabsetPanel(
        tabPanel("Input file",
                 fileInput("input_excel", "Excel containing the correlation data"),
                 fluidRow(
                   column(6,
                          uiOutput("tab1")
                   ),
                   column(6,
                          uiOutput("tab2")
                   )
                 ),
                 fluidRow(
                   column(6,
                          uiOutput("Yaxis",inline = T)
                   ),
                   
                   column(6,
                          uiOutput("Xaxis",inline = T)
                   )
                 ),
                 actionButton('visualize', 'Visualize the dataset'),
                 tags$br(),
                 tags$br(),
                 tags$br(),
                 tags$br(),
                 tags$div(tags$strong("Source code:")),
                 tags$a(img(src = "bikc_github.png", width = "100px"), href =
                          "https://github.com/BiKC")
        ),
        tabPanel("Plot settings",
                 sliderInput("dec", "Decimal places", 0, 5, 3, 1),
                 sliderInput("ncex", "Size of numbers in the correlation plot", 0.1, 10, 0.8, 0.1),
                 sliderInput("clcex", "Size of colorscale numbers", 0.1, 10, 0.8, 0.1),
                 sliderInput("tlcex", "Size of text labels (row and column names)", 0.1, 10, 0.8, 0.1),
                 sliderInput("scale", "Scale of the plot (default 1=1000x1000)", 0.5, 50, 1, 0.1),
                 sliderInput("labelcex", "Size of the X/Y labels (combined plots)", 1, 20, 3, 0.1),
                 selectInput("cmode", "Color mode", c("sequential", "diverging"), selected = "diverging"),
                 # Sequential colors
                 conditionalPanel(
                   condition = "input.cmode=='sequential'",
                   selectizeInput(
                     inputId = "sequentialcolor",
                     label = "Colorscale",
                     rev(c("Oranges","Purples","Reds","Blues","Greens","Greys","OrRd","YlOrRd","YlOrBr","YlGn")),
                     selected = "YlOrRd",
                     
                     options = list(render = I(
                       '{
                            option: function(item, escape) {
                              return "<div style=\'display:flex; justify-content:space-between\'><strong>" + escape(item.value) + "</strong><img src=\'" +escape(item.value)+".png\'>"
                            }
                          }'))
                   )
                 ),
                 # Diverging colors
                 conditionalPanel(
                   condition = "input.cmode=='diverging'",
                   selectizeInput(
                     inputId = "divergentcolor",
                     label = "Colorscale",
                     rev(c("RdBu", "BrBG", "PiYG", "PRGn", "PuOr", "RdYlBu")),
                     selected = "RdBu",
                     
                     options = list(render = I(
                       '{
                            option: function(item, escape) {
                              return "<div style=\'display:flex; justify-content:space-between\'><strong>" + escape(item.value) + "</strong><img src=\'" +escape(item.value)+".png\'>"
                            }
                          }'))
                   ),
                 )
                 
                 
        ),
        tabPanel("Download plots","WIP",icon("tools"))
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(tabsetPanel(id="outputPanel",
                          tabPanel("Instructions",
                                   h3("Correlation analysis tool"),
                                   div("The input should be provided in the form of an xlsx file. The file should contain the data in the following format:"),
                                   tags$table(
                                     tags$tr(
                                       tags$th(""),tags$th("bacteria1"),tags$th("bacteria2"),tags$th("bacteria3"),tags$th("bacteria4"),tags$th("bacteria5")
                                     ),
                                     tags$tr(
                                       tags$th("bacteria1"),tags$td("100"),tags$td("98.5"),tags$td("97.5"),tags$td("96.5"),tags$td("95.5")
                                     ),
                                     tags$tr(
                                       tags$th("bacteria2"),tags$td("98.5"),tags$td("100"),tags$td("98.5"),tags$td("97.5"),tags$td("96.5")
                                     ),
                                     tags$tr(
                                       tags$th("bacteria3"),tags$td("97.5"),tags$td("98.5"),tags$td("100"),tags$td("98.5"),tags$td("97.5")
                                     ),
                                     tags$tr(
                                       tags$th("bacteria4"),tags$td("96.5"),tags$td("97.5"),tags$td("98.5"),tags$td("100"),tags$td("98.5")
                                     ),
                                     tags$tr(
                                       tags$th("bacteria5"),tags$td("95.5"),tags$td("96.5"),tags$td("97.5"),tags$td("98.5"),tags$td("100")
                                     )
                                   ),
                                   div(strong("Note: "), "The diagonal should always be 100 since this is the same thing you are comparing."),
                                   div(strong("Note: "), "Row and column names are free to choose however you always need the same values in both. Avoid spaces in these column and row names!"),
                                   br(),
                                   tags$ol(
                                     tags$li("Upload the Excel file"),
                                     tags$li("Select the sheets containing the data. Note that the second sheet is optional. If provided, you can compare two correlation matrixes from different sheets."),
                                     tags$li("If two sheets are provided, you can change the labels for the X and Y axis for the combined plots. By default, the names of the sheets will be suggested."),
                                     tags$li("click on \"Visualize the dataset\". This can take a few seconds depending on the size of the dataset."),
                                     tags$li("To customize the plot, go to the tab \"Plot settings\" on the left and change the settings as you please.")
                                   ),
                                   br(),
                                   actionButton("Samplefile", "Run the analysis on a sample file"),
                                   downloadButton("downloadSampleFile","Download the sample file")
                          )
    ))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  getcolorScale <- function() {
    if (input$cmode == "sequential") {
      return(COL1(input$sequentialcolor, 200))
    }
    else {
      return(COL2(input$divergentcolor, 200))
    }
  }
  
  output$downloadSampleFile <- downloadHandler(
    filename = function() {
      "sample_file.xlsx"
    },
    content = function(con) {
      file.copy("www/moc_data.xlsx", con)
    }
  )
  
  wb <- reactive({
    if (!is.null(input$input_excel) | input$Samplefile==0) {
      loadWorkbook(input$input_excel$datapath)
    }
    else if (input$Samplefile>0){
      loadWorkbook("www/moc_data.xlsx")
    }
    else {return(NULL)}
  })
  
  
  observeEvent(c(input$input_excel,input$Samplefile), {
    if (is.null(input$input_excel) == F | input$Samplefile>0) {
      print("uhuh")
      sheets <- sheets(wb())
      # sheet selection boxes
      output$tab1 <- renderUI({
        selectInput(
          "sheet1",
          HTML("Worksheet with first correlation <br>matrix"),
          choices = sheets,
          selected = sheets[1]
        )
        
        
      })
      output$tab2 <- renderUI({
        selectInput(
          "sheet2",
          "Worksheet with second correlation matrix (optional)",
          choices = c(sheets, "None"),
          selected = sheets[min(2, length(sheets))]
        )
      })
      
      # Xaxis and Yaxis selection boxes + auto suggestion based on sheet names
      output$Yaxis <- renderUI({
        textInput("yas",label = "Name for matrix 1 (Y-axis on combined plot)",value = sheets[1])
      })
      output$Xaxis <- renderUI({
        textInput("xas",label = "Name for matrix 2 (X-axis on combined plot)",value = sheets[min(2, length(sheets))])
      })
    }
  })
  
  observeEvent(input$sheet2,{
    # if no second sheet is selected, don't use Xaxis/Yaxis
    if (input$sheet2=="None"){
      output$Xaxis<-NULL
      output$Yaxis<-NULL
    }
  })
  
  
  
  
  
  observeEvent(input$visualize, {
    withProgress(message = "Making plots", value = 0,{
      # Remove tabs if they exists
      try(removeTab("outputPanel","Matrix 1"))
      try(removeTab("outputPanel","Matrix 2"))
      try(removeTab("outputPanel","Matrix 1 vs Matrix 2 (ordered on matrix 1)"))
      try(removeTab("outputPanel","Matrix 1 vs Matrix 2 (ordered on matrix 2)"))
      
      
      if (is.null(wb())) {
        return()
      }
      
      disable(id = "visualize")
      workbook <- wb()
      appendTab("outputPanel",
                tabPanel(title = "Matrix 1",
                         imageOutput("ANI")
                ),select = T)
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
        tl.cex=input$tlcex,
        tl.pos = "lt",
        
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
          #oma = c(3 * input$labelcex, 0, 0, 3 * input$labelcex),
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
          tl.cex=input$tlcex,
          number.digits = input$dec,
          tl.pos = "lt",
          cl.cex = input$clcex
        )
        dev.off()
        # Return a list containing the filename
        list(src = outfile,
             width=1000 * input$scale,
             height=1000 * input$scale,
             contentType = 'image/png',
             alt = "Correlation plot 1")
      }, deleteFile = T)
      incProgress(1/4)
      if (input$sheet2!="None"){
        appendTab("outputPanel",
                  tabPanel(title = "Matrix 2",
                           
                           imageOutput("MALDI", width = "100%", height = "85vh")
                  ))
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
          tl.cex=input$tlcex,
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
            #oma = c(3 * input$labelcex, 0, 0, 3 * input$labelcex),
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
            tl.cex=input$tlcex,
            number.digits = input$dec,
            tl.pos = "lt",
            cl.cex = input$clcex
          )
          
          dev.off()
          # Return a list containing the filename
          list(src = outfile,
               width=1000 * input$scale,
               height=1000 * input$scale,
               contentType = 'image/png',
               alt = "Correlation plot 2")
        }, deleteFile = T)
      }
      incProgress(1/4)
      
      ## mixed plots
      if (input$sheet1!="None" && input$sheet2!="None"){
        # New MALDI matrix with order of columns from ANI matrix
        
        colANI <- colnames(ANI_PLOT$corr)
        # check if all columns are present in MALDI matrix
        if (!all(colANI %in% colnames(MALDI)) | !all(colnames(MALDI) %in% colANI)) {
          appendTab("outputPanel",
                    tabPanel(title = "Matrix 1 vs Matrix 2 (ordered on matrix 1)",
                             htmlOutput("ANIMALDI_Error", width = "100%", height = "15vh")
                    ))
          notInANI <- colnames(MALDI)[!(colnames(MALDI) %in% colANI)]
          notInMALDI <- colANI[!(colANI %in% colnames(MALDI))]
          output$ANIMALDI_Error <- renderUI({
            div(class = "alert alert-danger", role = "alert",{
              tagList(
                h3("Error: The columns of the two matrices are not the same. Please check your input."),
                # print the missing columns
                h4("Missing columns in Matrix 1:"),
                # print only the missing columns in Matrix 1, not all differences
                p(paste(notInANI, collapse = ", ")),
                br(),
                h4("Missing columns in Matrix 2:"),
                # print only the missing columns in Matrix 2, not all differences
                p(paste(notInMALDI, collapse = ", "))
              )
            })
          })
          
        }
        else {
          ani_maldi <- MALDI[colANI, colANI]
          
          appendTab("outputPanel",
                    tabPanel(title = "Matrix 1 vs Matrix 2 (ordered on matrix 1)",
                             imageOutput("ANIMALDI", width = "100%", height = "85vh"),
                    ))
          
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
              tl.cex = input$tlcex,
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
              tl.cex = input$tlcex,
              tl.pos = "n",
              cl.pos = "b",
              cl.cex = input$clcex,
              add = TRUE
            )
            
            # Add axis titles
            mtext(
              input$yas,
              side = 4,
              outer = T,
              line = input$labelcex,
              cex = input$labelcex
            )
            mtext(
              input$xas,
              side = 1,
              outer = T,
              line = input$labelcex,
              cex = input$labelcex
            )
            dev.off()
            # Return a list containing the filename
            list(src = outfile,
                 width=1000 * input$scale,
                 height=1000 * input$scale,
                 contentType = 'image/png',
                 alt = "ANIMALDI plot")
          }, deleteFile = TRUE)
        }
        incProgress(1/4)
        
        # check if all columns are present in ANI matrix
        colMALDI <- colnames(MALDI_PLOT$corr)
        if (!all(colMALDI %in% colnames(ANI)) | !all(colnames(ANI) %in% colMALDI)) {
          appendTab("outputPanel",
                    tabPanel(title = "Matrix 1 vs Matrix 2 (ordered on matrix 2)",
                             htmlOutput("MALDIANI_Error", width = "100%", height = "15vh")
                    ))
          notInANI <- colnames(MALDI)[!(colnames(MALDI) %in% colANI)]
          notInMALDI <- colANI[!(colANI %in% colnames(MALDI))]
          output$MALDIANI_Error <- renderUI({
              div(class = "alert alert-danger", role = "alert",{
              tagList(
                h3("Error: The columns of the two matrices are not the same. Please check your input."),
                # print the missing columns
                h4("Missing columns in Matrix 1:"),
                # print only the missing columns in Matrix 1, not all differences
                p(paste(notInANI, collapse = ", ")),
                br(),
                h4("Missing columns in Matrix 2:"),
                # print only the missing columns in Matrix 2, not all differences
                p(paste(notInMALDI, collapse = ", "))
            )
              })
          })
        }
        else {
          
          appendTab("outputPanel",
                    tabPanel(title = "Matrix 1 vs Matrix 2 (ordered on matrix 2)",
                             
                             imageOutput("MALDIANI", width = "100%", height = "85vh")
                    ))
          # New ANI matrix with order of columns from maldi matrix
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
              tl.cex = input$tlcex,
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
              diag = FALSE,
              tl.pos = "n",
              tl.cex = input$tlcex,
              cl.pos = "b",
              cl.cex = input$clcex,
              add = TRUE
            )
            # Add axis titles
            mtext(
              input$yas,
              side = 4,
              outer = T,
              line = input$labelcex,
              cex = input$labelcex
            )
            mtext(
              input$xas,
              side = 1,
              outer = T,
              line = input$labelcex,
              cex = input$labelcex
            )
            dev.off()
            # Return a list containing the filename
            list(src = outfile,
                 width=1000 * input$scale,
                 height=1000 * input$scale,
                 contentType = 'image/png',
                 alt = "ANI plot")
          }, deleteFile = T)
        }
      }
      incProgress(1/4)
      
      enable(id = "visualize")
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
