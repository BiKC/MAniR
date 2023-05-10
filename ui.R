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


jscode <- "
shinyjs.disableTab = function(name) {
var tab = $('.nav li a[data-value=\"' + name + '\"]');
tab.bind('click.tab', function(e) {
e.preventDefault();
return false;
});
tab.addClass('disabled');
// on hover, show disabled cursor
tab.hover(function() {
$(this).css('cursor','not-allowed');
}, function() {
$(this).css('cursor','auto');
});
}

shinyjs.enableTab = function(name) {
var tab = $('.nav li a[data-value=\"' + name + '\"]');
tab.unbind('click.tab');
tab.removeClass('disabled');
// reset hover style to hand pointing
tab.hover(function() {
$(this).css('cursor','pointer');
}, function() {
$(this).css('cursor','auto');
});
}

shinyjs.clickTab = function(name) {
var tab = $('.nav li a[data-value=\"' + name + '\"]');
tab.click();
}

shinyjs.goToMatrix1tab = function() {
    // set the active tab to the 'Matrix 1' tab if the current tab is the 'Instructions' tab
    // note, multiple tabs can be active at the same time, so we need to check if the 'Instructions' tab is active
    //instructions is the data-value of the a inside the li
    if ($('.nav li a[data-value=\"Instructions\"]').parent().hasClass('active')) {
        // click the 'Matrix 1' tab
        tab = $('.nav li a[data-value=\"Matrix\ 1\"]');
        tab.click();
    }
}
"

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    useShinyjs(),
    shinyjs::extendShinyjs(text = jscode, functions = c("disableTab", "enableTab", "clickTab", "goToMatrix1tab")),
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
                              ")),
                tags$script(src = "https://cdn.plot.ly/plotly-2.14.0.min.js"),
              tags$script(src = "custom_plotly.js")
              ),
    titlePanel("Correlation analysis"),
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            tabsetPanel(
                tabPanel(
                    "Input file",
                    fileInput("input_excel", "Excel containing the correlation data"),
                    fluidRow(
                        column(
                            6,
                            uiOutput("tab1")
                        ),
                        column(
                            6,
                            uiOutput("tab2")
                        )
                    ),
                    # fluidrow for metadata tab
                    fluidRow(
                        column(
                            6,
                            uiOutput("tab3")
                        )
                    ),
                    fluidRow(
                        column(
                            6,
                            uiOutput("Yaxis", inline = TRUE)
                        ),
                        column(
                            6,
                            uiOutput("Xaxis", inline = TRUE)
                        )
                    ),
                    actionButton("visualize", "Visualize the dataset"),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    tags$div(tags$strong("Source code:")),
                    tags$a(
                        img(src = "bikc_github.png", width = "100px", alt = "Github"),
                        href =
                            "https://github.com/BiKC"
                    )
                ),
                tabPanel(
                    "Plot settings",
                    switchInput("legacymode", "Legacy mode", value = FALSE),
                    sliderInput("dec", "Decimal places", 0, 3, 3, 1),
                        sliderInput(
                            "ncex", "Size of numbers in the plot",
                            0.1, 10, 1.2, 0.1
                        ),
                    # legacy settings
                    conditionalPanel(
                        condition = "input.legacymode==true",
                        sliderInput("clcex", "Size of colorscale numbers", 0.1, 10, 0.8, 0.1),
                        sliderInput(
                            "tlcex", "Size of text labels (row and column names)",
                            0.1, 10, 0.8, 0.1
                        ),
                        sliderInput(
                            "scale", "Scale of the plot (default 1=1000x1000)",
                            0.5, 50, 1, 0.1
                        ),
                        sliderInput(
                            "labelcex", "Size of the X/Y labels (combined plots)",
                            1, 20, 3, 0.1
                        ),
                    ),
                    # non-legacy settings
                    # checkbox for showing dendrograms
                    conditionalPanel(
                        condition = "input.legacymode==false",
                        # switchInput("add_title", "Add title", FALSE, size = "mini"),
                        conditionalPanel(
                            condition = "input.add_title==true",
                            textInput("title", "Title", "Correlation plot")
                        ),
                        h3("Single plot specific settings"),
                        switchInput("hide_colorbar", "Hide colorscale", TRUE, size = "mini"),
                        switchInput("dendrograms", "Show dendrograms", FALSE, size = "mini"),
                        switchInput("show_numbers", "Show numbers", TRUE, size = "mini"),
                        h3("Combined plot specific settings"),
                        switchInput("logscale", "Log scale", FALSE, size = "mini")
                    ),
                    selectInput("cmode", "Color mode",
                        c("sequential", "diverging"),
                        selected = "diverging"
                    ),
                    # Sequential colors
                    conditionalPanel(
                        condition = "input.cmode=='sequential'",
                        selectizeInput(
                            inputId = "sequentialcolor",
                            label = "Colorscale",
                            rev(c(
                                "Oranges", "Purples", "Reds", "Blues", "Greens",
                                "Greys", "OrRd", "YlOrRd", "YlOrBr", "YlGn"
                            )),
                            selected = "YlOrRd",
                            options = list(render = I(
                                '{
                  option: function(item, escape) {
                    return "<div style=\'display:flex;justify-content:space-between\'><strong>" + escape(item.value) + "</strong><img src=\'" +escape(item.value)+".png\'>"
                  }
                }'
                            ))
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
                      return "<div style=\'display:flex;justify-content:space-between\'><strong>" + escape(item.value) + "</strong><img src=\'" +escape(item.value)+".png\'>"
                    }
                  }'
                            ))
                        ),
                    ),
                ),
                tabPanel(
                    "Download plots",
                    # Note to warn user that generating plots can take a while
                    h3("Note:"),
                    div("Generating plots can take a while. Please be patient."),
                    tags$br(),
                    # message displaying it only downloads non-legacy plots as html
                    div("Only non-legacy plots are downloaded as html. Legacy plots are to be downloaded by right-clicking on the plot and selecting 'Save image as...'"),
                    tags$br(),
                    # Download button
                    # download buttons for matrix 1, matrix 2, matrix 1 vs matrix 2 (ordered by 1) and matrix 1 vs matrix 2 (ordered by 2)
                    # show actionbuttons for downloading plots
                    actionBttn("action_download1", "Download matrix 1", icon("download")), tags$br(),
                    actionBttn("action_download2", "Download matrix 2", icon("download")), tags$br(),
                    actionBttn("action_download3", "Download matrix 1 vs matrix 2 (ordered by 1)", icon("download")), tags$br(),
                    actionBttn("action_download4", "Download matrix 1 vs matrix 2 (ordered by 2)", icon("download")), tags$br(),
                    bsAlert("alert"),
                    # hide download buttons in conditional panel with false condition
                    div(id = "downloadButtons", style = "height: 0px; overflow: hidden;",
                        downloadButton("download1", "Download matrix 1", icon("download")),
                        downloadButton("download2", "Download matrix 2", icon("download")),
                        downloadButton("download3", "Download matrix 1 vs matrix 2 (ordered by 1)", icon("download")),
                        downloadButton("download4", "Download matrix 1 vs matrix 2 (ordered by 2)", icon("download")),
                    )
                    #downloadButton("download", "Download plots", icon("download"), disabled = TRUE),
                )
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                id = "outputPanel",
                tabPanel(
                    "Instructions",
                    div(id="myDiv"),
                    h3("Correlation analysis tool"),
                    div("The input should be provided in the form of an xlsx file.
           The file should contain the data in the following format:"),
                    tags$table(
                        tags$tr(
                            tags$th(""), tags$th("bacteria1"), tags$th("bacteria2"),
                            tags$th("bacteria3"), tags$th("bacteria4"), tags$th("bacteria5")
                        ),
                        tags$tr(
                            tags$th("bacteria1"), tags$td("100"), tags$td("98.5"),
                            tags$td("97.5"), tags$td("96.5"), tags$td("95.5")
                        ),
                        tags$tr(
                            tags$th("bacteria2"), tags$td("98.5"), tags$td("100"),
                            tags$td("98.5"), tags$td("97.5"), tags$td("96.5")
                        ),
                        tags$tr(
                            tags$th("bacteria3"), tags$td("97.5"), tags$td("98.5"),
                            tags$td("100"), tags$td("98.5"), tags$td("97.5")
                        ),
                        tags$tr(
                            tags$th("bacteria4"), tags$td("96.5"), tags$td("97.5"),
                            tags$td("98.5"), tags$td("100"), tags$td("98.5")
                        ),
                        tags$tr(
                            tags$th("bacteria5"), tags$td("95.5"), tags$td("96.5"),
                            tags$td("97.5"), tags$td("98.5"), tags$td("100")
                        )
                    ),
                    div(
                        strong("Note: "),
                        "The diagonal should always be 100 since this is the same thing you are comparing."
                    ),
                    div(
                        strong("Note: "),
                        "Row and column names are free to choose however you always need the same values in both.
        Avoid spaces in these column and row names!"
                    ),
                    br(),
                    tags$ol(
                        tags$li("Upload the Excel file"),
                        tags$li("Select the sheets containing the data. Note that the second sheet is optional. If provided, you can compare two correlation matrixes from different sheets."),
                        tags$li("If two sheets are provided, you can change the labels for the X and Y axis for the combined plots. By default, the names of the sheets will be suggested."),
                        tags$li("click on \"Visualize the dataset\". This can take a few seconds depending on the size of the dataset."),
                        tags$li("To customize the plot, go to the tab \"Plot settings\" on the left and change the settings as you please.")
                    ),
                    br(),
                    # advanced information about metadata, hidden in a collapsible panel
                    bsCollapsePanel(
                        #id = "advanced_info",
                        # title of the panel, including a arrow symbol to indicate that it is collapsible
                        title = tags$span(tags$i(class = "fa fa-arrow-right"), " Advanced information"),
                        div(
                            strong("Note: "),
                            "The metadata should be provided in the same Excel file as the data but in a separate sheet. The sheet should contain the data in the following format:"
                            # first column contains a sample id with the same name as the columns from the data sheet, the other columns contain the metadata with a header describing the metadata
                        ),
                        tags$table(
                            tags$tr(
                                tags$th("sample"), tags$th("metadata1"), tags$th("metadata2"),
                                tags$th("...")
                            ),
                            tags$tr(
                                tags$th("bacteria1"), tags$td("value1"), tags$td("value2"),
                                tags$td("...")
                            ),
                            tags$tr(
                                tags$th("bacteria2"), tags$td("value1"), tags$td("value2"),
                                tags$td("...")
                            ),
                            tags$tr(
                                tags$th("bacteria3"), tags$td("value1"), tags$td("value2"),
                                tags$td("...")
                            ),
                            tags$tr(
                                tags$th("bacteria4"), tags$td("value1"), tags$td("value2"),
                                tags$td("...")
                            ),
                            tags$tr(
                                tags$th("bacteria5"), tags$td("value1"), tags$td("value2"),
                                tags$td("...")
                            )
                        ),
                        div(
                            strong("Note: "),
                            "The first column should always contain the sample id. The name of this column is not important. The other columns can be named as you please."
                        ),
                        div(
                            strong("Note: "),
                            "The metadata sheet is optional. If you do not provide a metadata sheet, the plots will be generated without metadata."
                        )
                    ),
                    actionButton("Samplefile", "Run the analysis on a sample file"),
                    downloadButton("downloadSampleFile", "Download the sample file")
                ),
                tabPanel(
                    title = "Matrix 1",
                    conditionalPanel(
                        condition = "input.legacymode==false",
                        plotlyOutput("ANI", width = "100%", height = "80vh")
                    ),
                    conditionalPanel(
                        condition = "input.legacymode==true",
                        plotOutput("ANI_legacy", width = "100%", height = "80vh")
                    )
                ),
                tabPanel(
                    title = "Matrix 2",
                    conditionalPanel(
                        condition = "input.legacymode==false",
                        plotlyOutput("MALDI", width = "100%", height = "80vh")
                    ),
                    conditionalPanel(
                        condition = "input.legacymode==true",
                        plotOutput("MALDI_legacy", width = "100%", height = "80vh")
                    )
                ),
                tabPanel(
                    title = "Matrix 1 vs Matrix 2 (ordered on matrix 1)",
                    htmlOutput("ANIMALDI_Error"),
                    conditionalPanel(
                        condition = "input.legacymode==false",
                        plotlyOutput("ANIMALDI", width = "100%", height = "80vh")
                    ),
                    conditionalPanel(
                        condition = "input.legacymode==true",
                        plotOutput("ANIMALDI_legacy", width = "100%", height = "80vh")
                    )
                ),
                tabPanel(
                    title = "Matrix 1 vs Matrix 2 (ordered on matrix 2)",
                    htmlOutput("MALDIANI_Error"),
                    conditionalPanel(
                        condition = "input.legacymode==false",
                        plotlyOutput("MALDIANI", width = "100%", height = "80vh")
                    ),
                    conditionalPanel(
                        condition = "input.legacymode==true",
                        plotOutput("MALDIANI_legacy", width = "100%", height = "80vh")
                    )
                )
            )
        )
    )
)
