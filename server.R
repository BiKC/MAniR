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


# Define server logic required to draw a histogram
server <- function(input, output, session) {

    sendCustomMessage <- function(id,msg) {
        session$sendCustomMessage(id, msg)
    }

    debugStart <- function() {
        # click on Run tha analysis on a sample file button and then click on Visualize the results
        click("Samplefile")
        # click on visualise but do this in setTimeout so that the sample file is loaded first using shinyjs
        shinyjs::runjs("setTimeout(function(){
            $('#visualize').click();
        }, 500);")
    }

    # disable the tabPanels Matrix1, Matrix2
    shinyjs::js$disableTab("Matrix\ 1")
    shinyjs::js$disableTab("Matrix\ 2")
    #Matrix 1 vs Matrix 2 (ordered on matrix 1)
    shinyjs::js$disableTab("Matrix\ 1 vs Matrix\ 2\ (ordered\ on\ matrix\ 1)")
    #Matrix 1 vs Matrix 2 (ordered on matrix 2)
    shinyjs::js$disableTab("Matrix\ 1 vs Matrix\ 2\ (ordered\ on\ matrix\ 2)")

    #debugStart()

    getcolorscale <- function() {
        if (input$cmode == "sequential") {
            return(COL1(input$sequentialcolor, 200))
        } else {
            return(COL2(input$divergentcolor, 200))
        }
    }

    single_plot_legacy <- function(data) {
        par(
            mar = c(input$labelcex + 1, 4.1, 4.1, 2.1),
            xpd = TRUE
        )
        # single plot
        corrplot.mixed(
            data,
            lower = "color",
            lower.col = getcolorscale(),
            upper = "number",
            upper.col = getcolorscale(),
            is.corr = FALSE,
            order = "hclust",
            diag = "n",
            tl.col = "black",
            number.cex = input$ncex,
            tl.cex = input$tlcex,
            number.digits = input$dec,
            tl.pos = "lt",
            cl.cex = input$clcex
        )
    }

    single_plot <- function(data, metadata, file=NULL) {
        heatmaply(
            data,
            col = getcolorscale(),
            #dendrogram = input$dendrograms,
            # input$dec is the number of decimal places
            label_format_fun = function(...) format(..., digits = input$dec + 2),
            # add number to each cell also with input$dec for decimals shown and center it but only if show_numbers is TRUE
            cellnote = if (input$show_numbers) round(data, input$dec) else NULL,
            #cellnote = round(data, input$dec),
            cellnote_textposition = "middle center",
            # dendrogram only on the side
            show_dendrogram = c(input$dendrograms, FALSE),
            # add title if input$add_title is TRUE
            #main = if (input$add_title) input$title else "",
            # hide colorbar if input$hide_colorbar is TRUE
            hide_colorbar = input$hide_colorbar,
            # if metadata is not NULL, add it to the heatmap as side colors (row only)
            row_side_colors = if (!is.null(metadata)) metadata else NULL,
            key = TRUE,
            # if file is not NULL, save the heatmap to the file
            file = if (!is.null(file)) file else NULL,
            # use 1000 by 1000 if file is not NULL
            width = if (!is.null(file)) 1000 else NULL,
            height = if (!is.null(file)) 1000 else NULL,
            # input$ncex describe the size of the numbers
            cellnote_size = input$ncex * 10,
            # add a little bit of margin to the top
            margin = c(0, 0, 10, 0),
        )
    }

    combined_plot_legacy <- function(data1, data2) {
        par(
            mar = c(input$labelcex + 1, 4.1, 4.1, 2.1),
            oma = c(3 * input$labelcex, 0, 0, 3 * input$labelcex),
            xpd = TRUE
        )
        corrplot(
            data1,
            is.corr = FALSE,
            type = "upper",
            #order = "hclust",
            tl.col = "black",
            method = "color",
            col = getcolorscale(),
            addgrid.col = "grey",
            addCoef.col = "black",
            number.cex = input$ncex,
            number.digits = input$dec,
            diag = TRUE,
            tl.pos = "lt",
            tl.cex = input$tlcex,
            cl.cex = input$clcex
        )
        corrplot(
            data2,
            is.corr = FALSE,
            type = "lower",
            order = "original",
            tl.col = "black",
            method = "color",
            col = getcolorscale(),
            addgrid.col = "grey",
            addCoef.col = "black",
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
            outer = TRUE,
            line = input$labelcex,
            cex = input$labelcex
        )
        mtext(
            input$xas,
            side = 1,
            outer = TRUE,
            line = input$labelcex,
            cex = input$labelcex
        )
    }

    combined_plot <- function(data1, data2, order_x, order_y, metadata, file=NULL) {
        data1 <- as.data.frame(data1)
        data2 <- as.data.frame(data2)
        # reorder the data
        data1 <- data1[order_x, order_y]
        data2 <- data2[order_x, order_y]
        # make a copy of data1
        combined_data <- data1
        # replace lower triangle with data2
        combined_data[lower.tri(combined_data)] <- data2[lower.tri(data2)]
        combined_data <- as.matrix(combined_data)

        # create a copy of the data as original_data
        original_data <- combined_data

        if (input$logscale) {
            # scale upper triangle between 0 and 1
            combined_data[upper.tri(combined_data)] <- (log(combined_data[upper.tri(combined_data)]) - min(log(combined_data[upper.tri(combined_data)]))) / (max(log(combined_data[upper.tri(combined_data)])) - min(log(combined_data[upper.tri(combined_data)])))
            # scale lower triangle between 0 and 1
            combined_data[lower.tri(combined_data)] <- (log(combined_data[lower.tri(combined_data)]) - min(log(combined_data[lower.tri(combined_data)]))) / (max(log(combined_data[lower.tri(combined_data)])) - min(log(combined_data[lower.tri(combined_data)])))
        } else {
            # scale upper triangle between 0 and 1
            combined_data[upper.tri(combined_data)] <- (combined_data[upper.tri(combined_data)] - min(combined_data[upper.tri(combined_data)])) /
                (max(combined_data[upper.tri(combined_data)]) - min(combined_data[upper.tri(combined_data)]))
            # scale lower triangle between 0 and 1
            combined_data[lower.tri(combined_data)] <- (combined_data[lower.tri(combined_data)] - min(combined_data[lower.tri(combined_data)])) /
                (max(combined_data[lower.tri(combined_data)]) - min(combined_data[lower.tri(combined_data)]))
        }
        # make the diagonal 1
        diag(combined_data) <- 1
        # create custom hovertext. Include the original value and which matrix it came from (input$yas or input$xas)
        custom_text <- paste0(
            "Original value:",
            round(original_data, input$dec),
            "<br>",
            "Matrix:",
            ifelse(upper.tri(original_data), input$yas, input$xas)
            )

        # make it a matrix
        custom_text <- matrix(custom_text, nrow = nrow(original_data), ncol = ncol(original_data))

        # transpose the data (both combined_data and original_data)
        combined_data <- t(combined_data)
        original_data <- t(original_data)

        # flip rows of both
        combined_data <- combined_data[rev(1:nrow(combined_data)), ]
        original_data <- original_data[rev(1:nrow(original_data)), ]

        # same for custom_text
        custom_text <- t(custom_text)
        custom_text <- custom_text[rev(1:nrow(custom_text)), ]


        #print(custom_text)

        heatmaply(
            combined_data,
            col = getcolorscale(),
            # input$dec is the number of decimal places
            label_format_fun = function(...) format(..., digits = input$dec + 2),
            # add number to each cell also with input$dec for decimals shown and center it but only if show_numbers is TRUE
            cellnote = if (input$show_numbers) round(original_data, input$dec) else NULL,
            #cellnote = round(original_data, input$dec),
            cellnote_textposition = "middle center",
            # add the custom hovertext
            custom_hovertext = custom_text,
            # no dendrogram
            show_dendrogram = c(FALSE, FALSE),
            # do not reorder the rows or columns
            Rowv = order_x,
            Colv = order_y,
            #revC = TRUE,
            # add the axis titles
            xlab = input$xas,
            ylab = input$yas,
            # hide colorbar
            hide_colorbar = TRUE,
            # add the metadata if it exists
            row_side_colors = if (!is.null(metadata)) metadata else NULL,
            # title if input$add_title is TRUE
            #main = if (input$add_title) input$title else ""
            # size of numbers in cells is set by input$ncex
            cellnote_size = input$ncex*10,
            # add file name if file is not NULL
            file=file
        )
    }
    check_errors <- function(col1, col2, output_id) {
        if (!all(col1 %in% col2) || !all(col2 %in% col1)) {
            # The page has an output_id without the _Error suffix that normally displays the output plot.
            # We need to make sure this plot is not displayed when there is an error.
            output[[gsub("_Error", "", output_id)]] <- NULL

            not_in_matrix1 <- col2[!(col2 %in% col1)]
            not_in_matrix2 <- col1[!(col1 %in% col2)]
            output[[output_id]] <- renderUI({
                div(class = "alert alert-danger", role = "alert", {
                    tagList(
                        h3("Error: The columns of the two matrices are not the same. Please check your input."),
                        # print the missing columns
                        h4("Missing columns in Matrix 1:"),
                        # print only the missing columns in Matrix 1, not all differences
                        p(paste(not_in_matrix1, collapse = ", ")),
                        br(),
                        h4("Missing columns in Matrix 2:"),
                        # print only the missing columns in Matrix 2, not all differences
                        p(paste(not_in_matrix2, collapse = ", "))
                    )
                })
            })
            return(FALSE)
        }
        return(TRUE)
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
        if (is.null(input$input_excel) == FALSE | input$Samplefile > 0) {
            if (!is.null(input$input_excel)) {
                loadWorkbook(input$input_excel$datapath)
            } else if (input$Samplefile > 0) {
                loadWorkbook("www/moc_data.xlsx")
            }
        } else {
            return(NULL)
        }
    })


    observeEvent(c(input$input_excel, input$Samplefile), {
        if (is.null(input$input_excel) == FALSE | input$Samplefile > 0) {
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
            # metadata tab
            output$tab3 <- renderUI({
                selectInput(
                    "sheet3",
                    "Worksheet with metadata (optional)",
                    choices = c(sheets, "None"),
                    selected = if (length(sheets) > 2) sheets[3] else "None"
                )
            })

            # Xaxis and Yaxis selection boxes + auto suggestion based on sheet names
            output$Yaxis <- renderUI({
                textInput("yas", label = "Name for matrix 1 (Y-axis on combined plot)", value = sheets[1])
            })
            output$Xaxis <- renderUI({
                textInput("xas", label = "Name for matrix 2 (X-axis on combined plot)", value = sheets[min(2, length(sheets))])
            })
        }
    })

    observeEvent(input$sheet2, {
        # if no second sheet is selected, don't use Xaxis/Yaxis
        if (input$sheet2 == "None") {
            output$Xaxis <- NULL
            output$Yaxis <- NULL
        }
    })

    # observe input$legacymode, if status is switched, click the visualize button
    observeEvent(input$legacymode, {
        # do not run if no file is uploaded
        if (is.null(input$input_excel) == FALSE | input$Samplefile > 0) {
            # if legacymode is switched on, click the visualize button
            click("visualize")
        }
    })

    # observe input$scale, if changed, click the visualize button
    observeEvent(input$scale, {
        # do not run if no file is uploaded
        if (is.null(input$input_excel) == FALSE | input$Samplefile > 0) {
            # if scale is changed, click the visualize button
            click("visualize")
        }
    })




    observeEvent(input$visualize, {
        withProgress(message = "Making plots", value = 0, {
            workbook <- wb()
            # check that there is a workbook
            if (is.null(workbook)) {
                return()
            }
            disable(id = "visualize")

            # if metadata is not None, read metadata
            if (input$sheet3 != "None") {
                metadata <- as.data.frame(readWorkbook(workbook, sheet = input$sheet3, colNames = TRUE, rowNames = TRUE))
            } else {
                metadata <- NULL
            }

            # save metadata to session
            session$userData$metadata <- metadata

            first_correlation_matrix <-
                as.matrix(readWorkbook(workbook,
                    colNames = TRUE,
                    rowNames = TRUE,
                    input$sheet1
                ))
            rownames(first_correlation_matrix) <- trimws(rownames(first_correlation_matrix))
            colnames(first_correlation_matrix) <- trimws(colnames(first_correlation_matrix))

            # adjust format with correct rownames and make matrix
            first_correlation_plot <- corrplot.mixed(
                first_correlation_matrix,
                lower = "color",
                upper = "number",
                is.corr = FALSE,
                order = "hclust",
                diag = "n",
                tl.col = "black",
                number.cex = input$ncex,
                tl.cex = input$tlcex,
                tl.pos = "lt",
            )
            # save to session
            session$userData$first_correlation_matrix <- first_correlation_matrix
            # send first_correlation_matrix as custom message to the client together with the metadata
            # this is used to make the plotly plot
            sendCustomMessage("first_correlation_matrix", list(first_correlation_matrix, metadata))
            # if not legacy, use plotly
            if (input$legacymode == FALSE) {
                output$ANI <- renderPlotly({
                    ANI_plot <- single_plot(first_correlation_matrix, metadata)
                    session$userData$ANI_order_x <- ANI_plot$x$layout$xaxis$ticktext
                    session$userData$ANI_order_y <- ANI_plot$x$layout$yaxis$ticktext
                    # also save the plotly plot to a png file
                    #single_plot(first_correlation_matrix, metadata, paste0(session$userData$tmp_dir, "/ANI.png"))
                    #session$userData$ANI_plot <- ANI_plot
                    ANI_plot
                    # export the plotly plot to a png file
                    })
            } else {
                # update width and height of the plot based on input$scale (default 1000*1000 with scale=1)
                scale=input$scale
                shinyjs::runjs(paste0("$('#ANI_legacy').width(", scale, " * 1000)"))
                shinyjs::runjs(paste0("$('#ANI_legacy').height(", scale, " * 1000)"))
                output$ANI_legacy <- renderPlot(single_plot_legacy(first_correlation_matrix))
            }

            # enable the "Matrix 1" tab
            shinyjs::js$enableTab("Matrix\ 1")
            # click on the "Matrix 1" tab if we are on Instructions tab
            shinyjs::js$goToMatrix1tab()
            incProgress(1 / 4)

            if (input$sheet2 != "None") {
                ws <- readWorkbook(
                        workbook,
                        colNames = TRUE,
                        rowNames = TRUE,
                        sheet = input$sheet2
                    )
                rownames(ws) <- trimws(rownames(ws))
                colnames(ws) <- trimws(colnames(ws))
                # adjust format with correct rownames and make matrix
                second_correlation_matrix <- as.matrix(ws)
                second_correlation_plot <- corrplot.mixed(
                    second_correlation_matrix,
                    lower = "color",
                    upper = "number",
                    is.corr = FALSE,
                    order = "hclust",
                    diag = "n",
                    tl.col = "black",
                    number.cex = input$ncex,
                    tl.cex = input$tlcex,
                    tl.pos = "lt"
                )

                # save to session
                session$userData$second_correlation_matrix <- second_correlation_matrix

                # if not legacy, use plotly
                if (input$legacymode == FALSE) {
                    output$MALDI <- renderPlotly({
                    MALDI_plot <- single_plot(second_correlation_matrix, metadata)
                    session$userData$MALDI_order_x <- MALDI_plot$x$layout$xaxis$ticktext
                    session$userData$MALDI_order_y <- MALDI_plot$x$layout$yaxis$ticktext
                    # also save the plotly plot to a png file
                    #single_plot(second_correlation_matrix, metadata, paste0(session$userData$tmp_dir, "/MALDI.png"))
                    #session$userData$MALDI_plot <- MALDI_plot
                    MALDI_plot})
                } else {
                    # update width and height of the plot based on input$scale (default 1000*1000 with scale=1)
                    scale=input$scale
                    shinyjs::runjs(paste0("$('#MALDI_legacy').width(", scale, " * 1000)"))
                    shinyjs::runjs(paste0("$('#MALDI_legacy').height(", scale, " * 1000)"))
                    output$MALDI_legacy <- renderPlot(single_plot_legacy(second_correlation_matrix))
                }
                # enable the "Matrix 2" tab
                shinyjs::js$enableTab("Matrix\ 2")
            }
            incProgress(1 / 4)

            ## mixed plots
            if (input$sheet1 != "None" && input$sheet2 != "None") {
                # New MALDI matrix with order of columns from ANI matrix

                col_matrix1 <- colnames(first_correlation_plot$corr)
                #print(col_matrix1)
                # check if all columns are present in MALDI matrix
                if (check_errors(col_matrix1, colnames(second_correlation_matrix), "ANIMALDI_Error")) {
                    ani_maldi <- second_correlation_matrix[col_matrix1, col_matrix1]
                    first_correlation_matrix_ordered <- first_correlation_matrix[col_matrix1,col_matrix1]
                    #print(ani_maldi)
                    # save to session
                    session$userData$ani_maldi <- ani_maldi
                    if (input$legacymode == FALSE) {
                        output$ANIMALDI <- renderPlotly(combined_plot(first_correlation_matrix_ordered, ani_maldi, session$userData$ANI_order_x, session$userData$ANI_order_y, metadata))
                    } else {
                        # update width and height of the plot based on input$scale (default 1000*1000 with scale=1)
                        scale=input$scale
                        shinyjs::runjs(paste0("$('#ANIMALDI_legacy').width(", scale, " * 1000)"))
                        shinyjs::runjs(paste0("$('#ANIMALDI_legacy').height(", scale, " * 1000)"))
                        output$ANIMALDI_legacy <- renderPlot(combined_plot_legacy(first_correlation_matrix_ordered, ani_maldi))
                    }
                }
                # enable the "Matrix 1 vs Matrix 2 (ordered on matrix 1)" tab
                shinyjs::js$enableTab("Matrix\ 1\ vs\ Matrix\ 2\ (ordered\ on\ matrix\ 1)")
                incProgress(1 / 4)

                # check if all columns are present in ANI matrix
                col_matrix2 <- colnames(second_correlation_plot$corr)
                #print(col_matrix2)
                if (check_errors(col_matrix2, colnames(first_correlation_matrix), "MALDIANI_Error")) {
                    maldi_ani <- first_correlation_matrix[col_matrix2, col_matrix2]
                    second_correlation_matrix_ordered <- second_correlation_matrix[col_matrix2,col_matrix2]
                    #print(maldi_ani)
                    # save to session
                    session$userData$maldi_ani <- maldi_ani
                    if (input$legacymode == FALSE) {
                        # if session$userData$MALDI_order_x is NULL, we first need to render the MALDI plot, we don't need to do this if we already have the order :)
                        if (is.null(session$userData$MALDI_order_x)) {
                            MALDI_plot <- single_plot(second_correlation_matrix, metadata)
                            session$userData$MALDI_order_x <- MALDI_plot$x$layout$xaxis$ticktext
                            session$userData$MALDI_order_y <- MALDI_plot$x$layout$yaxis$ticktext
                        }
                        output$MALDIANI <- renderPlotly(combined_plot(maldi_ani, second_correlation_matrix_ordered, session$userData$MALDI_order_x, session$userData$MALDI_order_y, metadata))
                    } else {
                        # update width and height of the plot based on input$scale (default 1000*1000 with scale=1)
                        scale=input$scale
                        shinyjs::runjs(paste0("$('#MALDIANI_legacy').width(", scale, " * 1000)"))
                        shinyjs::runjs(paste0("$('#MALDIANI_legacy').height(", scale, " * 1000)"))
                        output$MALDIANI_legacy <- renderPlot(combined_plot_legacy(maldi_ani, second_correlation_matrix_ordered))
                    }
                }
                # enable the "Matrix 2 vs Matrix 1 (ordered on matrix 2)" tab
                shinyjs::js$enableTab("Matrix\ 1\ vs\ Matrix\ 2\ (ordered\ on\ matrix\ 2)")
            incProgress(1 / 4)
            }

            enable(id = "visualize")
        })
    })
    # download button # generate matrix1 (ANI) plot as html, add date and time to filename
    output$download1 <- downloadHandler(
        filename = function() {
            paste0(input$yas,"-",Sys.Date(),".html")
        },
        content = function(file) {
            tmpdir <- tempdir()
            #print(tmpdir)
            single_plot(session$userData$first_correlation_matrix, session$userData$metadata, paste0(tmpdir, "/ANI.html"))
            file.copy(paste0(tmpdir, "/ANI.html"), file)
        }
    )
    # repeat for matrix2 (MALDI)
    output$download2 <- downloadHandler(
        filename = function() {
            paste0(input$xas,"-",Sys.Date(),".html")
        },
        content = function(file) {
            tmpdir <- tempdir()
            single_plot(session$userData$second_correlation_matrix, session$userData$metadata, paste0(tmpdir, "/MALDI.html"))
            file.copy(paste0(tmpdir, "/MALDI.html"), file)
        }
    )
    # repeat for matrix1 vs matrix2 (ordered on matrix 1)
    output$download3 <- downloadHandler(
        filename = function() {
            paste0(input$yas,"_vs_",input$xas,"-",Sys.Date(),".html")
        },
        content = function(file) {
            tmpdir <- tempdir()
            combined_plot(session$userData$first_correlation_matrix, session$userData$ani_maldi, session$userData$ANI_order_x, session$userData$ANI_order_y, session$userData$metadata, paste0(tmpdir, "/ANIMALDI.html"))
            file.copy(paste0(tmpdir, "/ANIMALDI.html"), file)
        }
    )
    # repeat for matrix2 vs matrix1 (ordered on matrix 2)
    output$download4 <- downloadHandler(
        filename = function() {
            paste0(input$xas,"_vs_",input$yas,"-",Sys.Date(),".html")
        },
        content = function(file) {
            tmpdir <- tempdir()
            combined_plot(session$userData$maldi_ani, session$userData$second_correlation_matrix, session$userData$MALDI_order_x, session$userData$MALDI_order_y, session$userData$metadata, paste0(tmpdir, "/MALDIANI.html"))
            file.copy(paste0(tmpdir, "/MALDIANI.html"), file)
        }
    )

    # we have action buttons to check first if session$userData contains the data we need, if so we click button using shinyjs, otherwise return an alert message
    # ids of buttons are action_download1, action_download2, action_download3, action_download4
    observeEvent(input$action_download1, {
        if (!is.null(session$userData$first_correlation_matrix)) {
            shinyjs::runjs("$('#download1')[0].click()")
        } else {
            # alert message
            createAlert(session=session, anchorId = "alert",content = "Please upload a matrix first or select a sheet for matrix 1",style = "danger")
        }
    })
    observeEvent(input$action_download2, {
        if (!is.null(session$userData$second_correlation_matrix)) {
            shinyjs::runjs("$('#download2')[0].click()")
        } else {
            createAlert(session=session, anchorId = "alert",content = "Please upload a matrix first or select a sheet for matrix 2",style = "danger")
        }
    })
    observeEvent(input$action_download3, {
        if (!is.null(session$userData$first_correlation_matrix) && !is.null(session$userData$ani_maldi)) {
            shinyjs::runjs("$('#download3')[0].click()")
        } else {
            createAlert(session=session, anchorId = "alert",content = "Please upload a matrix first or select a sheet for matrix 1 and 2",style = "danger")
        }
    })
    observeEvent(input$action_download4, {
        if (!is.null(session$userData$second_correlation_matrix) && !is.null(session$userData$maldi_ani)) {
            shinyjs::runjs("$('#download4')[0].click()")
        } else {
            createAlert(session=session, anchorId = "alert",content = "Please upload a matrix first or select a sheet for matrix 1 and 2",style = "danger")
        }
    })

}
