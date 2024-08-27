CFA_module <- function(input, output, session) {
  #Import the response data---------------------------------------------------
  mydata <- reactive({

    if(is.null(input$CFA_res))
      return("Please upload the score data.")
    inFile <- input$CFA_res
    dataset <- bruceR::import(inFile$datapath)
    data <- as.data.frame(dataset)

    data <- dataset %>% unlist() %>% as.numeric() %>%
      matrix(ncol = ncol(dataset)) %>% as.data.frame()
    colnames(data) <- colnames(dataset)

    if(length(which(is.character(data %>% unlist()))) >=1){
      return("Data can not contain any string data.")
    }
    data
  })

  #Export the response data-------------------------------------------------
  output$CFA_Response <- DT::renderDataTable({
    Response <- mydata()%>%as.data.frame()
    # Response
    Response %>% DT_dataTable_Show()
  })

  #3. CFA--------------------------------------------------------------
  dimension_cfa <- reactive({#Import dimension

    if(is.null(input$dimensionfile_cfa))
      return(NULL)
    inFile <- input$dimensionfile_cfa
    dataset <- bruceR::import(inFile$datapath)
    data <- as.data.frame(dataset)
    if(sum(is.na(dataset)) >=1){
      return(data.frame("x" = "Missing values are not allowed."))
    }
    data
  })
  output$CFA_dimension <- DT::renderDataTable({
    if(is.null(input$dimensionfile_cfa))
      return(NULL)
    dimension_cfa() %>% DT_dataTable_Show()
  })
  CFA_mode <- function(dimension){
    items <- dimension[,1]
    factors_name <- colnames(dimension)[2:ncol(dimension)]
    model <- NULL
    for (i in 1:length(factors_name)) {
      item_number <- which(dimension[,i+1] == 1)
      model <- paste0(model,
                      paste0(factors_name[i], " =~ ",
                             paste0(items[item_number], collapse = " + ")),
                      ";"
      )
    }
    return(model)
  }

  CFA_reactive <- reactive({
    dimension <- dimension_cfa()
    Response <- mydata()%>%as.data.frame()
    estimator <- input$CFA_estimator %>% str_extract_all("[A-Z]+",simplify = T) %>%
      paste0(collapse = "")
    fit <- CFA(data = Response, model = CFA_mode(dimension),
               estimator = estimator,
               highorder = ifelse(is.null(input$CFA_HO),"", input$CFA_HO))
    fit
  })
  CFA_loading_rea <- reactive({
    fit <- CFA_reactive()
    round(bruceR::lavaan_summary(fit)$measure, digits = 3)

  })
  output$CFA_loading <- DT::renderDataTable({
    if(is.null(input$CFA_res))
      return(NULL)
    if(is.null(input$dimensionfile_cfa))
      return(NULL)
    CFA_loading_rea() %>% DT_dataTable_Show()

  })
  CFA_fit_index_rea <- reactive({
    fit <- CFA_reactive()
    as.data.frame(bruceR::lavaan_summary(fit)$fit)%>%round(digits = 3)

  })
  output$CFA_fit_index <- DT::renderDataTable({
    if(is.null(input$CFA_res))
      return(NULL)
    if(is.null(input$dimensionfile_cfa))
      return(NULL)
    CFA_fit_index_rea() %>% DT_dataTable_Show()

  })
  CFA_fit_plot_rea <- reactive({
    fit <- CFA_reactive()
    semPaths(object = fit, what = "path",
             style = "ram",
             layout = input$CFA_plot_style,
             whatLabels = ifelse(as.character(input$CFA_plot_par) ==
                                   "Standardized parameter estimate", "std", "par"))
  })
  output$CFA_dim_example <- downloadHandler(
    filename = function(){
      paste0("CFA_dimension_template.xlsx")
    },
    content = function(file){
      datalist <- list("Dimension example" = data.frame(
        "Column names" = paste0("Item", 1:12),
        "Factor 1" = c(rep(1,4),rep(0,4),rep(1,2),rep(0,2)),
        "Factor 2" = c(rep(0,4),rep(1,4),rep(0,2),rep(1,2)),
        "Factor 3" = c(rep(0,8),rep(1,2),rep(0,2))
      ))
      openxlsx::write.xlsx(x = datalist, file = file)
    }
  )

  output$CFA_fit_plot <- renderPlot({
    if(is.null(input$CFA_res))
      return(NULL)
    if(is.null(input$dimensionfile_cfa))
      return(NULL)
    CFA_fit_plot_rea()
  },height =  exprToFunction(input$CFA_plot_height),width = exprToFunction(input$CFA_plot_width))

  output$CFA_fit_plot1 <- renderUI({
    plotOutput(outputId = "CFA_fit_plot", height = paste0(input$CFA_plot_height,"px"),
               width = paste0(input$CFA_plot_width,"px"))
  })

  #CFA results
  output$CFA_file <- downloadHandler(
    filename = function(){
      paste0("CFA_result.xlsx")
    },
    content = function(file){
      openxlsx::write.xlsx(list("Factor loadings" =  CFA_loading_rea(),
                                "Model fit" = CFA_fit_index_rea()),file = file,
                           rowNames = TRUE)
    }
  )

}

