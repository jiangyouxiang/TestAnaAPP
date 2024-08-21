CRM_module <- function(input, output, session) {
  #Import the response data---------------------------------------------------
  mydata <- reactive({

    if(is.null(input$CRM_res))
      return("Please upload the score data.")
    inFile <- input$CRM_res
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
  # model formula
  output$CRM_info <- renderUI({
    generateCRMInfo()
  })
  #Export the response data-------------------------------------------------
  output$CRM_Response <- DT::renderDataTable({
    Response <- mydata()%>%as.data.frame()
    # Response
    Response %>% DT_dataTable_Show()
  })
  min_max_detect <- reactive({
    if(is.null(input$CRM_res))
      return(NULL)
    res <- mydata() %>% as.data.frame()
    max_min_data <- data.frame(
      "max.item" = sapply(res, function(x) max(x, na.rm = TRUE)),
      "min.item" = sapply(res, function(x) min(x, na.rm = TRUE))
    )
    rownames(max_min_data) <- colnames(res)
    max_min_data
  })

  v <- reactiveValues(data = NULL)
  observe({
    if(is.null(input$CRM_res))
      return(NULL)
    v$data <- min_max_detect()
  })
  #output the datatable based on the dataframe (and make it editable)
  output$my_datatable <- DT::renderDT({
    if(is.null(input$CRM_res))
      return(NULL)
    DT::datatable(v$data, editable = TRUE)
  })
  #when there is any edit to a cell, write that edit to the initial dataframe
  observeEvent(input$my_datatable_cell_edit, {
    #get values
    info = input$my_datatable_cell_edit
    i = as.numeric(info$row)
    j = as.numeric(info$col)
    k = as.numeric(info$value)
    #write values to reactive
    v$data[i,j] <- k
  })

  # print confirm information
  confirm_rea <- eventReactive(input$save,{
    if(is.null(input$CRM_res))
      return(NULL)
    if(is.null(v$data))
      return(NULL)
    req(input$save)
    if(any(is.na(v$data))){
      return(paste(
        shiny::p("Any missing values are not allowed.",style = "color:red;font-weight:bold"))
      )
    }else{
      return(paste(
        shiny::p("This data is ready to be saved.",style = "color:green;font-weight:bold"))
        )
    }
  })
  observeEvent(input$save, {
    output$confirm_info <- renderText({
      confirm_rea()
    })
  })

  ##10.1 Item parameters---------------------------------------------------------
  CRM_item_par_rea <- reactive({
    req(input$save)#The button is clicked
    data <- isolate(v$data)
    Response <- mydata() %>% as.data.frame()
    if(max(as.numeric(data %>% unlist())) < max(as.numeric(Response %>% unlist())) |
       min(as.numeric(data %>% unlist())) > min(as.numeric(Response %>% unlist())))
      stop("The range of uploaded data is smaller than the range of score data.")

    Response <- mydata() %>% as.data.frame()
    max_min_value <- v$data
    CRM <- EstCRMitem(Response, max_min_value$max.item,
                      max_min_value$min.item,
                      max.EMCycle = 2000, converge = 0.001)
    par <- CRM$param  %>% round(digits = 3)
    rownames(par) <- colnames(Response)
    colnames(par) <- c("Discrimination", "Difficult", "Scaling parameter")
    par
  })

  output$CRM_itempar <- DT::renderDataTable({
    if(is.null(input$CRM_res))
      return(NULL)
    req(input$save)

    Response <- mydata() %>% as.data.frame()
    if(any(is.na(Response)))
      return(data.frame("Any missing values are not allowed."))

    CRM_item_par_rea() %>% DT_dataTable_Show()
  })

  ##10.2 Person parameters--------------------------------------------------------
  CRM_theta_rea <- reactive({
    Response <- mydata() %>% as.data.frame()
    req(input$save)
    max_min_value <-  v$data
    par <- CRM_item_par_rea()
    CRMthetas <- EstCRMperson(data = Response, ipar = par,
                              max.item = max_min_value$max.item,
                              min.item = max_min_value$min.item
    )
    CRMthetas

  })

  output$CRM_person_par <- renderDataTable({
    if(is.null(input$CRM_res))
      return(NULL)
    req(input$save)
    Response <- mydata() %>% as.data.frame()
    if(any(is.na(Response)))
      return(data.frame("Any missing values are not allowed."))
    CRMthetas <- CRM_theta_rea()
    cbind("ID" = CRMthetas$thetas[,1], round(CRMthetas$thetas[,-1],digits = 3)) %>%
      as.data.frame()
  })
  ##10.3 Item fit index-------------------------------------------------------
  CRM_item_fit_rea <- reactive({
    if(is.null(input$CRM_res))
      return(NULL)
    req(input$save)
    Response <- mydata() %>% as.data.frame()
    max_min_value <-  v$data
    par <- CRM_item_par_rea()
    CRMthetas <- CRM_theta_rea()
    fit <- fitCRM(data = Response, ipar = par, est.thetas = CRMthetas,
                  max.item = max_min_value$max.item, group = input$CRM_fit_group)
    sta <- cbind("Interval"= fit$fit.stat[,1], fit$fit.stat[,-1] %>%
                   round(digits = 3)) %>% as.data.frame()
    rownames(sta) <- paste0("Group",1:input$CRM_fit_group)
    sta


  })

  output$CRM_item_fit <- DT::renderDataTable({
    if(is.null(input$CRM_res))
      return(NULL)
    req(input$save)
    Response <- mydata() %>% as.data.frame()
    if(any(is.na(Response)))
      stop("Any missing values are not allowed.")
    CRM_item_fit_rea() %>% DT_dataTable_Show()
  })

  ##10.4 Draw Three-Dimensional Item Category Response Curves-----------------------------

  CRM_plot_ICC <- reactive({
    if(is.null(input$CRM_ICC_item))
      return(NULL)
    req(input$save)

    Response <- mydata() %>% as.data.frame()
    max_min_value <-  v$data
    par <- CRM_item_par_rea()

    item_name <- ifelse(is.null(input$CRM_ICC_item), colnames(Response)[1], input$CRM_ICC_item)

    item <- which(colnames(Response) == item_name)
    figure <- plotCRM(ipar = par,item = item,
                      max.item = max_min_value$max.item,
                      min.item = max_min_value$min.item)
    figure
  })

  output$CRM_ICC <- renderPlot({
    if(is.null(input$CRM_res))
      return(NULL)
    req(input$save)
    Response <- mydata() %>% as.data.frame()
    if(any(is.na(Response)))
      stop("Any missing values are not allowed.")
    CRM_plot_ICC() %>% print()
  })


  output$CRM_item_selection <- renderUI({
    data <- mydata() %>% as.data.frame()
    N <- colnames(data)

    selectInput(inputId = "CRM_ICC_item",label = "Please select the item to be plotted.",
                choices = apply(matrix(N, ncol=1),
                                MARGIN = 1,FUN = as.vector,simplify = FALSE),selected = N[1],
                selectize = TRUE)
  })

  ##10.5 Download results----------------------------------------------------------------------
  output$CRM_results <- downloadHandler(
    filename = function(){
      paste0("CRM_results.xlsx")
    },
    content = function(file){
      CRMthetas <- CRM_theta_rea()
      datalist <- list("Item fit" = CRM_item_fit_rea(),
                       "Item parameters" = CRM_item_par_rea(),
                       "Person parameter" = CRMthetas$thetas)
      openxlsx::write.xlsx(x = datalist,file = file, rowNames = T)
    }
  )
}
