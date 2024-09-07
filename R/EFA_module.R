EFA_module <- function(input, output, session) {
  #Import the response data---------------------------------------------------
  mydata <- reactive({

    if(is.null(input$EFA_res))
      return("Please upload the score data.")
    inFile <- input$EFA_res
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
  output$EFA_Response <- DT::renderDataTable({
    Response <- mydata()%>%as.data.frame()
    # Response
    Response %>% DT_dataTable_Show()
  })


  #2. EFA-----------------------
  EFA_bartlett <- reactive({
    if(is.null(input$EFA_res))
      return(NULL)
    Response <- mydata()%>%as.matrix()
    n <- nrow(Response)
    p <- ncol(Response)
    R <- cor(Response)
    det_R <- det(R)
    df <- p * (p - 1) / 2
    chisq_stat <- -(n - 1 - (2 * p + 5) / 6) * log(det_R)
    data.frame(
      "Sample size" = n,
      "Number of items" = p,
      "chisq_stat" = chisq_stat,
      "df" = df,
      "p_value" = pchisq(chisq_stat, df = df, lower.tail = FALSE)
    )
  })
  output$EFA_bartlett <- DT::renderDataTable({
    if(is.null(input$EFA_res))
      return(NULL)
    EFA_bartlett() %>%round(digits = 3) %>% DT_dataTable_Show()
  })
  EFA_fit <- reactive({
    if(is.null(input$EFA_res))
      return(NULL)
    Response <- mydata()%>%as.data.frame()
    colnames(Response) <- paste0("Item",1:ncol(Response))
    fit <- bruceR::EFA(data = Response , var = "Item", items = 1:ncol(Response),
                       method = EFA_method(input$EFA_method),
                       rotation = EFA_rotation_method(input$rotation_method))
    fit
  })
  output$CTT_EFA_eigenvalues <- DT::renderDataTable({
    if(is.null(input$EFA_res))
      return(NULL)
    fit <- EFA_fit()
    as.data.frame(fit$eigenvalues)%>%round(digits = 3) %>% DT_dataTable_Show()
  })
  EFA_plot_rea <- reactive({
    fit <- EFA_fit()
    fit$scree.plot

  })
  output$EFA_plot <- renderPlot({#Scree plot
    if(is.null(input$EFA_res))
      return(NULL)
    EFA_plot_rea()
  })

  output$EFA_load <- DT::renderDataTable({#Factor loadings
    if(is.null(input$EFA_res))
      return(NULL)
    fit <- EFA_fit()
    as.data.frame(fit$loadings)%>%round(digits = 3) %>% DT_dataTable_Show()
  })
  # Downlaod EFA results---------------------------------
  output$EFA_result <- downloadHandler(
    filename = function(){
      paste0(input$EFA_method, "_", input$rotation_method,"_result.xlsx")
    },
    content = function(file){
      fit <- EFA_fit()
      datalist <- list("Eigenvalues"= as.data.frame(fit$eigenvalues)%>%round(digits = 3),
                       "Factor loadings" = as.data.frame(fit$loadings)%>%round(digits = 3))
      openxlsx::write.xlsx(x = datalist, file = file, rowNames =TRUE)
    }
  )

  #Download scree plot
  output$EFA_plotfile <- downloadHandler(
    filename = function(){
      paste0("EFA_scree_plot.jpeg")
    },
    content = function(file){

      jpeg(file, width = 1200, height = 800)
      if(!is.null(file)){
        EFA_plot_rea() %>% print()
      }
      dev.off()
    }
  )

}
