CTT_module <- function(input, output, session){
  #Import the response data---------------------------------------------------
  mydata <- reactive({

    if(is.null(input$CTT_res))
      return("Please upload the score data.")
    inFile <- input$CTT_res
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
  output$CTT_Response <- DT::renderDataTable({
    Response <- mydata()%>%as.data.frame()
    # Response
    Response %>% DT_dataTable_Show()
  })

  #1. Descriptive statistics--------------------------------------------
  desc_rea <- reactive({
    if(is.null(input$CTT_res))
      return(NULL)
    Response <- mydata()%>%as.data.frame()
    desc <- Describe(Response,digits = 3)$desc%>%as.data.frame()

    desc <- data.frame("Item" = colnames(Response),
                       "Sample size" = desc$N,
                       "Mean value" = round(desc$Mean, digits = 3),
                       "Standard deviation" = round(desc$SD, digits = 3),
                       "Median" = desc$Median,
                       "Minimum value" = desc$Min,
                       "Maximum value" = desc$Max,
                       "Skewness" = round(desc$Skewness,digits = 3),
                       "Kurtosis" = round(desc$Kurtosis,  digits = 3))
    desc
  })
  output$CTT_summary <- DT::renderDataTable({
    desc_rea() %>% DT_dataTable_Show()
  })

  #Distribution
  scores_plot_rea <- reactive({
    if(is.null(input$CTT_res))
      return(NULL)
    Response <- mydata()%>%as.data.frame()
    if(0==1){
      ss <- latticeExtra::mapplot()#useless code
    }
    scores_plot1 <- hist(rowSums(Response), breaks = 100,
                         main = "The distribution for total score", xlab = "Total score", ylab = "Frequency")
    scores_plot1
  })

  output$scores_plot <- renderPlot({
    scores_plot_rea()
  })

  #4. CTT ------------------------------------------------
  # item type
  output$item_type <- renderText({
    if(is.null(input$CTT_res))
      return(NULL)
    Response <- mydata()
    binary_item <- which(apply(Response, MARGIN = 2, FUN = cat_number) == 2)
    ordinal_item <- which(apply(Response, MARGIN = 2, FUN = cat_number) > 2)


    paste(shiny::p(strong("Items with binary response (dichotomous scoring):")),
          shiny::p(strong(paste0(colnames(Response)[binary_item], collapse = ", "),style = "color:red")),
          shiny::p(strong("Items with ordinal response (polytomous scoring):")),
          shiny::p(strong(paste0(colnames(Response)[ordinal_item], collapse = ", "),style = "color:red"))
          )

  })
  #Item parameters
  output$CTT_itempar <- DT::renderDataTable({
    if(is.null(input$CTT_res))
      return(NULL)
    Response <- mydata()
    cat_all <- apply(Response, MARGIN = 2, FUN = cat_number)#The number of categories.
    item_par <- item_ana(data = Response)%>%round(digits = 3)#The max score

    data.frame("The number of categories" = cat_all,
               item_par) %>% DT_dataTable_Show()

  })
  ###4.1 CTT reliability----------------------------------------------
  CTT_relibility_rea <- reactive({
    Response <- mydata()
    reli <- Alpha(data = Response,vars = colnames(Response))
    data.frame(reli$alpha[[1]],
               "omega" = reli$omega[[1]])%>%round(digits = 3)
  })
  output$CTT_reliability <- DT::renderDataTable({
    if(is.null(input$CTT_res))
      return(NULL)
    CTT_relibility_rea() %>% DT_dataTable_Show()
  })

  CTT_item_alpha_rea <- reactive({
    Response <- mydata()
    reli <- Alpha(data = Response,vars = colnames(Response))
    as.data.frame(reli$alpha[[2]])%>%round(digits = 3)
  })
  output$CTT_item_alpha <- DT::renderDataTable({
    if(is.null(input$CTT_res))
      return(NULL)
    CTT_item_alpha_rea() %>% DT_dataTable_Show()
  })
  ###4.2 Download reliability--------------------------------------
  output$CTT_relia_file <- downloadHandler(
    filename = function(){
      paste0("Test_reliability.xlsx")
    },
    content = function(file){
      datalist <- list("Test reliability" = CTT_relibility_rea(),
                       "Alpha coefficient" =  CTT_item_alpha_rea())
      openxlsx::write.xlsx(x = datalist, file = file, rowNames =TRUE)
    }
  )

  ###4.3 Correlation matrix---------------------------------
  CTT_relate_eff_rea <- reactive({
    Response <- mydata()
    rea <- bruceR::Corr(data = Response,method = "pearson",plot = FALSE)
    as.data.frame(rea$corr$r)%>%round(digits = 3)

  })
  output$CTT_relate_eff <- DT::renderDataTable({
    if(is.null(input$CTT_res))
      return(NULL)
    CTT_relate_eff_rea() %>% DT_dataTable_Show()

  })
  CTT_relate_p_rea <- reactive({
    Response <- mydata()
    rea <- bruceR::Corr(data = Response,method = "pearson",plot = FALSE)
    as.data.frame(rea$corr$p)%>%round(digits = 3)

  })
  output$CTT_relate_p <- DT::renderDataTable({
    if(is.null(input$CTT_res))
      return(NULL)
    CTT_relate_p_rea() %>% DT_dataTable_Show()
  })
  ###4.4 Downlaod correaltion matrix---------------------
  output$CTT_relatefile <- downloadHandler(
    filename = function(){
      paste0("Correlation_coefficient_matrix.xlsx")
    },
    content = function(file){
      datalist <- list("Pearson coefficient" = CTT_relate_eff_rea(),
                       "The P value " = CTT_relate_p_rea())
      openxlsx::write.xlsx(x = datalist, file = file, rowNames =TRUE)
    }
  )


  #6. Download discriptive statistics--------------------------------------------
  output$summary_result <- downloadHandler(
    filename = function(){
      paste0("Descriptive_statistics.xlsx")
    },
    content = function(file){
      desc <-  desc_rea()
      datalist <- list("Descriptive statistics" = desc)
      openxlsx::write.xlsx(x = datalist, file = file)
    }
  )
  #7. Download CTT results----------------------------------------------------
  output$CTT_result <- downloadHandler(
    filename = function(){
      paste0("CTT_results.xlsx")
    },
    content = function(file) {
      Response <- mydata() %>% as.data.frame()
      item_par <- item_ana(data = Response)%>%round(digits = 3)
      cat_all <- apply(Response, MARGIN = 2, FUN = cat_number)#The number of categories.
      datalist <- list("CTT_parameters" = data.frame(row.names = colnames(Response),
                                                     "The number of categories of scores" = cat_all,item_par))

      openxlsx::write.xlsx(x = datalist, file = file, rowNames =TRUE)
    })
  #Download the histogram
  output$scores_plotfile <- downloadHandler(
    filename = function(){
      paste0("Total_score_distribution.jpeg")
    },
    content = function(file){
      jpeg(file, width = 1200, height = 800)
      if(is.null(input$CTT_res))
        return(NULL)
      Response <- mydata()%>%as.data.frame()
      scores_plot1 <- hist(rowSums(Response), breaks = 100,
                           main = "Total score distribution", xlab = "Total score", ylab = "Frequency")

      scores_plot1
      dev.off()
    })

}

