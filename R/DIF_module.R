DIF_module <- function(input, output, session) {
  #Import the response data---------------------------------------------------
  mydata <- reactive({

    if(is.null(input$DIF_res))
      return("Please upload the score data.")
    data.f <- read_file(input$DIF_res)

    data.f
  })

  ## 11. DIF---------------------------------------------------------------------------------------
  DIF_file <- reactive({
    if(is.null(input$DIF_group_file))
      return(NULL)
    bruceR::import(input$DIF_group_file$datapath) %>% as.data.frame()
  })
  output$DIF_group_var_select <- renderUI({
    dif_var <- mydata() %>% as.data.frame() %>% colnames()
    checkboxGroupInput(inputId = "DIF_all_variable",inline = T,
                       label = "Please select variables that are not part of the answer data.",
                       choices = dif_var)
  })
  output$group_variables <- renderText({
    if(is.null(input$DIF_all_variable))
      return(NULL)
    paste0("The selected variables are: ", unlist(input$DIF_all_variable) %>% paste(collapse = ", "))

  })
  output$DIF_variable_selection <- renderUI({
    dif_var <- input$DIF_all_variable
    selectInput(inputId = "DIF_variable",
                label = "Please select a variable to be analyzed.",
                choices = dif_var, selected = dif_var[1], selectize = TRUE)
  })
  output$focal_name <- renderUI({
    if(is.null(input$DIF_variable))
      return(NULL)
    if(input$DIF_method == "Logistic Regression")
      return(NULL)
    choices <- mydata()
    choices <- choices[,input$DIF_variable] %>% table() %>% names()
    selectInput(inputId = "focal_name1",label = "Please select the focal group.",
                choices = choices , selected = choices[1], selectize = TRUE)
  })
  output$LR_model <- renderUI({
    if(is.null(input$DIF_variable))
      return(NULL)
    if(input$DIF_method != "Logistic Regression")
      return(NULL)
    selectInput(inputId = "LR_model",
                label = "Please select the model for trait estimation.",
                choices = c("GRM","GPCM"), selected = "GRM", selectize = TRUE)
  })
  #Export the response data-------------------------------------------------
  output$DIF_Response <- DT::renderDataTable({
    Response <- mydata()%>%as.data.frame()
    # Response
    Response %>% DT_dataTable_Show()
  })

  DIF_ana_rea <- reactive({
    if(is.null(input$DIF_res))
      return(NULL)
    if(is.null(input$DIF_method))
      return(NULL)
    Response <- mydata() %>% as.data.frame()
    DIF_var <- Response[,sprintf("%s",input$DIF_variable)] %>% as.character()

    Response_new <- Response[,!colnames(Response) %in% input$DIF_all_variable]
    if(input$DIF_method == "Mantel Haenszel"){
      if(is.null(input$focal_name1))
        return(NULL)
      fit <- difMH(Data = Response_new, group = DIF_var,
                   focal.name = input$focal_name1)
      result <- data.frame(
        Chi_square = fit$MH,
        P.value = fit$p.value,
        alphaMH = fit$alphaMH,
        delta_alphaMH = -2.35*log(fit$alphaMH)
      )

    }
    if(input$DIF_method == "Logistic Regression"){
      if(is.null(input$LR_model))
        return(NULL)
      fit <- lordif::lordif(resp.data = Response_new, model = input$LR_model,
                            group =DIF_var)
      result <- data.frame(
        "Item" = fit$stats$item,
        "n_cat" = fit$stats$ncat,
        "Chisq_12" = fit$stats$chi12,
        "df_12" = fit$stats$df12,
        "p_value_12" = stats::pchisq(q = fit$stats$chi12, df = fit$stats$df12),
        "Chisq_13" = fit$stats$chi13,
        "df_13" = fit$stats$df13,
        "p_value_13" = stats::pchisq(q = fit$stats$chi13, df = fit$stats$df13),
        "Chisq_23" = fit$stats$chi23,
        "df_23" = fit$stats$df23,
        "p_value_23" = stats::pchisq(q = fit$stats$chi23, df = fit$stats$df23),
        "Beta_12" =fit$stats$beta12,
        fit$stats[,7:15]
      )
    }
    if(input$DIF_method == "SIBTEST"){
      if(is.null(input$focal_name1))
        return(NULL)
      fit <-difSIBTEST(Data=Response_new, group = DIF_var,
                       focal.name = input$focal_name1)
      result <- data.frame(
        Beta = fit$Beta,
        SE = fit$SE,
        Chi_square = fit$X2,
        P.value = fit$p.value
      )
    }
    result <- cbind(round(result, digits = 3))
    rownames(result) <- colnames(Response_new)

    result %>% as.data.frame()
  })

  DIF_info_rea <- reactive({
    if(is.null(input$DIF_method))
      return(NULL)

    generateDIFInfo(method = input$DIF_method)
  })
  output$DIF_info <- renderUI({
    DIF_info_rea()
  })

  output$DIF_results <- DT::renderDataTable({
    if(is.null(input$DIF_res))
      return(NULL)
    if(is.null(input$DIF_method) )
      return(NULL)
    Response <- mydata() %>% as.data.frame()
    DIF_var <- Response[,sprintf("%s",input$DIF_variable)] %>% as.character()

    Response_new <- Response[,!colnames(Response) %in% input$DIF_all_variable]
    cat_nu <- apply(Response_new, MARGIN = 2, FUN = cat_number)

    if(any(cat_nu > 2) & input$DIF_method %in% c("SIBTEST","Mantel Haenszel")){
      stop("SIBTEST and Mantel-Haenszel methods are not suitable for polytomous response.")
    }

    DIF_ana_rea() %>% DT_dataTable_Show()

  })
  output$DIF_download <- downloadHandler(
    filename = function(){
      paste0("DIF_results.xlsx")
    },
    content = function(file){
      DIF_ana_rea() %>% openxlsx::write.xlsx(file = file, rowNames = T)
    }
  )
}
