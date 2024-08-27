UIRT_module <- function(input, output, session) {
  #Import the response data---------------------------------------------------
  mydata <- reactive({

    if(is.null(input$IRT_res))
      return("Please upload the score data.")
    inFile <- input$IRT_res
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
  output$IRT_Response <- DT::renderDataTable({
    Response <- mydata()%>%as.data.frame()
    # Response
    Response %>% DT_dataTable_Show()
  })

  #8. Unidimensional IRT analysis---------------------------------------
  IRT_fit_reactive <- reactive( {
    if(is.null(input$IRT_res))
      return(NULL)

    if(is.null(model_selected(input$modelselect))){
      return(NULL)
    }else{
      Response <- mydata()%>%as.data.frame()
      model <- model_selected(input$modelselect)

      IRT_fit <- mirt(data = Response, model = 1, itemtype = model,
                      method = est_IRT_method(input$IRT_est_method),
                      TOL = as.numeric(input$IRT_TOL),
                      technical = list(NCYCLES = as.numeric(input$IRT_ncycles)))
      IRT_fit
    }
  })
  ###8.1 IRTModel fit--------------------------------------------------------
  IRT_modelfit_relat_rea <- reactive({
    IRT_fit <- IRT_fit_reactive()
    anova(IRT_fit)%>%as.data.frame()%>%round(digits = 3)

  })

  output$IRT_modelfit_relat <- DT::renderDataTable({
    if(is.null(input$IRT_res))
      return(NULL)
    if(is.null(model_selected(input$modelselect)))
      return(NULL)
    IRT_modelfit_relat_rea() %>% DT_dataTable_Show()
  })

  IRT_modelfit_rea <- reactive({
    IRT_fit <- IRT_fit_reactive()
    Response <- mydata()%>%as.data.frame()
    cat_all <- apply(Response, MARGIN = 2, FUN = cat_number)
    if(length(which(cat_all > 2)) >=1 ){
      fit_index <- M2(obj = IRT_fit, type = "M2*",na.rm = T)%>%round(digits = 3)#M2*
    }else{
      fit_index <- M2(obj = IRT_fit, na.rm = T)%>%round(digits = 3)
    }
    as.data.frame(fit_index)
  })
  output$IRT_modelfit <- DT::renderDataTable({
    if(is.null(input$IRT_res))
      return(NULL)
    if(is.null(model_selected(input$modelselect)))
      return(NULL)
    IRT_modelfit_rea() %>% DT_dataTable_Show()

  })

  ###8.2 Independence test----------------------------
  IRT_Q3_rea  <- reactive({
    IRT_fit <- IRT_fit_reactive()
    Q3 <- mirt::residuals(object = IRT_fit,
                          independent_method(input$IRT_select_independent))%>%
      round(digits = 3)
    Q3 <- data.frame(Q3)
    Q3

  })
  output$IRT_Q3 <- DT::renderDataTable({
    if(is.null(input$IRT_res))
      return(NULL)
    if(is.null(model_selected(input$modelselect)))
      return(NULL)
    IRT_Q3_rea()  %>% DT_dataTable_Show()
  })


  ###8.3 Item fit-----------------------------------
  IRT_itemfit_rea <- reactive({
    IRT_fit <- IRT_fit_reactive()
    itemfit2 <- itemfit1<- mirt::itemfit(x = IRT_fit,
                                         fit_stats = itemfit_method(input$IRT_itemfit_method),
                                         na.rm = T)
    itemfit2[,c(2,4,5)] <- round(itemfit2[,c(2,4,5)],3)
    colnames(itemfit2) <- colnames(itemfit1)
    as.data.frame(itemfit2)
  })
  output$IRT_itemfit <- DT::renderDataTable({
    if(is.null(input$IRT_res))
      return(NULL)
    if(is.null(model_selected(input$modelselect)))
      return(NULL)
    IRT_itemfit_rea() %>% DT_dataTable_Show()
  })

  ###8.4 Item parameters-----------------------
  IRT_itempar_rea <- reactive({
    IRT_fit <- IRT_fit_reactive()
    item_par <- coef(IRT_fit, IRTpars = TRUE, simplify = TRUE)$items %>% as.data.frame() %>% round(digits = 3)

    colnames(item_par) <- colnames(item_par) %>%
      str_replace_all(pattern = "a", replacement = "Discrimination")%>%
      str_replace_all(pattern = "u", replacement = "Upper asymptote") %>%
      str_replace_all(pattern = "b",replacement = "Difficult")%>%
      str_replace_all(pattern = "g", replacement = "Guessing")
    data.frame( item_par)
  })
  output$IRT_itempar <- DT::renderDataTable({
    if(is.null(input$IRT_res))
      return(NULL)
    if(is.null(model_selected(input$modelselect)))
      return(NULL)

    IRT_itempar_rea() %>% DT_dataTable_Show()
  })

  # model formula
  IRT_formula_rea <- reactive({
    if(is.null(input$modelselect))
      return(NULL)
    generateIRTInfo(input$modelselect)
  })
  output$IRT_info <- renderUI({
    IRT_formula_rea()
  })
  ###8.5 Person parameters--------------------
  IRT_person_rea <- reactive({
    IRT_fit <- IRT_fit_reactive()
    Response <- mydata()%>%as.data.frame()
    IRT_person <- fscores(IRT_fit, method = est_person_method(input$IRT_person_est_method),
                          full.scores = T, response.pattern = Response)
    colnames(IRT_person) <- c("theta", "SE")
    data.frame("ID"= paste0(1:nrow(Response)), round(IRT_person, digits = 3))
  })
  output$IRT_person <- DT::renderDataTable({
    if(is.null(input$IRT_res))
      return(NULL)
    if(is.null(model_selected(input$modelselect)))
      return(NULL)
    IRT_person_rea() %>% DT_dataTable_Show()
  })

  ###8.6 WrightMap------
  IRT_wright_rea <- reactive({
    if(model_selected(input$modelselect) != "Rasch")
      return(NULL)
    Response <- mydata()%>%as.data.frame()
    cat_all <- apply(Response, MARGIN = 2, FUN = cat_number)

    item_par <- IRT_itempar_rea()

    IRT_person <- IRT_person_rea()

    thresholds <- item_par[,str_which(colnames(item_par) %>% str_to_lower(),
                                      pattern = "difficult")] %>% as.data.frame()

    if(is.null(dim(thresholds))){
      thresholds <- matrix(thresholds , ncol = 1)
      rownames(thresholds ) <- rownames(item_par)
      colnames(thresholds) <- "difficult"
    }

    wrightMap_new(person = as.numeric(IRT_person[,2]),
                  thresholds = thresholds,
                  point_label = input$IRT_point_label,
                  points_size = input$IRT_wright_map_p_size,
                  p_width =  input$IRT_wright_p_width )

  })
  output$IRT_wright <- renderPlot({
    if(is.null(input$IRT_res))
      return(NULL)

    if(model_selected(input$modelselect) != "Rasch")
      return(NULL)
    IRT_wright_rea()
  },height = exprToFunction(input$IRT_wright_map_height))

  ##8.7 ICC---------------------------------------
  output$IRT_ICC_item_selection <- renderUI({
    if(is.null(input$IRT_res))
      return(NULL)
    if(is.null(model_selected(input$modelselect)))
      return(NULL)
    Response <- mydata()%>%as.data.frame()

    checkboxGroupInput(inputId = "IRT_ICC_item_sele",label = "Item selection",
                       choices = colnames(Response),inline = T,
                       selected = colnames(Response))
  })
  IRT_ICC_rea <- eventReactive(c(input$IRT_ICC_itemlabel_size,input$IRT_ICC_title_size,
                                input$IRT_ICC_label_size,input$wrap_ncol,
                                input$IRT_ICC_item_sele),{
    sim_theta <- seq(-4,4,0.01)
    Response <- mydata()%>%as.data.frame()
    cat_all <- apply(Response, MARGIN = 2, FUN = cat_number)
    #Model fit
    IRT_fit  <- IRT_fit_reactive()
    prob <- probtrace(x = IRT_fit, Theta = sim_theta)
    ncol <- wrap_ncol_Value()
    #customized items
    if (is.null(input$IRT_ICC_item_sele)) {
      selected_items <- colnames(Response)
    }else{
      selected_items <- input$IRT_ICC_item_sele
    }
    plot_items <- which(colnames(Response) %in% selected_items)
    sele_cols <- which(( sub(pattern = "\\.[^\\.]*$", replacement = ".", x = colnames(prob))) %in%
                          paste0(selected_items,".P.")) # detect the columns
    prob_plot <- prob[,sele_cols] %>% as.data.frame()# residual matrix
    plot_wrap(theta = sim_theta,
              y_matrix = prob_plot,
              lines = "ICC",
              grade_vector = cat_all[plot_items],
              main_vector = colnames(Response)[plot_items],
              y_lab = "Probability",
              x_lab = "Theta",
              title = "Item Characteristic Curve",
              ncol = ncol,
              scale = "fixed",
              title_size = input$IRT_ICC_title_size,
              xy_size = input$IRT_ICC_label_size,
              Item_label_size = input$IRT_ICC_itemlabel_size)
  })

  output$IRT_ICC <-  renderPlot({
    if(is.null(input$IRT_res))
      return(NULL)
    if(is.null(model_selected(input$modelselect)))
      return(NULL)
    IRT_ICC_rea()
  },height =  exprToFunction(input$wrap_height))
  ##8.8 IIC---------------------------------------------------------------------
  IRT_iteminfo_rea <- reactive({
    sim_theta <- seq(-4,4,0.01)
    Response <- mydata()%>%as.data.frame()
    IRT_fit  <- IRT_fit_reactive()
    item_info <- testinfo(x = IRT_fit, Theta = sim_theta, individual = T)
    colnames(item_info) <- colnames(Response)
    as.data.frame(item_info)
  })
  # item selection
  output$IRT_IIC_item_selection <- renderUI({
    if(is.null(input$IRT_res))
      return(NULL)
    if(is.null(model_selected(input$modelselect)))
      return(NULL)
    Response <- mydata()%>%as.data.frame()
    checkboxGroupInput(inputId = "IRT_IIC_item_sele",label = "Item selection",
                       choices = colnames(Response),inline = T,
                       selected = colnames(Response))
  })
  IRT_IIC_rea <- eventReactive(c(input$IRT_IIC_itemlabel_size,input$IRT_IIC_label_size,
                                 input$IRT_IIC_title_size,input$wrap_ncol_iic,input$IRTiic_scale,
                                 input$IRT_IIC_item_sele),{
    sim_theta <- seq(-4,4,0.01)
    Response <- mydata()%>%as.data.frame()

    item_info <- IRT_iteminfo_rea()
    ncol <- as.numeric(input$wrap_ncol_iic)
    #customized items
    if (is.null(input$IRT_IIC_item_sele)) {
      selected_items <- colnames(Response)
    }else{
      selected_items <- input$IRT_IIC_item_sele
    }
    plot_items <- which(colnames(Response) %in% selected_items)
    item_info <- item_info[,plot_items]  %>% as.data.frame()

    plot_wrap(theta = sim_theta,
              y_matrix = item_info,
              lines = "IIC",
              main_vector = colnames(Response)[plot_items],
              y_lab = "Information",
              x_lab = "Theta",
              title = "Item Information Curve",
              ncol = ncol,
              scale = input$IRTiic_scale %>% stringr::str_to_lower(),
              title_size = input$IRT_IIC_title_size,
              xy_size = input$IRT_IIC_label_size,
              Item_label_size = input$IRT_IIC_itemlabel_size)
  })
  output$IRT_IIC <- renderPlot({
    if(is.null(input$IRT_res))
      return(NULL)
    if(is.null(model_selected(input$modelselect)))
      return(NULL)

    IRT_IIC_rea()
  },height =  exprToFunction(input$wrap_height_iic))

  ##8.9 TIC----------------------------------------------------------------------------
  IRT_TIC_rea<- reactive({

    IRT_fit  <- IRT_fit_reactive()
    test_infor <- mirt::plot(x = IRT_fit, type = "infoSE",
                             theta_lim = c(-4,4))
    test_infor
  })
  output$IRT_TIC <- renderPlot({
    if(is.null(input$IRT_res))
      return(NULL)
    if(is.null(model_selected(input$modelselect)))
      return(NULL)
    IRT_TIC_rea()
  })
  ##8.10 IRT figures' setup----------------------------------------------------

  wrap_ncol_Value <- reactive({
    as.numeric(input$wrap_ncol)
  })
  #Output height
  output$IRT_ICC1 <- renderUI({
    ncol <- wrap_ncol_Value()
    plotOutput(outputId = "IRT_ICC", height = paste0(input$wrap_height,"px"))
  })
  output$IRT_wright1 <- renderUI({
    plotOutput(outputId = "IRT_wright", height = paste0(input$IRT_wright_map_height,"px"))
  })
  output$IRT_IIC1 <- renderUI({
    ncol <- wrap_ncol_Value()
    plotOutput(outputId = "IRT_IIC", height = paste0(input$wrap_height_iic,"px"))
  })

  ##8.11 Download figures------------------------------------------------------------
  output$IRT_wrightfile <- downloadHandler(
    filename = function(){
      paste0("IRT_WrightMap.jpeg")
    },
    content = function(file){
      jpeg(file, width =  input$IRT_wright_map_height*1.618, height = input$IRT_wright_map_height)
      IRT_wright_rea() %>% print()
      dev.off()
    }
  )
  output$IRT_ICCfile <- downloadHandler(
    filename = function(){
      paste0("IRT_item_characteristic_curve.jpeg")
    },
    content = function(file){
      jpeg(file, width = input$wrap_height*1.618, height = input$wrap_height)
      IRT_ICC_rea()%>% print()
      dev.off()
    }
  )
  output$IRT_IICfile <- downloadHandler(
    filename = function(){
      paste0("IRT_item_information_curve.jpeg")
    },
    content = function(file){
      jpeg(file, width = input$wrap_height_iic*1.618, height = input$wrap_height_iic)
      IRT_IIC_rea()%>% print()
      dev.off()
    }
  )
  output$IRT_TICfile <- downloadHandler(
    filename = function(){
      paste0("IRT_test_information_curve.jpeg")
    },
    content = function(file){
      jpeg(file, width = 1200, height = 800)
      IRT_TIC_rea() %>% print()
      dev.off()
    }
  )
  ##8.12 Download results--------------------------------------------------------------
  output$IRT_resultfile <- downloadHandler(
    filename = function(){
      paste0("IRT_results.xlsx")
    },
    content = function(file){
      est_theta <-  IRT_person_rea()[,2]%>%as.numeric()#True theta
      IRT_fit  <- IRT_fit_reactive()
      Response <- mydata()%>%as.data.frame()
      item_info <- testinfo(x = IRT_fit, Theta = est_theta, individual = T)
      colnames(item_info) <- colnames(Response)

      sim_theta <- seq(-4,4,0.01)
      prob <- probtrace(x = IRT_fit, Theta = sim_theta)

      datalist <- list("Absolute model fit" = IRT_modelfit_rea(),
                       "Relative model fit" = IRT_modelfit_relat_rea(),
                       "Dependence test" = IRT_Q3_rea(),
                       "Item fit" = IRT_itemfit_rea(),
                       "Item parameters" = IRT_itempar_rea(),
                       "Person parameters" = IRT_person_rea(),
                       "Response probability" = data.frame("Theta" = sim_theta,
                                                           prob),
                       "Item information" = data.frame("Estimated theta"  = est_theta,
                                                       item_info),
                       "Test Information" = data.frame("Estimated theta"  = est_theta,
                                                       "Test Information" = rowSums(item_info),
                                                       "Messurement Error" = 1/sqrt(rowSums(item_info)))
      )
      openxlsx::write.xlsx(x = datalist, file = file, rowNames = T)
    }
  )
  Unidi_test <- reactive({
    if(is.null(input$IRT_res))
      return(NULL)
    Response <- mydata()%>%as.data.frame()
    colnames(Response) <- paste0("Item",1:ncol(Response))
    fit <- bruceR::EFA(data = Response , var = "Item", items = 1:ncol(Response),
                       method = EFA_method(input$EFA_method),
                       rotation = EFA_rotation_method(input$rotation_method))
    fit
  })

  ##8.13 Downlaod analysis report-------------------------------------------
  output$IRT_report <- downloadHandler(
    filename = function(){
      paste0("IRT_Analysis_Report.docx")
    },
    content = function(file){
      Response <- mydata()%>%as.data.frame()
      #Selections
      model <- input$modelselect
      IRT_est_method <- input$IRT_est_method
      IRT_person_est_method <- input$IRT_person_est_method
      cat_all <- apply(Response, MARGIN = 2, FUN = cat_number)
      if(all(cat_all == 2)){
        EFA_method <- "Principal Axis Factor Analysis"
      }else{
        EFA_method <- input$EFA_method
      }
      rotation_method <- input$rotation_method
      IRT_itemfit_method <- input$IRT_itemfit_method
      #Model fit
      IRT_modelfit_relat <- IRT_modelfit_relat_rea()
      IRT_modelfit <- IRT_modelfit_rea()
      #Hypothesis test
      fit <- Unidi_test()
      CTT_EFA_eigenvalues <- data.frame( rownames(fit$eigenvalues),
                                         as.data.frame(fit$eigenvalues)%>%round(digits = 3))
      IRT_select_independent <- independent_method(input$IRT_select_independent)
      IRT_Q3 <- IRT_Q3_rea()

      #Item fit
      IRT_itemfit <- IRT_itemfit_rea()
      #Item parameters
      IRT_itempar <- IRT_itempar_rea()
      #Figures
      IRT_wright <- IRT_wright_rea()
      IRT_ICC <- IRT_ICC_rea()
      IRT_TIC <- IRT_TIC_rea()
      IRT_IIC <- IRT_IIC_rea()

      wright_map_height <- input$IRTreport_wright_height
      wrap_height_value <- input$IRTreport_wrap_height
      wrap_height_value_iic <- input$IRTreport_wrap_height

      # highlight the significant results
      IRTreport_Q3_highlight <- input$IRTreport_Q3_h
      IRTreport_alpha_highlight <- input$IRTreport_alpha_h

      #Export analysis report
      path_sys <- system.file("rmd", "IRT_Analysis_Report.Rmd", package = "TestAnaAPP")
      src <- normalizePath(path_sys)
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src,"IRT_Analysis_Report.Rmd", overwrite = TRUE)

      rmarkdown::render("IRT_Analysis_Report.Rmd",output_file = file)

    }
  )
  # output$IRTreport_html <- renderUI({
  #   #Selections
  #   model <- input$modelselect
  #   IRT_est_method <- input$IRT_est_method
  #   IRT_person_est_method <- input$IRT_person_est_method
  #   EFA_method <- input$EFA_method
  #   rotation_method <- input$rotation_method
  #   IRT_itemfit_method <- input$IRT_itemfit_method
  #   #Model fit
  #   IRT_modelfit_relat <- IRT_modelfit_relat_rea()
  #   IRT_modelfit <- IRT_modelfit_rea()
  #   #Hypothesis test
  #   fit <- Unidi_test()
  #   CTT_EFA_eigenvalues <- data.frame( rownames(fit$eigenvalues),
  #                                      as.data.frame(fit$eigenvalues)%>%round(digits = 3))
  #   IRT_select_independent <- independent_method(input$IRT_select_independent)
  #   IRT_Q3 <- IRT_Q3_rea()
  #
  #   #Item fit
  #   IRT_itemfit <- IRT_itemfit_rea()
  #   #Item parameters
  #   IRT_itempar <- IRT_itempar_rea()
  #   #Figures
  #   IRT_wright <- IRT_wright_rea()
  #   IRT_ICC <- IRT_ICC_rea()
  #   IRT_TIC <- IRT_TIC_rea()
  #   IRT_IIC <- IRT_IIC_rea()
  #
  #   wright_map_height <- input$IRT_wright_map_height
  #   wrap_height_value <- input$wrap_height
  #
  #
  #   wrap_height_value_iic <- input$wrap_height_iic
  #
  #   #Export analysis report
  #   path_sys <- system.file("rmd", "IRT_Analysis_Report.Rmd", package = "TestAnaAPP")
  #   src <- normalizePath(path_sys)
  #   owd <- setwd(tempdir())
  #   on.exit(setwd(owd))
  #   file.copy(src,"IRT_Analysis_Report_html.Rmd", overwrite = TRUE)
  #
  #
  #   includeHTML(render("IRT_Analysis_Report_html.Rmd",
  #                      output_format = html_document(toc = TRUE,
  #                                                    toc_depth = 3,
  #                                                    toc_float = TRUE,
  #                                                    number_sections = TRUE,
  #                                                    anchor_sections = TRUE)))
  #
  # })

}
