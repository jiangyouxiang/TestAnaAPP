MIRT_module <- function(input, output, session) {
  #Import the response data---------------------------------------------------
  mydata <- reactive({

    if(is.null(input$MIRT_res))
      return("Please upload the score data.")
    inFile <- input$MIRT_res
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
  output$MIRT_Response <- DT::renderDataTable({
    Response <- mydata()%>%as.data.frame()
    # Response
    Response %>% DT_dataTable_Show()
  })

  #9. MIRT Analysis-------------------------------------------
  #Read dimension ifnormation
  dimension <- reactive({

    if(is.null(input$dimensionfile))
      return(NULL)
    inFile <- input$dimensionfile
    dataset <- bruceR::import(inFile$datapath)

    dataset1 <- dataset[,-1] %>% unlist() %>% as.numeric() %>%
      matrix(nrow = nrow(dataset),ncol = (ncol(dataset)-1))
    rownames(dataset1) <- dataset[,1]
    colnames(dataset1) <- colnames(dataset)[-1]

    if(str_detect(colnames(dataset1),pattern = " ") %>% any()){
      colnames(dataset1) <- colnames(dataset1) %>% str_replace_all(pattern = " ",replacement = "_")
    }

    data <- as.data.frame(dataset1)
    if(sum(is.na(dataset1)) >=1){
      stop("Data cannot contain missing values!")
    }
    data
  })
  #download the dimension example
  output$dimension_download <- downloadHandler(
    filename = function() {
      paste0("Dimension_example.xlsx")
    },
    content = function(file) {
      write.xlsx(data.frame("Column name" = paste0("Item",1:10),
                           "Trait_1" = c(rep(1,5),rep(0,5)),
                           "Trait_2" = c(rep(0,5),rep(1,5))),
                file)
    }
  )

  output$dimension_example <- DT::renderDataTable({
    dim_data <- dimension()
    if(is.null(input$dimensionfile)){
      return(NULL)
    }else{
      Response <- mydata()
      if(nrow(dim_data) != ncol(Response) ){#
        stop("The data in the first column of the imported file is inconsistent with the data file column name!")
      }else if( any(rownames(dim_data)%>% as.vector() == colnames(Response))==F){
        stop("The data in the first column of the imported file is inconsistent with the data file column name!
             Please note that the program does not support column names consisting of only numbers.")
      }
      return(dim_data%>% DT_dataTable_Show())
    }
  })

  dimension_output <- reactive({
    dim_data <- dimension()
    mode <- dimension_recode(Qmatrix = dim_data)
    if(input$include_cov == "Yes"){
      return(mode$COV)
    }else{
      return(mode$dim)
    }
  })
  output$dimension_code <- renderText({
    if(is.null(input$dimensionfile))
      return(NULL)
    dimension_output()%>%as.character()
  })

  ##9.1 MIRT Model fit----------------------------------------------
  MIRT_fit_rea <- reactive({
    if(is.null(model_selected(input$modelselect1)))
      return(NULL)
    dim_data <- dimension()
    Response <- mydata()

    if(nrow(dim_data) != ncol(Response) ){#
      stop("The data in the first column of the imported file is inconsistent with the data file column name!")
    }else if( any(rownames(dim_data)%>% as.vector() == colnames(Response))==F){
      stop("The data in the first column of the imported file is inconsistent with the data file column name!
             Please note that the program does not support column names consisting of only numbers.")

    }

    mirtCluster()
    if(input$include_cov == "Yes"){
      MIRT_fit <- mirt(data = Response,model = dimension_recode(Qmatrix = dim_data)$COV,
                       itemtype = model_selected(value = input$modelselect1),
                       method = est_IRT_method(input$MIRT_est_method),
                       TOL = as.numeric(input$MIRT_TOL),
                       technical = list(NCYCLES = as.numeric(input$MIRT_ncycles)))
    }else if(input$include_cov == "No"){
      MIRT_fit <- mirt(data = Response,model = dimension_recode(Qmatrix = dim_data)$dim,
                       itemtype = model_selected(value = input$modelselect1),
                       method = est_IRT_method(input$MIRT_est_method),
                       TOL = as.numeric(input$MIRT_TOL),
                       technical = list(NCYCLES = as.numeric(input$MIRT_ncycles)))
    }
    MIRT_fit
  })

  #MIRT Model fit indices
  MIRT_modelfit_relat_rea <- reactive({
    MIRT_fit <- MIRT_fit_rea()
    anova(MIRT_fit)%>%as.data.frame()%>%round(digits = 3)

  })

  output$MIRT_modelfit_relat <- DT::renderDataTable({
    if(is.null(input$MIRT_res))
      return(NULL)
    if(is.null(model_selected(input$modelselect1)))
      return(NULL)
    MIRT_modelfit_relat_rea() %>% DT_dataTable_Show()
  })

  MIRT_modelfit_rea <- reactive({
    MIRT_fit <-  MIRT_fit_rea()
    Response <- mydata()%>%as.data.frame()
    cat_all <- apply(Response, MARGIN = 2, FUN = cat_number)
    if(length(which(cat_all > 2)) >=1 ){
      fit_index <- M2(obj = MIRT_fit, type = "M2*",na.rm = T)%>%round(digits = 3)#M2*
    }else{
      fit_index <- M2(obj = MIRT_fit,na.rm = T)%>%round(digits = 3)
    }
    as.data.frame(fit_index)
  })
  output$MIRT_modelfit <- DT::renderDataTable({#
    if(is.null(input$dimensionfile))
      return(NULL)
    if(is.null(model_selected(input$modelselect1)))
      return(NULL)
    MIRT_modelfit_rea() %>% DT_dataTable_Show()
  })

  ##9.2 Independence test------------------------------------------------------------------------

  MIRT_Q3_rea  <- reactive({
    MIRT_fit <- MIRT_fit_rea()
    Q3 <- mirt::residuals(object = MIRT_fit, independent_method(input$MIRT_select_independent),
                          QMC = TRUE)%>%
      round(digits = 3)
    Q3 <- data.frame( Q3 )
    Q3
  })
  output$MIRT_Q3 <- DT::renderDataTable({
    if(is.null(input$dimensionfile))
      return(NULL)
    if(is.null(model_selected(input$modelselect1)))
      return(NULL)
    MIRT_Q3_rea() %>% DT_dataTable_Show()
  })

  ##9.3 Item fit----------------------------------------------------------------
  MIRT_itemfit_rea <- reactive({
    MIRT_fit <- MIRT_fit_rea()
    itemfit2 <- itemfit1<- mirt::itemfit(x = MIRT_fit,
                                         fit_stats = itemfit_method(input$MIRT_itemfit_method),
                                         na.rm = T)
    itemfit2[,c(2,4,5)] <- round(itemfit2[,c(2,4,5)],3)
    colnames(itemfit2) <- colnames(itemfit1)
    as.data.frame(itemfit2)
  })
  output$MIRT_itemfit <- DT::renderDataTable({
    if(is.null(input$dimensionfile))
      return(NULL)
    if(is.null(model_selected(input$modelselect1)))
      return(NULL)
    MIRT_itemfit_rea() %>% DT_dataTable_Show()
  })

  ##9.5 Item parameters--------------------------------------------------------------
  diff_trans <- function(item_par, F_n, MDISC){
    if(str_count(colnames(item_par),"d0")%>%sum() >= 1){
      item_par <- item_par[,-which(colnames(item_par)=="d0")]
    }
    if(str_count(colnames(item_par),"ak")%>%sum() >= 1){
      item_par <- item_par[,-str_which(colnames(item_par),pattern = "ak")]
    }

    di_diff <- which(str_detect(colnames(item_par), pattern = "d"))#how many difficulty
    if(length(di_diff)==1){
      colnames_new <- colnames(item_par)
      colnames_new[di_diff] <- "DIFFICULT"
      colnames(item_par) <- colnames_new
    }
    grade<- str_count(colnames(item_par), "d")%>%sum()
    dis <- MDISC#
    if(grade == 0){
      item_par[, "DIFFICULT"] <- -item_par[, "DIFFICULT"]/dis
      item_par <- cbind("MDISC" = dis, item_par)
    }else{
      diff <- item_par[,paste0("d",1:grade)]#
      diff[,"d1"] <- -item_par[,"d1"]/dis
      for (i in 2:grade) {
        diff[,paste0("d", i)] <-
          -(item_par[,paste0("d", i)]-item_par[,paste0("d", (i-1))])/dis
      }
      colnames(diff) <- paste0("b", 1:grade)
      if(length(di_diff)==1){
        item_par <- cbind("MDISC" = dis,"DIFFICULT" = item_par[, "DIFFICULT"],
                          item_par[,1:F_n], diff)
      }else{
        item_par <- cbind("MDISC" = dis,item_par[,1:F_n], diff)
      }
    }
    return(item_par)
  }

  MIRT_itempar_rea <- reactive({
    MIRT_fit <- MIRT_fit_rea()
    dim_data <- dimension()
    mode <- dimension_recode(Qmatrix = dim_data)
    if(model_selected(value = input$modelselect1) == "graded"){
      item_par <- coef(MIRT_fit, IRTparms = TRUE, simplify = TRUE)$items

      item_par_d <- str_which(colnames(item_par),pattern = "d")
      new_diff <- (-1*item_par[,item_par_d]/MDISC(MIRT_fit)) %>% as.data.frame()
      if(ncol(new_diff) == 1){
        colnames(new_diff) <- "d1"
      }
      item_par <- cbind(item_par[,-item_par_d],new_diff)

      colnames(item_par) <- colnames(item_par)%>%
        str_replace_all(pattern = "a", replacement = "Discrimination")%>%
        str_replace_all(pattern = "u", replacement = "Upper asymptote") %>%
        str_replace_all(pattern = "d",replacement = "Difficult")%>%
        str_replace_all(pattern = "g", replacement = "Guessing")

    }else{
      item_par_raw <- coef(MIRT_fit, simplify = TRUE)$items
      item_par <- diff_trans(item_par = item_par_raw,
                             F_n = mode$F_n, MDISC = MDISC(MIRT_fit))
      colnames(item_par) <- colnames(item_par)%>%
        str_replace_all(pattern = "a", replacement = "Discrimination")%>%
        str_replace_all(pattern = "u", replacement = "Upper asymptote") %>%
        str_replace_all(pattern = "b",replacement = "Difficult")%>%
        str_replace_all(pattern = "g", replacement = "Guessing")
    }
    as.data.frame(item_par) %>% round(digits = 3)
  })
  output$MIRT_itempar <- DT::renderDataTable({
    if(is.null(input$dimensionfile))
      return(NULL)
    if(is.null(model_selected(input$modelselect1)))
      return(NULL)
    MIRT_itempar_rea() %>% DT_dataTable_Show()
  })

  cov_est_rea <- reactive({
    MIRT_fit <- MIRT_fit_rea()
    if(input$include_cov == "Yes"){
      cov <- coef(object = MIRT_fit, simplify = TRUE)$cov%>%round(digits = 3)
      data.frame("Dimension" = colnames(cov),
                 cov)
    }else{
      return(data.frame("x" = "The covariance matrix is not selected."))
    }
  })
  output$cov_est <- DT::renderDataTable({
    if(is.null(input$dimensionfile))
      return(NULL)
    if(is.null(model_selected(input$modelselect1)))
      return(NULL)
    cov_est_rea() %>% DT_dataTable_Show()
  })
  # MRIT formula
  MIRT_formula_rea <- eventReactive(input$modelselect1,{
    if(is.null(input$modelselect1))
      return(NULL)
    generateMIRTInfo(input$modelselect1)
  })
  output$MIRT_info <- renderUI({
    if(is.null(input$modelselect1))
      return(NULL)
    MIRT_formula_rea()
  })


  ##9.6 Person parameters--------------------
  MIRT_person_rea <- reactive({
    MIRT_fit <- MIRT_fit_rea()
    Response <- mydata()
    dim_data <- dimension() %>% as.data.frame()
    mode <- dimension_recode(Qmatrix = dim_data )

    MIRT_person <- fscores(MIRT_fit, method = est_person_method(input$MIRT_person_est_method),
                           full.scores = T, response.pattern = Response,QMC = TRUE)
    colnames(MIRT_person) <- c(mode$F_names, paste0("SE_",mode$F_names))
    data.frame("ID" =  paste0(1:nrow(Response)), round( MIRT_person, digits = 3))
  })
  output$MIRT_person <- DT::renderDataTable({
    if(is.null(input$dimensionfile))
      return(NULL)
    if(is.null(model_selected(input$modelselect1)))
      return(NULL)
    MIRT_person_rea() %>% DT_dataTable_Show()
  })

  ##9.7 Wright map------------------------------------------------------------------
  #Dimension selection
  output$MIRT_wright_dim_select <- renderUI({
    if(is.null(input$dimensionfile)){
      selectInput(inputId = "wright_dim_select",label = "Dimension selection",
                  choices = apply(matrix(paste0("F",1:3),ncol=1),
                                  MARGIN = 1,FUN = as.vector,simplify = FALSE),
                  selectize = TRUE,selected = "All")
    }else{
      dim_data <- dimension()
      mode <- dimension_recode(Qmatrix = dim_data)
      selectInput(inputId = "wright_dim_select",label = "Dimension selection",
                  choices = apply(matrix(mode$F_names,ncol=1),
                                  MARGIN = 1,FUN = as.vector,simplify = FALSE),
                  selectize = TRUE)
    }
  })
  MIRT_wright_rea <- reactive({
    if(model_selected(input$modelselect1) != "Rasch")
      return(NULL)
    dim_data <- dimension() %>% as.data.frame()
    mode <- dimension_recode(Qmatrix = dim_data )
    if(mode$is.within_item==TRUE){
      return(NULL)
    }
    Response <- mydata()%>%as.data.frame()
    cat_all <- apply(Response, MARGIN = 2, FUN = cat_number)
    if(is.null(input$wright_dim_select)){
      wright_dim <- as.vector(mode$F_names)[1]
    }else{
      wright_dim <-  input$wright_dim_select %>% as.character()
    }

    #Item parameters
    item_par <- MIRT_itempar_rea()
    dim_items <- which(dim_data[,wright_dim] == 1)

    if(length(dim_items)==1){
      item_par_dim <- item_par[dim_items,] %>% matrix(nrow = 1)
      colnames(item_par_dim) <- colnames(item_par)
      rownames(item_par_dim) <- rownames(item_par)[dim_items]
    }else{
      item_par_dim <- item_par[dim_items,]
    }
    #Person parameters
    MIRT_person <- MIRT_person_rea()[,-1]


    thresholds <- item_par_dim[,c(str_which(colnames(item_par) %>% str_to_lower(),
                                            pattern = "difficult"))]  %>% as.data.frame()

    if(is.null(dim(thresholds))){
      thresholds <- matrix(thresholds , ncol = 1)
      rownames(thresholds ) <- rownames(item_par_dim)
      colnames(thresholds) <- "difficult"
    }

    wrightMap_new(person = MIRT_person[,wright_dim],
                  thresholds = thresholds ,
                  point_label = input$MIRT_point_label,
                  points_size = input$MIRT_wright_map_p_size,
                  p_width = input$MIRT_wright_p_width)
  })
  output$MIRT_wright <- renderPlot({
    if(is.null(input$dimensionfile))
      return(NULL)

    if(model_selected(input$modelselect1) != "Rasch")
      return(NULL)
    MIRT_wright_rea()
  },height = exprToFunction(input$MIRT_wright_map_height))

  ##9.8 ICC------------------------------------------------
  output$MIRT_ICC_item_selection <- renderUI({
    if(is.null(input$MIRT_res))
      return(NULL)
    if(is.null(model_selected(input$modelselect1)))
      return(NULL)
    Response <- mydata()%>%as.data.frame()

    checkboxGroupInput(inputId = "MIRT_ICC_item_sele",label = "Item selection",
                       choices = colnames(Response),inline = T,
                       selected = colnames(Response))
  })
  MIRT_ICC_rea <- eventReactive(c(input$MIRT_ICC_title_size,input$MIRT_ICC_label_size,
                                  input$MIRT_ICC_itemlabel_size,input$MIRT_wrap_ncol,
                                  input$MIRT_ICC_item_sele),{
    dim_data <- dimension()
    mode <- dimension_recode(Qmatrix = dim_data )
    if(mode$is.within_item==TRUE){
      return(NULL)
    }
    sim_theta <- seq(-4,4,0.01)
    Response <- mydata()%>%as.data.frame()

    cat_all <- apply(Response, MARGIN = 2, FUN = cat_number)
    #Model fit
    MIRT_fit  <- MIRT_fit_rea()
    prob <- probtrace(x = MIRT_fit, Theta = matrix(rep(sim_theta,mode$F_n),
                                                   nrow = length(sim_theta),ncol = mode$F_n))
    ncol <- MIRT_wrap_ncol_value()

    #customized items
    req(input$MIRT_ICC_item_sele) #require the input
    selected_items <- input$MIRT_ICC_item_sele
    plot_items <- which(colnames(Response) %in% selected_items)
    sele_cols <- which((sub(pattern = "\\.[^\\.]*$", replacement = ".", x = colnames(prob))) %in%
                         paste0(selected_items,".P.")) # detect the columns
    prob_plot <- prob[,sele_cols]  %>% as.data.frame()# residual matrix

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
              title_size = input$MIRT_ICC_title_size,
              xy_size = input$MIRT_ICC_label_size,
              Item_label_size = input$MIRT_ICC_itemlabel_size)

  })

  output$MIRT_ICC <-  renderPlot({
    if(is.null(input$dimensionfile))
      return(NULL)
    if(is.null(model_selected(input$modelselect1)))
      return(NULL)
    MIRT_ICC_rea()
  },height =  exprToFunction(input$MIRT_wrap_height))
  ##9.9 IIC----------------------------------------------------------
  Item_infor<- function(object,theta,Qmatrix,colnames){
    D <- ncol(Qmatrix)
    degrees <- rep(0, D)

    TRUE_information <- testinfo(object, Theta = theta[,1:D],
                                 degrees = degrees, individual = T)
    colnames(TRUE_information) <- colnames
    dim_inf <- matrix(NA, ncol = D, nrow = nrow(theta));
    colnames(dim_inf) <- paste0(colnames(Qmatrix),"-information")

    for(i in 1:D){
      items <- which(Qmatrix[,i]==1)
      if(length(items)>1){
        dim_inf[,i] <- rowSums(TRUE_information[,items])
      }else{
        dim_inf[,i] <- TRUE_information[,items]
      }
    }
    result <- list(Item_information = TRUE_information,
                   dim_information =cbind(theta, dim_inf))
    return(result)
  }

  MIRT_iteminfo_rea <- reactive({
    sim_theta <- seq(-4,4,0.01)
    MIRT_fit  <- MIRT_fit_rea()
    dim_data <- dimension()
    Response <- mydata()
    mode <- dimension_recode(Qmatrix = dim_data)
    item_info1 <- Item_infor(object = MIRT_fit,theta = matrix(rep(sim_theta,mode$F_n),
                                                              ncol = mode$F_n,
                                                              nrow = length(sim_theta)),
                             Qmatrix = dim_data,colnames = colnames(Response))$Item_information
    as.data.frame(item_info1)
  })
  # item selection
  output$MIRT_IIC_item_selection <- renderUI({
    if(is.null(input$MIRT_res))
      return(NULL)
    if(is.null(model_selected(input$modelselect1)))
      return(NULL)
    Response <- mydata()%>%as.data.frame()

    checkboxGroupInput(inputId = "MIRT_IIC_item_sele",label = "Item selection",
                       choices = colnames(Response),inline = T,
                       selected = colnames(Response))
  })
  MIRT_IIC_rea <- eventReactive(c(input$MIRT_IIC_title_size,input$MIRT_IIC_label_size,
                                  input$MIRT_IIC_itemlabel_size,input$MIRT_wrap_ncol_iic,
                                  input$MIRTiic_scale,input$MIRT_IIC_item_sele),{
    dim_data <- dimension()
    mode <- dimension_recode(Qmatrix = dim_data )
    if(mode$is.within_item==TRUE){
      return(NULL)
    }
    sim_theta <- seq(-4,4,0.01)
    Response <- mydata()%>%as.data.frame()
    item_info <- MIRT_iteminfo_rea()
    ncol <- as.numeric(input$MIRT_wrap_ncol_iic)
    # custom items
    req(input$MIRT_IIC_item_sele) #require the input
    selected_items <- input$MIRT_IIC_item_sele
    plot_items <- which(colnames(Response) %in% selected_items)
    item_info <- item_info[,plot_items] %>% as.data.frame()

    plot_wrap(theta = sim_theta,
              y_matrix = item_info,
              lines = "IIC",
              main_vector = colnames(Response)[plot_items],
              y_lab = "Information",
              x_lab = "Theta",
              title = "Item Information Curve",
              ncol = ncol,
              scale = input$MIRTiic_scale %>% stringr::str_to_lower(),
              title_size = input$MIRT_IIC_title_size,
              xy_size = input$MIRT_IIC_label_size,
              Item_label_size = input$MIRT_IIC_itemlabel_size)
  })
  output$MIRT_IIC <- renderPlot({
    if(is.null(input$dimensionfile))
      return(NULL)
    if(is.null(model_selected(input$modelselect1)))
      return(NULL)
    MIRT_IIC_rea()
  },height =  exprToFunction(input$MIRT_wrap_height_iic))

  ##9.10 TIC---------------------------------------------------------------------
  output$MIRT_TIC_dim_select <- renderUI({

    if(is.null(input$dimensionfile)){
      selectInput(inputId = "MIRT_dim_select",label = "Dimension selection",
                  choices = apply(matrix(paste0("F",1:3),ncol=1),
                                  MARGIN = 1,FUN = as.vector,simplify = FALSE),
                  selectize = TRUE)
    }else{
      dim_data <- dimension()
      mode <- dimension_recode(Qmatrix = dim_data)
      selectInput(inputId = "MIRT_dim_select",label = "Dimension selection",
                  choices = apply(matrix(mode$F_names,ncol=1),
                                  MARGIN = 1,FUN = as.vector,simplify = FALSE),
                  selectize = TRUE)
    }
  })
  MIRT_TIC_rea<- reactive({
    dim_data <- dimension()
    mode <- dimension_recode(Qmatrix = dim_data )
    if(mode$is.within_item==TRUE){
      return(NULL)
    }
    if(is.null(input$MIRT_dim_select))
      return(NULL)
    sim_theta <- seq(-4,4,0.01)
    MIRT_fit  <- MIRT_fit_rea()
    dim_data <- dimension()
    Response <- mydata()
    mode <- dimension_recode(Qmatrix = dim_data)
    item_info1 <- Item_infor(object = MIRT_fit,theta = matrix(rep(sim_theta,mode$F_n),
                                                              ncol = mode$F_n,
                                                              nrow = length(sim_theta)),
                             Qmatrix = dim_data,colnames = colnames(Response))$dim_information
    colnames(item_info1) <- c(mode$F_names, paste0(mode$F_names,"infor"))
    sim_theta1_infor1 <- item_info1[,c(input$MIRT_dim_select,paste0(input$MIRT_dim_select,"infor"))]
    test_infor<- plotrix::twoord.plot(lx = sim_theta1_infor1[,1],ly = sim_theta1_infor1[,2],
                                      rx = sim_theta1_infor1[,1],ry = 1/sqrt(sim_theta1_infor1[,2]),
                                      main = paste0("Test Information and Measurement Error of ",input$MIRT_dim_select),
                                      ylab = "Test information",
                                      rylab = "Measurement error",
                                      xlab = "Latent Trait",
                                      rcol = "red",
                                      lytickpos = seq(0, max(sim_theta1_infor1[,2]),
                                                      ceiling(max(sim_theta1_infor1[,2])/5)),
                                      lylim = c(0,(max(sim_theta1_infor1[,2])+0.5)),

                                      type = c("l","p"),rpch = 1)
    text(x = sim_theta1_infor1[which.max(sim_theta1_infor1[,2]),1],
         y = max(sim_theta1_infor1[,2])
         ,labels = paste(round(max(sim_theta1_infor1[,2]),3)))

  })
  output$MIRT_TIC <- renderPlot({
    if(is.null(input$dimensionfile))
      return(NULL)
    if(is.null(model_selected(input$modelselect1)))
      return(NULL)
    MIRT_TIC_rea()
  })
  ##9.11 MIRT figures' setup----------------------------------------
  MIRT_wrap_ncol_value <- reactive({
    as.numeric(input$MIRT_wrap_ncol)
  })

  #Display figures on UI
  output$MIRT_ICC1 <- renderUI({
    S <- MIRT_wrap_ncol_value()
    plotOutput(outputId = "MIRT_ICC", height = paste0(input$MIRT_wrap_height,"px"))
  })
  output$MIRT_wright1 <- renderUI({
    plotOutput(outputId = "MIRT_wright", height = paste0(input$MIRT_wright_map_height,"px"),width = "auto")
  })
  output$MIRT_IIC1 <- renderUI({
    S <- MIRT_wrap_ncol_value()
    plotOutput(outputId = "MIRT_IIC", height = paste0(input$MIRT_wrap_height_iic,"px"))
  })


  ##9.12 Download figures-----------------------------------------------------------------
  output$MIRT_wrightfile <- downloadHandler(
    filename = function(){
      paste0("MIRT_WrightMap.jpeg")
    },
    content = function(file){
      jpeg(file, width = input$MIRT_wright_map_height*1.618, height = input$MIRT_wright_map_height)
      MIRT_wright_rea() %>% print()
      dev.off()
    }
  )
  output$MIRT_ICCfile <- downloadHandler(
    filename = function(){
      paste0("MIRT_item_characteristic_curve.jpeg")
    },
    content = function(file){
      jpeg(file, width = input$MIRT_wrap_height*1.618, height = input$MIRT_wrap_height)
      MIRT_ICC_rea() %>% print()
      dev.off()
    }
  )
  output$MIRT_IICfile <- downloadHandler(
    filename = function(){
      paste0("MIRT_item_information_curve.jpeg")
    },
    content = function(file){
      jpeg(file, width = input$MIRT_wrap_height_iic*1.618, height = input$MIRT_wrap_height_iic)
      MIRT_IIC_rea() %>% print()
      dev.off()
    }
  )

  ##9.12 Download results----------------------------------------------------------
  output$MIRT_resultfile <- downloadHandler(
    filename = function(){
      paste0("MIRT_results.xlsx")
    },
    content = function(file){

      MIRT_fit  <- MIRT_fit_rea()
      dim_data <- dimension() %>% as.data.frame()
      Response <- mydata() %>% as.data.frame()
      mode <- dimension_recode(Qmatrix = dim_data)
      est_theta <- MIRT_person_rea()[,2:(ncol(dim_data)+1)]
      item_info1 <- Item_infor(object = MIRT_fit,
                               theta = est_theta,
                               Qmatrix = dim_data,
                               colnames = colnames(Response))
      item_info <- item_info1$Item_information
      colnames(item_info ) <- colnames(Response)
      dim_infor <- item_info1$dim_information
      colnames(dim_infor) <- c(mode$F_names, paste0(mode$F_names, "_Information"))

      sim_theta <- seq(-4,4,0.01)
      prob <- probtrace(x = MIRT_fit, Theta = matrix(rep(sim_theta,mode$F_n),
                                                     nrow = length(sim_theta),
                                                     ncol = mode$F_n))
      #information value for plot IIC
      sim_theta_infor <- Item_infor(object = MIRT_fit,
                                    theta = matrix(rep(sim_theta,mode$F_n),
                                                   nrow = length(sim_theta),
                                                   ncol = mode$F_n),
                                    Qmatrix = dim_data,
                                    colnames = colnames(Response))



      if(mode$is.within_item==FALSE){
        datalist <- list("Score data" = Response,
                         "Dimension" = dim_data ,
                         "Model fit" = MIRT_modelfit_rea(),
                         "Dependence test" = MIRT_Q3_rea(),
                         "Item fit" = MIRT_itemfit_rea(),
                         "Item parameters" = MIRT_itempar_rea(),
                         "Person parameters" = MIRT_person_rea(),
                         "Response probability" = data.frame("Simulated theta" = sim_theta,
                                                             prob),
                         "Item information" = item_info,
                         "Test information" = dim_infor,
                         "Item information for plot" = data.frame("Simulated theta" = sim_theta,
                                                                  sim_theta_infor$Item_information),
                         "Test information for plot" = sim_theta_infor$dim_information)
      }else{
        datalist <- list("Score data" = Response,
                         "Dimension" = dim_data ,
                         "Model fit" = MIRT_modelfit_rea(),
                         "Dependence test" = MIRT_Q3_rea(),
                         "Item fit" = MIRT_itemfit_rea(),
                         "Item parameters" = MIRT_itempar_rea(),
                         "Person parameters" = MIRT_person_rea(),
                         "Response probability" = data.frame("Simulated theta" = sim_theta,
                                                             prob),
                         "Item information" = item_info)
      }
      openxlsx::write.xlsx(x = datalist, file = file, rowNames = T)
    }
  )

  ##9.13 Downlaod analysis report-------------------------------------------
  output$MIRT_report <- downloadHandler(
    filename = function(){
      paste0("MIRT_Analysis_Report.docx")
    },
    content = function(file){
      Response <- mydata() %>% as.data.frame()
      #Selections

      model <- input$modelselect1
      MIRT_est_method <- input$MIRT_est_method
      MIRT_person_est_method <- input$MIRT_person_est_method
      MIRT_itemfit_method <- input$MIRT_itemfit_method
      #Model fit
      MIRT_fit <- MIRT_fit_rea()
      dimension <- dimension()  %>% as.data.frame()
      mode <- dimension_recode(Qmatrix = dimension)
      MIRT_modelfit_relat <- MIRT_modelfit_relat_rea()
      MIRT_modelfit <- MIRT_modelfit_rea()
      #Hypothesis test
      MIRT_select_independent <- independent_method(input$MIRT_select_independent)
      MIRT_Q3 <- MIRT_Q3_rea()

      #Item fit
      MIRT_itemfit <- MIRT_itemfit_rea()
      #Item parameters
      MIRT_itempar <- MIRT_itempar_rea()
      #Figures
      MIRT_wright <- MIRT_wright_rea()
      MIRT_ICC <- MIRT_ICC_rea()
      MIRT_TIC <- MIRT_TIC_rea()

      MIRT_IIC <- MIRT_IIC_rea()
      #Test information
      sim_theta <- seq(-4,4,0.01)
      Response <- mydata() %>% as.data.frame()
      item_info1 <- Item_infor(object = MIRT_fit,theta = matrix(rep(sim_theta,mode$F_n),
                                                                ncol = mode$F_n,
                                                                nrow = length(sim_theta)),
                               Qmatrix = dimension, colnames = colnames(Response))$dim_information
      colnames(item_info1) <- c(mode$F_names, paste0(mode$F_names,"infor"))

      wright_map_height <- input$MIRTreport_wright_height
      wrap_height_value <- input$MIRTreport_wrap_height
      wrap_height_value_iic <- input$MIRTreport_wrap_height

      #highlight some values
      MIRTreport_Q3_highlight <- input$MIRTreport_Q3_h
      MIRTreport_alpha_highlight <- input$MIRTreport_alpha_h

      #Export analysis report
      path_sys <- system.file("rmd", "MIRT_Analysis_Report.Rmd", package = "TestAnaAPP")
      src <- normalizePath(path_sys)
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src,"MIRT_Analysis_Report.Rmd", overwrite = TRUE)

      rmarkdown::render("MIRT_Analysis_Report.Rmd",output_file = file)

    }
  )
}
