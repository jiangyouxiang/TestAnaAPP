#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny dplyr stringr mirt ggplot2 openxlsx bruceR tidySEM cowplot plotrix config golem
#' @noRd
app_server <- function(input, output, session) {
  options(shiny.maxRequestSize = 1024^4)
  #Input selections--------------------------------------------
  EFA_method <- function(value){
    if(value == "Principal Component Analysis"){
      return("pca")
    }
    if(value == "Principal Axis Factor Analysis"){
      return("pa")
    }
    if(value == "Maximum Likelihood Factor Analysis"){
      return("ml")
    }
    if(value == "Minimum Residual Factor Analysis"){
      return("minres")
    }
    if(value == "Unweighted Least Squares Factor Analysis"){
      return("uls")
    }
    if(value == "Ordinary Least Squares Factor Analysis"){
      return("ols")
    }
    if(value == "Weighted Least Squares Factor Analysis"){
      return("wls")
    }
    if(value == "Generalized Least Squares Factor Analysis"){
      return("gls")
    }
    if(value == "Alpha Factor Analysis (Kaiser & Coffey, 1965)"){
      return("alpha")
    }
  }
  EFA_rotation_method <- function(value){
    if(value == "Varimax"){
      return("varimax")
    }
    if(value == "Direct Oblimin"){
      return("oblimin")
    }
    if(value == "Promax"){
      return("promax")
    }
    if(value == "Quartimax"){
      return("quartimax")
    }
    if(value == "Equamax"){
      return("equamax")
    }
  }
  model_selected <- function(value){
    if(value == "Rasch model (1PL)"){
      return("Rasch")
    }
    if(value == "Two parameters logistic model (2PL)"){
      return("2PL")
    }
    if(value ==  "Three parameters logistic model (3PL)"){
      return("3PL")
    }
    if(value ==  "Four parameters logistic model (4PL)"){
      return("4PL")
    }
    if(value ==  "Partial credit model (PCM)"){
      return("Rasch")
    }
    if(value ==  "Graded response model (GRM)"){
      return("graded")
    }
    if(value ==  "Generalized partial credit model (GPCM)"){
      return("gpcm")
    }
    if(value == "NULL"){
      return(NULL)
    }
  }
  est_IRT_method <- function(value){
    if(value == "standard EM algorithm"){
      return("EM")
    }
    if(value == "quasi-Monte Carlo EM estimation"){
      return("QMCEM")
    }
    if(value == "Monte Carlo EM estimation"){
      return("MCEM")
    }
    if(value == "Stochastic EM algorithm"){
      return("SEM")
    }
  }
  est_person_method <- function(value){
    if(value == "expected a-posteriori (EAP)"){
      return("EAP")
    }
    if(value =="maximum a-posteriori (MAP)"){
      return("MAP")
    }
    if(value == "maximum likelihood (ML)"){
      return("ML")
    }
    if(value == "weighted likelihood estimation (WLE)"){
      return("WLE")
    }
  }
  independent_method <- function(value){#独立性检验所用的方法
    if(value == "LD-X2 (Chen & Thissen, 1997)"){
      return("LD")
    }
    if(value == "Q3 (Yen, 1984)"){
      return("Q3")
    }
  }
  itemfit_method <- function(value){
    if(value == "chi-squared test (Kang & Chen, 2007)"){
      return("S_X2")
    }
    if(value == "Zh (Drasgow, Levine, & Williams, 1985)"){
      return("Zh")
    }
    if(value == "chi-squared method (Bock,1972)"){
      return("X2")
    }
    if(value == "G2 statistic (McKinley & Mills, 1985)"){
      return("G2")
    }
    if(value == "Stone’s (2000) fit statistics"){
      return("X2*")
    }
  }

  #Import the response data---------------------------------------------------
  mydata <- reactive({

    if(is.null(input$res_data))
      return("Please upload the score data.")
    inFile <- input$res_data
    dataset <- bruceR::import(inFile$datapath)
    data <- as.data.frame(dataset)
    if(length(which(is.character(data))) >=1){
      return("Data can not contain any string data.")
    }
    data
  })

  #Export the response data-------------------------------------------------
  output$Response <- DT::renderDataTable({
    Response <- mydata()%>%as.data.frame()
    # Response
    Response %>% DT_dataTable_Show()
  })

  #Introduction of this platform-----------------------------------
  output$info <- renderText({
    paste(shiny::p(strong('Package: '), "TestAnaAPP"),
          shiny::p(strong('Dependence: '), "mirt, shiny, shinydashboard, tidyverse, ggplot2 et al."),
          shiny::p(strong('Description: '), "This application enables exploratory factor analysis,
    confirmatory factor analysis, classical measurement theory analysis,
    single and multi-dimensional item response theory analysis, through
    the shiny interactive interface. It also facilitates the visualization
    of the results. Users can easily download the analysis results from the
    interactive interface. Additionally, users can download a concise report
    about item and test quailty throught the interactive interface."),
          shiny::p(strong('Anthor: '), "Youxiang Jiang"),
          shiny::p(strong('Email: '), tags$a(href="mailto:jiangyouxiang34@163.com", "jiangyouxiang34@163.com"))
         # shiny::p(strong())

    )
  })
  #1. Descriptive statistics--------------------------------------------
  desc_rea <- reactive({
    if(is.null(input$res_data))
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
                       "Kurtisis" = round(desc$Kurtosis,  digits = 3))
    desc
  })
  output$CTT_summary <- DT::renderDataTable({
    desc_rea() %>% DT_dataTable_Show()
  })

  #Distribution
  scores_plot_rea <- reactive({
    if(is.null(input$res_data))
      return(NULL)
    Response <- mydata()%>%as.data.frame()
    scores_plot1 <- hist(rowSums(Response), breaks = 100,
                         main = "The distribution for total score", xlab = "Total score", ylab = "Frequency")
    scores_plot1
  })

  output$scores_plot <- renderPlot({
    scores_plot_rea()
  })

  #2. EFA-----------------------
  EFA_fit <- reactive({
    if(is.null(input$res_data))
      return(NULL)
    Response <- mydata()%>%as.data.frame()
    colnames(Response) <- paste0("Item",1:ncol(Response))
    fit <- bruceR::EFA(data = Response , var = "Item", items = 1:ncol(Response),
               method = EFA_method(input$EFA_method),
               rotation = EFA_rotation_method(input$rotation_method))
    fit
  })
  output$CTT_EFA_eigenvalues <- DT::renderDataTable({
    if(is.null(input$res_data))
      return(NULL)
    fit <- EFA_fit()
    as.data.frame(fit$eigenvalues)%>%round(digits = 3) %>% DT_dataTable_Show()
  })
  EFA_plot_rea <- reactive({
    if(is.null(input$res_data))
      return(NULL)
    fit <- EFA_fit()
    fit$scree.plot
  })
  output$EFA_plot <- renderPlot({#Scree plot
    EFA_plot_rea()
  })

  output$EFA_load <- DT::renderDataTable({#Factor loadings
    if(is.null(input$res_data))
      return(NULL)
    fit <- EFA_fit()
    as.data.frame(fit$loadings)%>%round(digits = 3) %>% DT_dataTable_Show()
  })
  #3. CFA--------------------------------------------------------------
  dimension_cfa <- reactive({#Import dimension

    if(is.null(input$dimensionfile_cfa))
      return(NULL)
    inFile <- input$dimensionfile_cfa
    dataset <- bruceR::import(inFile$datapath)
    data <- as.data.frame(dataset)
    if(sum(is.na(dataset)) >=1){
      return("Data cannot contain missing values!")
    }
    data
  })
  output$CFA_dimension_example <- renderDataTable({
    if(is.null(input$res_data))
      return(NULL)

    if(is.null(input$dimensionfile_cfa)){#If the user did not upload the file.
      data_exp <- data.frame("Column name" = colnames(mydata()),
                             "Dimensions (e.g. F1, F2, F3, etc.)" = c(rep("F1",3),rep("F2",(ncol(mydata())-3))))
      as.data.frame(data_exp)
    }else{
      dimension_cfa()
    }
  })
  dimension_recode <- function(dimension){
    dimnames <- table(dimension[,2])%>%names()

    S <- vector(mode = "character")
    for (i in 1:length(dimnames)) {
      Ss <- paste0(dimnames[i]," = " ,paste0(dimension[which(dimension[,2]==dimnames[i]),1],",", collapse = ""))#不包含协方差矩阵的
      if(i == length(dimnames)){
        S <- c(S, str_sub(string = Ss, start = 1, end = (str_length(Ss)-1)))#Remove the last ",".
      }else{
        S <- c(S, str_sub(string = Ss, start = 1, end = (str_length(Ss)-1)))
      }

    }
    cov <- paste0("COV = ",paste0(dimnames, "*", collapse = ""))

    S <- paste0(S, "
                ",collapse = "")#No COV matrix
    SS <- paste0(S, str_sub(cov, start = 1, end = (str_length(cov)-1)),collapse = "")#Include COV matrix

    return(list(dim = S, COV = SS, F_names = dimnames, F_n = length(dimnames)))
  }

  CFA_mode <- function(dimension){
    dim_mode <- dimension_recode(dimension)$dim#The relationship between item and dimension.

    return(str_replace_all(string = dim_mode, pattern = "=",replacement = "=~")%>%
             str_replace_all(pattern =",",replacement = "+")%>%
             str_replace_all(pattern = "\n", replacement = ";"))

  }

  CFA_reactive <- reactive({

    dimension <- dimension_cfa()

    Response <- mydata()%>%as.data.frame()
    fit <- CFA(data = Response, model = CFA_mode(dimension))
    fit

  })
  CFA_loading_rea <- reactive({
    fit <- CFA_reactive()
    round(lavaan_summary(fit)$measure, digits = 3)

  })
  output$CFA_loading <- DT::renderDataTable({
    if(is.null(input$res_data))
      return(NULL)
    if(is.null(input$dimensionfile_cfa))
      return(NULL)
    CFA_loading_rea() %>% DT_dataTable_Show()

  })
  CFA_fit_index_rea <- reactive({
    fit <- CFA_reactive()

    as.data.frame(lavaan_summary(fit)$fit)%>%round(digits = 3)

  })
  output$CFA_fit_index <- DT::renderDataTable({
    if(is.null(input$res_data))
      return(NULL)
    if(is.null(input$dimensionfile_cfa))
      return(NULL)
    CFA_fit_index_rea() %>% DT_dataTable_Show()

  })
  CFA_fit_plot_rea <- reactive({
    fit <- CFA_reactive()
    graph_sem(model = fit)
  })

  output$CFA_fit_plot <- renderPlot({
    if(is.null(input$res_data))
      return(NULL)
    if(is.null(input$dimensionfile_cfa))
      return(NULL)
    CFA_fit_plot_rea()
  },height =  exprToFunction(input$CFA_plot_height))

  output$CFA_fit_plot1 <- renderUI({
    plotOutput(outputId = "CFA_fit_plot", height = paste0(input$CFA_plot_height,"px"))
  })

  #CFA results

  output$CFA_plot_file <- downloadHandler(
    filename = function(){
      paste0("CFA_path_diagram.jpeg")
    },
    content = function(file){
      jpeg(filename = file, width = input$CFA_plot_height*1.618, height = input$CFA_plot_height )
      print(CFA_fit_plot_rea())
      dev.off()

    }
  )

  output$CFA_file <- downloadHandler(
    filename = function(){
      paste0("CFA_result.xlsx")
    },
    content = function(file){
      write.xlsx(list("Factor loadings" =  CFA_loading_rea(),
                      "Model fit" = CFA_fit_index_rea()),file = file,
                 rowNames = TRUE)
    }
  )
  #4. CTT ------------------------------------------------
  #Item parameters
  output$CTT_itempar <- DT::renderDataTable({
    if(is.null(input$res_data))
      return(NULL)
    Response <- mydata()
    if(length(which(is.na(Response)))>=1){
      return(data.frame(X = "For the calculation of CTT parameters, the score data cannot contain missing values."))
    }
    cat_all <- apply(Response, MARGIN = 2, FUN = cat_number)#The number of categories.
    item_par <- item_ana(data = Response,
                         cat_2 =  which( cat_all==2),#0 or 1
                         cat_m = which(cat_all >2),#Multi-level scores
                         item_s = apply(Response,2,max))%>%round(digits = 3)#The max score

    data.frame("Item" = colnames(Response),
               "The number of categories" = cat_all,
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
    if(is.null(input$res_data))
      return(NULL)
    CTT_relibility_rea() %>% DT_dataTable_Show()
  })

  CTT_item_alpha_rea <- reactive({
    Response <- mydata()
    reli <- Alpha(data = Response,vars = colnames(Response))
    as.data.frame(reli$alpha[[2]])%>%round(digits = 3)
  })
  output$CTT_item_alpha <- DT::renderDataTable({
    if(is.null(input$res_data))
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
      write.xlsx(x = datalist, file = file, rowNames =TRUE)
    }
  )

  ###4.3 Correlation matrix---------------------------------
  CTT_relate_eff_rea <- reactive({
    Response <- mydata()
    rea <- bruceR::Corr(data = Response,method = "pearson",plot = FALSE)
    as.data.frame(rea$corr$r)%>%round(digits = 3)

  })
  output$CTT_relate_eff <- DT::renderDataTable({
    if(is.null(input$res_data))
      return(NULL)
    CTT_relate_eff_rea() %>% DT_dataTable_Show()

  })
  CTT_relate_p_rea <- reactive({
    Response <- mydata()
    rea <- bruceR::Corr(data = Response,method = "pearson",plot = FALSE)
    as.data.frame(rea$corr$p)%>%round(digits = 3)

  })
  output$CTT_relate_p <- DT::renderDataTable({
    if(is.null(input$res_data))
      return(NULL)
    CTT_relate_p_rea() %>% DT_dataTable_Show()
  })
  ###4.4 Downlaod correaltion matrix---------------------
  output$CTT_relatefile <- downloadHandler(
    filename = function(){
      paste0("Correlation coefficient matrix.xlsx")
    },
    content = function(file){
      datalist <- list("Pearson coefficient" = CTT_relate_eff_rea(),
                       "The P value corresponding to the correlation coefficient" = CTT_relate_p_rea())
      write.xlsx(x = datalist, file = file, rowNames =TRUE)
    }
  )

  #5. Downlaod EFA results---------------------------------
  output$EFA_result <- downloadHandler(
    filename = function(){
      paste0(input$EFA_method, "_", input$rotation_method,"_result.xlsx")
    },
    content = function(file){
      fit <- EFA_fit()
      datalist <- list("Eigenvalues"= as.data.frame(fit$eigenvalues)%>%round(digits = 3),
                       "Factor loadings" = as.data.frame(fit$loadings)%>%round(digits = 3))
      write.xlsx(x = datalist, file = file, rowNames =TRUE)
    }
  )
  #6. Download discriptive statistics--------------------------------------------
  output$summary_result <- downloadHandler(
    filename = function(){
      paste0("Descriptive statistics.xlsx")
    },
    content = function(file){
      desc <-  desc_rea()
      datalist <- list("Descriptive statistics" = desc)
      write.xlsx(x = datalist, file = file)
    }
  )
  #7. Download CTT results----------------------------------------------------
  output$CTT_result <- downloadHandler(
    filename = function(){
      paste0("CTT_results.xlsx")
    },
    content = function(file) {
      Response <- mydata() %>% as.data.frame()
      cat_all <- apply(Response, MARGIN = 2, FUN = cat_number)
      item_par <- item_ana(data = Response,
                           cat_2 =  which( cat_all==2),
                           cat_m = which(cat_all >2),
                           item_s = apply(Response,2,max))%>%round(digits = 3)

      datalist <- list("CTT_parameters" = data.frame(row.names = colnames(Response),
                                                     "The number of categories of scores" = cat_all,item_par))

      write.xlsx(x = datalist, file = file, rowNames =TRUE)
    })
  #Download the histogram
  output$scores_plotfile <- downloadHandler(
    filename = function(){
      paste0("Total score distribution.jpeg")
    },
    content = function(file){
      jpeg(file, width = 1200, height = 800)
      if(is.null(input$res_data))
        return(NULL)
      Response <- mydata()%>%as.data.frame()
      scores_plot1 <- hist(rowSums(Response), breaks = 100,
                           main = "Total score distribution", xlab = "Total score", ylab = "Frequency")

      print(scores_plot1)
      dev.off()
    })
  #Download scree plot
  output$EFA_plotfile <- downloadHandler(
    filename = function(){
      paste0("EFA_scree plot.jpeg")
    },
    content = function(file){

      jpeg(file, width = 1200, height = 800)
      print(EFA_plot_rea())
      dev.off()
    }
  )

  #8. Unidimensional IRT analysis---------------------------------------

  IRT_fit_reactive <- reactive( {
    if(is.null(input$res_data))
      return(NULL)

    if(is.null(model_selected(input$modelselect))){
      return(NULL)
    }else{
      Response <- mydata()%>%as.data.frame()
      model <- model_selected(input$modelselect)

      IRT_fit <- mirt(data = Response, model = 1, itemtype = model,
                      method = est_IRT_method(input$IRT_est_method))
      IRT_fit
    }
  })
  ###8.1 IRTModel fit--------------------------------------------------------
  IRT_modelfit_relat_rea <- reactive({
    IRT_fit <- IRT_fit_reactive()
    anova(IRT_fit)%>%as.data.frame()%>%round(digits = 3)

  })

  output$IRT_modelfit_relat <- renderDataTable({
    if(is.null(input$res_data))
      return(NULL)
    if(is.null(model_selected(input$modelselect)))
      return(NULL)
    IRT_modelfit_relat_rea()
  })

  IRT_modelfit_rea <- reactive({
    IRT_fit <- IRT_fit_reactive()
    Response <- mydata()%>%as.data.frame()
    cat_all <- apply(Response, MARGIN = 2, FUN = cat_number)
    if(length(which(cat_all > 2)) >=1 ){
      fit_index <- M2(obj = IRT_fit, type = "M2*",na.rm = T)%>%round(digits = 3)#M2*
    }else{
      fit_index <- M2(obj = IRT_fit, type = "M2",na.rm = T)%>%round(digits = 3)
    }
    as.data.frame(fit_index)
  })
  output$IRT_modelfit <- DT::renderDataTable({
    if(is.null(input$res_data))
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
    if(is.null(input$res_data))
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
  output$IRT_itemfit <- renderDataTable({
    if(is.null(input$res_data))
      return(NULL)
    if(is.null(model_selected(input$modelselect)))
      return(NULL)
    IRT_itemfit_rea()
  })

  ###8.4 Item parameters-----------------------
  IRT_itempar_rea <- reactive({
    IRT_fit <- IRT_fit_reactive()
    item_par <- coef(IRT_fit, IRTpars = TRUE, simplify = TRUE)$items %>% as.data.frame() %>% round(digits = 3)

    colnames(item_par) <- colnames(item_par) %>%
      str_replace_all(pattern = "a", replacement = "Discrimination")%>%
      str_replace_all(pattern = "u", replacement = "one-slip") %>%
      str_replace_all(pattern = "b",replacement = "Difficult")%>%
      str_replace_all(pattern = "g", replacement = "Guess")
    data.frame( item_par)
  })
  output$IRT_itempar <- DT::renderDataTable({
    if(is.null(input$res_data))
      return(NULL)
    if(is.null(model_selected(input$modelselect)))
      return(NULL)

    IRT_itempar_rea() %>% DT_dataTable_Show()
  })

  ###8.5 Person parameters--------------------
  IRT_person_rea <- reactive({
    IRT_fit <- IRT_fit_reactive()
    Response <- mydata()%>%as.data.frame()
    IRT_person <- fscores(IRT_fit, method = est_person_method(input$IRT_person_est_method),
                          full.scores = T, response.pattern = Response)
    colnames(IRT_person) <- c("theta", "SE")
    as.data.frame(IRT_person)%>%round(digits = 3)
  })
  output$IRT_person <- renderDataTable({
    if(is.null(input$res_data))
      return(NULL)
    if(is.null(model_selected(input$modelselect)))
      return(NULL)
    IRT_person_rea()
  })

  ###8.6 WrightMap------
  IRT_wright_rea <- reactive({
    Response <- mydata()%>%as.data.frame()
    cat_all <- apply(Response, MARGIN = 2, FUN = cat_number)

    item_par <- IRT_itempar_rea()

    IRT_person <- IRT_person_rea()

    wrightMap_new(person = IRT_person[,1],
                  thresholds = item_par[,str_which(colnames(item_par) %>% str_to_lower(),
                                                   pattern = "difficult")],
                  points_size = input$IRT_wright_map_p_size,
                  p_width =  input$IRT_wright_p_width )

  })
  output$IRT_wright <- renderPlot({
    if(is.null(input$res_data))
      return(NULL)
    if(is.null(model_selected(input$modelselect)))
      return(NULL)
    IRT_wright_rea()
  },height = exprToFunction(input$IRT_wright_map_height))

  ##8.7 ICC---------------------------------------
  IRT_ICC_rea <- reactive({
    sim_theta <- seq(-4,4,0.01)
    Response <- mydata()%>%as.data.frame()
    cat_all <- apply(Response, MARGIN = 2, FUN = cat_number)
    #Model fit
    IRT_fit  <- IRT_fit_reactive()
    prob <- probtrace(x = IRT_fit, Theta = sim_theta)
    ncol <- wrap_ncol_Value()
    plot_wrap(x = sim_theta,
              y_matrix = prob,
              lines = "ICC",
              grade_vector = cat_all,
              main_vector = colnames(Response),
              y_lab = "Probability",
              x_lab = "Theta",
              title = "Item Characteristic Curve",
              ncol = ncol,
              scale = "fixed")
  })

  output$IRT_ICC <-  renderPlot({
    if(is.null(input$res_data))
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
  IRT_IIC_rea <- reactive({
    sim_theta <- seq(-4,4,0.01)
    Response <- mydata()%>%as.data.frame()

    item_info <- IRT_iteminfo_rea()
    ncol <- wrap_ncol_Value()
    plot_wrap(x = sim_theta,
              y_matrix = item_info,
              lines = "IIC",
              main_vector = colnames(Response),
              y_lab = "Information",
              x_lab = "Theta",
              title = "Item Information Curve",
              ncol = ncol,
              scale = "free")
  })
  output$IRT_IIC <- renderPlot({
    if(is.null(input$res_data))
      return(NULL)
    if(is.null(model_selected(input$modelselect)))
      return(NULL)
    IRT_IIC_rea()
  },height =  exprToFunction(input$wrap_height))

  ##8.9 TIC----------------------------------------------------------------------------
  IRT_TIC_rea<- reactive({

    IRT_fit  <- IRT_fit_reactive()
    test_infor <- mirt::plot(x = IRT_fit, type = "infoSE",
                             theta_lim = c(-4,4))
    test_infor
  })
  output$IRT_TIC <- renderPlot({
    if(is.null(input$res_data))
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
    plotOutput(outputId = "IRT_IIC", height = paste0(input$wrap_height,"px"))
  })

  ##8.11 Download figures------------------------------------------------------------
  output$IRT_wrightfile <- downloadHandler(
    filename = function(){
      paste0("IRT_WrightMap.jpeg")
    },
    content = function(file){
      jpeg(file, width =  input$IRT_wright_map_height*1.618, height = input$IRT_wright_map_height)
      IRT_wright_rea()%>% print()
      dev.off()
    }
  )
  output$IRT_ICCfile <- downloadHandler(
    filename = function(){
      paste0("IRT_item characteristic curve.jpeg")
    },
    content = function(file){
      jpeg(file, width = input$wrap_height*1.618, height = input$wrap_height)
      print(IRT_ICC_rea())
      dev.off()
    }
  )
  output$IRT_IICfile <- downloadHandler(
    filename = function(){
      paste0("IRT_item information curve.jpeg")
    },
    content = function(file){
      jpeg(file, width = input$wrap_height*1.618, height = input$wrap_height)
      print(IRT_IIC_rea())
      dev.off()
    }
  )
  output$IRT_TICfile <- downloadHandler(
    filename = function(){
      paste0("IRT_test information curve.jpeg")
    },
    content = function(file){
      jpeg(file, width = 1200, height = 800)
      print(IRT_TIC_rea())
      dev.off()
    }
  )
  ##8.12 Download results--------------------------------------------------------------
  output$IRT_resultfile <- downloadHandler(
    filename = function(){
      paste0("IRT_results.xlsx")
    },
    content = function(file){
      sim_theta <-  IRT_person_rea()[,1]%>%as.numeric()#True theta
      IRT_fit  <- IRT_fit_reactive()
      Response <- mydata()%>%as.data.frame()
      item_info <- testinfo(x = IRT_fit, Theta = sim_theta, individual = T)
      colnames(item_info) <- colnames(Response)
      datalist <- list("Absolute model fit" = IRT_modelfit_rea(),
                       "Relative model fit" = IRT_modelfit_relat_rea(),
                       "Dependence test" = IRT_Q3_rea(),
                       "Item fit" = IRT_itemfit_rea(),
                       "Item parameters" = IRT_itempar_rea(),
                       "Person parameters" = IRT_person_rea(),
                       "Item information" = data.frame("theta"  = sim_theta,
                                                       item_info),
                       "Test Information" = data.frame("Test Information" = rowSums(item_info),
                                                       "Messurement Error" = 1/sqrt(rowSums(item_info)))
      )
      write.xlsx(x = datalist, file = file, rowNames = T)
    }
  )

  ##8.13 Downlaod analysis report-------------------------------------------
  output$IRT_report <- downloadHandler(
    filename = function(){
      paste0("IRT Analysis Report.docx")
    },
    content = function(file){
      #Selections
      model <- input$modelselect
      IRT_est_method <- input$IRT_est_method
      IRT_person_est_method <- input$IRT_person_est_method
      EFA_method <- input$EFA_method
      rotation_method <- input$rotation_method
      IRT_itemfit_method <- input$IRT_itemfit_method
      #Model fit
      IRT_modelfit_relat <- IRT_modelfit_relat_rea()
      IRT_modelfit <- IRT_modelfit_rea()
      #Hypothesis test
      fit <- EFA_fit()
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

      wright_map_height <- input$IRT_wright_map_height
      wrap_height_value <- input$wrap_height
      #Export analysis report
      path_sys <- system.file("rmd", "IRT Analysis Report.Rmd", package = "TestAnaAPP")
      src <- normalizePath(path_sys)
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src,"IRT Analysis Report.Rmd", overwrite = TRUE)
      library(rmarkdown)
      rmarkdown::render("IRT Analysis Report.Rmd",output_file = file)

    }
  )

  #9. MIRT Analysis-------------------------------------------
  #Read dimension ifnormation
  dimension <- reactive({

    if(is.null(input$dimensionfile))
      return(NULL)
    inFile <- input$dimensionfile
    dataset <- bruceR::import(inFile$datapath)
    data <- as.data.frame(dataset)
    if(sum(is.na(dataset)) >=1){
      return("Data cannot contain missing values!")
    }
    data
  })


  output$dimension_example <- renderDataTable({
    if(is.null(input$res_data))
      return(NULL)

    if(is.null(input$dimensionfile)){
      data_exp <- data.frame("Column name" = colnames(mydata()),
                             "Dimensions (e.g. F1, F2, F3, etc.)" = c(rep("F1",3),rep("F2",(ncol(mydata())-3))))
      as.data.frame(data_exp)
    }else{
      dimension()
    }
  })

  dimension_output <- reactive({
    dim_data <- dimension()
    mode <- dimension_recode(dim_data)
    if(input$include_cov == "Yes"){
      return(mode$COV)
    }else{
      return(mode$dim)
    }
  })
  output$dimension_code <- renderText({
    if(is.null(input$dimensionfile))
      return(NULL)
    dimension_output()
  })

  ##9.1 MIRT Model fit----------------------------------------------
  MIRT_fit_rea <- reactive({
    if(is.null(model_selected(input$modelselect1)))
      return(NULL)
    dim_data <- dimension()
    Response <- mydata()
    if(sum(length(names(table(dim_data[,1]))==colnames(Response))) != ncol(Response)){#如果存在列名不等于行名
      stop("The data in the first column of the imported file is inconsistent with the data file column name!")
    }
    mirtCluster()
    if(input$include_cov == "Yes"){
      MIRT_fit <- mirt(data = Response,model = dimension_recode(dim_data)$COV,
                       itemtype = model_selected(value = input$modelselect1),
                       method = est_IRT_method(input$MIRT_est_method))
    }else if(input$include_cov == "No"){
      MIRT_fit <- mirt(data = Response,model = dimension_recode(dim_data)$dim,
                       itemtype = model_selected(value = input$modelselect1),
                       method = est_IRT_method(input$MIRT_est_method))
    }
    MIRT_fit
  })

  #MIRT Model fit indices
  MIRT_modelfit_relat_rea <- reactive({
    MIRT_fit <- MIRT_fit_rea()
    anova(MIRT_fit)%>%as.data.frame()%>%round(digits = 3)

  })

  output$MIRT_modelfit_relat <- renderDataTable({
    if(is.null(input$res_data))
      return(NULL)
    if(is.null(model_selected(input$modelselect1)))
      return(NULL)
    MIRT_modelfit_relat_rea()
  })

  MIRT_modelfit_rea <- reactive({
    MIRT_fit <-  MIRT_fit_rea()
    Response <- mydata()%>%as.data.frame()
    cat_all <- apply(Response, MARGIN = 2, FUN = cat_number)
    if(length(which(cat_all > 2)) >=1 ){
      fit_index <- M2(obj = MIRT_fit, type = "M2*",na.rm = T)%>%round(digits = 3)#M2*
    }else{
      fit_index <- M2(obj = MIRT_fit, type = "M2",na.rm = T)%>%round(digits = 3)
    }
    as.data.frame(fit_index)
  })
  output$MIRT_modelfit <- DT::renderDataTable({#输出
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
  output$MIRT_itemfit <- renderDataTable({
    if(is.null(input$dimensionfile))
      return(NULL)
    if(is.null(model_selected(input$modelselect1)))
      return(NULL)
    MIRT_itemfit_rea()
  })

  ##9.5 Item parameters-----------------------

  diff_trans <- function(item_par, F_n, MDISC){
    if(str_count(colnames(item_par),"d0")%>%sum() >= 1){
      item_par <- item_par[,-which(colnames(item_par)=="d0")]
    }
    if(str_count(colnames(item_par),"ak")%>%sum() >= 1){
      item_par <- item_par[,-str_which(colnames(item_par),pattern = "ak")]
    }
    di_diff <- which(colnames(item_par)=="d")
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
    mode <- dimension_recode(dim_data)
    item_par_raw <- coef(MIRT_fit, simplify = TRUE)$items
    item_par <- diff_trans(item_par = item_par_raw,
                           F_n = mode$F_n, MDISC = MDISC(MIRT_fit))
    colnames(item_par) <- colnames(item_par)%>%
      str_replace_all(pattern = "a", replacement = "Discrimination")%>%
      str_replace_all(pattern = "u", replacement = "one-slip") %>%
      str_replace_all(pattern = "b",replacement = "Difficult")%>%
      str_replace_all(pattern = "g", replacement = "Guess")

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
      return(data.frame("The covariance matrix is not selected."))
    }
  })
  output$cov_est <- renderDataTable({
    if(is.null(input$dimensionfile))
      return(NULL)
    if(is.null(model_selected(input$modelselect1)))
      return(NULL)
    cov_est_rea()
  })


  ##9.6 Person parameters--------------------
  MIRT_person_rea <- reactive({
    MIRT_fit <- MIRT_fit_rea()
    Response <- mydata()
    MIRT_person <- fscores(MIRT_fit, method = est_person_method(input$MIRT_person_est_method),
                           full.scores = T, response.pattern = Response,QMC = TRUE)
    as.data.frame(MIRT_person)%>%round(digits = 3)
  })
  output$MIRT_person <- renderDataTable({
    if(is.null(input$dimensionfile))
      return(NULL)
    if(is.null(model_selected(input$modelselect1)))
      return(NULL)
    MIRT_person_rea()
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
      mode <- dimension_recode(dim_data)
      selectInput(inputId = "wright_dim_select",label = "Dimension selection",
                  choices = apply(matrix(mode$F_names,ncol=1),
                                  MARGIN = 1,FUN = as.vector,simplify = FALSE),
                  selectize = TRUE)
    }
  })
  MIRT_wright_rea <- reactive({
    Response <- mydata()%>%as.data.frame()
    cat_all <- apply(Response, MARGIN = 2, FUN = cat_number)
    dim_data <- dimension()
    mode <- dimension_recode(dim_data )

    if(is.null(input$wright_dim_select)){
      wright_dim <- as.vector(mode$F_names)[1]
    }else{
      wright_dim <-  input$wright_dim_select
    }

    #Item parameters
    item_par <- MIRT_itempar_rea()
    dim_items <- which(dim_data[,2] == wright_dim)
    item_par_dim <- item_par[dim_items,]
    #Person parameters
    MIRT_person <- MIRT_person_rea()

    wrightMap_new(person = MIRT_person[,wright_dim],
                  thresholds = item_par_dim[,c(str_which(colnames(item_par) %>% str_to_lower(),
                                                         pattern = "difficult"))],
                  points_size = input$MIRT_wright_map_p_size,
                  p_width = input$MIRT_wright_p_width)
  })
  output$MIRT_wright <- renderPlot({
    if(is.null(input$dimensionfile))
      return(NULL)
    if(is.null(model_selected(input$modelselect1)))
      return(NULL)
    MIRT_wright_rea()
  },height = exprToFunction(input$MIRT_wright_map_height))

  ##9.8 ICC------------------------------------------------
  MIRT_ICC_rea <- reactive({
    sim_theta <- seq(-4,4,0.01)
    Response <- mydata()%>%as.data.frame()
    dim_data <- dimension()
    mode <- dimension_recode(dim_data)
    cat_all <- apply(Response, MARGIN = 2, FUN = cat_number)
    #Model fit
    MIRT_fit  <- MIRT_fit_rea()
    prob <- probtrace(x = MIRT_fit, Theta = matrix(rep(sim_theta,mode$F_n),
                                                   nrow = length(sim_theta),ncol = mode$F_n))
    ncol <- MIRT_wrap_ncol_value()
    plot_wrap(x = sim_theta,
              y_matrix = prob,
              lines = "ICC",
              grade_vector = cat_all,
              main_vector = colnames(Response),
              y_lab = "Probability",
              x_lab = "Theta",
              title = "Item Characteristic Curve",
              ncol = ncol,
              scale = "fixed")

  })

  output$MIRT_ICC <-  renderPlot({
    if(is.null(input$dimensionfile))
      return(NULL)
    if(is.null(model_selected(input$modelselect1)))
      return(NULL)
    MIRT_ICC_rea()
  },height =  exprToFunction(input$MIRT_wrap_height))
  ##9.9 IIC----------------------------------------------------------
  Item_infor<- function(object,theta,mode,colnames){
    D <- mode$F_n
    degrees <- rep(45, D)

    TRUE_information <- testinfo(object, Theta = theta[,1:mode$F_n],
                                 degrees = degrees, individual = T)
    colnames(TRUE_information) <- colnames
    dim_inf <- matrix(NA, ncol = D, nrow = nrow(theta));
    colnames(dim_inf) <- paste0(mode$F_names[1:D],"-information")

    for(i in 1:D){
      items <- str_split(mode$dim, pattern = "\n")[[1]][i]%>%
        str_remove_all(pattern = " ")%>%
        str_remove_all(pattern = mode$F_names[i])%>%
        str_remove_all(pattern = "=")%>%
        str_split(pattern = ",")
      if(length(items[[1]])>1){
        dim_inf[,i] <- rowSums(TRUE_information[,items[[1]]])
      }else{
        dim_inf[,i] <- TRUE_information[,items[[1]]]
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
    mode <- dimension_recode(dim_data)
    item_info1 <- Item_infor(object = MIRT_fit,theta = matrix(rep(sim_theta,mode$F_n),
                                                              ncol = mode$F_n,
                                                              nrow = length(sim_theta)),
                             mode = mode,colnames = colnames(Response))$Item_information
    as.data.frame(item_info1)
  })
  MIRT_IIC_rea <- reactive({
    sim_theta <- seq(-4,4,0.01)
    Response <- mydata()%>%as.data.frame()
    item_info <- MIRT_iteminfo_rea()
    ncol <- MIRT_wrap_ncol_value()
    plot_wrap(x = sim_theta,
              y_matrix = item_info,
              lines = "IIC",
              main_vector = colnames(Response),
              y_lab = "Information",
              x_lab = "Theta",
              title = "Item Information Curve",
              ncol = ncol,
              scale = "free")
  })
  output$MIRT_IIC <- renderPlot({
    if(is.null(input$dimensionfile))
      return(NULL)
    if(is.null(model_selected(input$modelselect1)))
      return(NULL)
    MIRT_IIC_rea()
  },height =  exprToFunction(input$MIRT_wrap_height))

  ##9.10 TIC---------------------------------------------------------------------
  output$MIRT_TIC_dim_select <- renderUI({
    if(is.null(input$dimensionfile)){
      selectInput(inputId = "MIRT_dim_select",label = "Dimension selection",
                  choices = apply(matrix(paste0("F",1:3),ncol=1),
                                  MARGIN = 1,FUN = as.vector,simplify = FALSE),
                  selectize = TRUE)
    }else{
      dim_data <- dimension()
      mode <- dimension_recode(dim_data)
      selectInput(inputId = "MIRT_dim_select",label = "Dimension selection",
                  choices = apply(matrix(mode$F_names,ncol=1),
                                  MARGIN = 1,FUN = as.vector,simplify = FALSE),
                  selectize = TRUE)
    }
  })
  MIRT_TIC_rea<- reactive({
    if(is.null(input$MIRT_dim_select))
      return(NULL)
    sim_theta <- seq(-4,4,0.01)
    MIRT_fit  <- MIRT_fit_rea()
    dim_data <- dimension()
    Response <- mydata()
    mode <- dimension_recode(dim_data)
    item_info1 <- Item_infor(object = MIRT_fit,theta = matrix(rep(sim_theta,mode$F_n),
                                                              ncol = mode$F_n,
                                                              nrow = length(sim_theta)),
                             mode = mode,colnames = colnames(Response))$dim_information
    colnames(item_info1) <- c(mode$F_names, paste0(mode$F_names,"infor"))
    sim_theta1_infor1 <- item_info1[,c(input$MIRT_dim_select,paste0(input$MIRT_dim_select,"infor"))]
    test_infor<- twoord.plot(lx = sim_theta1_infor1[,1],ly = sim_theta1_infor1[,2],
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
    plotOutput(outputId = "MIRT_IIC", height = paste0(input$MIRT_wrap_height,"px"))
  })


  ##9.12 Download figures-----------------------------------------------------------------
  output$MIRT_wrightfile <- downloadHandler(
    filename = function(){
      paste0("MIRT_WrightMap.jpeg")
    },
    content = function(file){
      jpeg(file, width = input$MIRT_wright_map_height*1.618, height = input$MIRT_wright_map_height)
      print(MIRT_wright_rea())
      dev.off()
    }
  )
  output$MIRT_ICCfile <- downloadHandler(
    filename = function(){
      paste0("MIRT_item characteristic curve.jpeg")
    },
    content = function(file){
      jpeg(file, width = input$MIRT_wrap_height*1.618, height = input$MIRT_wrap_height)
      print(MIRT_ICC_rea())
      dev.off()
    }
  )
  output$MIRT_IICfile <- downloadHandler(
    filename = function(){
      paste0("MIRT_item information curve.jpeg")
    },
    content = function(file){
      jpeg(file, width = input$MIRT_wrap_height*1.618, height = input$MIRT_wrap_height)
      print(MIRT_IIC_rea())
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
      mode <- dimension_recode(dim_data)
      sim_theta <- MIRT_person_rea()[,as.vector(mode$F_name)]
      item_info1 <- Item_infor(object = MIRT_fit,
                               theta = sim_theta,
                               mode = mode,
                               colnames = colnames(Response))
      item_info <- item_info1$Item_information
      colnames(item_info ) <- colnames(Response)
      dim_infor <- item_info1$dim_information
      colnames(dim_infor) <- c(mode$F_names, paste0(mode$F_names, "_Information"))

      datalist <- list("Score data" = Response,
                       "Dimension" = dim_data ,
                       "Model fit" = MIRT_modelfit_rea(),
                       "Dependence test" = MIRT_Q3_rea(),
                       "Item fit" = MIRT_itemfit_rea(),
                       "Item parameters" = MIRT_itempar_rea(),
                       "Person parameters" = MIRT_person_rea(),
                       "Item information" = item_info,
                       "Test information" = dim_infor)
      bruceR::export(x = datalist,file = file)
      print(datalist)
      write.xlsx(x = datalist, file = file, rowNames = T)
    }
  )

  ##9.13 Downlaod analysis report-------------------------------------------
  output$MIRT_report <- downloadHandler(
    filename = function(){
      paste0("MIRT Analysis Report.docx")
    },
    content = function(file){
      #Selections
      model <- input$modelselect1
      MIRT_est_method <- input$MIRT_est_method
      MIRT_person_est_method <- input$MIRT_person_est_method
      MIRT_itemfit_method <- input$MIRT_itemfit_method
      #Model fit
      MIRT_fit <- MIRT_fit_rea()
      dimension <- dimension()
      mode <- dimension_recode(dimension)
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
                               mode = mode,colnames = colnames(Response))$dim_information
      colnames(item_info1) <- c(mode$F_names, paste0(mode$F_names,"infor"))

      if(is.null(input$MIRT_dim_select)){
        TIC_dim <- as.vector(mode$F_names)[1]
      }else{
        TIC_dim <- input$MIRT_dim_select
      }

      sim_theta1_infor1 <- item_info1[,c(TIC_dim,paste0(TIC_dim,"infor"))]
      F_name <- TIC_dim

      wright_map_height <- input$MIRT_wright_map_height
      wrap_height_value <- input$wrap_height
      #Export analysis report
      path_sys <- system.file("rmd", "MIRT Analysis Report.Rmd", package = "TestAnaAPP")
      src <- normalizePath(path_sys)
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src,"MIRT Analysis Report.Rmd", overwrite = TRUE)
      library(rmarkdown)
      rmarkdown::render("MIRT Analysis Report.Rmd",output_file = file)

    }
  )

}

#Functions-----------------------------------------------------
cat_number <- function(vector){
  number <- Freq(x = vector)%>%nrow()
  return(number)
}

box_show_theme <- function(value){
  return(shinycssloaders::withSpinner(ui_element = value,color="#0dc5c1",type=6,size = 1.5 ))
}
DT_dataTable_Show <- function(x){
  DT::datatable(x,
                filter =list(position = 'top', clear = TRUE, plain = TRUE),
                options = list(scrollX = TRUE))
}



#CTT item parameters
item_ana<- function(data, cat_2, cat_m, item_s){#Difficult, discrimination and CV
  if(length(which(is.na(data)))>=1){
    data<- na.omit(data)
    warning("The NA were deleted.")
  }
  item_analysis<- matrix(NA, nrow = ncol(data), ncol = 4)
  rownames(item_analysis)<- colnames(data)
  colnames(item_analysis)<- c("Difficult","Discrimination","Coefficient of variation", "Item-total correlation")

  data1<- cbind(data,rowSums(data))
  data1<- data1[order(data1[,ncol(data1)],decreasing = F),]
  zf <- data1[,ncol(data1)]
  low_N <- which(zf <= round(quantile(zf, 0.27), 2))
  hig_N <- which(zf >= round(quantile(zf, 0.73), 2))

  low_data<- data1[low_N,]
  high_data<- data1[hig_N,]

  for (i in cat_2) {
    #Difficult
    item_analysis[i,1]<- ((mean(high_data[,i])+mean(low_data[,i]))/item_s[i])/2#按公式计算第i题的Difficult,作文题除外
    #Discrimination
    item_analysis[i,2]<- (mean(high_data[,i])-mean(low_data[,i]))/item_s[i]#按公式计算区分度

    #CV
    item_analysis[i,3]<- (sd(data[,i])/mean(data[,i]))*100#按公式计算差异系数
    item_analysis[i,4] <- cor(x = data[,i], y = zf)
  }

  for (j in cat_m) {
    #Difficult
    item_analysis[j,1]<- mean(data1[,j])/item_s[j]
    #Discrimination
    item_analysis[j,2]<- ((mean(high_data[,j])-mean(low_data[,j]))/item_s[j])

    #CV
    item_analysis[j,3]<- (sd(data[,j])/mean(data[,j]))*100
    item_analysis[j,4] <- cor(x = data[,j], y = zf)
  }

  return(item_analysis)
}
plot_wrap <- function(x,              #x
                      y_matrix,       #y
                      lines = "ICC",  #or IIC
                      is.include.zore = FALSE,
                      grade_vector = NULL,     #The number of curve for a single item.
                      main_vector,    #Main
                      y_lab = NULL,
                      x_lab = NULL,
                      title = NULL,   #The main for wrap.
                      ncol = 5,
                      scale = "fixed"){

  #A single curve
  if(lines == "IIC"){
    colnames(y_matrix) <- main_vector
    plot_data <- bind_cols(theta = x, y_matrix)%>%
      pivot_longer(cols = -theta, names_to = "Item", values_to = "y")%>%
      mutate(Item = factor(Item, levels = main_vector))

    #plot
    gra <- ggplot(plot_data,mapping = aes(x = theta, y = y))+
      geom_line(linewidth =1.2)+
      labs(x =  x_lab ,y = y_lab, title =  title)+
      theme(plot.title = element_text(hjust = 0.5,size = 8),
            axis.title = element_text(size = 7))+
      facet_wrap(facets = ~Item, ncol = ncol, scales = scale)
  }else if(lines == "ICC"){

    di_items <- which(grade_vector == 1)
    varname <- colnames(y_matrix)%>%str_split(pattern = ".P.",simplify = T)%>%
      .[,1]%>%unique()
    if(sum(varname != main_vector)){
      colnames_y_matrix <- vector(mode = "character")
      low_grade <- ifelse(test = is.include.zore, yes = 0, no = 1)
      for (i in 1:length(main_vector)) {
        if(grade_vector[i]==2 & is.include.zore == F){
          low_grade <- 0
          max_grade <- 1
        }else{
          low_grade <- low_grade
          max_grade <- (grade_vector[i]-ifelse(low_grade==0,1,0))
        }
        for (j in low_grade:max_grade) {
          colnames_y_matrix <- c(colnames_y_matrix, paste0(main_vector[i],
                                                           ".P.",j))
        }
      }
      colnames(y_matrix) <- colnames_y_matrix
    }
    plot_data1 <- bind_cols(theta = x, y_matrix)%>%
      pivot_longer(cols = -theta, names_to = c("Item","score"),values_to = "y",
                   names_sep = ".P.")%>%
      mutate(score = factor(str_sub(score,start = 1,end = 1),
                            levels = 0:max(score)),
             Item = factor(Item, levels = main_vector))

    if(is.include.zore == F){
      plot_data <-  plot_data1 %>%
        filter(score != 0)
    }else{
      plot_data <- plot_data1
    }

    gra <-  ggplot(plot_data, mapping = aes(x = theta, y = y,
                                            colour = score, linetype = score))+
      geom_line(linewidth = 1.05)+
      labs(x = x_lab, y = y_lab, title = title)+
      theme(legend.position = "top",
            plot.title = element_text(hjust = 0.5,size = 8),
            axis.title = element_text(size = 7))+
      facet_wrap(facets = ~Item, ncol = ncol, scales = scale)
  }

  return(gra)
}
#WrightMap
wrightMap_new <- function(person, thresholds, points_size,p_width){
  #histogram for person parameters
  person <- data.frame(x = person)
  histogram <- ggplot(person, aes(x = x)) +
    geom_histogram(fill = "gray", color = "black",
                   bins = 50) +
    labs(x="Latent trait",y = "Frequency")+
    scale_x_continuous(limits = c(-4,4),breaks = -4:4)+
    scale_y_continuous(position = "right")+scale_y_reverse()+
    theme_minimal()+
    theme(axis.ticks.x = element_blank(),
          #axis.text.x = element_blank(),
          axis.ticks.y = element_blank(),
          plot.margin=unit(c(0,0,0,0),'cm'),
          panel.grid.major.x =element_blank(),
          panel.grid.minor.x=element_blank())+
    coord_flip()

  #points plot for item parameters
  if(ncol(thresholds) >=1 ){

    item_names <- rownames(thresholds)
    if(is.list(thresholds)){
      thresholds <- unlist(thresholds) %>% as.numeric() %>%
        matrix(nrow = nrow(thresholds),ncol = ncol(thresholds))
      rownames(thresholds) <- item_names
    }
    colnames(thresholds) <- paste0(1:ncol(thresholds))
    y = as.numeric(thresholds)
    labels <- vector(mode = "character")
    for (i in 1:ncol(thresholds)) {
      labels <- c(labels,
                  paste0(rownames(thresholds),"_",colnames(thresholds)[i]))
    }
    data <- data.frame(y = y, labels = labels)
  }else{
    data <- data.frame(y = as.numeric(thresholds),
                       labels = rownames(thresholds))
  }
  points_plot <- ggplot() +
    labs(x = "Item",y = "Thresholds")+
    annotate(geom = "text",
             x = data$labels,
             y = data$y,
             hjust = 0,
             label = data$labels,
             size = points_size)+
    theme_minimal()+
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          plot.margin=unit(c(0,0,0,0.1),'cm'),
          panel.grid.major.x  = element_blank(),
          panel.grid.minor.x = element_blank())+
    scale_y_continuous(position = "right",
                       limits = c(-4,4),breaks = -4:4)

  combined_plot <- plot_grid(histogram, points_plot,labels = NULL,
                             rel_widths = c(1, p_width), align = "h")
  return(combined_plot)
}
