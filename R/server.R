#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny dplyr stringr mirt ggplot2 golem EstCRM rmarkdown officer officedown flextable
#' @importFrom openxlsx write.xlsx read.xlsx
#' @importFrom plotrix twoord.plot
#' @importFrom cowplot plot_grid
#' @importFrom grDevices jpeg dev.off
#' @importFrom graphics hist text
#' @importFrom stats cor na.omit quantile sd
#' @importFrom bruceR EFA Alpha CFA Corr Describe Freq import lavaan_summary
#' @importFrom semPlot semPaths
#' @importFrom tidyr pivot_longer
#' @importFrom latticeExtra mapplot
#' @importFrom difR difMH difSIBTEST
#' @importFrom lordif lordif
#' @importFrom DT datatable DTOutput renderDT
#' @importFrom stats pchisq
#' @importFrom utils read.csv
#' @noRd
app_server <- function(input, output, session) {
  options(shiny.maxRequestSize = 1024^4)
  #Introduction of this platform-----------------------------------
  output$info <- renderText({
    paste(shiny::h3(strong("Package Information")),
          shiny::p(strong('Package: '), "TestAnaAPP"),
          shiny::p(strong('Version: '), "1.1.2"),
          shiny::p(strong('Dependence: '), "config, ggplot2, mirt, shinydashboard, EstCRM, etc."),
          shiny::p(strong('Description: '), "This application provides exploratory and confirmatory factor analysis,
                   classical test theory, unidimensional and multidimensional item response theory,
                   and continuous item response model analysis, through the 'shiny' interactive interface.
                   In addition,  it offers rich functionalities for visualizing and downloading results.
                   Users can download figures, tables, and analysis reports via the interactive interface. "),
          shiny::p(strong('License: '), "GPL-3"),
          shiny::p(strong('Anthors: '), "Youxiang Jiang, Qing Zeng, and Hongbo Wen."),
          shiny::p(strong('Email: '), tags$a(href="mailto:jiangyouxiang34@163.com", "jiangyouxiang34@163.com")),
          shiny::p(strong('Github: '), tags$a(href="https://github.com/jiangyouxiang/TestAnaAPP",
                                              "https://github.com/jiangyouxiang/TestAnaAPP")),
          shiny::p(strong("Feedback: "), tags$a(href="https://github.com/jiangyouxiang/TestAnaAPP/issues",
                                                   "https://github.com/jiangyouxiang/TestAnaAPP/issues")),
          shiny::p(strong("Citation: "), "Jiang, Y., Zeng, Q., & Wen, H. (2024). TestAnaAPP: An interactive R-shiny application for various test analysis methods. SoftwareX, 28, 101967.",
                   tags$a(href="https://doi.org/10.1016/j.softx.2024.101967",
                          "https://doi.org/10.1016/j.softx.2024.101967")),

          shiny::h3(strong("Software Feature Overview")),
          shiny::p(strong("1. Factor Analysis (EFA & CFA)")),
          shiny::p("This module combines exploratory and confirmatory factor analysis tools. Users can identify
                   underlying factor structures with EFA and validate hypothesized factor models with CFA. This
                   module is essential for understanding the relationships among observed variables and testing
                   model fit."),
          shiny::p(strong("2. Classical Test Theory (CTT)")),
          shiny::p("This module provides a comprehensive set of tools for analyzing the reliability and validity
                   of test scores. Users can evaluate the internal consistency of test items, calculate item
                   discrimination and difficulty, and assess the correlation between test items."),
          shiny::p(strong("3. Item Response Theory (IRT)")),
          shiny::p("This module offers a range of tools for analyzing item response theory. Users can estimate
                   item parameters, evaluate test reliability, and assess the fit of the model. This module is
                   essential for understanding the relationship between test items and test takers."),
          shiny::p(strong("4. Multidimensional IRT (MIRT)")),
          shiny::p("Specifically designed for multidimensional data, the MIRT module allows users to analyze
                   test data across multiple dimensions, revealing the multifaceted abilities of examinees."),
          shiny::p(strong("5. Continuous Response Model (CRM)")),
          shiny::p("The CRM module handles modeling and analysis of continuous response data, suitable
                   for tests or questionnaires with continuous scoring."),
          shiny::p(strong("6. Differential Item Functioning (DIF)")),
          shiny::p("This module detects differential item functioning across different groups, helping identify
                   potential biases in the test to ensure fairness and validity."),


          shiny::h3(strong("How to use this application for data analysis?")),
          shiny::p("1. When you see this interface, it indicates that you have successfully installed this program,
                   which is the first step to using 'TestAnaAPP'."),
          shiny::p("2. You need to understand that 'TestAnaAPP' presents various analysis contents in modular forms.
                   When you need to perform a specific analysis using 'TestAnaAPP', you can directly navigate to
                   that interface after uploading the data. "),
          shiny::p("3. When it involves dimensional information of the test, you need to upload an XLSX file
                   in the interface of uploading dimensional information to illustrate the test structure.
                   Please edit your document following the examples provided in 'TestAnaAPP'. "),
          shiny::p("4. During the operation, please carefully read the textual prompts presented on each interface
                   to ensure that the program can execute your intentions correctly. "),

          shiny::h3(strong("If you have any questions or suggestions, please contact us.")),
          br(),br()

    )
  })

  EFA_module(input, output, session)
  CFA_module(input, output, session)
  CTT_module(input, output, session)
  UIRT_module(input, output, session)
  MIRT_module(input, output, session)
  CRM_module(input, output, session)
  DIF_module(input, output, session)
}

#Functions-----------------------------------------------------
cat_number <- function(vector){
  number <- table(vector,useNA = "no")%>%length()
  return(number)
}

box_show_theme <- function(value){
  return(shinycssloaders::withSpinner(ui_element = value,color="#0dc5c1",type=6,size = 1.5 ))
}
DT_dataTable_Show <- function(x){
  if(is.data.frame(x) == F){
    as.data.frame(x)
  }
  DT::datatable(x,
                filter =list(position = 'top', clear = TRUE, plain = TRUE),
                options = list(scrollX = TRUE))
}
dimension_recode <- function(dimension = NULL, Qmatrix){
  if(is.null(dimension)){
    item_names <- rownames(Qmatrix)
    dimnames <- colnames(Qmatrix)
    S <- vector(mode = "character")
    for (i in 1:ncol(Qmatrix)) {
      Ss <- c(dimnames[i],"=",paste0(item_names[which(Qmatrix[,i]==1)],","))%>%
        paste0(collapse = "")
      if(i==length(dimnames)){
        S <- c(S, str_sub(string = Ss, start = 1, end = (str_length(Ss)-1)))#Remove the last ",".
      }else{
        S <- c(S, str_sub(string = Ss, start = 1, end = (str_length(Ss)-1)))
      }
    }
    cov <- paste0("COV = ",paste0(dimnames, "*", collapse = ""))

    S <- paste0(S, "
                ",collapse = "")#No COV matrix
    SS <- paste0(S, str_sub(cov, start = 1, end = (str_length(cov)-1)),collapse = "")#Include COV matrix
    is.within_item <- ifelse(sum(Qmatrix)>length(item_names),TRUE, FALSE)
  }else{
    dimnames <- table(dimension[,2])%>%names()

    S <- vector(mode = "character")
    for (i in 1:length(dimnames)) {
      Ss <- paste0(dimnames[i]," = " ,paste0(dimension[which(dimension[,2]==dimnames[i]),1],",", collapse = ""))
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
    is.within_item <- NULL

  }
  return(list(dim = S, COV = SS, F_names = dimnames, F_n = length(dimnames),
              is.within_item = is.within_item))
}
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
  if(value == "None (not suggested)"){
    return("none")
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
independent_method <- function(value){
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
  # if(value == "Zh (Drasgow, Levine, & Williams, 1985)"){
  #   return("Zh")
  # }
  if(value == "chi-squared method (Bock,1972)"){
    return("X2")
  }
  if(value == "G2 statistic (McKinley & Mills, 1985)"){
    return("G2")
  }
  # if(value == "Stone's (2000) fit statistics"){
  #   return("X2*")
  # }
}

#CTT item parameters
item_ana<- function(data){#Difficult, discrimination and CV
  # if(length(which(is.na(data)))>=1){
  #   data<- na.omit(data)
  #   warning("The case with NA were deleted.")
  # }
  cat_all <- apply(data, MARGIN = 2, FUN = cat_number)#The number of categories.
  cat_2 =  which( cat_all == 2)#0 or 1
  cat_m = which(cat_all >2)#Multi-level scores
  item_s = apply(data, 2, max, na.rm = T)

  item_analysis<- matrix(NA, nrow = ncol(data), ncol = 4)
  rownames(item_analysis)<- colnames(data)
  colnames(item_analysis)<- c("Difficulty","Discrimination","Coefficient of variation", "Item-total correlation")

  data1<- cbind(data,rowSums(data,na.rm = T))
  data1<- data1[order(data1[,ncol(data1)],decreasing = F),]
  zf <- data1[,ncol(data1)]
  low_N <- which(zf <= round(quantile(zf, 0.27), 2))
  hig_N <- which(zf >= round(quantile(zf, 0.73), 2))

  low_data<- data1[low_N,]
  high_data<- data1[hig_N,]

  for (i in cat_2) {
    low_mean <- mean(low_data[,i],na.rm = T)
    #Difficult
    item_analysis[i,1]<- ((mean(high_data[,i],na.rm = T)+
                             ifelse(is.na(low_mean),0,low_mean))/item_s[i])/2#
    #Discrimination
    item_analysis[i,2]<- (mean(high_data[,i],na.rm = T)-
                            ifelse(is.na(low_mean),0,low_mean))/item_s[i]#

    #CV
    item_analysis[i,3]<- (sd(data[,i],na.rm = T)/mean(data[,i],na.rm = T))*100
    item_analysis[i,4] <- point_biserial(binary_item = data[,i],
                                         total_score = rowSums(data,na.rm = T))
  }

  for (j in cat_m) {
    low_mean <- mean(low_data[,j],na.rm = T)
    #Difficult
    item_analysis[j,1]<- mean(data1[,j],na.rm = T)/item_s[j]
    #Discrimination
    item_analysis[j,2]<- ((mean(high_data[,j],na.rm = T)-
                             ifelse(is.na(low_mean),0,low_mean))/item_s[j])

    #CV
    item_analysis[j,3]<- (sd(data[,j],na.rm = T)/mean(data[,j],na.rm = T))*100
    item_analysis[j,4] <- cor(x = data[,j], y = rowSums(data,na.rm = T))
  }

  return(item_analysis)
}
# Function to calculate point-biserial correlation
point_biserial <- function(binary_item, total_score) {
  # Calculate the means and standard deviation
  mean_total <- mean(total_score)
  sd_total <- sd(total_score)
  # Calculate the means of total scores for the two groups
  if(max(binary_item,na.rm = T) >= 1){#
    binary_item[which(binary_item == max(binary_item,na.rm = T))] <- 1
    binary_item[which(binary_item == min(binary_item,na.rm = T))] <- 0
  }
  mean_1 <- mean(total_score[binary_item == max(binary_item,na.rm = T)],na.rm = T)
  mean_0 <- mean(total_score[binary_item == min(binary_item,na.rm = T)],na.rm = T)

  # Calculate the proportion of 1s (p) and 0s (q)
  p <- mean(binary_item,na.rm = T)
  q <- 1 - p

  # Calculate the point-biserial correlation
  r_pb <- ((mean_1 - mean_0)* sqrt(p * q) / sd_total)
  if(length(table(binary_item))==1){
    r_pb <- NA
  }

  return(r_pb)
}
plot_wrap <- function(theta,
                      y_matrix,
                      lines = "ICC",  #or IIC
                      is.include.zore = FALSE,
                      grade_vector = NULL,     #The number of curve for a single item.
                      main_vector,    #Main
                      y_lab = NULL,
                      x_lab = NULL,
                      title = NULL,   #The main for wrap.
                      ncol = 5,
                      scale = "fixed",
                      title_size = 15,
                      xy_size = 12,
                      Item_label_size = 10){
  if(length(theta) == 0 | ncol(y_matrix) == 0){
    stop("The length of theta or y_matrix is 0.")
  }
  y <- NULL
  score <- NULL
  Item <- NULL
  Score <- NULL

  #A single curve
  if(lines == "IIC"){
    y_matrix <- as.data.frame(y_matrix)
    colnames(y_matrix) <- main_vector
    plot_data <- bind_cols("theta" = theta, y_matrix) %>%
      pivot_longer(cols = -1, names_to = "Item", values_to = "y") %>%
      mutate("Item" = factor(Item, levels = main_vector))
    colnames(plot_data) <- c("theta","Item","y")

    #plot
    gra <- ggplot(plot_data, mapping = aes(x = theta, y = y))+
      geom_line(linewidth =1.05)+
      labs(x =  x_lab ,y = y_lab, title =  title)+
      theme_classic()+
      theme(plot.title = element_text(hjust = 0.5,size = title_size),
            axis.title = element_text(size = xy_size),
            strip.background = element_blank(),
            strip.text = element_text(size = Item_label_size))+
      facet_wrap(facets = ~Item, ncol = ncol, scales = scale)
  }else if(lines == "ICC"){

    di_items <- which(grade_vector == 1)
    varname <- colnames(y_matrix)%>%str_split(pattern = "\\.P\\.",simplify = T)
    varname <- varname[,1] %>% unique()
    if(sum(varname != main_vector) >= 1){
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
                                                           "\\.P\\.",j))
        }
      }
      colnames(y_matrix) <- colnames_y_matrix
    }
    plot_data1 <- bind_cols("theta" = theta, y_matrix) %>%
      pivot_longer(cols = -1, names_to = c("Item","score"),values_to = "y",
                   names_sep = "\\.P\\.") %>%
      mutate(
        "Score" = factor(str_sub(score,start = 1,end = 1),
                         levels = 0:max(score)),
        "Item" = factor(Item, levels = main_vector)
      )
    colnames(plot_data1) <- c("theta","Item","Score","y")

    if(is.include.zore == F){
      plot_data <- plot_data1[which(plot_data1$Score != 0), ]
    }else{
      plot_data <- plot_data1
    }

    gra <-  ggplot( plot_data, mapping = aes(x = theta,
                                             y = y,
                                             colour = Score,
                                             linetype = Score))+
      geom_line(linewidth = 1.05)+
      labs(x = x_lab, y = y_lab, title = title)+
      theme_classic()+
      theme(legend.position = "right",
            plot.title = element_text(hjust = 0.5,size = title_size),
            axis.title = element_text(size = xy_size),
            strip.background = element_blank(),
            strip.text = element_text(size = Item_label_size))+
      facet_wrap(facets = ~Item, ncol = ncol, scales = scale)
  }

  return(gra)
}
utils::globalVariables(c("xxx"))
#WrightMap
wrightMap_new <- function(person,
                          thresholds,
                          binwidth = 0.5,
                          point_label,
                          points_size,
                          p_width) {
  if (missing(person)) {
    stop("'theta' needs to be specified", call. = FALSE)
  }
  #empty thresholds
  if(nrow(thresholds) == 0){
    stop("The length of thresholds is 0.")
  }
  if(point_label == "Numeric"){
    names1 <- 1:nrow(thresholds)

  }else if(point_label == "Column names"){
    names1 <- rownames(thresholds)
  }
  #points plot for item parameters
  if(ncol(thresholds) >1 ){

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
                  paste0(names1,
                         "_",colnames(thresholds)[i]))
    }
    b <- y
    ITEM.NAMES <- labels
  }else{
    if(is.data.frame(thresholds)){
      thresholds <- as.matrix(thresholds)
    }
    b <- as.numeric(thresholds)
    ITEM.NAMES <- names1
  }

  df.person <- data.frame(person = person)

  person.cut.points <- seq(
    min(c(person, b), na.rm = TRUE) - binwidth / 2,
    max(c(person, b), na.rm = TRUE) + binwidth / 2, binwidth / 2
  )
  b.cut.points <- cut(b, person.cut.points, include.lowest = TRUE)
  levels(b.cut.points) <- person.cut.points[-length(person.cut.points)] + diff(person.cut.points) / 2
  b.cut.points <- as.numeric(paste(b.cut.points))

  df.b <- data.frame(item = as.character(ITEM.NAMES), b = b, y = b.cut.points)
  df.b$x <- 0
  for (i in unique(df.b$y)) {
    n <- nrow(df.b[df.b$y == i, ])
    df.b[df.b$y == i, "x"] <- 1:n
  }

  df.b$item <- as.character(df.b$item)
  maxn <- max(nchar(df.b$item))

  if (point_label == "Numeric") {
    while (any(nchar(df.b$item) < maxn)) {
      df.b$item <- ifelse(nchar(df.b$item) < maxn, paste0("0", df.b$item), df.b$item)
    }
  } else {
    df.b$item <- as.character(df.b$item)
    while (any(nchar(df.b$item) < maxn)) {
      df.b$item <- ifelse(nchar(df.b$item) < maxn, paste0(df.b$item, " "), df.b$item)
    }
  }

  df.b$item[df.b$x > 1] <- paste("|", df.b$item[df.b$x > 1])

  lim.x.min <- min(c(person, b), na.rm = TRUE) - binwidth
  lim.x.max <- max(c(person, b), na.rm = TRUE) + binwidth

  #histogram for person parameters
  df.person <- data.frame("xxx" = person)
  histogram <- ggplot(df.person, aes(x = xxx)) +
    geom_histogram(fill = "gray", color = "black",na.rm = T,
                   binwidth = binwidth) +
    labs(x="Latent trait",y = "Frequency")+
    scale_x_continuous(limits = c(lim.x.min, lim.x.max))+
    scale_y_continuous(position = "right")+scale_y_reverse()+
    theme_minimal()+
    theme(axis.ticks.x = element_blank(),
          #axis.text.x = element_blank(),
          axis.ticks.y = element_blank(),
          plot.margin=unit(c(0,0,0,0),'cm'),
          panel.grid.major.x =element_blank(),
          panel.grid.minor.x=element_blank())+
    coord_flip()

  points_plot <- ggplot(df.b, aes(x = .data$x, y = .data$y, label = .data$item)) +
    geom_text(hjust = 0, size = points_size, vjust = 0.5, na.rm = TRUE) +
    scale_y_continuous(position = "right",limits = c(lim.x.min, lim.x.max)) +
    scale_x_continuous(limits = c(min(df.b$x), max(df.b$x) + 0.75)) +
    labs(x = "Item",y = "Thresholds")+
    theme_minimal() +
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          plot.margin=unit(c(0,0,0,0.1),'cm'),
          panel.grid.major.x  = element_blank(),
          panel.grid.minor.x = element_blank())

  combined_plot <- cowplot::plot_grid(histogram, points_plot,labels = NULL,
                                      rel_widths = c(1, p_width), align = "h")
  return(combined_plot)
}

DT_dataTable_Show <- function(x){
  DT::datatable(as.data.frame(x),
                filter =list(position = 'top', clear = TRUE, plain = TRUE),
                options = list(scrollX = TRUE))


}

read_file <- function(path){
  inFile <- path
  # Detecting file type
  if(grepl(".xlsx", inFile$datapath)){
    dataset <- openxlsx::read.xlsx(inFile$datapath,sheet = 1)
  }else if(grepl(".xls", inFile$datapath)){
    stop("XLS file is not supported.")
  }else if(grepl(".csv", inFile$datapath)){
    dataset <- utils::read.csv(inFile$datapath,header = T)
  }else{
    dataset <- bruceR::import(inFile$datapath)
  }
  data <- as.data.frame(dataset)

  data <- dataset %>% unlist() %>% as.numeric() %>%
    matrix(ncol = ncol(dataset)) %>% as.data.frame()
  colnames(data) <- colnames(dataset)

  data
}
