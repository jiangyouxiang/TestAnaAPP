#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shinydashboard
#' @noRd
app_ui <- function() {
 dashboardPage(skin = "blue",
                      dashboardHeader(title = "TEST ANALYSIS PLATFORM",titleWidth = 400),#The name of this platform
                      #Pages----------------------------------------------------------------------------------
                      dashboardSidebar(
                        sidebarMenu(id="sidebarmenu",
                                    menuItem("TestAnaAPP",tabName = "info",icon = icon("info-circle")),#Introduction
                                    menuItem("Upload Response *",tabName = "uploaddata",icon = icon("table")),
                                    menuItem("Exploratory Factor Analysis",tabName = "EFA",icon = icon("list-alt"),
                                             menuSubItem("Factor Loading",tabName = "loading",icon = shiny::icon("angle-double-right")),
                                             menuSubItem("Scree Plot",tabName = "screen_plot",icon = shiny::icon("angle-double-right"))),

                                    menuItem("Confirmatory Factor Analysis",tabName = "CFA",icon = icon("list-alt"),
                                             menuSubItem("Factor Loading",tabName = "loading_CFA",icon = shiny::icon("angle-double-right")),
                                             menuSubItem("Path Diagram",tabName = "path_plot",icon = shiny::icon("angle-double-right"))),

                                    menuItem("Classic Test Theory",tabName = "CTT",icon = icon("th"),
                                             menuSubItem("Descriptive Statistics",tabName = "summary",icon = shiny::icon("angle-double-right")),
                                             menuSubItem("CTT Parameters",tabName = "CTT_par",icon = shiny::icon("angle-double-right")),
                                             menuSubItem("CTT Reliability",tabName = "CTT_relia",icon = shiny::icon("angle-double-right")),
                                             menuSubItem("Correlation Coefficient",tabName = "relate_eff",icon = shiny::icon("angle-double-right"))),

                                    menuItem("Item Response Theory",tabName = "IRT",icon = icon("cogs"),
                                             menuSubItem("Model fit *",tabName = "IRTmodelfit",icon = shiny::icon("angle-double-right")),
                                             menuSubItem("Hypothesis Testing",tabName = "IRTassum_test",icon = shiny::icon("angle-double-right")),
                                             menuSubItem("Item Fit",tabName = "IRTitemfit",icon = shiny::icon("angle-double-right")),
                                             menuSubItem("Item Parameters",tabName = "IRTitempar",icon = shiny::icon("angle-double-right")),
                                             menuSubItem("Person Parameters",tabName = "IRTperson",icon = shiny::icon("angle-double-right")),
                                             menuSubItem("WrightMap",tabName = "IRTwright",icon = shiny::icon("angle-double-right")),
                                             menuSubItem("Item Characteristic Curve",tabName = "IRTicc",icon = shiny::icon("angle-double-right")),
                                             menuSubItem("Item Information Curve",tabName = "IRTiic",icon = shiny::icon("angle-double-right")),
                                             menuSubItem("Test Information Curve",tabName = "IRTtic",icon = shiny::icon("angle-double-right"))),

                                    menuItem("Multidimensional IRT",tabName = "MIRT",icon = icon("cogs"),
                                             menuSubItem("Upload Dimension *",tabName = "MIRTdim_info",icon = shiny::icon("angle-double-right")),
                                             menuSubItem("Model Fit *",tabName = "MIRTmodelfit",icon = shiny::icon("angle-double-right")),
                                             menuSubItem("Hypothesis testing",tabName = "MIRTassum_test",icon = shiny::icon("angle-double-right")),
                                             menuSubItem("Item Fit",tabName = "MIRTitemfit",icon = shiny::icon("angle-double-right")),
                                             menuSubItem("Item Parameters",tabName = "MIRTitempar",icon = shiny::icon("angle-double-right")),
                                             menuSubItem("Person Parameters",tabName = "MIRTperson",icon = shiny::icon("angle-double-right")),
                                             menuSubItem("Wright Map",tabName = "MIRTwright",icon = shiny::icon("angle-double-right")),
                                             menuSubItem("Item Characteristic Curve",tabName = "MIRTicc",icon = shiny::icon("angle-double-right")),
                                             menuSubItem("Item Information Curve",tabName = "MIRTiic",icon = shiny::icon("angle-double-right")),
                                             menuSubItem("Test Information Curve",tabName = "MIRTtic",icon = shiny::icon("angle-double-right")))
                        )),
                      dashboardBody(
                        #A. Introduction page---------------------------------------------------------------------------
                        tabItems(
                          tabItem(tabName = "info",
                                  fluidRow(
                                    box(title = "Welcome!", solidHeader = TRUE, status = "info",
                                        htmlOutput("info"),width = 12)
                                  )),
                          #B. Upload response data---------------------------------------------------------------------------
                          tabItem(
                            tabName = "uploaddata",
                            fluidRow(column(8,
                                            box(title="Score Data", DT::dataTableOutput("Response")%>%
                                                  box_show_theme(),
                                                solidHeader = TRUE, status = "warning",width = 12),
                            ),
                            column(4,
                                   box(title = "Upload File", status = "warning", solidHeader = TRUE,
                                       fileInput("res_data", "Please upload the test data file that needs to be analyzed.
                                               You can upload TXT, CSV, Excel, SPSS, Stata, or other file formats.
                                               Please make sure that the file only contains the score data, and the column names should not be named with pure numbers.",
                                                 placeholder="File",buttonLabel = "Browse",
                                                 accept = c("xlsx","xls","csv","sav","txt","dta")
                                       ),width = 16))
                            )),
                          #C. EFA page-----------------------------------------------------------------------
                          tabItem(
                            tabName = "loading",
                            fluidRow(column(8,
                                            box(title="Eigenvalues", DT::dataTableOutput("CTT_EFA_eigenvalues")%>%
                                                  box_show_theme(),
                                                solidHeader = TRUE, status = "info",width = 12),
                                            box(title="Factor Loadings", DT::dataTableOutput("EFA_load")%>%
                                                  box_show_theme(),
                                                solidHeader = TRUE, status = "info",width = 12)
                            ),
                            column(4,
                                   box(title = "Method",status = "warning", solidHeader = TRUE,width = 12,
                                       selectInput(inputId = "EFA_method",label = "Parameter estimation methods",selectize = TRUE,
                                                   choices = list("Principal Component Analysis",
                                                                  "Principal Axis Factor Analysis",
                                                                  "Maximum Likelihood Factor Analysis",
                                                                  "Minimum Residual Factor Analysis",
                                                                  "Unweighted Least Squares Factor Analysis",
                                                                  "Ordinary Least Squares Factor Analysis",
                                                                  "Weighted Least Squares Factor Analysis",
                                                                  "Generalized Least Squares Factor Analysis",
                                                                  "Alpha Factor Analysis (Kaiser & Coffey, 1965)"),
                                                   selected = "Principal Component Analysis"),
                                       selectInput(inputId = "rotation_method",label = "Factor rotation methods",
                                                   selectize = TRUE,
                                                   choices = list("Varimax",
                                                                  "Direct Oblimin",
                                                                  "Promax",
                                                                  "Quartimax",
                                                                  "Equamax"),
                                                   selected = "Varimax"),
                                       submitButton( "Updata results")),
                                   box(title = "Download results", status = "success",solidHeader = TRUE,
                                       downloadButton(outputId = "EFA_result", label = "Download"),
                                       width = 12)))
                          ),
                          tabItem(
                            tabName = "screen_plot",
                            fluidRow(column(8,
                                            box(title="Scree Plot", plotOutput("EFA_plot")%>%
                                                  box_show_theme(),

                                                solidHeader = TRUE, status = "info",width = 12)),
                                     column(4,
                                            box(title = "Download figure",status = "success",solidHeader = TRUE, width = 12,
                                                downloadButton(outputId = "EFA_plotfile", label = "Download"))))
                          ),
                          #D. CFA page-------------------------------------------------------------
                          tabItem(
                            tabName = "loading_CFA",
                            fluidRow(column(8,
                                            box(title = "Test Structure",status =  "warning",
                                                solidHeader = TRUE, width = 12,
                                                dataTableOutput("CFA_dimension_example")%>%
                                                  box_show_theme()),
                                            box(title = "Model Fit index",status =  "info",
                                                solidHeader = TRUE, width = 12,
                                                DT::dataTableOutput("CFA_fit_index")%>%
                                                  box_show_theme()),
                                            box(title = "Factor Loadings",status =  "info",
                                                solidHeader = TRUE, width = 12,
                                                DT::dataTableOutput("CFA_loading")%>%
                                                  box_show_theme())),
                                     column(4,
                                            box(title = "Upload Dimension",solidHeader = TRUE,status = "warning",width = 12,
                                                fileInput(inputId = "dimensionfile_cfa",
                                                          "Please upload an Excel file with example data on the left side to illustrate the correspondence between each question and dimension.
                                                        Please note that currently we do not support data where one item measures multiple dimensions simultaneously.",
                                                          placeholder="File",buttonLabel = "Browse",
                                                          accept = c("xlsx","xls","csv"))),
                                            box(title = "Download results",status = "success",solidHeader = TRUE, width = 12,
                                                downloadButton(outputId = "CFA_file", label = "Download")))
                            )
                          ),

                          tabItem(
                            tabName = "path_plot",
                            fluidRow(column(12,
                                            box(title = "Path Diagram",status =  "info",
                                                solidHeader = TRUE, width = 12,
                                                uiOutput("CFA_fit_plot1")%>%
                                                  box_show_theme(),
                                                sliderInput(inputId = "CFA_plot_height",label = "Adjust height",
                                                            min = 300,max = 1800,step = 30,value = 800),
                                                br(),
                                                submitButton( "Updata plot"),
                                                br(),
                                                downloadButton("CFA_plot_file",label = "Download"))))
                          ),

                          #E. CTT page-----------------------------------------------------------------------------
                          tabItem(
                            tabName = "summary",
                            fluidRow(column(8,
                                            box(title="Descriptive Statistics", DT::dataTableOutput("CTT_summary")%>%
                                                  box_show_theme(),
                                                solidHeader = TRUE, status = "info",width = 12),
                                            box(title="Total Score distribution", plotOutput("scores_plot")%>%
                                                  box_show_theme(),
                                                downloadButton(outputId = "scores_plotfile", label = "Download"),#下载
                                                solidHeader = TRUE, status = "info",width = 12)),
                                     column(4,
                                            box(title = "Download results",status = "success",solidHeader = TRUE,  width = 12,
                                                downloadButton(outputId = "summary_result", label = "Download")))
                            )),
                          tabItem(
                            tabName = "CTT_par",
                            fluidRow(column(8,
                                            box(title="Item parameters", DT::dataTableOutput("CTT_itempar")%>%
                                                  box_show_theme(),
                                                solidHeader = TRUE, status = "info",width = 12)),
                                     column(4,
                                            box(title = "Download results", status = "success",solidHeader = TRUE,
                                                downloadButton(outputId = "CTT_result", label = "Download"),
                                                width = 12)))
                          ),
                          tabItem(
                            tabName = "CTT_relia",
                            fluidPage(column(8,
                                             box(title = "Test reliability",
                                                 DT::dataTableOutput("CTT_reliability")%>%
                                                   box_show_theme(),solidHeader = TRUE,
                                                 status = "info",width = 12),
                                             box(title = "Alpha coefficient after item deletion",
                                                 DT::dataTableOutput("CTT_item_alpha")%>%
                                                   box_show_theme(),solidHeader = TRUE,
                                                 status = "info",width = 12)),
                                      column(4,
                                             box(title = "Download reliability",status = "success",
                                                 solidHeader = TRUE, width = 12,
                                                 downloadButton(outputId = "CTT_relia_file",label = "Download"))))
                          ),
                          tabItem(
                            tabName = "relate_eff",
                            fluidPage(column(8,
                                             box(title = "Pearson coefficient matrix",
                                                 DT::dataTableOutput("CTT_relate_eff")%>%
                                                   box_show_theme(),solidHeader = TRUE,
                                                 status = "info",width = 12),
                                             box(title = "The P value corresponding to above correlation",
                                                 DT::dataTableOutput("CTT_relate_p")%>%
                                                   box_show_theme(),solidHeader = TRUE,
                                                 status = "info",width = 12)),
                                      column(4,
                                             box(title = "Download matrix",status = "success",
                                                 solidHeader = TRUE, width = 12,
                                                 downloadButton(outputId = "CTT_relatefile",label = "Dowload"))))
                          ),
                          #F. IRT page----------------------------------------------------------------------------------
                          tabItem(
                            tabName = "IRTmodelfit",
                            fluidRow(column(8,
                                            box(title = "Relative fit indices",solidHeader = TRUE,
                                                status = "info",width = 12,
                                                dataTableOutput("IRT_modelfit_relat")%>%
                                                  box_show_theme()),
                                            box(title="Absolute fit indices",DT::dataTableOutput("IRT_modelfit")%>%
                                                  box_show_theme(),
                                                solidHeader = TRUE,
                                                status = "info",width = 12),
                            ),
                            column(4,
                                   box(title="Basic settings",
                                       solidHeader = TRUE,status = "warning",width = 12,

                                       selectInput(inputId = "modelselect",label = "Model selection",selectize = T,
                                                   choices = list("NULL",
                                                                  "Rasch model (1PL)",
                                                                  "Two parameters logistic model (2PL)",
                                                                  "Three parameters logistic model (3PL)" ,
                                                                  "Four parameters logistic model (4PL)" ,
                                                                  "Graded response model (GRM)" ,
                                                                  "Partial credit model (PCM)",
                                                                  "Generalized partial credit model (GPCM)"),
                                                   selected = NULL),
                                       "Note: the Rasch model, 2PL, 3PL, and 4PL model
                                              are only applicable to items with two response categories",
                                       br(),br(),

                                       selectInput(inputId = "IRT_est_method",label = "Estimation method",
                                                   selectize = TRUE,
                                                   choices = list("standard EM algorithm",
                                                                  "quasi-Monte Carlo EM estimation",
                                                                  "Monte Carlo EM estimation",
                                                                  "Stochastic EM algorithm"),
                                                   selected = "standard EM algorithm"),
                                       "Note: For unidimensional models, it is recommended to use the standard EM algorithm. For multidimensional models,
                                              it is recommended to use quasi-Monte Carlo EM estimation.",
                                       br(),br(),

                                       submitButton( "Updata results")),
                                   box(title = "Download results",solidHeader = TRUE,status = "success",width = 12,
                                       downloadButton(outputId = "IRT_resultfile", label = "Download results"),
                                       br(),
                                       br(),
                                       downloadButton(outputId = "IRT_report",label = "Download analysis report")))
                            )),
                          tabItem(
                            tabName = "IRTassum_test",
                            fluidPage(column(8,
                                             box(title="Independence test",status = "info",width = 12,solidHeader = TRUE,
                                                 DT::dataTableOutput("IRT_Q3")%>%
                                                   box_show_theme()
                                             )),
                                      column(4,
                                             box(title = "Index selection",width = 12,solidHeader = TRUE,
                                                 status = "warning",
                                                 selectInput(inputId = "IRT_select_independent",label = NULL,
                                                             choices = list("LD-X2 (Chen & Thissen, 1997)",
                                                                            "Q3 (Yen, 1984)"),
                                                             selected = "Q3 (Yen, 1984)",selectize = TRUE),
                                                 submitButton("Updata results"))))
                          ),
                          tabItem(
                            tabName = "IRTitemfit",
                            fluidPage(column(8,
                                             box(title="Item fit",dataTableOutput("IRT_itemfit")%>%
                                                   box_show_theme(),
                                                 solidHeader = TRUE,
                                                 status = "info",width = 12)),
                                      column(4,
                                             box(title = "Indices selection",width = 12,solidHeader = TRUE,
                                                 status = "warning",
                                                 selectInput("IRT_itemfit_method", label = NULL,
                                                             choices = list("chi-squared test (Kang & Chen, 2007)",
                                                                            "chi-squared method (Bock,1972)",
                                                                            "G2 statistic (McKinley & Mills, 1985)"),
                                                             selected = "chi-squared test (Kang & Chen, 2007)",
                                                             selectize = TRUE),
                                                 submitButton( "Updata results"))))
                          ),
                          tabItem(
                            tabName = "IRTitempar",
                            fluidPage(column(8,
                                             box(title="Item parameters",DT::dataTableOutput("IRT_itempar")%>%
                                                   box_show_theme(),
                                                 solidHeader = TRUE,
                                                 status = "info",width = 12)))
                          ),
                          tabItem(
                            tabName = "IRTperson",
                            fluidPage(column(8,
                                             box(title = "Person parameter", solidHeader = TRUE, status = "info",
                                                 dataTableOutput("IRT_person")%>%
                                                   box_show_theme(),
                                                 width = 12)),
                                      column(4,
                                             box(title = "Estimation method",width = 12,solidHeader = TRUE,
                                                 status = "warning",
                                                 selectInput(inputId = "IRT_person_est_method",
                                                             label = NULL,
                                                             choices = list("expected a-posteriori (EAP)",
                                                                            "maximum a-posteriori (MAP)",
                                                                            "maximum likelihood (ML)",
                                                                            "weighted likelihood estimation (WLE)"),
                                                             selected = "expected a-posteriori (EAP)",selectize = TRUE),
                                                 submitButton( "Updata plot"))))
                          ),
                          tabItem(
                            tabName = "IRTwright",
                            fluidPage(column(8,
                                             box(title = "WrightMap", solidHeader = TRUE, status = "info",
                                                 uiOutput("IRT_wright1")%>%
                                                   box_show_theme(),
                                                 width = 12)),
                                      column(4,
                                             box(title = "Adjust plot",width = 12,solidHeader = TRUE,
                                                 status = "warning",
                                                 sliderInput(inputId = "IRT_wright_map_height",label = "The height of WrightMap.",
                                                             min = 300,max = 1800,step = 30,value = 400),
                                                 br(),
                                                 sliderInput(inputId = "IRT_wright_map_p_size",label = "The size of points.",
                                                             min = 1,max = 7,step = 0.5,value = 3),
                                                 br(),
                                                 sliderInput(inputId = "IRT_wright_p_width",label = "The width of right plot.",
                                                             min = 1, max = 5, value = 1.618, step = 0.25),
                                                 submitButton( "Updata plot")),
                                             box(title = "Download figure",solidHeader = TRUE,status = "success",width = 12,
                                                 downloadButton(outputId = "IRT_wrightfile", label = "Download"))))
                          ),
                          tabItem(
                            tabName = "IRTicc",
                            fluidPage(column(8,
                                             box(title = "Item characteristic curve (ICC)", solidHeader = TRUE, status = "info",
                                                 uiOutput(outputId = "IRT_ICC1")%>%
                                                   box_show_theme(),
                                                 width = 12)),
                                      column(4,
                                             box(title = "Adjust the figures (ICC and IIC)",width = 12,solidHeader = TRUE,
                                                 status = "warning",
                                                 sliderInput(inputId = "wrap_height", label = "Select height",
                                                             min = 300, max = 1800,value = 400,step = 30),
                                                 br(),
                                                 selectInput(inputId = "wrap_ncol",
                                                             label = "Select column number",
                                                             choices = list("2","3","4","5","6","7"),
                                                             selected = "4"),
                                                 submitButton( "Updata plot")),
                                             box(title = "Download figure",solidHeader = TRUE,status = "success",width = 12,
                                                 downloadButton(outputId = "IRT_ICCfile", label = "Download"))))
                          ),
                          tabItem(
                            tabName = "IRTiic",
                            fluidPage(column(8,
                                             box(title = "Item information curve (IIC)", solidHeader = TRUE, status = "info",
                                                 uiOutput("IRT_IIC1")%>%
                                                   box_show_theme(),
                                                 width = 12)),
                                      column(4,
                                             box(title = "Download figure",solidHeader = TRUE,status = "success",width = 12,
                                                 downloadButton(outputId = "IRT_IICfile", label = "Download"))))
                          ),
                          tabItem(tabName = "IRTtic",
                                  fluidRow(
                                    column(8,
                                           box(title = "Test information curve (TIC)", solidHeader = TRUE, status = "info",
                                               plotOutput("IRT_TIC")%>%
                                                 box_show_theme(),
                                               width = 12)),
                                    column(4,
                                           box(title = "Download figure",solidHeader = TRUE,status = "success",width = 12,
                                               downloadButton(outputId = "IRT_TICfile", label = "Download"))))
                          ),
                          #I. MIRT page---------------------------------------------------------------------------------
                          tabItem(
                            tabName = "MIRTdim_info",
                            fluidPage(column(8,
                                             box(title = "Test dimension", solidHeader = TRUE,width = 12,
                                                 status = "warning",
                                                 dataTableOutput(outputId = "dimension_example")%>%
                                                   box_show_theme()),
                                             box(title = "Dimension and item",solidHeader = TRUE,width = 12,
                                                 status = "warning",
                                                 textOutput("dimension_code")%>%
                                                   box_show_theme())),
                                      column(4,
                                             box(title = "Upload dimension information",solidHeader = TRUE,status = "warning",width = 12,
                                                 fileInput(inputId = "dimensionfile",
                                                           "Please upload an Excel file with example data on the left side to
                                                         illustrate the correspondence between each question and dimension.
                                                         Please note that currently we do not support data
                                                         where one item measures multiple dimensions simultaneously.",
                                                           placeholder="File",buttonLabel = "Browse",
                                                           accept = c("xlsx","xls","csv")))))
                          ),

                          tabItem(
                            tabName = "MIRTmodelfit",
                            fluidPage(column(8,
                                             box(title = "Relative model fit indices",solidHeader = TRUE,
                                                 status = "info",width = 12,
                                                 dataTableOutput("MIRT_modelfit_relat")%>%
                                                   box_show_theme()),
                                             box(title="Absolute fit indices",DT::dataTableOutput("MIRT_modelfit")%>%
                                                   box_show_theme(),
                                                 solidHeader = TRUE,
                                                 status = "info",width = 12)),
                                      column(4,
                                             box(title="MIRT setup",
                                                 solidHeader = TRUE,status = "warning",width = 12,

                                                 selectInput(inputId = "modelselect1",label = "Model selection",selectize = T,
                                                             choices = list("NULL",
                                                                            "Rasch model (1PL)",
                                                                            "Two parameters logistic model (2PL)",
                                                                            "Three parameters logistic model (3PL)" ,
                                                                            "Four parameters logistic model (4PL)" ,
                                                                            "Graded response model (GRM)" ,
                                                                            "Partial credit model (PCM)",
                                                                            "Generalized partial credit model (GPCM)"),
                                                             selected = "NULL"),

                                                 selectInput(inputId = "MIRT_est_method",label = "Method selection",
                                                             selectize = TRUE,
                                                             choices = list("standard EM algorithm",
                                                                            "quasi-Monte Carlo EM estimation",
                                                                            "Monte Carlo EM estimation",
                                                                            "Stochastic EM algorithm"),
                                                             selected = "quasi-Monte Carlo EM estimation"),
                                                 "Note: For unidimensional models, it is recommended to use the standard EM algorithm.
                                               For multidimensional models,
                                               it is recommended to use quasi-Monte Carlo EM estimation.",
                                                 br(),br(),
                                                 selectInput(inputId = "include_cov",label = "Is it necessary to estimate the covariance matrix?",
                                                             selectize = TRUE,
                                                             choices = list("Yes",
                                                                            "No"),
                                                             selected = "No"),
                                                 "Note: Estimating the covariance matrix can be time-consuming and,
                                               depending on the parameter estimation method, may encounter errors in some cases. However,
                                               it is necessary to estimate the covariance in multidimensional models.",
                                                 br(),br(),

                                                 submitButton( "Updata results")),

                                             box(title = "Download results",solidHeader = TRUE,status = "success",width = 12,
                                                 downloadButton(outputId = "MIRT_resultfile", label = "Download"),
                                                 br(),
                                                 br(),
                                                 downloadButton(outputId = "MIRT_report",label = "Download analysis report")
                                             )))

                          ),
                          tabItem(
                            tabName = "MIRTassum_test",
                            fluidPage(column(8,
                                             box(title="Independence test",solidHeader = TRUE,
                                                 status = "info",width = 12,
                                                 DT::dataTableOutput("MIRT_Q3")%>%
                                                   box_show_theme()
                                             )),
                                      column(4,
                                             box(title = "Indices selection",width = 12,solidHeader = TRUE,
                                                 status = "warning",
                                                 selectInput(inputId = "MIRT_select_independent",label = NULL,
                                                             choices = list("LD-X2 (Chen & Thissen, 1997)",
                                                                            "Q3 (Yen, 1984)"),
                                                             selected = "Q3 (Yen, 1984)",selectize = TRUE),
                                                 submitButton("Updata results"))))
                          ),
                          tabItem(
                            tabName = "MIRTitemfit",
                            fluidPage(column(8,
                                             box(title="Item fit",dataTableOutput("MIRT_itemfit")%>%
                                                   box_show_theme(),
                                                 solidHeader = TRUE,
                                                 status = "info",width = 12)),
                                      column(4,
                                             box(title = "Indices selection",width = 12,solidHeader = TRUE,
                                                 status = "warning",
                                                 selectInput("MIRT_itemfit_method", label = NULL,
                                                             choices = list("chi-squared test (Kang & Chen, 2007)"),
                                                             selected = "chi-squared test (Kang & Chen, 2007)",
                                                             selectize = TRUE),
                                                 submitButton( "Updata results"))))
                          ),
                          tabItem(
                            tabName = "MIRTitempar",
                            fluidPage(column(8,
                                             box(title="Item parameters",DT::dataTableOutput("MIRT_itempar")%>%
                                                   box_show_theme(),
                                                 solidHeader = TRUE,
                                                 status = "info",width = 12),
                                             box(title = "Covariance matrix",
                                                 dataTableOutput("cov_est")%>%
                                                   box_show_theme(),
                                                 solidHeader = TRUE,
                                                 status = "info",width = 12)))
                          ),
                          tabItem(
                            tabName = "MIRTperson",
                            fluidPage(column(8,
                                             box(title = "Person parameters", solidHeader = TRUE, status = "info",
                                                 dataTableOutput("MIRT_person")%>%
                                                   box_show_theme(),width = 12)),
                                      column(4,
                                             box(title = "Method selection",width = 12,solidHeader = TRUE,
                                                 status = "warning",
                                                 selectInput(inputId = "MIRT_person_est_method",
                                                             label = NULL,
                                                             choices = list("expected a-posteriori (EAP)",
                                                                            "maximum a-posteriori (MAP)",
                                                                            "maximum likelihood (ML)",
                                                                            "weighted likelihood estimation (WLE)"),
                                                             selected = "expected a-posteriori (EAP)",selectize = TRUE),
                                                 submitButton( "Updata plot"))))
                          ),
                          tabItem(
                            tabName = "MIRTwright",
                            fluidPage(column(8,
                                             box(title = "WrightMap", solidHeader = TRUE, status = "info",width = 12,
                                                 uiOutput("MIRT_wright1")%>%
                                                   box_show_theme() )),
                                      column(4,
                                             box(title = "Adjust plot",width = 12,solidHeader = TRUE,
                                                 status = "warning",
                                                 sliderInput(inputId = "MIRT_wright_map_height",label = "The height of WrightMap.",
                                                             min = 300,max = 1800,step = 30,value = 400),
                                                 br(),
                                                 sliderInput(inputId = "MIRT_wright_map_p_size",label = "The size of points.",
                                                             min = 1,max = 7,step = 0.5,value = 3),
                                                 br(),
                                                 sliderInput(inputId = "MIRT_wright_p_width",label = "The width of right plot.",
                                                             min = 1, max = 5, value = 1.618, step = 0.25),
                                                 uiOutput("MIRT_wright_dim_select"),
                                                 submitButton( "Updata plot")),
                                             box(title = "Download figure",solidHeader = TRUE,status = "success",width = 12,
                                                 downloadButton(outputId = "MIRT_wrightfile", label = "Download"))))
                          ),
                          tabItem(
                            tabName = "MIRTicc",
                            fluidPage(column(8,
                                             box(title = "Item characteristic curve (ICC)", solidHeader = TRUE, status = "info",
                                                 uiOutput(outputId = "MIRT_ICC1")%>%
                                                   box_show_theme(),
                                                 width = 12)),
                                      column(4,

                                             box(title = "Adjust the figures (ICC and IIC)",width = 12,solidHeader = TRUE,
                                                 status = "warning",
                                                 sliderInput(inputId = "MIRT_wrap_height", label = "Select height",
                                                             min = 300,max = 1800,step = 30,value = 400),
                                                 br(),
                                                 selectInput(inputId = "MIRT_wrap_ncol",
                                                             label = "Select column number",
                                                             choices = list("2","3","4","5","6","7"),
                                                             selected = "4"),
                                                 submitButton( "Updata plot")),
                                             box(title = "Download figure",solidHeader = TRUE,status = "success",width = 12,
                                                 downloadButton(outputId = "MIRT_ICCfile", label = "Download"))))
                          ),
                          tabItem(
                            tabName = "MIRTiic",
                            fluidPage(column(8,
                                             box(title = "Item information curve (IIC)", solidHeader = TRUE, status = "info",
                                                 uiOutput("MIRT_IIC1")%>%
                                                   box_show_theme(),
                                                 width = 12)),
                                      column(4,
                                             box(title = "Download figure",solidHeader = TRUE,status = "success",width = 12,
                                                 downloadButton(outputId = "MIRT_IICfile", label = "Download"))))
                          ),
                          tabItem(
                            tabName = "MIRTtic",
                            fluidPage(column(8,
                                             box(title = "Test information curve (TIC)", solidHeader = TRUE, status = "info",width = 12,
                                                 plotOutput("MIRT_TIC")%>%
                                                   box_show_theme())),
                                      column(4,
                                             box(title = "Selection of dimension",width = 12,solidHeader = TRUE,
                                                 status = "warning",
                                                 uiOutput("MIRT_TIC_dim_select"),
                                                 submitButton( "Updata plot"))
                                      ))
                          )
                        )))

}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "TestAnaAPP"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

box_show_theme <- function(value){
  return(shinycssloaders::withSpinner(ui_element = value,color="#0dc5c1",type=6,size = 1.5 ))
}
DT_dataTable_Show <- function(x){
  DT::datatable(x,
                filter =list(position = 'top', clear = TRUE, plain = TRUE),
                options = list(scrollX = TRUE))
}
