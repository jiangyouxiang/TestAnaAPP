#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shinydashboard shinycssloaders
#' @noRd
#'
#'
app_ui <- function() {
  options(shiny.legacy.datatable = TRUE)
  dashboardPage(skin = "blue",
                dashboardHeader(title = "Test Analysis Application",titleWidth = 700),#The name of this platform
                      #Pages----------------------------------------------------------------------------------
                      dashboardSidebar(
                        sidebarMenu(id="sidebarmenu",
                                     menuItem("TestAnaAPP",tabName = "info",icon = icon("info-circle")),#Introduction
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
                                               menuSubItem("Test Information Curve",tabName = "IRTtic",icon = shiny::icon("angle-double-right")),
                                               menuSubItem("Analysis Report",tabName = "IRTreport_tab",icon = shiny::icon("angle-double-right"))),

                                     menuItem("Multidimensional IRT",tabName = "MIRT",icon = icon("cogs"),
                                               menuSubItem("Upload Response Scores ",tabName = "MIRTdim_info",icon = icon("table")),
                                               menuSubItem("Model Fit *",tabName = "MIRTmodelfit",icon = shiny::icon("angle-double-right")),
                                               menuSubItem("Hypothesis testing",tabName = "MIRTassum_test",icon = shiny::icon("angle-double-right")),
                                               menuSubItem("Item Fit",tabName = "MIRTitemfit",icon = shiny::icon("angle-double-right")),
                                               menuSubItem("Item Parameters",tabName = "MIRTitempar",icon = shiny::icon("angle-double-right")),
                                               menuSubItem("Person Parameters",tabName = "MIRTperson",icon = shiny::icon("angle-double-right")),
                                               menuSubItem("Wright Map",tabName = "MIRTwright",icon = shiny::icon("angle-double-right")),
                                               menuSubItem("Item Characteristic Curve",tabName = "MIRTicc",icon = shiny::icon("angle-double-right")),
                                               menuSubItem("Item Information Curve",tabName = "MIRTiic",icon = shiny::icon("angle-double-right")),
                                               menuSubItem("Test Information Curve",tabName = "MIRTtic",icon = shiny::icon("angle-double-right")),
                                               menuSubItem("Analysis Report",tabName = "MIRTreport_tab",icon = shiny::icon("angle-double-right"))),
                                     menuItem("Continuous Response Model", tabName = "CRM_model", icon = icon("cogs"),
                                               menuSubItem("Upload Response Scores ",tabName = "CRM_maxmin_print",icon = icon("table")),
                                               menuSubItem("Item Fit",tabName = "CRM_itemfit",icon = shiny::icon("angle-double-right")),
                                               menuSubItem("Item Parameters",tabName = "CRM_itempara",icon = shiny::icon("angle-double-right")),
                                               menuSubItem("Person Parameter",tabName = "CRM_personpar",icon = shiny::icon("angle-double-right")),
                                               menuSubItem("Item Category Response Curves",tabName = "CRM_ICC",icon =  shiny::icon("angle-double-right"))),
                                     menuItem("Differential Item Function",tabName = "DIF",icon = icon("cogs"),
                                               menuSubItem("Upload Response Scores ",tabName = "DIF_group",icon = icon("table")),
                                               menuSubItem("DIF Analysis",tabName = "DIF_analysis",icon = shiny::icon("angle-double-right")))
                        )),
                      dashboardBody(
                        #A. Introduction page---------------------------------------------------------------------------
                        tabItems(
                          tabItem(tabName = "info",
                                  fluidRow(
                                    box(title = "Welcome!", solidHeader = TRUE, status = "info",
                                        htmlOutput("info"),width = 12,height = "auto"
                                        )
                                  )),

                          #B. EFA page-----------------------------------------------------------------------
                          tabItem(
                            tabName = "loading",
                            fluidRow(column(8,
                                            box(title="Score Data", solidHeader = TRUE, status = "warning",width = 12,
                                                br(),
                                                tags$b("Note: "), "If necessary, please convert the scoring direction of the
                                                reverse-scored items before data analysis, and then upload them to this platform.",
                                                br(),br(),br(),
                                                DT::dataTableOutput("EFA_Response")%>%
                                                  box_show_theme()),

                                            box(title="Eigenvalues", DT::dataTableOutput("CTT_EFA_eigenvalues")%>%
                                                  box_show_theme(),
                                                solidHeader = TRUE, status = "info",width = 12),
                                            box(title="Factor Loadings (Rotated)", DT::dataTableOutput("EFA_load")%>%
                                                  box_show_theme(),
                                                solidHeader = TRUE, status = "info",width = 12)
                            ),
                            column(4,
                                   box(title = "Upload Score Data", status = "warning", solidHeader = TRUE,
                                       fileInput("EFA_res", "Kindly submit the test data file that requires analysis.
                                                              Acceptable file formats for uploading include TXT, CSV, Excel, and SPSS.
                                                              Ensure that the file solely consists of score data, and refrain from
                                                              using purely numerical column names.",
                                                 placeholder="File",buttonLabel = "Browse",
                                                 accept = c("xlsx","xls","csv","sav","txt")
                                       ),width = 12),
                                   box(title = "Method",status = "warning", solidHeader = TRUE,width = 12,
                                       selectInput(inputId = "EFA_method",label = "Extraction method",selectize = TRUE,
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
                                       submitButton( "Update results")),
                                   box(title = "Download Results", status = "success",solidHeader = TRUE,
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
                                            box(title = "Download Figure",status = "success",solidHeader = TRUE, width = 12,
                                                downloadButton(outputId = "EFA_plotfile", label = "Download"))))
                          ),
                          #C. CFA page-------------------------------------------------------------
                          tabItem(
                            tabName = "loading_CFA",
                            fluidRow(column(8,
                                            box(title="Score Data", solidHeader = TRUE, status = "warning",width = 12,
                                                br(),
                                                tags$b("Note: "), "If necessary, please convert the scoring direction of the
                                                reverse-scored items before data analysis, and then upload them to this platform.",
                                                br(),br(),br(),
                                                DT::dataTableOutput("CFA_Response")%>%
                                                  box_show_theme()),
                                            box(title = "Test Structure",status =  "warning",
                                                solidHeader = TRUE, width = 12,
                                                tags$b("Note: "),"Edit the dimension information of the quiz according
                                                to the template file and upload it.",
                                                downloadButton(outputId = "CFA_dim_example",label = "Download template"),
                                                br(),br(),
                                                fileInput(inputId = "dimensionfile_cfa",label = "Test structure information.",
                                                          placeholder="File",buttonLabel = "Browse",
                                                          accept = c("xlsx","xls","csv")),
                                                DT::dataTableOutput("CFA_dimension")%>%
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
                                            box(title = "Upload Score Data", status = "warning", solidHeader = TRUE,
                                                fileInput("CFA_res", "Kindly submit the test data file that requires analysis.
                                                              Acceptable file formats for uploading include TXT, CSV, Excel, and SPSS.
                                                              Ensure that the file solely consists of score data, and refrain from
                                                              using purely numerical column names.",
                                                          placeholder="File",buttonLabel = "Browse",
                                                          accept = c("xlsx","xls","csv","sav","txt")
                                                ),width = 12),
                                            box(title="Basic Settings",
                                                solidHeader = TRUE,status = "warning",width = 12,
                                                selectInput(inputId = "CFA_estimator",label = "Estimator selection",
                                                            selectize = T,
                                                            choices = list("Maximum Likelihood",
                                                                           "Generalized Least Squares",
                                                                           "Weighted Least Squares",
                                                                           "Unweighted Least Squares",
                                                                           "Diagonally Weighted Least Squares",
                                                                           "Distributionally-weighted Least Squares"),
                                                            selected = "Maximum Likelihood"),

                                                br(),br(),

                                                textInput(inputId = "CFA_HO",label = "Higher-order factor name",
                                                          placeholder = "If there is no higher-order factor, please leave it blank."),

                                                br(),br(),

                                                submitButton( "Update results")),
                                            box(title = "Download Results",status = "success",solidHeader = TRUE, width = 12,
                                                downloadButton(outputId = "CFA_file", label = "Download")))
                            )
                          ),

                          tabItem(
                            tabName = "path_plot",
                            fluidRow(column(12,
                                            box(title = "Path Diagram",status =  "info",
                                                solidHeader = TRUE, width = 12,
                                                br(),br(),
                                                uiOutput("CFA_fit_plot1")%>%
                                                  box_show_theme(),
                                                br(),br(),
                                                selectInput(inputId = "CFA_plot_par", label = "What should the edge
                                                            labels indicate in the path diagram? ",
                                                            selectize = T, choices = list("Parameter estimate",
                                                                                          "Standardized parameter estimate"),
                                                            selected = "Standardized parameter estimate"),
                                                br(),br(),

                                                selectInput(inputId = "CFA_plot_style", label = "Select the layout style of the path diagram",
                                                            selectize = T, choices = list("tree",
                                                                                          "circle"),
                                                            selected = "tree"),
                                                br(),br(),
                                                sliderInput(inputId = "CFA_plot_height",label = "Adjust height",
                                                            min = 300,max = 1800,step = 30,value = 800),
                                                br(),
                                                submitButton( "Update plot"))))
                          ),

                          #D. CTT page-----------------------------------------------------------------------------
                          tabItem(
                            tabName = "summary",
                            fluidRow(column(8,
                                            box(title="Score Data", solidHeader = TRUE, status = "warning",width = 12,
                                                br(),
                                                tags$b("Note: "), "If necessary, please convert the scoring direction of the
                                                reverse-scored items before data analysis, and then upload them to this platform.",
                                                tags$b("Please do not upload total score data for quizzes or dimensions."),
                                                br(),br(),br(),
                                                DT::dataTableOutput("CTT_Response")%>%
                                                  box_show_theme()),
                                            box(title="Descriptive Statistics", DT::dataTableOutput("CTT_summary")%>%
                                                  box_show_theme(),
                                                solidHeader = TRUE, status = "info",width = 12),
                                            box(title="Total Score distribution", plotOutput("scores_plot")%>%
                                                  box_show_theme(),
                                                downloadButton(outputId = "scores_plotfile", label = "Download"),#下载
                                                solidHeader = TRUE, status = "info",width = 12)),
                                     column(4,
                                            box(title = "Upload Score Data", status = "warning", solidHeader = TRUE,
                                                fileInput("CTT_res", "Kindly submit the test data file that requires analysis.
                                                              Acceptable file formats for uploading include TXT, CSV, Excel, and SPSS.
                                                              Ensure that the file solely consists of score data, and refrain from
                                                              using purely numerical column names.",
                                                          placeholder="File",buttonLabel = "Browse",
                                                          accept = c("xlsx","xls","csv","sav","txt")
                                                ),width = 12),
                                            box(title = "Download Results",status = "success",solidHeader = TRUE,  width = 12,
                                                downloadButton(outputId = "summary_result", label = "Download")))
                            )),
                          tabItem(
                            tabName = "CTT_par",
                            fluidRow(column(8,
                                            box(title = "Note", solidHeader = TRUE,status = "warning",width = 12,
                                                "1. For binary scoring items (items with only two type of scores), 'TestAnaAPP'
                                                categorizes the participants into a high-score group and a low-score group based on their
                                                total scores being above or equal to the 73rd percentile and below or equal to the 27th percentile,
                                                respectively. Then the pass rate of each item in the high-score and low-score groups is calculated,
                                                and the average pass rate of each item is taken as its difficulty level.",
                                                br(),
                                                "2. For multiple scoring items (items with more than two type of scores), 'TestAnaAPP' calculates the
                                                difficulty of each item by dividing the average score on the item by the maximum score possible on the item.",
                                                br(),
                                                "Regardless of the number of scoring categories for an item, 'TestAnaAPP' calculates the difference between
                                                the mean scores of the high-score and low-score groups on the item and divides it by the maximum possible
                                                score on the item to obtain the item discrimination index.",
                                                br(),
                                                "3. Coefficient of variation is obtained by multiplying the ratio of standard deviation to
                                                mean on the item by 100%.",
                                                br(),
                                                "4. The correlation coefficient is the Pearson correlation coefficient between item scores and
                                                the total score.",
                                                br(),
                                                "5. If the score data includes missing values, 'TestAnaAPP' will delete any case that inludes missing data.",
                                                br(),

                                                ),
                                            box(title="Item parameters", DT::dataTableOutput("CTT_itempar")%>%
                                                  box_show_theme(),
                                                solidHeader = TRUE, status = "info",width = 12)),
                                     column(4,
                                            box(title = "Download Results", status = "success",solidHeader = TRUE,
                                                downloadButton(outputId = "CTT_result", label = "Download"),
                                                width = 12)))
                          ),
                          tabItem(
                            tabName = "CTT_relia",
                            fluidPage(column(8,
                                             box(title = "Test Reliability",
                                                 DT::dataTableOutput("CTT_reliability")%>%
                                                   box_show_theme(),solidHeader = TRUE,
                                                 status = "info",width = 12),
                                             box(title = "Alpha coefficient after item deletion",
                                                 DT::dataTableOutput("CTT_item_alpha")%>%
                                                   box_show_theme(),solidHeader = TRUE,
                                                 status = "info",width = 12)),
                                      column(4,
                                             box(title = "Download Reliability",status = "success",
                                                 solidHeader = TRUE, width = 12,
                                                 downloadButton(outputId = "CTT_relia_file",label = "Download"))))
                          ),
                          tabItem(
                            tabName = "relate_eff",
                            fluidPage(column(8,
                                             box(title = "Pearson Coefficient Matrix",
                                                 DT::dataTableOutput("CTT_relate_eff")%>%
                                                   box_show_theme(),solidHeader = TRUE,
                                                 status = "info",width = 12),
                                             box(title = "The P value corresponding to above correlation",
                                                 DT::dataTableOutput("CTT_relate_p")%>%
                                                   box_show_theme(),solidHeader = TRUE,
                                                 status = "info",width = 12)),
                                      column(4,
                                             box(title = "Download Matrix",status = "success",
                                                 solidHeader = TRUE, width = 12,
                                                 downloadButton(outputId = "CTT_relatefile",label = "Dowload"))))
                          ),
                          #E. IRT page----------------------------------------------------------------------------------
                          tabItem(
                            tabName = "IRTmodelfit",
                            fluidRow(column(8,
                                            box(title="Score Data", solidHeader = TRUE, status = "warning",width = 12,
                                                br(),
                                                tags$b("Note: "), "If necessary, please convert the scoring direction of the
                                                reverse-scored items before data analysis, and then upload them to this platform.",
                                                tags$b("Please do not upload total score data for quizzes or dimensions."),
                                                br(),br(),br(),
                                                DT::dataTableOutput("IRT_Response")%>%
                                                  box_show_theme()),
                                            box(title = "Relative fit indices",solidHeader = TRUE,
                                                status = "info",width = 12,
                                                DT::dataTableOutput("IRT_modelfit_relat")%>%
                                                  box_show_theme()),
                                            box(title="Absolute fit indices",DT::dataTableOutput("IRT_modelfit")%>%
                                                  box_show_theme(),
                                                solidHeader = TRUE,
                                                status = "info",width = 12),
                            ),
                            column(4,
                                   box(title = "Upload Score Data", status = "warning", solidHeader = TRUE,
                                       fileInput("IRT_res", "Kindly submit the test data file that requires analysis.
                                                              Acceptable file formats for uploading include TXT, CSV, Excel, and SPSS.
                                                              Ensure that the file solely consists of score data, and refrain from
                                                              using purely numerical column names.",
                                                 placeholder="File",buttonLabel = "Browse",
                                                 accept = c("xlsx","xls","csv","sav","txt")
                                       ),width = 12),
                                   box(title="Basic Settings",
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
                                       tags$b("Note: "),"the Rasch model, 2PL, 3PL, and 4PL model
                                              are only applicable to items with two response categories",
                                       br(),br(),

                                       selectInput(inputId = "IRT_est_method",label = "Estimation method",
                                                   selectize = TRUE,
                                                   choices = list("standard EM algorithm",
                                                                  "quasi-Monte Carlo EM estimation",
                                                                  "Monte Carlo EM estimation",
                                                                  "Stochastic EM algorithm"),
                                                   selected = "standard EM algorithm"),
                                       tags$b("Note: "),"In the case of unidimensional models, it is advisable to employ the
                                       standard EM algorithm.",
                                       br(),br(),
                                       selectInput("IRT_TOL",label = "Convergence criterion",
                                                   selectize = TRUE,
                                                   choices = list("0.01","0.001","0.0001","0.00001","0.0000001"),
                                                   selected = "0.00001"),
                                       br(),
                                       selectInput("IRT_ncycles",label = "Maximum number of iterations",
                                                   selectize = TRUE,
                                                   choices = list("500","1000","2000","3000","5000"),
                                                   selected = "500"),
                                       submitButton( "Update results")))
                            )),
                          tabItem(
                            tabName = "IRTassum_test",
                            fluidPage(column(8,
                                             box(title="Independence Test",status = "info",width = 12,solidHeader = TRUE,
                                                 DT::dataTableOutput("IRT_Q3")%>%
                                                   box_show_theme()
                                             )),
                                      column(4,
                                             box(title = "Index Selection",width = 12,solidHeader = TRUE,
                                                 status = "warning",
                                                 selectInput(inputId = "IRT_select_independent",label = NULL,
                                                             choices = list("LD-X2 (Chen & Thissen, 1997)",
                                                                            "Q3 (Yen, 1984)"),
                                                             selected = "Q3 (Yen, 1984)",selectize = TRUE),
                                                 submitButton("Update results"))))
                          ),
                          tabItem(
                            tabName = "IRTitemfit",
                            fluidPage(column(8,
                                             box(title="Item Fit",DT::dataTableOutput("IRT_itemfit")%>%
                                                   box_show_theme(),
                                                 solidHeader = TRUE,
                                                 status = "info",width = 12)),
                                      column(4,
                                             box(title = "Indices Selection",width = 12,solidHeader = TRUE,
                                                 status = "warning",
                                                 selectInput("IRT_itemfit_method", label = NULL,
                                                             choices = list("chi-squared test (Kang & Chen, 2007)",
                                                                            "chi-squared method (Bock,1972)",
                                                                            "G2 statistic (McKinley & Mills, 1985)"),
                                                             selected = "chi-squared test (Kang & Chen, 2007)",
                                                             selectize = TRUE),
                                                 submitButton( "Update results"))))
                          ),
                          tabItem(
                            tabName = "IRTitempar",
                            fluidPage(column(8,
                                             box(title="Item Parameters",
                                                 tags$b("Note: "),"For detailed formulas for the model and interpretations of parameters,
                                                 please download the analysis report from the Analysis Report section.",
                                                 br(),
                                                 DT::dataTableOutput("IRT_itempar")%>%
                                                   box_show_theme(),
                                                 solidHeader = TRUE,
                                                 status = "info",width = 12)),
                                      column(4,h3("IRT Model Formula and Parameters"),
                                             withMathJax(),
                                             uiOutput("IRT_info"),
                                             br(),
                                             tags$b("See the IRT analysis report for more details.")
                                             ))
                          ),
                          tabItem(
                            tabName = "IRTperson",
                            fluidPage(column(8,
                                             box(title = "Person Parameter", solidHeader = TRUE, status = "info",
                                                 tags$b("Note: "), "The ID in the data corresponds to the row number of the subject's raw score data.",
                                                 br(),br(),
                                                 DT::dataTableOutput("IRT_person")%>%
                                                   box_show_theme(),
                                                 width = 12)),
                                      column(4,
                                             box(title = "Estimation Method",width = 12,solidHeader = TRUE,
                                                 status = "warning",
                                                 selectInput(inputId = "IRT_person_est_method",
                                                             label = NULL,
                                                             choices = list("expected a-posteriori (EAP)",
                                                                            "maximum a-posteriori (MAP)",
                                                                            "maximum likelihood (ML)",
                                                                            "weighted likelihood estimation (WLE)"),
                                                             selected = "expected a-posteriori (EAP)",selectize = TRUE),
                                                 submitButton( "Update plot"))))
                          ),
                          tabItem(
                            tabName = "IRTwright",
                            fluidPage(column(8,
                                             box(title = "WrightMap", solidHeader = TRUE, status = "info",
                                                 tags$b("Note: "), "You can only draw WrightMap for Rasch and partial credit models.",
                                                 br(),br(),
                                                 uiOutput("IRT_wright1")%>%
                                                   box_show_theme(),
                                                 width = 12)),
                                      column(4,
                                             box(title = "Customize Drawing",width = 12,solidHeader = TRUE,
                                                 status = "warning",
                                                 sliderInput(inputId = "IRT_wright_map_height",label = "The height of WrightMap.",
                                                             min = 300,max = 1800,step = 30,value = 400),
                                                 br(),
                                                 sliderInput(inputId = "IRT_wright_map_p_size",label = "The size of points.",
                                                             min = 1,max = 7,step = 0.5,value = 3),
                                                 br(),
                                                 selectInput(inputId = "IRT_point_label",
                                                             label = "The label of the point.",
                                                             choices = list("Numeric","Column names"),
                                                             selected = "Numeric"),
                                                 br(),
                                                 sliderInput(inputId = "IRT_wright_p_width",label = "The width of right plot.",
                                                             min = 1, max = 5, value = 1.618, step = 0.25),
                                                 submitButton( "Update plot")),
                                             box(title = "Download Figure",solidHeader = TRUE,status = "success",width = 12,
                                                 downloadButton(outputId = "IRT_wrightfile", label = "Download"))))
                          ),
                          tabItem(
                            tabName = "IRTicc",
                            fluidPage(column(8,
                                             box(title = "Item Characteristic Curve (ICC)", solidHeader = TRUE, status = "info",
                                                 uiOutput(outputId = "IRT_ICC1")%>%
                                                   box_show_theme(),
                                                 width = 12)),
                                      column(4,
                                             box(title = "Customize Drawing (ICC)",width = 12,solidHeader = TRUE,
                                                 status = "warning",
                                                 uiOutput("IRT_ICC_item_selection"),
                                                 br(),
                                                 sliderInput(inputId = "wrap_height", label = "Select height",
                                                             min = 300, max = 1800,value = 400,step = 30),
                                                 br(),
                                                 sliderInput(inputId = "IRT_ICC_title_size",
                                                             label = "Select title size",
                                                             min = 10, max = 30,value = 20,step = 1),
                                                 br(),
                                                 sliderInput(inputId = "IRT_ICC_label_size",
                                                             label = "Select label size",
                                                             min = 10, max = 30,value = 15,step = 1),
                                                 br(),
                                                 sliderInput(inputId = "IRT_ICC_itemlabel_size",
                                                             label = "Select item label size",
                                                             min = 10, max = 30,value = 15,step = 1),
                                                 br(),
                                                 selectInput(inputId = "wrap_ncol",
                                                             label = "Select column number",
                                                             choices = list("2","3","4","5","6","7"),
                                                             selected = "4"),
                                                 submitButton( "Update results")),
                                             box(title = "Download Figure",solidHeader = TRUE,status = "success",width = 12,
                                                 downloadButton(outputId = "IRT_ICCfile", label = "Download"))))
                          ),
                          tabItem(
                            tabName = "IRTiic",
                            fluidPage(column(8,
                                             box(title = "Item Information Curve (IIC)", solidHeader = TRUE, status = "info",
                                                 uiOutput("IRT_IIC1")%>%
                                                   box_show_theme(),
                                                 width = 12)),
                                      column(4,
                                             box(title = "Customize Drawing (IIC)", solidHeader = TRUE,status = "warning",width = 12,
                                                 uiOutput("IRT_IIC_item_selection"),
                                                 br(),
                                                 selectInput(inputId = "IRTiic_scale",label = "Free or fixed y axis",
                                                             choices = list("Free","Fixed"),selected = "Free"),
                                                 br(),
                                                 sliderInput(inputId = "wrap_height_iic", label = "Select height",
                                                             min = 300, max = 1800,value = 400,step = 30),
                                                 br(),
                                                 sliderInput(inputId = "IRT_IIC_title_size",
                                                             label = "Select title size",
                                                             min = 10, max = 30,value = 20,step = 1),
                                                 br(),
                                                 sliderInput(inputId = "IRT_IIC_label_size",
                                                             label = "Select label size",
                                                             min = 10, max = 30,value = 15,step = 1),
                                                 br(),
                                                 sliderInput(inputId = "IRT_IIC_itemlabel_size",
                                                             label = "Select item label size",
                                                             min = 10, max = 30,value = 15,step = 1),

                                                 br(),
                                                 selectInput(inputId = "wrap_ncol_iic",
                                                             label = "Select column number",
                                                             choices = list("2","3","4","5","6","7"),
                                                             selected = "4"),

                                                 submitButton( "Update results")),
                                             box(title = "Download Figure",solidHeader = TRUE,status = "success",width = 12,
                                                 downloadButton(outputId = "IRT_IICfile", label = "Download"))))
                          ),
                          tabItem(tabName = "IRTtic",
                                  fluidRow(
                                    column(8,
                                           box(title = "Test Information Curve (TIC)", solidHeader = TRUE, status = "info",
                                               plotOutput("IRT_TIC")%>%
                                                 box_show_theme(),
                                               width = 12)),
                                    column(4,
                                           box(title = "Download Figure",solidHeader = TRUE,status = "success",width = 12,
                                               downloadButton(outputId = "IRT_TICfile", label = "Download"))))
                          ),
                          tabItem(tabName = "IRTreport_tab",
                                  fluidRow(

                                    column(8,
                                           box(title = "Download Results",solidHeader = TRUE,status = "success",width = 12,
                                               "Download all the analysis results of unidimensional IRT in the 'TestAnaAPP'.",br(),
                                               br(),
                                               downloadButton(outputId = "IRT_resultfile", label = "Download results"),
                                               br(),
                                               br()),
                                           box(title = "Generate Data Analysis Reports",solidHeader = TRUE,status = "success",width = 12,
                                               "TestAnaAPP will generate a data analysis report based on the following settings:",
                                               br(),
                                               br(),
                                               sliderInput(inputId = "IRTreport_Q3_h",label = "Highlight the values where the absolute value of the
                                                           local dependency indicator is greater than?",
                                                           min = 0, max = 1,value = 0.22,step = 0.01),
                                               br(),
                                               sliderInput(inputId = "IRTreport_alpha_h", label = "Highlight the values where the discrimination
                                                           is lower than?", min = 0, max = 2,value = 0.5,step = 0.1),
                                               br(),
                                               sliderInput(inputId = "IRTreport_wrap_height", label = "Select the height (inch) of the facet plot
                                                           (item characteristic curve and item information curve)",
                                                           min = 3, max = 30,value = 10,step = 0.2),
                                               tags$b("Note: "),"the column number of the facet plot can be set in the above specific section.",
                                               br(),br(),
                                               sliderInput(inputId = "IRTreport_wright_height", label = "Select the height (inch) of the wright map",
                                                           min = 3, max = 10,value = 5,step = 0.2),
                                               br(),
                                               submitButton("Update Settings"),
                                               br(),
                                               tags$b("Please first set the drawing parameters in the Item Characteristic Curve and Item Information Curve modules."),
                                               br(),
                                               downloadButton(outputId = "IRT_report",label = "Download analysis report")
                                               )))
                          ),
                          #F. MIRT page---------------------------------------------------------------------------------
                          tabItem(
                            tabName = "MIRTdim_info",
                            fluidPage(column(8,
                                             box(title="Score Data", solidHeader = TRUE, status = "warning",width = 12,
                                                 br(),
                                                 tags$b("Note: "), "If necessary, please convert the scoring direction of the
                                                reverse-scored items before data analysis, and then upload them to this platform.",
                                                 tags$b("Please do not upload total score data for quizzes or dimensions."),
                                                 br(),br(),br(),
                                                 DT::dataTableOutput("MIRT_Response")%>%
                                                   box_show_theme()),
                                             box(title = "Test dimension", solidHeader = TRUE,width = 12,
                                                 status = "warning",
                                                 tags$b("Note: "),"The Q-matrix is a numerical matrix utilized to delineate which items
                                                 measure which traits. In the Q-matrix, the number of rows corresponds to the number of
                                                 items, and the number of columns corresponds to the number of traits. A value of 1 in
                                                 the matrix indicates that the item is used to assess the corresponding trait, while a
                                                 value of 0 signifies that the item does not assess the trait.",
                                                 downloadButton("dimension_download",label = "Download template"),
                                                 br(),br(),
                                                 DT::dataTableOutput(outputId = "dimension_example")%>%
                                                   box_show_theme()),
                                             box(title = "Dimension and Item",solidHeader = TRUE,width = 12,
                                                 status = "warning",
                                                 textOutput("dimension_code")%>%
                                                   box_show_theme())),
                                      column(4,
                                             box(title = "Upload Score Data", status = "warning", solidHeader = TRUE,
                                                 fileInput("MIRT_res", "Kindly submit the test data file that requires analysis.
                                                              Acceptable file formats for uploading include TXT, CSV, Excel, and SPSS.
                                                              Ensure that the file solely consists of score data, and refrain from
                                                              using purely numerical column names.",
                                                           placeholder="File",buttonLabel = "Browse",
                                                           accept = c("xlsx","xls","csv","sav","txt")
                                                 ),width = 12),
                                             box(title = "Upload Dimension Information",solidHeader = TRUE,status = "warning",width = 12,
                                                 fileInput(inputId = "dimensionfile",
                                                           "Kindly upload an Excel file with example data on the left side to demonstrate
                                                           the relationship between each question and dimension.",
                                                           placeholder="File",buttonLabel = "Browse",
                                                           accept = c("xlsx","xls","csv")))))
                          ),
                          tabItem(
                            tabName = "MIRTmodelfit",
                            fluidPage(column(8,
                                             box(title = "Relative model fit indices",solidHeader = TRUE,
                                                 status = "info",width = 12,
                                                 DT::dataTableOutput("MIRT_modelfit_relat")%>%
                                                   box_show_theme()),
                                             box(title="Absolute fit indices",DT::dataTableOutput("MIRT_modelfit")%>%
                                                   box_show_theme(),
                                                 solidHeader = TRUE,
                                                 status = "info",width = 12)),
                                      column(4,
                                             box(title="MIRT Setup",
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
                                                 tags$b("Note: "), "For unidimensional models, it is recommended to use the standard EM algorithm.
                                                 For multidimensional models,
                                                 it is recommended to use quasi-Monte Carlo EM estimation.",
                                                 br(),br(),
                                                 selectInput("MIRT_TOL",label = "Convergence criterion",
                                                             selectize = TRUE,
                                                             choices = list("0.01","0.001","0.0001","0.00001","0.0000001"),
                                                             selected = "0.00001"),
                                                 br(),
                                                 selectInput("MIRT_ncycles",label = "Maximum number of iterations",
                                                             selectize = TRUE,
                                                             choices = list("500","1000","2000","3000","5000"),
                                                             selected = "500"),
                                                 br(),
                                                 selectInput(inputId = "include_cov",label = "Is it necessary to estimate the covariance matrix?",
                                                             selectize = TRUE,
                                                             choices = list("Yes",
                                                                            "No"),
                                                             selected = "No"),
                                                 tags$b("Note: "), "Estimating the covariance matrix can be time-consuming and may encounter
                                                 errors in some cases, depending on the parameter estimation method. However,
                                                 it is necessary to estimate the covariance in multidimensional models.",
                                                 br(),br(),

                                                 submitButton( "Update results"))))

                          ),
                          tabItem(
                            tabName = "MIRTassum_test",
                            fluidPage(column(8,
                                             box(title="Independence Test",solidHeader = TRUE,
                                                 status = "info",width = 12,
                                                 DT::dataTableOutput("MIRT_Q3")%>%
                                                   box_show_theme()
                                             )),
                                      column(4,
                                             box(title = "Indices Selection",width = 12,solidHeader = TRUE,
                                                 status = "warning",
                                                 selectInput(inputId = "MIRT_select_independent",label = NULL,
                                                             choices = list("LD-X2 (Chen & Thissen, 1997)",
                                                                            "Q3 (Yen, 1984)"),
                                                             selected = "Q3 (Yen, 1984)",selectize = TRUE),
                                                 submitButton("Update results"))))
                          ),
                          tabItem(
                            tabName = "MIRTitemfit",
                            fluidPage(column(8,
                                             box(title="Item Fit",DT::dataTableOutput("MIRT_itemfit")%>%
                                                   box_show_theme(),
                                                 solidHeader = TRUE,
                                                 status = "info",width = 12)),
                                      column(4,
                                             box(title = "Indices Selection",width = 12,solidHeader = TRUE,
                                                 status = "warning",
                                                 selectInput("MIRT_itemfit_method", label = NULL,
                                                             choices = list("chi-squared test (Kang & Chen, 2007)"),
                                                             selected = "chi-squared test (Kang & Chen, 2007)",
                                                             selectize = TRUE),
                                                 submitButton( "Update results"))))
                          ),
                          tabItem(
                            tabName = "MIRTitempar",
                            fluidPage(column(8,
                                             box(title="Item Parameters",
                                                 tags$b("Note: "),"For detailed formulas for the model and interpretations of parameters,
                                                 please download the analysis report from the Analysis Report section.",
                                                 br(),
                                                 DT::dataTableOutput("MIRT_itempar")%>%
                                                   box_show_theme(),
                                                 solidHeader = TRUE,
                                                 status = "info",width = 12),
                                             box(title = "Covariance Matrix",
                                                 DT::dataTableOutput("cov_est")%>%
                                                   box_show_theme(),
                                                 solidHeader = TRUE,
                                                 status = "info",width = 12)),
                                      column(4,h3("MIRT Model Formula and Parameters"),
                                             withMathJax(),
                                             uiOutput("MIRT_info"),
                                             br(),
                                             tags$b("See the MIRT analysis report for more details.")
                                      ))
                          ),
                          tabItem(
                            tabName = "MIRTperson",
                            fluidPage(column(8,
                                             box(title = "Person Parameters", solidHeader = TRUE, status = "info",
                                                 tags$b("Note: "), "The ID in the data corresponds to the row number of the subject's raw score data.",
                                                 br(),br(),
                                                 DT::dataTableOutput("MIRT_person")%>%
                                                   box_show_theme(),width = 12)),
                                      column(4,
                                             box(title = "Method Selection",width = 12,solidHeader = TRUE,
                                                 status = "warning",
                                                 selectInput(inputId = "MIRT_person_est_method",
                                                             label = NULL,
                                                             choices = list("expected a-posteriori (EAP)",
                                                                            "maximum a-posteriori (MAP)",
                                                                            "maximum likelihood (ML)",
                                                                            "weighted likelihood estimation (WLE)"),
                                                             selected = "expected a-posteriori (EAP)",selectize = TRUE),
                                                 submitButton( "Update plot"))))
                          ),
                          tabItem(
                            tabName = "MIRTwright",
                            fluidPage(column(8,
                                             box(title = "WrightMap", solidHeader = TRUE, status = "info",width = 12,
                                                 tags$b("Note: "), "You can only draw WrightMap for Rasch and partial credit models.
                                                 The within-item multidimensional test does not currently support drawing.",
                                                 br(),br(),
                                                 uiOutput("MIRT_wright1")%>%
                                                   box_show_theme() )),
                                      column(4,
                                             box(title = "Customize Drawing",width = 12,solidHeader = TRUE,
                                                 status = "warning",
                                                 sliderInput(inputId = "MIRT_wright_map_height",label = "The height of WrightMap.",
                                                             min = 300,max = 1800,step = 30,value = 400),
                                                 br(),
                                                 sliderInput(inputId = "MIRT_wright_map_p_size",label = "The size of points.",
                                                             min = 1,max = 7,step = 0.5,value = 3),
                                                 br(),
                                                 selectInput(inputId = "MIRT_point_label",
                                                             label = "The label of the point.",
                                                             choices = list("Numeric","Column names"),
                                                             selected = "Numeric"),
                                                 br(),
                                                 sliderInput(inputId = "MIRT_wright_p_width",label = "The width of right plot.",
                                                             min = 1, max = 5, value = 1.618, step = 0.25),
                                                 uiOutput("MIRT_wright_dim_select"),
                                                 submitButton( "Update plot")),
                                             box(title = "Download Figure",solidHeader = TRUE,status = "success",width = 12,
                                                 downloadButton(outputId = "MIRT_wrightfile", label = "Download"))))
                          ),
                          tabItem(
                            tabName = "MIRTicc",
                            fluidPage(column(8,
                                             box(title = "Item Characteristic Curve (ICC)", solidHeader = TRUE, status = "info",
                                                 tags$b("Note: "), "The within-item multidimensional test does not currently support drawing.",
                                                 br(),br(),
                                                 uiOutput(outputId = "MIRT_ICC1")%>%
                                                   box_show_theme(),
                                                 width = 12)),
                                      column(4,

                                             box(title = "Customize Drawing (ICC)",width = 12,solidHeader = TRUE,
                                                 status = "warning",
                                                 uiOutput("MIRT_ICC_item_selection"),
                                                 br(),
                                                 sliderInput(inputId = "MIRT_wrap_height", label = "Select height",
                                                             min = 300,max = 1800,step = 30,value = 400),
                                                 br(),
                                                 sliderInput(inputId = "MIRT_ICC_title_size",
                                                             label = "Select title size",
                                                             min = 10,max = 30,step = 1,value = 20),
                                                 br(),
                                                 sliderInput(inputId = "MIRT_ICC_label_size",
                                                             label = "Select label size",
                                                             min = 10,max = 30,step = 1,value = 15),
                                                 br(),
                                                 sliderInput(inputId = "MIRT_ICC_itemlabel_size",
                                                             label = "Select item label size",
                                                             min = 10,max = 30,step = 1,value = 15),
                                                 br(),
                                                 selectInput(inputId = "MIRT_wrap_ncol",
                                                             label = "Select column number",
                                                             choices = list("2","3","4","5","6","7"),
                                                             selected = "4"),
                                                 submitButton( "Update results")),
                                             box(title = "Download Figure",solidHeader = TRUE,status = "success",width = 12,
                                                 downloadButton(outputId = "MIRT_ICCfile", label = "Download"))))
                          ),
                          tabItem(
                            tabName = "MIRTiic",
                            fluidPage(column(8,
                                             box(title = "Item Information Curve (IIC)", solidHeader = TRUE, status = "info",
                                                 tags$b("Note: "), "The within-item multidimensional test does not currently support drawing.",
                                                 br(),br(),
                                                 uiOutput("MIRT_IIC1")%>%
                                                   box_show_theme(),
                                                 width = 12)),
                                      column(4,
                                             box(title = "Customize Drawing (IIC)", solidHeader = TRUE,status = "warning",width = 12,
                                                 uiOutput("MIRT_IIC_item_selection"),
                                                 br(),
                                                 selectInput(inputId = "MIRTiic_scale",label = "Free or fixed y axis",
                                                             choices = list("Free","Fixed"),selected = "Free"),
                                                 br(),
                                                 sliderInput(inputId = "MIRT_wrap_height_iic", label = "Select height",
                                                             min = 300,max = 1800,step = 30,value = 400),
                                                 br(),
                                                 sliderInput(inputId = "MIRT_IIC_title_size",
                                                             label = "Select title size",
                                                             min = 10,max = 30,step = 1,value = 20),
                                                 br(),
                                                 sliderInput(inputId = "MIRT_IIC_label_size",
                                                             label = "Select label size",
                                                             min = 10,max = 30,step = 1,value = 20),
                                                 br(),
                                                 sliderInput(inputId = "MIRT_IIC_itemlabel_size",
                                                             label = "Select item label size",
                                                             min = 10,max = 30,step = 1,value = 15),
                                                 br(),
                                                 selectInput(inputId = "MIRT_wrap_ncol_iic",
                                                             label = "Select column number",
                                                             choices = list("2","3","4","5","6","7"),
                                                             selected = "4"),

                                                 submitButton( "Update results")),
                                             box(title = "Download Figure",solidHeader = TRUE,status = "success",width = 12,
                                                 downloadButton(outputId = "MIRT_IICfile", label = "Download"))))
                          ),
                          tabItem(
                            tabName = "MIRTtic",
                            fluidPage(column(8,
                                             box(title = "Test Information Curve (TIC)", solidHeader = TRUE, status = "info",width = 12,
                                                 tags$b("Note: "), "The within-item multidimensional test does not currently support drawing.",
                                                 br(),br(),
                                                 plotOutput("MIRT_TIC")%>%
                                                   box_show_theme())),
                                      column(4,
                                             box(title = "Selection of Dimension",width = 12,solidHeader = TRUE,
                                                 status = "warning",
                                                 uiOutput("MIRT_TIC_dim_select"),
                                                 submitButton( "Update plot"))
                                      ))
                          ),
                          tabItem(
                            tabName = "MIRTreport_tab",
                            fluidPage(column(8,
                                             box(title = "Download Results",solidHeader = TRUE,status = "success",width = 12,
                                                 "Download all the analysis results of multidimensional IRT in the 'TestAnaAPP'.",br(),
                                                 br(),
                                                 downloadButton(outputId = "MIRT_resultfile", label = "Download results"),
                                                 br(),
                                                 br()),
                                             box(title = "Generate Data Analysis Reports",solidHeader = TRUE,status = "success",width = 12,
                                                 "TestAnaAPP will generate a data analysis report based on the following settings:",
                                                 br(),
                                                 br(),
                                                 sliderInput(inputId = "MIRTreport_Q3_h",label = "Highlight the values where the absolute value of the
                                                           local dependency indicator is greater than?",
                                                             min = 0, max = 1,value = 0.22,step = 0.01),
                                                 br(),
                                                 sliderInput(inputId = "MIRTreport_alpha_h", label = "Highlight the values where the discrimination
                                                           is lower than?", min = 0, max = 2,value = 0.5,step = 0.1),
                                                 br(),
                                                 sliderInput(inputId = "MIRTreport_wrap_height", label = "Select the height (inch) of the facet plot
                                                             (item characteristic curve and item information curve)",
                                                             min = 3, max = 30,value = 11,step = 0.2),
                                                 tags$b("Note: "),"the column number of the facet plot can be set in the above specific section.",
                                                 br(),br(),
                                                 sliderInput(inputId = "MIRTreport_wright_height", label = "Select the height (inch) of the wright map",
                                                             min = 3, max = 10,value = 5,step = 0.2),
                                                 br(),
                                                 submitButton("Update Settings"),
                                                 br(),
                                                 tags$b("Please first set the drawing parameters in the Item Characteristic Curve and Item Information Curve modules."),
                                                 br(),
                                                 downloadButton(outputId = "MIRT_report",label = "Download analysis report"))))


                          ),
                          #G. Continuous response model--------------------------------------------------------------
                          tabItem(
                            tabName = "CRM_maxmin_print",

                            fluidPage(column(8,
                                             box(title = "Note", solidHeader = TRUE, status = "warning",width = 12,

                                                 "Samejima (1973) proposed the Continuous Response Model (CRM) as a
                                                 limiting form of the graded response model for continuous item scores.
                                                 Wang & Zeng (1998) proposed a re-parameterized version of the Continuous
                                                 Response Model (CRM). In this application, the default model used is a
                                                 re-parameterized version of the Continuous Response Model (CRM), and
                                                 the method of estimation involves marginal maximum likelihood and the
                                                 Expectation-Maximization (EM) algorithm.",
                                                 br(),
                                                 uiOutput("CRM_info"),

                                                 br(), br(),
                                                 tags$b("Reference"),br(),
                                                 "Samejima, F.(1973). Homogeneous Case of the Continuous Response Model.
                                                 Psychometrika, 38(2), 203-219.",
                                                 br(),
                                                 "Wang, T. & Zeng, L.(1998). Item Parameter Estimation for a Continuous
                                                 Response Model Using an EM Algorithm. Applied Psychological Measurement,
                                                 22(4), 333-343.",
                                                 br(),
                                                 "Zopluoglu C (2022). _EstCRM: Calibrating Parameters for the Samejima's
                                                 Continuous IRT Model_. R package version 1.6, <https://CRAN.R-project.org/package=EstCRM>",
                                                 br()
                                             ),
                                             box(title="Score Data", solidHeader = TRUE, status = "warning",width = 12,
                                                 br(),
                                                 tags$b("Note: "), "If necessary, please convert the scoring direction of the
                                                reverse-scored items before data analysis, and then upload them to this platform.",
                                                 tags$b("Please do not upload total score data for quizzes or dimensions."),
                                                 br(),br(),br(),
                                                 DT::dataTableOutput("CRM_Response")%>%
                                                   box_show_theme()),
                                             box(title = "Theoretical maximum and minimum (editable)",solidHeader = TRUE,
                                                 status = "warning",width = 12,
                                                 tags$b("Note: "),"The maximum and minimum scores are theoretical values that are used to
                                                 transform the raw data. The maximum and minimum scores are used to calculate
                                                 the probability of the response to each item.",br(),
                                                 tags$b("Please confirm the correctness of the maximum and minimum scores before proceeding to the next step.",
                                                        style = "color: red;"),
                                                 br(),br(),
                                                 DT::DTOutput("my_datatable"),
                                                 br(),"                                   ",
                                                 actionButton("save",label = "Confirm",width = "40%",
                                                              style = "background-color: red; color: white;"), br(),
                                                 htmlOutput("confirm_info"))
                            ),
                            column(4,
                                   box(title = "Upload Score Data", status = "warning", solidHeader = TRUE,
                                       fileInput("CRM_res", "Kindly submit the test data file that requires analysis.
                                                              Acceptable file formats for uploading include TXT, CSV, Excel, and SPSS.
                                                              Ensure that the file solely consists of score data, and refrain from
                                                              using purely numerical column names.",
                                                 placeholder="File",buttonLabel = "Browse",
                                                 accept = c("xlsx","xls","csv","sav","txt")
                                       ),width = 12)
                                   )
                            )
                          ),
                          tabItem(

                            tabName = "CRM_itemfit",
                            fluidPage(column(8,
                                             box(title = "Item Fit", solidHeader = TRUE, status = "info",width = 12,

                                                 DT::dataTableOutput(outputId = "CRM_item_fit")%>%
                                                   box_show_theme())),
                                      column(4,
                                             box(title = "Setup for Item Fit",
                                                 solidHeader = TRUE,status = "warning",width = 12,

                                                 sliderInput(inputId = "CRM_fit_group",
                                                             label = "The number of ability groups to compute item fit residual statistics.",
                                                             min = 2, max = 50, value = 20,step = 1),
                                                 "Compute item fit residual statistics for the Continuous Response Model
                                                 as described in Ferrando (2002).",
                                                 br(),br(),
                                                 tags$b("Reference"),br(),
                                                 "Ferrando, P.J.(2002). Theoretical and Empirical Comparison between Two Models for
                                                 Continuous Item Responses. Multivariate Behavioral Research, 37(4), 521-542.",
                                                 br(),br(),
                                                 tags$b("Note: If you have updated the Max/min value, click the button below.",
                                                        style = "color: red;"),
                                                 submitButton( "Update results")),
                                             box(title = "Download Results",solidHeader = TRUE,status = "success",width = 12,
                                                 "Download all the analysis results for continuous response model in the 'TestAnaAPP'.",br(),
                                                 downloadButton(outputId = "CRM_results",label = "Download"))))
                            ),
                          tabItem(tabName = "CRM_itempara",
                                  fluidPage(column(8,
                                                   box(title = "Item Parameters",solidHeader = TRUE, status = "info",width = 12,
                                                       DT::dataTableOutput(outputId = "CRM_itempar")%>%
                                                         box_show_theme())))

                                  ),

                          tabItem(tabName = "CRM_personpar",
                                  fluidPage(column(8,
                                                   box(title = "Person Parameter",solidHeader = TRUE, status = "info",width = 12,
                                                       tags$b("Note: "), br(),
                                                       "1. The maximum likelihood estimate (MLE) was used to estimate person parameter.",br(),
                                                       "2. The ID in the data corresponds to the row number of the subject's raw score data.",
                                                       br(),br(),
                                                       dataTableOutput(outputId = "CRM_person_par")%>%
                                                         box_show_theme())))
                          ),
                          tabItem(tabName = "CRM_ICC",
                                  fluidPage(column(8,
                                                   box(title = "Three-Dimensional Item Category Response Curves",solidHeader = TRUE,
                                                       status = "info",width = 12,
                                                       plotOutput(outputId = "CRM_ICC")%>%
                                                         box_show_theme()
                                                       )),
                                            column(4,
                                                   box(title = "Item Selection",width = 12,solidHeader = TRUE,
                                                       status = "warning",
                                                       uiOutput(outputId = "CRM_item_selection"),

                                                       submitButton( "Update plot"))))),
                          # H. DIF-----------------------------------------------------------------------------------------
                          tabItem(tabName = "DIF_group",
                                  fluidPage(column(8,
                                                   box(title="Score Data", solidHeader = TRUE, status = "warning",width = 12,
                                                       br(),
                                                       tags$b("Note: "), "If necessary, please convert the scoring direction of the
                                                       reverse-scored items before data analysis, and then upload them to this platform.",
                                                       tags$b("Please do not upload total score data for quizzes or dimensions."),
                                                       br(),br(),br(),
                                                       DT::dataTableOutput("DIF_Response")%>%
                                                         box_show_theme())),

                                            column(4,
                                                   box(title = "Upload Score Data", status = "warning", solidHeader = TRUE,
                                                       fileInput("DIF_res", "Kindly submit the test data file that requires analysis.
                                                              Acceptable file formats for uploading include TXT, CSV, Excel, and SPSS.
                                                              Ensure that the file solely consists of score data, and refrain from
                                                              using purely numerical column names.",
                                                                 placeholder="File",buttonLabel = "Browse",
                                                                 accept = c("xlsx","xls","csv","sav","txt")
                                                       ),width = 12),
                                                   box(title = "Variable Selection", solidHeader = TRUE, status = "warning",width = 12,
                                                       uiOutput(outputId = "DIF_group_var_select"),
                                                       submitButton( "Confirm"),
                                                       br(),br(),

                                                       textOutput(outputId = "group_variables"))))
                                  ),
                          tabItem(tabName = "DIF_analysis",
                                  fluidPage(column(8,
                                                   box(title = "Analysis Results of DIF",solidHeader = TRUE, status = "info",width = 12,
                                                       DT::dataTableOutput( "DIF_results")%>%
                                                         box_show_theme())),
                                            column(4,
                                                   box(title = "Settings for DIF",solidHeader = TRUE, status = "warning",width = 12,
                                                       "TestAnaAPP provides several methods for DIF analysis. You can choose the method
                                                       you want to use.",
                                                       br(),br(),
                                                       selectInput(inputId = "DIF_method",
                                                                   label = "Select the method for DIF analysis",
                                                                   choices = c("Mantel Haenszel","Logistic Regression","SIBTEST"),
                                                                   selected = "Mantel Haenszel"),
                                                       br(),br(),
                                                       selectInput(inputId = "sig_level",
                                                                   label = "Significance level",
                                                                   choices = c("0.01","0.05","0.1"),
                                                                   selected = "0.05"),
                                                       br(),br(),
                                                       "In addition, you can choose a variable (uploaded in the previous step) to analyze DIF.",
                                                       br(),br(),
                                                       uiOutput(outputId = "DIF_variable_selection"),
                                                       br(),br(),
                                                       submitButton( "Confirm"),
                                                       br(),
                                                       uiOutput(outputId = "focal_name"),
                                                       br(),br(),
                                                       submitButton( "Update results")),
                                                   box(title = "Download Results",solidHeader = TRUE,status = "success",width = 12,
                                                       "Download all the analysis results for DIF in the 'TestAnaAPP'.",br(),
                                                       downloadButton(outputId = "DIF_download",label = "Download")))))

                        )))

}
box_show_theme <- function(value){
  return(shinycssloaders::withSpinner(ui_element = value,color="#0dc5c1",type=6,size = 1.5 ))
}
DT_dataTable_Show <- function(x){
  DT::datatable(x,
                filter =list(position = 'top', clear = TRUE, plain = TRUE),
                options = list(scrollX = TRUE))
}
