# TestAnaAPP
 
This application enables exploratory factor analysis, confirmatory factor analysis, classical measurement theory analysis, unidimensional item response theory, multidimensional item response theory, and continuous item response model analysis, through the shiny interactive interface. It also facilitates the visualization of the results. Users can easily download the analysis results from the interactive interface. Additionally, users can download a concise report about items and test quality on the interactive interface.

If you want to use this application to analysis data, you should install TestAnaAPP package in R.

```
# install packages
install.packages("devtools")
devtools::install_github("jiangyouxiang/TestAnaAPP") 
library(TestAnaAPP)
TestAnaAPP::run_app() #run this application in R
```

