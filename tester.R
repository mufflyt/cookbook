

set.seed(1978)   
rm(list = setdiff(ls(), lsf.str())) #cleans all environment except functions
invisible(gc())
options(scipen = 3)
options(warning.length = 2000L)


## Default repo
local({r <- getOption("repos")
r["CRAN"] <- "http://cran.r-project.org" 
options(repos=r)
})

#Bespoke functions
tm_write2pdf <- 
  function(object, filename) {  
    #pass filename and title with quotations, do not add .pdf
    print("Function Sanity Check: Creating Arsenal Table as a PDF")
    arsenal::write2pdf(object,(paste0(filename, ".pdf")), 
                       #puts the file into a subdirectory called tables
                       keep.md = TRUE,
                       quiet = TRUE) # passed to rmarkdown::render
    #pander::openFileInOS((paste0(filename, ".pdf"))) #sweet
  }


# Set libPaths ----
#.libPaths("/Users/tylermuffly/.exploratory/R/4.1")

# Install Packages ----
pkgs <- (c('caret', 'readxl', 'XML', 'reshape2', 'devtools', 'purrr', 'readr', 'ggplot2', 'dplyr', 'magick', 'janitor', 'lubridate', 'hms', 'tidyr', 'stringr', 'openxlsx', 'forcats', 'RcppRoll', 'tibble', 'bit64', 'munsell', 'scales', 'rgdal', 'tidyverse', "foreach", "PASWR", "rms", "pROC", "ROCR", "nnet", "packrat", "DynNom", "caTools", "mlbench", "randomForest", "ipred", "xgboost", "Metrics", "RANN", "AppliedPredictiveModeling", "shiny", "earth", "fastAdaboost", "Boruta", "glmnet", "ggforce", "tidylog", "InformationValue", "pscl", "scoring", "DescTools", "gbm", "Hmisc", "arsenal", "pander", "moments", "leaps", "MatchIt", "car", "mice", "rpart", "beepr", "fansi", "utf8", "lmtest", "ResourceSelection", "rmarkdown", "rattle", "rmda", "funModeling", "tinytex", "caretEnsemble", "Rmisc", "corrplot", "progress", "perturbR", "vctrs", "highr", "labeling", "DataExplorer", "rsconnect", "inspectdf", "ggpubr", "tableone", "knitr", "drake", "visNetwork", "rpart.plot", "RColorBrewer", "kableExtra", "kernlab", "naivebayes", "e1071", "data.table", "skimr", "naniar", "english", "mosaic", "broom", "mltools", "tidymodels", "tidyquant", "rsample", "yardstick", "parsnip", "dials", "cowplot", "lime", "flexdashboard", "shinyjs", "shinyWidgets", "plotly", "BH", "vip", "ezknitr", "here", "corrgram", "factoextra", "parallel", "doParallel", "odbc", "RSQLite", "discrim", "doMC",  "summarytools", "remotes", "fs", "PerformanceAnalytics", "correlationfunnel", "psych", "RSelenium", "robotstxt", "corpcor", "mctest", "ppcor", "compareGroups", "fastDummies", "grpreg", "Matrix", "rstudioapi", "tune", "glue", "catboost", "beepr", "miniUI", "klaR", "moments",
"ranger", 'R.methodsS3', 'plotROC', 'MLmetrics', 'humaniformat', 'DiagrammeR', 'DiagrammeRsvg', 'stargazer', 'coefplot', 'sjPlot', 'sjmisc', "renv", "recipes", "timeDate", "ggthemes"))
#"ggmap", "sf",'rsvg', 

#install.packages(pkgs,dependencies = c("Depends", "Suggests", "Imports", "LinkingTo"), repos = "https://cloud.r-project.org")  #run this first time
lapply(pkgs, require, character.only = TRUE)
rm(pkgs)

#Renv stuff
renv::settings$snapshot.type("all")
#renv::snapshot()

# install.packages("remotes")
#remotes::install_github("lmullen/genderdata")
#install.packages("genderdata", repos = "http://packages.ropensci.org")
options(download.file.method = "libcurl")
#devtools::install_github('catboost/catboost', subdir = 'catboost/R-package')
#devtools::install_url('https://github.com/catboost/catboost/releases/download/v0.23.2/catboost-R-Darwin-0.23.2.tgz', INSTALL_opts = c("--no-multiarch"))


# Knitr Options ----
knitr::opts_chunk$set(comment = "#>", echo = FALSE, fig.width = 6, message = FALSE, warning = FALSE, error = FALSE, strip.white = TRUE, include = FALSE)

# Set code width to 60 to contain within PDF margins
knitr::opts_chunk$set(tidy = F, tidy.opts = list(width.cutoff = 60))

# Set all figures to be centered
knitr::opts_chunk$set(fig.align = "center")

# Set and preserve par(mfcol()) between chunks (calls to it can be hidden)
knitr::opts_knit$set(global.par = T)


#Loads up exploratory.io ----
if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")
}
#devtools::install_github("paulhendricks/anonymizer")
library(anonymizer)

#devtools::install_github("tidyverse/glue")
library(glue)

#install.packages("backports")
library(backports)

#install.packages("psych")
library(psych)

#devtools::install_github("exploratory-io/exploratory_func")
library(exploratory)

remotes::install_github("r-link/corrmorant")
library(corrmorant)
