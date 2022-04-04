

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


# Set libPaths ----
#.libPaths("/Users/tylermuffly/.exploratory/R/4.1")

# Install Packages ----
pkgs <- (c('caret', 'readxl', 'XML', 'reshape2', 'devtools', 'tidyverse', 'magick', 'hms', 'tidyr', 'sjPlot', 'sjmisc', 'knitr', 'here'))

#install.packages(pkgs,dependencies = c("Depends", "Suggests", "Imports", "LinkingTo"), repos = "https://cloud.r-project.org")  #run this first time
lapply(pkgs, require, character.only = TRUE)
rm(pkgs)

#Renv stuff
devtools::install_github("https://github.com/rstudio/renv",dependencies = c("Depends", "Suggests", "Imports", "LinkingTo"), repos = "https://cloud.r-project.org", upgrade = c("always")))
library(renv)

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
devtools::install_github("paulhendricks/anonymizer", dependencies = c("Depends", "Suggests", "Imports", "LinkingTo"), upgrade = c("always"))
library(anonymizer)

devtools::install_github("tidyverse/glue",dependencies = c("Depends", "Suggests", "Imports", "LinkingTo"), upgrade = c("always"))
library(glue)

install.packages("backports",dependencies = c("Depends", "Suggests", "Imports", "LinkingTo"), upgrade = c("always"))
library(backports)

install.packages("psych",dependencies = c("Depends", "Suggests", "Imports", "LinkingTo"), upgrade = c("always"))
library(psych)

devtools::install_github("exploratory-io/exploratory_func",dependencies = c("Depends", "Suggests", "Imports", "LinkingTo"), upgrade = c("always"))
library(exploratory)

remotes::install_github("r-link/corrmorant")
library(corrmorant)

tm_write2pdf <- 
  function(object, filename) {  
    #pass filename and title with quotations, do not add .pdf
    print("Function Sanity Check: Creating Arsenal Table as a PDF")
    arsenal::write2pdf(object, (here::here("tables", (paste0(filename, ".pdf")))), 
                       #puts the file into a subdirectory called tables
                       keep.md = TRUE,
                       quiet = TRUE) # passed to rmarkdown::render
  }

tm_write2word <- function(object, filename) {  
  #pass filename and title with quotations
  print("Function Sanity Check: Creating Arsenal Table as a Word Document")
  arsenal::write2word(object, (here::here("tables", (paste0(filename, ".doc")))),
                      keep.md = TRUE,
                      quiet = TRUE) # passed to rmarkdown::render
}

tm_t_test <- function(variable){
  print("Function Sanity Test: t-test")
  output <- stats::t.test(y=variable[train$Match_Status == "Matched"],
                          x=variable[train$Match_Status == "Did not match"],
                          alternative = ("two.sided"),
                          paired = FALSE,
                          conf.level = 0.95,
                          var.equal = TRUE)
  
  print("X is group that did not match and Y is group that did match:")
  return(output)
}


tm_chi_square_test <- function (variable) {
  print("Function Sanity Test: chi-square test")
  chisq <- stats::chisq.test(variable, train$Match_Status, correct = FALSE)
  return(chisq)
}

#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 
#####  Directory
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ####
set_here(path = ".", verbose = TRUE)
results_folder <- here("results")
images_folder <- here("images")
code_folder <- here("Code")
data_folder <- "~/Dropbox/Nomogram (Personal)/nomogram/data/Archives/machine_readable/"

#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 
#####  CONSTANTS
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ####
yvar <- "Match_Status"
