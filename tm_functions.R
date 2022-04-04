########################################################################
# Logistic Regression Model to Predict Matching for medical students applying to OBGYN: Source All
# Denver Health and Hospital Authority, 2020

#brew install cask
#brew install wget
#brew cask install basictex
#brew install pandoc
#brew install pkg-config
#.rs.restartR() #restarts R

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

#Additional knitr
knitr::knit_hooks$set(inline=inline_hook)
knitr::opts_chunk$set(dpi = 800) 

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

#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 
#####  Directory
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ####
here::set_here(path = ".", verbose = TRUE)
results_folder <- here::here("results")
images_folder <- here::here("images")
code_folder <- here::here("Code")
#data_folder <- "~/Dropbox/Nomogram (Personal)/nomogram/data/Archives/machine_readable/"

#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 
#####  CONSTANTS
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ####
#yvar <- "Match_Status"

########### Bespoke Functions ----
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 
#####  Functions for nomogram
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 
create_plot_num <- 
  function(data) {
    print("Function Sanity Check: Plot Numeric Features")
    funModeling::plot_num(data, path_out = results_folder)
  } 

create_plot_cross_plot <- 
  function(data) {
    print("Function Sanity Check: Cross Plot Features")
    funModeling::cross_plot(data, 
                            input=(colnames(data)), 
                            target="Match_Status", 
                            #path_out = results_folder,
                            auto_binning = TRUE)
  } #, auto_binning = FALSE, #Export results
#For numerical variables, cross_plot has by default the auto_binning=T, which automatically calls the equal_freq function with n_bins=10 (or the closest number).

create_profiling_num <- 
  function(data) {
    print("Function Sanity Check: Plot Numeric Features")
    funModeling::profiling_num(data)
  }

#https://www.kaggle.com/pjmcintyre/titanic-first-kernel#final-checks
tm_nomogram_prep <- function(df){  #signature of the function
  set.seed(1978)                  #body of the function
  print("Function Sanity Check: Creation of Nomogram")
  test <- rms::nomogram(df,
                        #lp.at = seq(-3,4,by=0.5),
                        fun = plogis,
                        fun.at = c(0.001, 0.01, 0.05, seq(0.2, 0.8, by = 0.2), 0.95, 0.99, 0.999),
                        funlabel = "Chance of Matching in OBGYN",
                        lp =FALSE,
                        #conf.int = c(0.1,0.7),
                        abbrev = F,
                        minlength = 9)
  
  tm_plot <- plot(test, lplabel="Linear Predictor",
                  cex.sub = 0.3, cex.axis=0.4, cex.main=1, cex.lab=0.2, ps=10, xfrac=1,
                  col.conf=c('red','green'),
                  conf.space=c(0.1,0.5),
                  label.every=1,
                  col.grid = gray(c(0.8, 0.95)),
                  which="Match_Status")
  return(tm_plot)
}

tm_rpart_plot = function(df){
  print("Function Sanity Check: Plot Decision Trees using package rpart with Leaves")
  tm_rpart_plot_leaves <- rpart.plot::rpart.plot(df, yesno = 2, type = 5, extra = +100, fallen.leaves = TRUE, varlen = 0, faclen = 0, roundint = TRUE, clip.facs = TRUE, shadow.col = "gray", main = "Tree Model of Medical Students Matching into OBGYN Residency\n(Matched or Unmatched)", box.palette = c("red", "green"))  
  tm_fancy_rpart <- fancyRpartPlot(df)
  return(c(tm_rpart_plot_leaves, tm_fancy_rpart))
}

# Draws a nice table one plot
tm_arsenal_table = function(df, by){
  print("Function Sanity Check: Create Arsenal Table using arsenal package")
  table_variable_within_function <- arsenal::tableby(by ~ .,
                                                     data=df, control = tableby.control(test = TRUE,
                                                                                        total = F,
                                                                                        digits = 1L,
                                                                                        digits.p = 2L,
                                                                                        digits.count = 0L,
                                                                                        numeric.simplify = F,
                                                                                        numeric.stats =
                                                                                          c("median",
                                                                                            "q1q3"),
                                                                                        cat.stats =
                                                                                          c("Nmiss",
                                                                                            "countpct"),
                                                                                        stats.labels = list(Nmiss = "N Missing",
                                                                                                            Nmiss2 ="N Missing",
                                                                                                            meansd = "Mean (SD)",
                                                                                                            medianrange = "Median (Range)",
                                                                                                            median ="Median",
                                                                                                            medianq1q3 = "Median (Q1, Q3)",
                                                                                                            q1q3 = "Q1, Q3",
                                                                                                            iqr = "IQR",
                                                                                                            range = "Range",
                                                                                                            countpct = "Count (Pct)",
                                                                                                            Nevents = "Events",
                                                                                                            medSurv ="Median Survival",
                                                                                                            medTime = "Median Follow-Up")))
  final <- summary(table_variable_within_function,
                   text=T,
                   title = 'Table: Applicant Descriptive Variables by Matched or Did Not Match from 2015 to 2018',
                   #labelTranslations = mylabels, #Seen in additional functions file
                   pfootnote=TRUE)
  return(final)
}

#Draws a nice plot of the variable strengths using ANOVA.  
tm_chart_strength_of_variables <- function(df) {
  print("Function Sanity Check: Plotting ANOVA dataframe for variable strength")
  plot <- plot(anova(df), cex=1, cex.lab=1.3, cex.axis = 0.9)
  return(plot)
}

#Helpful plot of variable importance for variable selection
tm_variable_importance = function(df) {
  print("Function Sanity Check: Evaluate Variable Importance")
  rf_imp <- varImp(df, scale = FALSE)
  rf_imp <- rf_imp$importance
  rf_gini <- data.frame(Variables = row.names(rf_imp), MeanDecreaseGini = rf_imp$Overall)
  
  rf_plot <- ggplot(rf_gini, aes(x=reorder(Variables, MeanDecreaseGini), y=MeanDecreaseGini, fill=MeanDecreaseGini)) +
    geom_bar(stat='identity') + coord_flip() + theme(legend.position="none") + labs(x="") +
    ggtitle('Variable Importance Random Forest') + theme(plot.title = element_text(hjust = 0.5))
  return(rf_plot)
}

calc_metrics <- function(model, new_data, truth) {
  truth_expr <- enquo(truth)
  
  suppressWarnings({
    model %>%
      stats::predict(new_data = new_data) %>%
      bind_cols(new_data %>% select(!! truth_expr)) %>%
      yardstick::metrics(truth = !! truth_expr, 
                         estimate = .pred) %>%
      dplyr::select(-.estimator) %>%
      tidyr::spread(.metric, .estimate)
  })
  
}

#Tidymodels feature plots
box_fun_plot = function(data, x, y) {
  ggplot2::ggplot(data = data, ggplot2::aes(x = .data[[x]],
                                            y = .data[[y]],
                                            fill = .data[[x]])) +
    geom_boxplot() +
    labs(title = y,
         x = x,
         y = y) +
    theme(
      legend.position = "none"
    )
}

density_fun_plot = function(data, x, y) {
  ggplot(data = data, aes(x = .data[[y]],
                          fill = .data[[x]])) +
    geom_density(alpha = 0.7) +
    labs(title = y,
         x = y) +
    theme(
      legend.position = "none"
    )
}

#Recipe function - First, I create a recipe where I define the transformations I want to apply to my data. In this case I create a simple recipe to change all character variables to factors.
#https://ryjohnson09.netlify.com/post/caret-and-tidymodels/

#The order in which you infer, center, scale, etc does matter (see this post).

# 1) Impute
# 2) Individual transformations for skewness and other issues
# 3) Discretize (if needed and if you have no other choice)
# 4) Create dummy variables
# 5) Create interactions
# 6) Normalization steps (center, scale, range, etc)
# 7) Multivariate transformation (e.g. PCA, spatial sign, etc)
# 
# recipe_simple <- function(dataset) {
#   recipes::recipe(Match_Status ~ ., data = dataset) %>%
#     recipes::step_string2factor(all_nominal(), -all_outcomes()) %>%
#     recipes::add_role(Match_Status, new_role = "outcome") %>% 
#     recipes::add_role(starts_with("Count_of_"), new_role = "predictor") %>% 
#     recipes::add_role(white_non_white, Age, Gender, Couples_Match, US_or_Canadian_Applicant, Medical_Education_Interrupted, Alpha_Omega_Alpha, Military_Service_Obligation, #USMLE_Step_1_Score, 
#              Visa_Sponsorship_Needed, Medical_Degree, new_role = "predictor") %>%
#     recipes::step_corr(all_numeric(), method = "pearson", threshold = 0.9)
# }   #Cool looks for correlation
# Add dummy variables for "class" variables
#recipes::step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>%  #Deal with categories
# Center and Scale
# recipes::step_center(all_predictors()) %>% 
# recipes::step_scale(all_predictors())


#Now I need to create a function that contains my model that can be iterated over each split of the data. I also want a function that will make predictions using said model.
fit_mod_func <- function(split, spec){
  print("Function Sanity Check: Function to fit her model using parsnip package")
  parsnip::fit(object = spec,
               formula = Match_Status ~ .,
               data = rsample::analysis(split))
}

predict_func <- function(split, model){
  print("Function Sanity Check: Prediction Function")
  # Extract the assessment data
  assess <- rsample::assessment(split)
  # Make prediction
  pred <- stats::predict(model, new_data = assess)
  dplyr::as_tibble(cbind(assess, pred[[1]]))
}

# Will calculate accuracy of classification
#perf_metrics <- yardstick::metric_set(accuracy)

# Create a function that will take the prediction and compare to truth
rf_metrics <- function(pred_df){
  print("Function Sanity Check: Compare the truth to the predictions")
  perf_metrics(
    pred_df,
    truth = Match_Status,
    estimate = res # res is the column name for the predictions
  )
}

#https://tidymodels.github.io/rsample/articles/Working_with_rsets.html
tm_holdout_results <- function(splits, ...) {
  # Fit the model to the 90%
  mod <- glm(..., data = analysis(splits), family = binomial)
  # Save the 10%
  holdout <- assessment(splits)
  # `augment` will save the predictions with the holdout data set
  res <- broom::augment(mod, newdata = holdout)
  # Class predictions on the assessment set from class probs
  lvls <- levels(holdout$Match_Status)
  predictions <- factor(ifelse(res$.fitted > 0, lvls[2], lvls[1]),
                        levels = lvls)
  # Calculate whether the prediction was correct
  res$correct <- predictions == holdout$Match_Status
  # Return the assessment data set with the additional columns
  res
}

tm_confusion_matrix_graph <- function (model_name, label) {  #label should be in quotation marks
  print("Function Sanity Check: Create Confusino Matrix Graphs")
  model_name %>%  #Plug in the model using the training data
    stats::predict(new_data = test_baked) %>%  #Predict the test_data with the new model
    bind_cols(test_baked) %>%
    conf_mat(Match_Status, .pred_class) %>%
    purrr::pluck(1) %>%
    tibble::as_tibble() %>%
    ggplot2::ggplot(aes(Prediction, Truth, alpha = n)) +  
    ggplot2::geom_tile(show.legend = FALSE) +
    ggplot2::geom_text(aes(label = n), colour = "white", alpha = 1, size = 8) +
    labs(
      title = paste('Confusion matrix using:', label, sep = " ")) }

# HR 201: PREDICTING EMPLOYEE ATTRITION WITH H2O AND LIME ----
# CHAPTER 4: H2O MODELING ----
# Extracts and H2O model name by a position so can more easily use h2o.getModel()
extract_h2o_model_name_by_position <- function(h2o_leaderboard, n = 1, verbose = T) {
  print("Function Sanity Check: Extracts H2O model name as a position")
  
  model_name <- h2o_leaderboard %>%
    as.tibble() %>%
    dplyr::slice(n) %>%
    pull(model_id)
  
  if (verbose) message(model_name)
  
  return(model_name)
  
}


# Visualize the H2O leaderboard to help with model selection
plot_h2o_leaderboard <- function(h2o_leaderboard, order_by = c("auc", "logloss"), 
                                 n_max = 20, size = 4, include_lbl = TRUE) {
  print("Function Sanity Check: H2O leaderboard visualization")
  # Setup inputs
  order_by <- tolower(order_by[[1]])
  
  leaderboard_tbl <- h2o_leaderboard %>%
    as.tibble() %>%
    dplyr::mutate(model_type = str_split(model_id, "_", simplify = T) %>% .[,1]) %>%
    rownames_to_column(var = "rowname") %>%
    dplyr::mutate(model_id = paste0(rowname, ". ", as.character(model_id)) %>% as.factor())
  
  # Transformation
  if (order_by == "auc") {
    
    data_transformed_tbl <- leaderboard_tbl %>%
      dplyr::slice(1:n_max) %>%
      dplyr::mutate(
        model_id   = as_factor(model_id) %>% reorder(auc),
        model_type = as.factor(model_type)
      ) %>%
      tidyr::gather(key = key, value = value, 
                    -c(model_id, model_type, rowname), factor_key = T)
    
  } else if (order_by == "logloss") {
    
    data_transformed_tbl <- leaderboard_tbl %>%
      dplyr::slice(1:n_max) %>%
      dplyr::mutate(
        model_id   = as_factor(model_id) %>% reorder(logloss) %>% fct_rev(),
        model_type = as.factor(model_type)
      ) %>%
      tidyr::gather(key = key, value = value, -c(model_id, model_type, rowname), factor_key = T)
    
  } else {
    stop(paste0("order_by = '", order_by, "' is not a permitted option."))
  }
  
  # Visualization
  print("Function Sanity Check: Creating Visualization")
  g <- data_transformed_tbl %>%
    ggplot(aes(value, model_id, color = model_type)) +
    geom_point(size = size) +
    facet_wrap(~ key, scales = "free_x") +
    #tidyquant::theme_tq() +
    scale_color_tq() +
    labs(title = "Leaderboard Metrics",
         subtitle = paste0("Ordered by: ", toupper(order_by)),
         y = "Model Postion, Model ID", x = "")
  
  if (include_lbl) g <- g + geom_label(aes(label = round(value, 2), hjust = "inward"))
  
  return(g)
  
}


# Convert a leaderboard into a Performance Diagnostic Dashboard
# containing an ROC Plot, Precision vs Recall Plot, Gain Plot, and Lift Plot
plot_h2o_performance <- function(h2o_leaderboard, newdata, order_by = c("auc", "logloss"),
                                 max_models = 3, size = 1.5) {
  print("Function Sanity Check: Plot H2O performance")
  # Inputs
  
  leaderboard_tbl <- h2o_leaderboard %>%
    as.tibble() %>%
    dplyr::slice(1:max_models)
  
  newdata_tbl <- newdata %>%
    as.tibble()
  
  order_by <- tolower(order_by[[1]])
  order_by_expr <- rlang::sym(order_by)
  
  h2o.no_progress()
  
  # 1. Model metrics
  print("Function Sanity Check: Check Model Performance Metrics")
  get_model_performance_metrics <- function(model_id, test_tbl) {
    
    model_h2o <- h2o.getModel(model_id)
    perf_h2o  <- h2o.performance(model_h2o, newdata = as.h2o(test_tbl))
    
    perf_h2o %>%
      h2o.metric() %>%
      as.tibble() %>%
      select(threshold, tpr, fpr, precision, recall)
    
  }
  
  model_metrics_tbl <- leaderboard_tbl %>%
    dplyr::mutate(metrics = map(model_id, get_model_performance_metrics, newdata_tbl)) %>%
    unnest() %>%
    dplyr::mutate(
      model_id = as_factor(model_id) %>% 
        fct_reorder(!! order_by_expr, .desc = ifelse(order_by == "auc", TRUE, FALSE)),
      auc  = auc %>% 
        round(3) %>% 
        as.character() %>% 
        as_factor() %>% 
        fct_reorder(as.numeric(model_id)),
      logloss = logloss %>% 
        round(4) %>% 
        as.character() %>% 
        as_factor() %>% 
        fct_reorder(as.numeric(model_id))
    )
  
  
  # 1A. ROC Plot
  
  p1 <- model_metrics_tbl %>%
    ggplot(aes_string("fpr", "tpr", color = "model_id", linetype = order_by)) +
    geom_line(size = size) +
    #tidyquant::theme_tq() +
    scale_color_tq() +
    labs(title = "ROC", x = "FPR", y = "TPR") +
    theme(legend.direction = "vertical")
  
  # 1B. Precision vs Recall
  
  p2 <- model_metrics_tbl %>%
    ggplot(aes_string("recall", "precision", color = "model_id", linetype = order_by)) +
    geom_line(size = size) +
    #tidyquant::theme_tq() +
    scale_color_tq() +
    labs(title = "Precision Vs Recall", x = "Recall", y = "Precision") +
    theme(legend.position = "none")
  
  
  # 2. Gain / Lift
  
  get_gain_lift <- function(model_id, test_tbl) {
    
    model_h2o <- h2o.getModel(model_id)
    perf_h2o  <- h2o.performance(model_h2o, newdata = as.h2o(test_tbl)) 
    
    perf_h2o %>%
      h2o.gainsLift() %>%
      as.tibble() %>%
      select(group, cumulative_data_fraction, cumulative_capture_rate, cumulative_lift)
    
  }
  
  gain_lift_tbl <- leaderboard_tbl %>%
    dplyr::mutate(metrics = map(model_id, get_gain_lift, newdata_tbl)) %>%
    unnest() %>%
    dplyr::mutate(
      model_id = as_factor(model_id) %>% 
        fct_reorder(!! order_by_expr, .desc = ifelse(order_by == "auc", TRUE, FALSE)),
      auc  = auc %>% 
        round(3) %>% 
        as.character() %>% 
        as_factor() %>% 
        fct_reorder(as.numeric(model_id)),
      logloss = logloss %>% 
        round(4) %>% 
        as.character() %>% 
        as_factor() %>% 
        fct_reorder(as.numeric(model_id))
    ) %>%
    rename(
      gain = cumulative_capture_rate,
      lift = cumulative_lift
    ) 
  
  # 2A. Gain Plot
  
  p3 <- gain_lift_tbl %>%
    ggplot(aes_string("cumulative_data_fraction", "gain", 
                      color = "model_id", linetype = order_by)) +
    geom_line(size = size) +
    geom_segment(x = 0, y = 0, xend = 1, yend = 1, 
                 color = "black", size = size) +
    #tidyquant::theme_tq() +
    scale_color_tq() +
    expand_limits(x = c(0, 1), y = c(0, 1)) +
    labs(title = "Gain",
         x = "Cumulative Data Fraction", y = "Gain") +
    theme(legend.position = "none")
  
  # 2B. Lift Plot
  
  p4 <- gain_lift_tbl %>%
    ggplot(aes_string("cumulative_data_fraction", "lift", 
                      color = "model_id", linetype = order_by)) +
    geom_line(size = size) +
    geom_segment(x = 0, y = 1, xend = 1, yend = 1, 
                 color = "black", size = size) +
    #tidyquant::theme_tq() +
    scale_color_tq() +
    expand_limits(x = c(0, 1), y = c(0, 1)) +
    labs(title = "Lift",
         x = "Cumulative Data Fraction", y = "Lift") +
    theme(legend.position = "none")
  
  
  # Combine using cowplot
  p_legend <- get_legend(p1)
  p1 <- p1 + theme(legend.position = "none")
  
  p <- cowplot::plot_grid(p1, p2, p3, p4, ncol = 2) 
  
  p_title <- ggdraw() + 
    draw_label("H2O Model Metrics", size = 18, fontface = "bold", 
               colour = palette_light()[[1]])
  
  p_subtitle <- ggdraw() + 
    draw_label(glue("Ordered by {toupper(order_by)}"), size = 10,  
               colour = palette_light()[[1]])
  
  ret <- plot_grid(p_title, p_subtitle, p, p_legend, 
                   ncol = 1, rel_heights = c(0.05, 0.05, 1, 0.05 * max_models))
  
  h2o.show_progress()
  
  return(ret)
  
}

# Precision vs Recall
load_model_performance_metrics <- function(path, test_tbl) {
  print("Function Sanity Check: Load Model Performance Metrics")
  model_h2o <- h2o.loadModel(path)
  perf_h2o  <- h2o.performance(model_h2o, newdata = as.h2o(test_tbl)) 
  
  perf_h2o %>%
    h2o.metric() %>%
    as.tibble() %>%
    mutate(auc = h2o.auc(perf_h2o)) %>%
    select(tpr, fpr, auc, precision, recall)
}


plot_features_tq <- function(explanation, ncol) {
  print("Function Sanity Check: Plot Features using the TQ theme")
  data_transformed <- explanation %>%
    as.tibble() %>%
    mutate(
      feature_desc = as_factor(feature_desc) %>% 
        fct_reorder(abs(feature_weight), .desc = FALSE),
      key     = ifelse(feature_weight > 0, "Supports", "Contradicts") %>% 
        fct_relevel("Supports"),
      case_text    = glue("Case: {case}"),
      label_text   = glue("Label: {label}"),
      prob_text    = glue("Probability: {round(label_prob, 2)}"),
      r2_text      = glue("Explanation Fit: {model_r2 %>% round(2)}")
    ) %>%
    select(feature_desc, feature_weight, key, case_text:r2_text)
  
  
  data_transformed %>%
    ggplot(aes(feature_desc, feature_weight, fill = key)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    #theme_tq() +
    scale_fill_tq() +
    labs(y = "Weight", x = "Feature") +
    theme(title = element_text(size = 9)) +
    facet_wrap(~ case_text + label_text + prob_text + r2_text,
               ncol = ncol, scales = "free")
  
}



plot_explanations_tq <- function(explanation) {
  
  data_transformed <- explanation %>%
    as.tibble() %>%
    dplyr::mutate(
      case    = as_factor(case),
      order_1 = rank(feature) 
    ) %>%
    dplyr::group_by(feature) %>%
    dplyr::mutate(
      order_2 = rank(feature_value)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      order = order_1 * 1000 + order_2
    ) %>%
    dplyr::mutate(
      feature_desc = as.factor(feature_desc) %>% 
        fct_reorder(order, .desc =  T) 
    ) %>%
    dplyr::select(case, feature_desc, feature_weight, label)
  
  data_transformed %>%
    ggplot(aes(case, feature_desc)) +
    geom_tile(aes(fill = feature_weight)) +
    facet_wrap(~ label) +
    #theme_tq() +
    scale_fill_gradient2(low = palette_light()[[2]], mid = "white",
                         high = palette_light()[[1]]) +
    theme(
      panel.grid = element_blank(),
      legend.position = "right",
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    ) +
    labs(y = "Feature", x = "Case", 
         fill = glue("Feature
                         Weight"))
  
}


plot_hist_facet <- function(data, fct_reorder = FALSE, fct_rev = FALSE, 
                            bins = 10, fill = palette_light()[[3]], color = "white", ncol = 5, scale = "free") {
  print("Function Sanity Check: Plot Histogram that is Faceted")
  data_factored <- data %>%
    mutate_if(is.character, as.factor) %>%
    mutate_if(is.factor, as.numeric) %>%
    tidyr::gather(key = key, value = value, factor_key = TRUE) 
  
  if (fct_reorder) {
    data_factored <- data_factored %>%
      mutate(key = as.character(key) %>% as.factor())
  }
  
  if (fct_rev) {
    data_factored <- data_factored %>%
      mutate(key = fct_rev(key))
  }
  
  g <- data_factored %>%
    ggplot(aes(x = value, group = key)) +
    geom_histogram(bins = bins, fill = fill, color = color) +
    facet_wrap(~ key, ncol = ncol, scale = scale) #+ 
  #theme_tq()
  
  return(g)
  
}


cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat) 
  p.mat
}

get_cor <- function(data, target, use = "pairwise.complete.obs",
                    fct_reorder = FALSE, fct_rev = FALSE) {
  print("Function Sanity Check: Get Correlation of Features")
  feature_expr <- enquo(target)
  feature_name <- quo_name(feature_expr)
  
  data_cor <- data %>%
    mutate_if(is.character, as.factor) %>%
    mutate_if(is.factor, as.numeric) %>%
    cor(use = use) %>%
    as.tibble() %>%
    mutate(feature = names(.)) %>%
    select(feature, !! feature_expr) %>%
    filter(!(feature == feature_name)) %>%
    mutate_if(is.character, as_factor)
  
  if (fct_reorder) {
    data_cor <- data_cor %>% 
      mutate(feature = fct_reorder(feature, !! feature_expr)) %>%
      arrange(feature)
  }
  
  if (fct_rev) {
    data_cor <- data_cor %>% 
      mutate(feature = fct_rev(feature)) %>%
      arrange(feature)
  }
  
  return(data_cor)
  
}

plot_cor <- function(data, target, fct_reorder = FALSE, fct_rev = FALSE, 
                     include_lbl = TRUE, lbl_precision = 2, lbl_position = "outward",
                     size = 2, line_size = 1, vert_size = 1, 
                     color_pos = palette_light()[[1]], color_neg = palette_light()[[2]]) {
  print("Function Sanity Check: Plot Correlation of Features")
  feature_expr <- enquo(target)
  feature_name <- quo_name(feature_expr)
  
  data_cor <- data %>%
    get_cor(!! feature_expr, fct_reorder = fct_reorder, fct_rev = fct_rev) %>%
    mutate(feature_name_text = round(!! feature_expr, lbl_precision)) %>%
    mutate(Correlation = case_when(
      (!! feature_expr) >= 0 ~ "Positive",
      TRUE                   ~ "Negative") %>% as.factor())
  
  g <- data_cor %>%
    ggplot(aes_string(x = feature_name, y = "feature", group = "feature")) +
    geom_point(aes(color = Correlation), size = size) +
    geom_segment(aes(xend = 0, yend = feature, color = Correlation), size = line_size) +
    geom_vline(xintercept = 0, color = palette_light()[[1]], size = vert_size) +
    expand_limits(x = c(-1, 1)) +
    #theme_tq() +
    scale_color_manual(values = c(color_neg, color_pos)) 
  
  if (include_lbl) g <- g + geom_label(aes(label = feature_name_text), hjust = lbl_position)
  
  return(g)
  
}


# ggpairs: A lot of repetitive typing can be reduced 
plot_ggpairs <- function(data, color = NULL, density_alpha = 0.5) {
  print("Function Sanity Check: Plot ggpairs")
  color_expr <- enquo(color)
  
  if (rlang::quo_is_null(color_expr)) {
    
    g <- data %>%
      ggpairs(lower = "blank") 
    
  } else {
    
    color_name <- quo_name(color_expr)
    
    g <- data %>%
      ggpairs(mapping = aes_string(color = color_name), 
              lower = "blank", legend = 1,
              diag = list(continuous = wrap("densityDiag", 
                                            alpha = density_alpha))) +
      theme(legend.position = "bottom")
  }
  
  return(g)
  
}


get_roc_metric <- function(data_tr_sample, target, best_vars) 
{
  # data_tr_sample=data_sol
  # target = target_var_s
  # best_vars=names_2
  
  fitControl <- caret::trainControl(method = "cv", 
                                    number = 3, 
                                    summaryFunction = twoClassSummary,
                                    classProbs = TRUE)
  
  data_model=select(data_tr_sample, one_of(best_vars))
  
  mtry = sqrt(ncol(data_model))
  tunegrid = expand.grid(.mtry=round(mtry))
  
  fit_model_1 = caret::train(x=data_model, 
                             y= target, 
                             method = "rf", 
                             trControl = fitControl,
                             metric = "ROC",
                             tuneGrid=tunegrid
  )
  
  metric=fit_model_1$results["ROC"][1,1]
  
  return(metric)
}




get_accuracy_metric <- function(data_tr_sample, target, best_vars) 
{
  print("Function Sanity Check: Check Accuracy Metrics")
  data_model=select(data_tr_sample, one_of(best_vars))
  
  fitControl <- trainControl(method = "cv", 
                             number = 3, 
                             summaryFunction = twoClassSummary)
  
  data_model=select(data_tr_sample, one_of(best_vars))
  
  mtry = sqrt(ncol(data_model))
  tunegrid = expand.grid(mtry=round(mtry))
  
  fit_model_1 = caret::train(x=data_model, 
                             y= target, 
                             method = "rf",
                             tuneGrid = tunegrid)
  
  
  
  metric=fit_model_1$results["Accuracy"][1,1]
  return(metric)
}  

# https://github.com/tobiolatunji/Readmission_Prediction/blob/master/diabetes_readmission.R
# pseudo R-squared for logistic regression model
logisticPseudoR2s <- function(LogModel) {
  print("Function Sanity Check: Calculate a logistic PseudoR2s")
  dev <- LogModel$deviance 
  nullDev <- LogModel$null.deviance 
  modelN <-  length(LogModel$fitted.values)
  R.l <-  1 -  dev / nullDev
  R.cs <- 1- exp ( -(nullDev - dev) / modelN)
  R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2  ", round(R.l, 3), "\n")
  cat("Cox and Snell R^2        ", round(R.cs, 3), "\n")
  cat("Nagelkerke R^2           ", round(R.n, 3),    "\n")
}


#Variable Importance Function
tm_vip <- function (object, title, ...) {
  print("Function Sanity Check: Variable Importance Function")
  vip <- vip::vip(object = object, 
                  bar = TRUE,
                  horizontal = TRUE,
                  shape = 1,
                  color = "grey35",
                  fill = "grey35",
                  all_permutations = TRUE,
                  num_features = 10L,
                  alpha = 1) +
    ggtitle(title)
  return(vip)
}

tm_ggsave <- function (object, filename, ...){  #make sure the file name has quotation marks around it.  
  print("Function Sanity Check: Saving a ggplot image as a TIFF")
  ggplot2::ggsave(here::here("results", filename), object, device = "tiff", width = 10, height = 7, dpi = 200)
}


tm_print_save <- function (filename) {
  print("Function Sanity Check: Saving TIFF of what is in the viewer")
  dev.print(tiff, (here::here("results", filename)), compression = "lzw",width=2000, height=2000, bg="transparent", res = 200, units = "px" )
  dev.off()
}

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

export_formattable <- function(f, file, width = "100%", height = NULL, 
                               background = "white", delay = 0.2)
{
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}

mse = function(ytru, yhat) mean((yhat - ytru)^2)

#' @title Calculate the Brier score for a binary classifier.
#'
#' @description
#' The Brier score for a binary classifer is defined as the mean squared error  
#' of the predicted probabilities and the true target values (0's or 1's). It 
#' can be used as a model evaluation metric. The smaller, the better.
#' 
#' @param ytru A numeric vector of 0's and 1's.
#' @param pred_prob A numeric vector of predicted probabilities.
#'
#' @return Returns a single number.
brier_score_binary = function(ytru, pred_prob) mse(ytru, pred_prob)

get_coefs = function(model) {
  broom::tidy(model) %>% 
    mutate(odds_ratio = exp(estimate),
           signif = case_when(p.value < 0.001 ~ '***',
                              p.value < 0.01 ~ '**',
                              p.value < 0.05 ~ '*',
                              TRUE ~ ''))
}

tm_broom <- function(model){
  print("Function Sanity Test: Model Evaluation")
  model_output <- broom::tidy(step_model, conf.int=TRUE) %>% 
    dplyr::select(-statistic, -std.error) %>% 
    dplyr::filter(term != "(Intercept)") %>%
    dplyr::arrange(p.value) %>%
    dplyr::select(term, estimate, conf.low, conf.high, p.value, tidyselect::everything())
  return(model_output)
}

load_matching_data <-
  function(location, year) {
    print("Function Sanity Check: Reading in data")
    data_folder <- "~/Dropbox (Personal)/Nomogram/nomogram/data/Archives/machine_readable/"
    #Read in each file by location_year.csv.  "data/Archives/machine_readable/CU_2017.csv"
    
    pathandfilename <-paste0(data_folder, location,"_", year, ".csv")
    df <- exploratory::read_delim_file(pathandfilename , ",", quote = "\"", skip = 0 , col_names = TRUE , na = c('','NA') , locale=readr::locale(encoding = "UTF-8", decimal_mark = ".", grouping_mark = "," ), trim_ws = TRUE , progress = TRUE) %>%
      #col_types = cols(AAMC_ID = "n", Applicant_Name = "f", .default="?") #This was causing an error parsing for some reason.  
      readr::type_convert() %>%
      exploratory::clean_data_frame() %>%
      dplyr::mutate(Year = year) %>%
      dplyr::filter(`Count of Peer Reviewed Book Chapter` != "Obstetrics-Gynecology|1076220C0 (Categorical)") %>% #data cleaning
      dplyr::filter(Gender %in% c("Female", "Male")) %>% #data QA
      dplyr::mutate(`Date of Birth` = lubridate::mdy(`Date of Birth`)) %>% #Data formatting into date variable
      janitor::clean_names(case = "parsed") %>% #changing all columns to the same formatting: parsed
      dplyr::mutate(Date_of_Birth_year = lubridate::year(Date_of_Birth)) %>%
      dplyr::mutate(Year_numeric = as.numeric(Year)) %>%
      dplyr::mutate(Year_numeric = exploratory::recode(Year_numeric, `1` = 2017, `2` = 2018, `3` = 2019, `4` = 2020)) %>%
      dplyr::mutate(Age = Year_numeric - Date_of_Birth_year) %>%
      dplyr::mutate(Location = location) %>%
      dplyr::mutate_at(vars(Alpha_Omega_Alpha_Yes_No, Gender, Gold_Humanism_Honor_Society_Yes_No, Military_Service_Obligation, Participating_as_a_Couple_in_NRMP, US_or_Canadian_Applicant, Visa_Sponsorship_Needed, Medical_Education_or_Training_Interrupted, Medical_School_Type, Felony_Conviction, Misdemeanor_Conviction, Malpractice_Cases_Pending, Year, Count_of_Oral_Presentation, Count_of_Peer_Reviewed_Book_Chapter, Count_of_Peer_Reviewed_Journal_Articles_Abstracts, Count_of_Peer_Reviewed_Journal_Articles_Abstracts_Other_than_Published, Count_of_Poster_Presentation, Count_of_Scientific_Monograph), funs(factor))
  }

# Function to read in data for 2020 ----
load_matching_data_2020 <-
  function(location, year) {
    #year: 2020
    print("Function Sanity Check: Reading in data")
    data_folder <- "~/Dropbox (Personal)/Nomogram/nomogram/data/Archives/machine_readable/"
    
    pathandfilename <-paste0(data_folder, location,"_", year, ".csv")
    df <- exploratory::read_delim_file(pathandfilename , ",", quote = "\"", skip = 0 , col_names = TRUE , na = c('','NA') , locale=readr::locale(encoding = "UTF-8", decimal_mark = ".", grouping_mark = "," ), trim_ws = TRUE , progress = TRUE, col_types = cols(AAMC_ID = "i", Applicant_Name = "f", .default="?")) %>%
      readr::type_convert() %>%
      exploratory::clean_data_frame() %>%
      dplyr::rename(`US or Canadian Applicant` = `Graduate of US or Canadian Medical School`) %>%  #This is different for the 2020 data set read in.  
      dplyr::select(`AAMC ID`, `Applicant Name`, `Medical Education or Training Interrupted`, `Malpractice Cases Pending`, `Felony Conviction`, `Misdemeanor Conviction`, `Alpha Omega Alpha (Yes/No)`, `Date of Birth`, `First Name`, Gender, `Gold Humanism Honor Society (Yes/No)`, `Last Name`, `Middle Name`, `Military Service Obligation`, `Participating as a Couple in NRMP`, 
                    #`Self Identify`,
                    `US or Canadian Applicant`, `Visa Sponsorship Needed`, `Medical Degree`, `Medical School of Graduation`, `Medical School Type`, `USMLE Step 1 Score`, `Tracks Applied by Applicant`, `Count of Oral Presentation`, `Count of Peer Reviewed Book Chapter`, `Count of Peer Reviewed Journal Articles/Abstracts`, `Count of Peer Reviewed Journal Articles/Abstracts(Other than Published)`, `Count of Poster Presentation`, `Count of Scientific Monograph`) %>%
      dplyr::mutate(Year = year) %>%
      dplyr::filter(`Count of Peer Reviewed Book Chapter` != "Obstetrics-Gynecology|1076220C0 (Categorical)") %>% #data cleaning
      dplyr::filter(Gender %in% c("Female", "Male")) %>% #data QA
      dplyr::mutate(`Date of Birth` = lubridate::mdy(`Date of Birth`)) %>% #Data formatting into date variable
      janitor::clean_names(case = "parsed") %>% #changing all columns to the same formatting: parsed
      dplyr::mutate(Date_of_Birth_year = lubridate::year(Date_of_Birth)) %>%
      dplyr::mutate(Year_numeric = as.numeric(Year)) %>%
      dplyr::mutate(Year_numeric = exploratory::recode(Year_numeric, `1` = 2017, `2` = 2018, `3` = 2019, `4` = 2020)) %>%
      dplyr::mutate(Age = Year_numeric - Date_of_Birth_year) %>%
      dplyr::mutate(Location = location) %>%
      dplyr::mutate_at(vars(Alpha_Omega_Alpha_Yes_No, Gender, Gold_Humanism_Honor_Society_Yes_No, Military_Service_Obligation, Participating_as_a_Couple_in_NRMP, US_or_Canadian_Applicant, Visa_Sponsorship_Needed, Medical_Education_or_Training_Interrupted, Medical_School_Type, Felony_Conviction, Misdemeanor_Conviction, Malpractice_Cases_Pending, Year, Count_of_Oral_Presentation, Count_of_Peer_Reviewed_Book_Chapter, Count_of_Peer_Reviewed_Journal_Articles_Abstracts, Count_of_Peer_Reviewed_Journal_Articles_Abstracts_Other_than_Published, Count_of_Poster_Presentation, Count_of_Scientific_Monograph), funs(factor))
  }

data_cleaning <-
  function (df) {
    #df <- All_location_Applicants
    print("Function Sanity Check: Cleaning the data")
    All_location_Quality_Control <- df %>% 
      #filter(Withdrawn_by_Applicant == "No") %>%  #Exclusion criteria  #kicked out all 2020 participants for some reason.  UGGGHH!  
      dplyr::filter(Age > 24) %>%                 #Exclusion criteria
      dplyr::filter(Date_of_Birth_year < 2010) %>%#Exclusion criteria
      dplyr::filter(Date_of_Birth_year >1950) %>% #Exclusion criteria
      
      dplyr::select(-ID) %>%                      #Quality Control
      dplyr::filter(!is.na(AAMC_ID)) %>%          #Quality Control
      dplyr::filter(str_detect(Tracks_Applied_by_Applicant, "Gyn")) %>% #Quality Control
      
      dplyr::distinct(AAMC_ID, .keep_all = TRUE) %>% # Keep only applicants for the first time they applied
      
      dplyr::filter(Gender %in% c("Female", "Male")) %>% #Selects only male or females
      dplyr::select(-Alpha_Omega_Alpha, -Gold_Humanism_Honor_Society, -Couples_Match) %>%
      dplyr::mutate(Medical_School_Type = exploratory::recode(Medical_School_Type, `Canadian School` = "U.S. Public School", `International School,International School` = "International School", `International School,International School,International School` = "International School", `U.S. Public School,U.S. Public School` = "U.S. Public School", `U.S. Private School,International School` = "U.S. Private School", `U.S. Private School,International School,U.S. Private School,U.S. Private School` = "U.S. Private School")) %>%
      dplyr::mutate(Gold_Humanism_Honor_Society_Yes_No = exploratory::recode(Gold_Humanism_Honor_Society_Yes_No, `No Response` = "No")) %>%
      dplyr::mutate(Alpha_Omega_Alpha_Yes_No = exploratory::recode(Alpha_Omega_Alpha_Yes_No, `No Response` = "No")) %>%
      dplyr::mutate(Medical_Degree = exploratory::recode(Medical_Degree, M.D. = "MD", `M.B.,B.S.` = "MD", M.C. = "MD", `M.D./Ph.D.` = "MD", D.O. = "DO", `DO/MBA` = "DO", M.B.B.Ch.B = "MD", `DO/PhD` = "DO", `M.D./M.P.H.` = "MD", M.B.B.Ch. = "MD", `M.A./M.D.` = "MD", `M.D./Other` = "MD", `M.S./M.D.` = "MD", `DO/MPH` = "DO", `M.D./M.B.A.` = "MD", M.B. = "MD", `B.A./M.D.` = "MD", `B.S./M.D.` = "MD", M.B.Ch.B. = "MD", B.MED = "MD", M.Surg. = "MD", M.Med. = "MD", `M.D.,C.M.` = "MD", B.A.O. = "MD", `M.Surg.,M.B.,B.S.` = "MD", `M.D./Ph.D.,M.D.` = "MD", `M.S./M.D.,M.B.,B.S.` = "MD", `M.B.,B.S.,M.D.` = "MD", `M.B.,B.S.,M.B.,B.S.` = "MD", `M.D./Ph.D.,M.B.B.Ch.,M.Med.` = "MD", `DO/MA` = "DO", `M.D.,M.B.,B.S.` = "MD", `M.D.,M.D.` = "MD", `M.B.B.Ch.,M.D.,M.Med.` = "MD", `DO/MS` = "DO", `B.A.O.,M.D.` = "MD", `M.D.,M.D./Ph.D.` = "MD", `BS/DO` = "DO", `DO/MSED` = "DO", `BS/DO` = "DO", `DO/MSED` = "DO", D.H.Sc = "MD", .default = "MD")) %>% #clean all the honorrifics
      dplyr::mutate(Visa_Sponsorship_Needed = exploratory::impute_na(Visa_Sponsorship_Needed, type = "value", val = "No")) %>%
      dplyr::mutate(USMLE_Pass_Fail = USMLE_Step_1_Score, USMLE_Pass_Fail_replaced = case_when( #change step 1 to pass/fail
        USMLE_Pass_Fail >= 194 ~ "Passed",
        USMLE_Pass_Fail < 194 ~ "Failed attempt")) %>%
      
      dplyr::rename(Alpha_Omega_Alpha = Alpha_Omega_Alpha_Yes_No, Couples_Match = Participating_as_a_Couple_in_NRMP, Type_of_medical_school = Medical_School_Type, Gold_Humanism_Honor_Society = Gold_Humanism_Honor_Society_Yes_No) %>% #rename
      exploratory::reorder_cols(Age, Self_Identify, Gender, US_or_Canadian_Applicant, Type_of_medical_school, Medical_Degree, Military_Service_Obligation, Visa_Sponsorship_Needed, Medical_Education_or_Training_Interrupted, Misdemeanor_Conviction, Alpha_Omega_Alpha, Gold_Humanism_Honor_Society, Couples_Match, Count_of_Oral_Presentation, Count_of_Peer_Reviewed_Book_Chapter, Count_of_Peer_Reviewed_Journal_Articles_Abstracts, Count_of_Peer_Reviewed_Journal_Articles_Abstracts_Other_than_Published, Count_of_Poster_Presentation, AAMC_ID, Applicant_Name) 
  } 


#Formula that creates a formula of all the variables in a dataset to be inputted into a lrm model
linearvars.lrm <- function(data){
  #get all the columns except for 'Match_Status'
  linearvars <- setdiff(sort(colnames(data)),'Match_Status') #Use select with all_merged to remove variables that should not go into the model like year and location
  
  linearphrase <- paste(linearvars, collapse=" + ") #combine the linear terms with the rest of the formula
  
  fullformula <- stats::as.formula( paste0('Match_Status == "Matched" ~', linearphrase) )
  
  return(fullformula)
}

### rmarkdown Defaults ----
options(tinytex.verbose = TRUE)
theme_set(theme_sjplot())

### Multiple Cores ----
doMC::registerDoMC(cores = detectCores()-1) #Use multiple cores for processing

###tableby labels
mylabels <- list(white_non_white = "Race", Age = "Age, years", Gender = "Sex", Couples_Match = "Participating in the Couples Match", US_or_Canadian_Applicant = "US or Canadian Applicant", Medical_Education_Interrupted = "Medical Education Process was Interrupted", Alpha_Omega_Alpha = "Alpha Omega Alpha", Military_Service_Obligation = "Military Service Obligation", USMLE_Step_1_Score = "USMLE Step 1 Score", Military_Service_Obligation = "Military Service Obligations", Count_of_Poster_Presentation = "Count of Poster Presentations", Count_of_Oral_Presentation = "Count of Oral Presentations", Count_of_Articles_Abstracts = "Count of Published Abstracts", Count_of_Peer_Reviewed_Book_Chapter = "Count of Peer Reviewed Book Chapters", Count_of_Other_than_Published = "Count of Other Published Products", Count_of_Online_Publications = "Count of Online Publications", Visa_Sponsorship_Needed = "Visa Sponsorship is Needed", Medical_Degree = "Medical Degree Training")

#### Decision Curve Analysis Function
dca <- function(data, outcome, predictors, xstart=0.01, xstop=0.99, xby=0.01, 
                ymin=-0.05, probability=NULL, harm=NULL,graph=TRUE, intervention=FALSE, 
                interventionper=100, smooth=FALSE,loess.span=0.10) {
  
  # LOADING REQUIRED LIBRARIES
  require(stats)
  
  # data MUST BE A DATA FRAME
  if (class(data)!="data.frame") {
    stop("Input data must be class data.frame")
  }
  
  #ONLY KEEPING COMPLETE CASES
  data=data[complete.cases(data[append(outcome,predictors)]),append(outcome,predictors)]
  
  # outcome MUST BE CODED AS 0 AND 1
  if (max(data[[outcome]])>1 | min(data[[outcome]])<0) {
    stop("outcome cannot be less than 0 or greater than 1")
  }
  # xstart IS BETWEEN 0 AND 1
  if (xstart<0 | xstart>1) {
    stop("xstart must lie between 0 and 1")
  }
  
  # xstop IS BETWEEN 0 AND 1
  if (xstop<0 | xstop>1) {
    stop("xstop must lie between 0 and 1")
  }
  
  # xby IS BETWEEN 0 AND 1
  if (xby<=0 | xby>=1) {
    stop("xby must lie between 0 and 1")
  }
  
  # xstart IS BEFORE xstop
  if (xstart>=xstop) {
    stop("xstop must be larger than xstart")
  }
  
  #STORING THE NUMBER OF PREDICTORS SPECIFIED
  pred.n=length(predictors)
  
  #IF probability SPECIFIED ENSURING THAT EACH PREDICTOR IS INDICATED AS A YES OR NO
  if (length(probability)>0 & pred.n!=length(probability)) {
    stop("Number of probabilities specified must be the same as the number of predictors being checked.")
  }
  
  #IF harm SPECIFIED ENSURING THAT EACH PREDICTOR HAS A SPECIFIED HARM
  if (length(harm)>0 & pred.n!=length(harm)) {
    stop("Number of harms specified must be the same as the number of predictors being checked.")
  }
  
  #INITIALIZING DEFAULT VALUES FOR PROBABILITES AND HARMS IF NOT SPECIFIED
  if (length(harm)==0) {
    harm=rep(0,pred.n)
  }
  if (length(probability)==0) {
    probability=rep(TRUE,pred.n)
  }
  
  
  #CHECKING THAT EACH probability ELEMENT IS EQUAL TO YES OR NO, 
  #AND CHECKING THAT PROBABILITIES ARE BETWEEN 0 and 1
  #IF NOT A PROB THEN CONVERTING WITH A LOGISTIC REGRESSION
  for(m in 1:pred.n) { 
    if (probability[m]!=TRUE & probability[m]!=FALSE) {
      stop("Each element of probability vector must be TRUE or FALSE")
    }
    if (probability[m]==TRUE & (max(data[predictors[m]])>1 | min(data[predictors[m]])<0)) {
      stop(paste(predictors[m],"must be between 0 and 1 OR sepcified as a non-probability in the probability option",sep=" "))  
    }
    if(probability[m]==FALSE) {
      model=NULL
      pred=NULL
      model=glm(data.matrix(data[outcome]) ~ data.matrix(data[predictors[m]]), family=binomial("logit"))
      pred=data.frame(model$fitted.values)
      pred=data.frame(pred)
      names(pred)=predictors[m]
      data=cbind(data[names(data)!=predictors[m]],pred)
      print(paste(predictors[m],"converted to a probability with logistic regression. Due to linearity assumption, miscalibration may occur.",sep=" "))
    }
  }
  
  # THE PREDICTOR NAMES CANNOT BE EQUAL TO all OR none.
  if (length(predictors[predictors=="all" | predictors=="none"])) {
    stop("Prediction names cannot be equal to all or none.")
  }  
  
  #########  CALCULATING NET BENEFIT   #########
  N=dim(data)[1]
  event.rate=colMeans(data[outcome])
  
  # CREATING DATAFRAME THAT IS ONE LINE PER THRESHOLD PER all AND none STRATEGY
  nb=data.frame(seq(from=xstart, to=xstop, by=xby))
  names(nb)="threshold"
  interv=nb
  
  nb["all"]=event.rate - (1-event.rate)*nb$threshold/(1-nb$threshold)
  nb["none"]=0
  
  # CYCLING THROUGH EACH PREDICTOR AND CALCULATING NET BENEFIT
  for(m in 1:pred.n){
    for(t in 1:length(nb$threshold)){
      # COUNTING TRUE POSITIVES AT EACH THRESHOLD
      tp=mean(data[data[[predictors[m]]]>=nb$threshold[t],outcome])*sum(data[[predictors[m]]]>=nb$threshold[t])
      # COUNTING FALSE POSITIVES AT EACH THRESHOLD
      fp=(1-mean(data[data[[predictors[m]]]>=nb$threshold[t],outcome]))*sum(data[[predictors[m]]]>=nb$threshold[t])
      #setting TP and FP to 0 if no observations meet threshold prob.
      if (sum(data[[predictors[m]]]>=nb$threshold[t])==0) {
        tp=0
        fp=0
      }
      
      # CALCULATING NET BENEFIT
      nb[t,predictors[m]]=tp/N - fp/N*(nb$threshold[t]/(1-nb$threshold[t])) - harm[m]
    }
    interv[predictors[m]]=(nb[predictors[m]] - nb["all"])*interventionper/(interv$threshold/(1-interv$threshold))
  }
  
  # CYCLING THROUGH EACH PREDICTOR AND SMOOTH NET BENEFIT AND INTERVENTIONS AVOIDED 
  for(m in 1:pred.n) {
    if (smooth==TRUE){
      lws=loess(data.matrix(nb[!is.na(nb[[predictors[m]]]),predictors[m]]) ~ data.matrix(nb[!is.na(nb[[predictors[m]]]),"threshold"]),span=loess.span)
      nb[!is.na(nb[[predictors[m]]]),paste(predictors[m],"_sm",sep="")]=lws$fitted
      
      lws=loess(data.matrix(interv[!is.na(nb[[predictors[m]]]),predictors[m]]) ~ data.matrix(interv[!is.na(nb[[predictors[m]]]),"threshold"]),span=loess.span)
      interv[!is.na(nb[[predictors[m]]]),paste(predictors[m],"_sm",sep="")]=lws$fitted
    }
  }
  
  # PLOTTING GRAPH IF REQUESTED
  if (graph==TRUE) {
    require(graphics)
    
    # PLOTTING INTERVENTIONS AVOIDED IF REQUESTED
    if(intervention==TRUE) {
      # initialize the legend label, color, and width using the standard specs of the none and all lines
      legendlabel <- NULL
      legendcolor <- NULL
      legendwidth <- NULL
      legendpattern <- NULL
      
      #getting maximum number of avoided interventions
      ymax=max(interv[predictors],na.rm = TRUE)
      
      #INITIALIZING EMPTY PLOT WITH LABELS
      plot(x=nb$threshold, y=nb$all, type="n" ,xlim=c(xstart, xstop), ylim=c(ymin, ymax), xlab="Threshold probability", ylab=paste("Net reduction in interventions per",interventionper,"patients"))
      
      #PLOTTING INTERVENTIONS AVOIDED FOR EACH PREDICTOR
      for(m in 1:pred.n) {
        if (smooth==TRUE){
          lines(interv$threshold,data.matrix(interv[paste(predictors[m],"_sm",sep="")]),col=m,lty=2)
        } else {
          lines(interv$threshold,data.matrix(interv[predictors[m]]),col=m,lty=2)
        }
        
        # adding each model to the legend
        legendlabel <- c(legendlabel, predictors[m])
        legendcolor <- c(legendcolor, m)
        legendwidth <- c(legendwidth, 1)
        legendpattern <- c(legendpattern, 2)
      }
    } else {
      # PLOTTING NET BENEFIT IF REQUESTED
      
      # initialize the legend label, color, and width using the standard specs of the none and all lines
      legendlabel <- c("None", "All")
      legendcolor <- c(17, 8)
      legendwidth <- c(2, 2)
      legendpattern <- c(1, 1)
      
      #getting maximum net benefit
      ymax=max(nb[names(nb)!="threshold"],na.rm = TRUE)
      
      # inializing new benfit plot with treat all option
      plot(x=nb$threshold, y=nb$all, type="l", col=8, lwd=2 ,xlim=c(xstart, xstop), ylim=c(ymin, ymax), xlab="Threshold probability", ylab="Net benefit")
      # adding treat none option
      lines(x=nb$threshold, y=nb$none,lwd=2)
      #PLOTTING net benefit FOR EACH PREDICTOR
      for(m in 1:pred.n) {
        if (smooth==TRUE){
          lines(nb$threshold,data.matrix(nb[paste(predictors[m],"_sm",sep="")]),col=m,lty=2) 
        } else {
          lines(nb$threshold,data.matrix(nb[predictors[m]]),col=m,lty=2)
        }
        # adding each model to the legend
        legendlabel <- c(legendlabel, predictors[m])
        legendcolor <- c(legendcolor, m)
        legendwidth <- c(legendwidth, 1)
        legendpattern <- c(legendpattern, 2)
      }
    }
    # then add the legend
    legend("topright", legendlabel, cex=0.8, col=legendcolor, lwd=legendwidth, lty=legendpattern)
    
  }
  
  #RETURNING RESULTS
  results=list() 
  results$N=N
  results$predictors=data.frame(cbind(predictors,harm,probability))
  names(results$predictors)=c("predictor","harm.applied","probability")
  results$interventions.avoided.per=interventionper
  results$net.benefit=nb
  results$interventions.avoided=interv
  
  return(results)
  
}  

options(tidymodels.dark = TRUE)

# if (!requireNamespace("BiocManager", quietly=TRUE))
#   install.packages("BiocManager")
# BiocManager::install("ggbio")

### From stackoverflow, Brier score
BigSummary <- function (data, lev = NULL, model = NULL) {
  pr_auc <- try(MLmetrics::PRAUC(data[, lev[2]],
                                 ifelse(data$obs == lev[2], 1, 0)),
                silent = TRUE)
  brscore <- try(mean((data[, lev[2]] - ifelse(data$obs == lev[2], 1, 0)) ^ 2),
                 silent = TRUE)
  rocObject <- try(pROC::roc(ifelse(data$obs == lev[2], 1, 0), data[, lev[2]],
                             direction = "<", quiet = TRUE), silent = TRUE)
  if (inherits(pr_auc, "try-error")) pr_auc <- NA
  if (inherits(brscore, "try-error")) brscore <- NA
  rocAUC <- if (inherits(rocObject, "try-error")) {
    NA
  } else {
    rocObject$auc
  }
  tmp <- unlist(e1071::classAgreement(table(data$obs,
                                            data$pred)))[c("diag", "kappa")]
  out <- c(Acc = tmp[[1]],
           Kappa = tmp[[2]],
           AUCROC = rocAUC,
           AUCPR = pr_auc,
           Brier = brscore,
           Precision = caret:::precision.default(data = data$pred,
                                                 reference = data$obs,
                                                 relevant = lev[2]),
           Recall = caret:::recall.default(data = data$pred,
                                           reference = data$obs,
                                           relevant = lev[2]),
           F = caret:::F_meas.default(data = data$pred, reference = data$obs,
                                      relevant = lev[2]))
  out
}



dca <- function(data, outcome, predictors, xstart=0.01, xstop=0.99, xby=0.01, 
                ymin=-0.05, probability=NULL, harm=NULL,graph=TRUE, intervention=FALSE, 
                interventionper=100, smooth=FALSE,loess.span=0.10) {
  
  # LOADING REQUIRED LIBRARIES
  require(stats)
  
  # data MUST BE A DATA FRAME
  if (class(data)!="data.frame") {
    stop("Input data must be class data.frame")
  }
  
  #ONLY KEEPING COMPLETE CASES
  data=data[complete.cases(data[append(outcome,predictors)]),append(outcome,predictors)]
  
  # outcome MUST BE CODED AS 0 AND 1
  if (max(data[[outcome]])>1 | min(data[[outcome]])<0) {
    stop("outcome cannot be less than 0 or greater than 1")
  }
  # xstart IS BETWEEN 0 AND 1
  if (xstart<0 | xstart>1) {
    stop("xstart must lie between 0 and 1")
  }
  
  # xstop IS BETWEEN 0 AND 1
  if (xstop<0 | xstop>1) {
    stop("xstop must lie between 0 and 1")
  }
  
  # xby IS BETWEEN 0 AND 1
  if (xby<=0 | xby>=1) {
    stop("xby must lie between 0 and 1")
  }
  
  # xstart IS BEFORE xstop
  if (xstart>=xstop) {
    stop("xstop must be larger than xstart")
  }
  
  #STORING THE NUMBER OF PREDICTORS SPECIFIED
  pred.n=length(predictors)
  
  #IF probability SPECIFIED ENSURING THAT EACH PREDICTOR IS INDICATED AS A YES OR NO
  if (length(probability)>0 & pred.n!=length(probability)) {
    stop("Number of probabilities specified must be the same as the number of predictors being checked.")
  }
  
  #IF harm SPECIFIED ENSURING THAT EACH PREDICTOR HAS A SPECIFIED HARM
  if (length(harm)>0 & pred.n!=length(harm)) {
    stop("Number of harms specified must be the same as the number of predictors being checked.")
  }
  
  #INITIALIZING DEFAULT VALUES FOR PROBABILITES AND HARMS IF NOT SPECIFIED
  if (length(harm)==0) {
    harm=rep(0,pred.n)
  }
  if (length(probability)==0) {
    probability=rep(TRUE,pred.n)
  }
  
  
  #CHECKING THAT EACH probability ELEMENT IS EQUAL TO YES OR NO, 
  #AND CHECKING THAT PROBABILITIES ARE BETWEEN 0 and 1
  #IF NOT A PROB THEN CONVERTING WITH A LOGISTIC REGRESSION
  for(m in 1:pred.n) { 
    if (probability[m]!=TRUE & probability[m]!=FALSE) {
      stop("Each element of probability vector must be TRUE or FALSE")
    }
    if (probability[m]==TRUE & (max(data[predictors[m]])>1 | min(data[predictors[m]])<0)) {
      stop(paste(predictors[m],"must be between 0 and 1 OR sepcified as a non-probability in the probability option",sep=" "))  
    }
    if(probability[m]==FALSE) {
      model=NULL
      pred=NULL
      model=glm(data.matrix(data[outcome]) ~ data.matrix(data[predictors[m]]), family=binomial("logit"))
      pred=data.frame(model$fitted.values)
      pred=data.frame(pred)
      names(pred)=predictors[m]
      data=cbind(data[names(data)!=predictors[m]],pred)
      print(paste(predictors[m],"converted to a probability with logistic regression. Due to linearity assumption, miscalibration may occur.",sep=" "))
    }
  }
  
  # THE PREDICTOR NAMES CANNOT BE EQUAL TO all OR none.
  if (length(predictors[predictors=="all" | predictors=="none"])) {
    stop("Prediction names cannot be equal to all or none.")
  }  
  
  #########  CALCULATING NET BENEFIT   #########
  N=dim(data)[1]
  event.rate=colMeans(data[outcome])
  
  # CREATING DATAFRAME THAT IS ONE LINE PER THRESHOLD PER all AND none STRATEGY
  nb=data.frame(seq(from=xstart, to=xstop, by=xby))
  names(nb)="threshold"
  interv=nb
  
  nb["all"]=event.rate - (1-event.rate)*nb$threshold/(1-nb$threshold)
  nb["none"]=0
  
  # CYCLING THROUGH EACH PREDICTOR AND CALCULATING NET BENEFIT
  for(m in 1:pred.n){
    for(t in 1:length(nb$threshold)){
      # COUNTING TRUE POSITIVES AT EACH THRESHOLD
      tp=mean(data[data[[predictors[m]]]>=nb$threshold[t],outcome])*sum(data[[predictors[m]]]>=nb$threshold[t])
      # COUNTING FALSE POSITIVES AT EACH THRESHOLD
      fp=(1-mean(data[data[[predictors[m]]]>=nb$threshold[t],outcome]))*sum(data[[predictors[m]]]>=nb$threshold[t])
      #setting TP and FP to 0 if no observations meet threshold prob.
      if (sum(data[[predictors[m]]]>=nb$threshold[t])==0) {
        tp=0
        fp=0
      }
      
      # CALCULATING NET BENEFIT
      nb[t,predictors[m]]=tp/N - fp/N*(nb$threshold[t]/(1-nb$threshold[t])) - harm[m]
    }
    interv[predictors[m]]=(nb[predictors[m]] - nb["all"])*interventionper/(interv$threshold/(1-interv$threshold))
  }
  
  # CYCLING THROUGH EACH PREDICTOR AND SMOOTH NET BENEFIT AND INTERVENTIONS AVOIDED 
  for(m in 1:pred.n) {
    if (smooth==TRUE){
      lws=loess(data.matrix(nb[!is.na(nb[[predictors[m]]]),predictors[m]]) ~ data.matrix(nb[!is.na(nb[[predictors[m]]]),"threshold"]),span=loess.span)
      nb[!is.na(nb[[predictors[m]]]),paste(predictors[m],"_sm",sep="")]=lws$fitted
      
      lws=loess(data.matrix(interv[!is.na(nb[[predictors[m]]]),predictors[m]]) ~ data.matrix(interv[!is.na(nb[[predictors[m]]]),"threshold"]),span=loess.span)
      interv[!is.na(nb[[predictors[m]]]),paste(predictors[m],"_sm",sep="")]=lws$fitted
    }
  }
  
  # PLOTTING GRAPH IF REQUESTED
  if (graph==TRUE) {
    require(graphics)
    
    # PLOTTING INTERVENTIONS AVOIDED IF REQUESTED
    if(intervention==TRUE) {
      # initialize the legend label, color, and width using the standard specs of the none and all lines
      legendlabel <- NULL
      legendcolor <- NULL
      legendwidth <- NULL
      legendpattern <- NULL
      
      #getting maximum number of avoided interventions
      ymax=max(interv[predictors],na.rm = TRUE)
      
      #INITIALIZING EMPTY PLOT WITH LABELS
      plot(x=nb$threshold, y=nb$all, type="n" ,xlim=c(xstart, xstop), ylim=c(ymin, ymax), xlab="Threshold probability", ylab=paste("Net reduction in interventions per",interventionper,"patients"))
      
      #PLOTTING INTERVENTIONS AVOIDED FOR EACH PREDICTOR
      for(m in 1:pred.n) {
        if (smooth==TRUE){
          lines(interv$threshold,data.matrix(interv[paste(predictors[m],"_sm",sep="")]),col=m,lty=2)
        } else {
          lines(interv$threshold,data.matrix(interv[predictors[m]]),col=m,lty=2)
        }
        
        # adding each model to the legend
        legendlabel <- c(legendlabel, predictors[m])
        legendcolor <- c(legendcolor, m)
        legendwidth <- c(legendwidth, 1)
        legendpattern <- c(legendpattern, 2)
      }
    } else {
      # PLOTTING NET BENEFIT IF REQUESTED
      
      # initialize the legend label, color, and width using the standard specs of the none and all lines
      legendlabel <- c("None", "All")
      legendcolor <- c(17, 8)
      legendwidth <- c(2, 2)
      legendpattern <- c(1, 1)
      
      #getting maximum net benefit
      ymax=max(nb[names(nb)!="threshold"],na.rm = TRUE)
      
      # inializing new benfit plot with treat all option
      plot(x=nb$threshold, y=nb$all, type="l", col=8, lwd=2 ,xlim=c(xstart, xstop), ylim=c(ymin, ymax), xlab="Threshold probability", ylab="Net benefit")
      # adding treat none option
      lines(x=nb$threshold, y=nb$none,lwd=2)
      #PLOTTING net benefit FOR EACH PREDICTOR
      for(m in 1:pred.n) {
        if (smooth==TRUE){
          lines(nb$threshold,data.matrix(nb[paste(predictors[m],"_sm",sep="")]),col=m,lty=2) 
        } else {
          lines(nb$threshold,data.matrix(nb[predictors[m]]),col=m,lty=2)
        }
        # adding each model to the legend
        legendlabel <- c(legendlabel, predictors[m])
        legendcolor <- c(legendcolor, m)
        legendwidth <- c(legendwidth, 1)
        legendpattern <- c(legendpattern, 2)
      }
    }
    # then add the legend
    legend("topright", legendlabel, cex=0.8, col=legendcolor, lwd=legendwidth, lty=legendpattern)
    
  }
  
  #RETURNING RESULTS
  results=list() 
  results$N=N
  results$predictors=data.frame(cbind(predictors,harm,probability))
  names(results$predictors)=c("predictor","harm.applied","probability")
  results$interventions.avoided.per=interventionper
  results$net.benefit=nb
  results$interventions.avoided=interv
  
  return(results)
  
}  

```

## Ok, let's run this as a job so my console is free for viz while I'm tuning
#library(rstudioapi)
#jobRunScript("wfsets_tune.R", name = "wfsets_tune", exportEnv = "R_GlobalEnv")

#Cool hex plots
# library(ggthemes)
# 
# d %>% 
#   ggplot(aes(launch_speed, launch_angle)) +
#   geom_hex() +
#   scale_fill_viridis_c(option = "magma") +
#   theme_minimal() +
#   theme(text = element_text(family = "concert")) +
#   labs(x = "Launch Speed", y = "Launch Angle", fill = "Count",
#        title = "Most Launch Speeds are Around 100 MPH with Launch Angles Right Around Zero",
#        subtitle = "The Launch Speed of My Code is About 0 With An Angle of AHHHHH")

all_cores <- parallel::detectCores()
doParallel::registerDoParallel(cores=all_cores-2)

# Addressing github token issue
library(gitcreds)
#gitcreds_set()

print("All done!")