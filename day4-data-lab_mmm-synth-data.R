#install.packages("Robyn")
#install.packages("reticulate")

# load robyn
library("Robyn")

# download python https://www.python.org/downloads/ (watch out to download the version matching your operating system and a version prior to 3.10)
# if you're using mac with an intel processor, use https://www.python.org/ftp/python/3.9.13/python-3.9.13-macosx10.9.pkg
# if you're using mac with another processor, use https://www.python.org/ftp/python/3.9.13/python-3.9.13-macos11.pkg
# if you're using windows, use https://www.python.org/ftp/python/3.9.13/python-3.9.13.exe (32-bit) or https://www.python.org/ftp/python/3.9.13/python-3.9.13-amd64.exe (64-bit)

# 1. load reticulate
library("reticulate")
# 2. create virtual environment
virtualenv_create("r-reticulate")
# 3. use the created environment 
use_virtualenv("r-reticulate", required = TRUE)
# 4. point Python path to the python file in the virtual environment. Below is
#    an example for MacOS M1 or above. The "~" is my home dir "/Users/gufengzhou".
#    Show hidden files in case you want to locate the file yourself.
Sys.setenv(RETICULATE_PYTHON = "~/.virtualenvs/r-reticulate/bin/python")
# 5. Check python path
py_config() # If the first path is not as 4, do 6
# 6. Restart R session, run #4 first, then load library("reticulate"), check
#    py_config() again, python should have path as in #4.
#    If you see: "NOTE: Python version was forced by RETICULATE_PYTHON_FALLBACK"
#    if you're using RStudio, go to Global Options > Python, and uncheck the
#    box for "Automatically activate project-local Python environments".
# 7. Install numpy if py_config shows it's not available
py_install("numpy", pip = TRUE)
# 8. Install nevergrad
py_install("nevergrad", pip = TRUE)
# 9. If successful, py_config() should show numpy and nevergrad with installed paths
# 10. Everytime R session is restarted, you need to run #4 first to assign python
#    path before loading Robyn
# 11. Alternatively, add the line RETICULATE_PYTHON = "~/.virtualenvs/r-reticulate/bin/python"
#    in the file Renviron in the the R directory to force R to always use this path by
#    default. One way to create and edit the Renviron file is to install the package "usethis" and run
#    the function usethis::edit_r_environ(). For Unix/Mac, there's also another Renviron file
#    located at path "/Library/Frameworks/R.framework/Resources/etc/". Add the line from above to this file.
#    This way, you don't need to run #4 everytime. Restart R session after editing.


## Force multi-core use when running RStudio
Sys.setenv(R_FUTURE_FORK_ENABLE = "true")
options(future.fork.enable = TRUE)

# Set to FALSE to avoid the creation of files locally
create_files <- TRUE

## IMPORTANT: Must install and setup the python library "Nevergrad" once before using Robyn
## Guide: https://github.com/facebookexperimental/Robyn/blob/main/demo/install_nevergrad.R

################################################################
#### Step 1: Load data

# set wd
setwd("/Users/jr/Dropbox/Talks:teaching/Mannheim Business School/Data")
# load day 4 dataset
mmm_data <- read.csv("day4-data.csv", header=TRUE, sep=",")

## Check simulated dataset or load your own dataset
data("dt_simulated_weekly")
head(dt_simulated_weekly)

## Check holidays from Prophet
# 59 countries included. If your country is not included, please manually add it.
# Tipp: any events can be added into this table, school break, events etc.
data("dt_prophet_holidays")
head(dt_prophet_holidays)

# Directory where you want to export results to (will create new folders)
robyn_directory <-"/Users/jr/Dropbox/Talks:teaching/Mannheim Business School/Data"
#robyn_directory <- "~/Desktop"

################################################################
#### Step 2a: For first time user: Model specification in 4 steps

#### 2a-1: First, specify input variables

## All sign control are now automatically provided: "positive" for media & organic
## variables and "default" for all others. User can still customise signs if necessary.
## Documentation is available, access it anytime by running: ?robyn_inputs
InputCollect <- robyn_inputs(
  dt_input = mmm_data, ### where you can add a different dataset
  dt_holidays = dt_prophet_holidays,
  date_var = "date", # date format must be "2020-01-01"
  dep_var = "sales", # there should be only one dependent variable
  dep_var_type = "revenue", # "revenue" (ROI) or "conversion" (CPA)
  prophet_vars = c("trend", "season", "weekday"), # "trend","season", "weekday" & "holiday"
  prophet_country = "US", # input country code. Check: dt_prophet_holidays
  context_vars = c("unemployment"), # e.g. competitors, discount, unemployment etc
  paid_media_spends = c("facebook_newsfeed_spend", "youtube_brand_spend", "search_spend", "youtube_performance_spend", "newspaper_spend", "tv_spend"), # mandatory input
  paid_media_vars = c("facebook_newsfeed_impressions", "youtube_brand_impressions", "search_clicks", "youtube_performance_impressions", "newspaper_readership", "tv_gross_rating_points"), # mandatory.
  # paid_media_vars must have same order as paid_media_spends. Use media exposure metrics like
  # impressions, GRP etc. If not applicable, use spend instead.
  #organic_vars = "newsletter", # marketing activity without media spend
  # factor_vars = c("events"), # force variables in context_vars or organic_vars to be categorical
  window_start = "2020-01-01",
  window_end = "2022-07-01",
  adstock = "geometric" # geometric, weibull_cdf or weibull_pdf.
)
print(InputCollect)

#### 2a-2: Second, define and add hyperparameters

## Default media variable for modelling has changed from paid_media_vars to paid_media_spends.
## Also, calibration_input are required to be spend names.
## hyperparameter names are based on paid_media_spends names too. See right hyperparameter names:
hyper_names(adstock = InputCollect$adstock, all_media = InputCollect$all_media)

## Guide to setup & understand hyperparameters

## Robyn's hyperparameters have four components:
## - Adstock parameters (theta or shape/scale)
## - Saturation parameters (alpha/gamma)
## - Regularisation parameter (lambda). No need to specify manually
## - Time series validation parameter (train_size)

## 1. IMPORTANT: set plot = TRUE to create example plots for adstock & saturation
## hyperparameters and their influence in curve transformation.
plot_adstock(plot = FALSE)
plot_saturation(plot = FALSE)

## 2. Get correct hyperparameter names:
# All variables in paid_media_spends and organic_vars require hyperparameter and will be
# transformed by adstock & saturation.
# Run hyper_names(adstock = InputCollect$adstock, all_media = InputCollect$all_media)
# to get correct media hyperparameter names. All names in hyperparameters must equal
# names from hyper_names(), case sensitive. Run ?hyper_names to check function arguments.

## 3. Hyperparameter interpretation & recommendation:

## Geometric adstock: Theta is the only parameter and means fixed decay rate. Assuming TV
# spend on day 1 is 100€ and theta = 0.7, then day 2 has 100*0.7=70€ worth of effect
# carried-over from day 1, day 3 has 70*0.7=49€ from day 2 etc. Rule-of-thumb for common
# media genre: TV c(0.3, 0.8), OOH/Print/Radio c(0.1, 0.4), digital c(0, 0.3). Also,
# to convert weekly to daily we can transform the parameter to the power of (1/7),
# so to convert 30% daily to weekly is 0.3^(1/7) = 0.84.

## Weibull CDF adstock: The Cumulative Distribution Function of Weibull has two parameters,
# shape & scale, and has flexible decay rate, compared to Geometric adstock with fixed
# decay rate. The shape parameter controls the shape of the decay curve. Recommended
# bound is c(0, 2). The larger the shape, the more S-shape. The smaller, the more
# L-shape. Scale controls the inflexion point of the decay curve. We recommend very
# conservative bounce of c(0, 0.1), because scale increases the adstock half-life greatly.
# When shape or scale is 0, adstock will be 0.

## Weibull PDF adstock: The Probability Density Function of the Weibull also has two
# parameters, shape & scale, and also has flexible decay rate as Weibull CDF. The
# difference is that Weibull PDF offers lagged effect. When shape > 2, the curve peaks
# after x = 0 and has NULL slope at x = 0, enabling lagged effect and sharper increase and
# decrease of adstock, while the scale parameter indicates the limit of the relative
# position of the peak at x axis; when 1 < shape < 2, the curve peaks after x = 0 and has
# infinite positive slope at x = 0, enabling lagged effect and slower increase and decrease
# of adstock, while scale has the same effect as above; when shape = 1, the curve peaks at
# x = 0 and reduces to exponential decay, while scale controls the inflexion point; when
# 0 < shape < 1, the curve peaks at x = 0 and has increasing decay, while scale controls
# the inflexion point. When all possible shapes are relevant, we recommend c(0.0001, 10)
# as bounds for shape; when only strong lagged effect is of interest, we recommend
# c(2.0001, 10) as bound for shape. In all cases, we recommend conservative bound of
# c(0, 0.1) for scale. Due to the great flexibility of Weibull PDF, meaning more freedom
# in hyperparameter spaces for Nevergrad to explore, it also requires larger iterations
# to converge. When shape or scale is 0, adstock will be 0.

## Hill function for saturation: Hill function is a two-parametric function in Robyn with
# alpha and gamma. Alpha controls the shape of the curve between exponential and s-shape.
# Recommended bound is c(0.5, 3). The larger the alpha, the more S-shape. The smaller, the
# more C-shape. Gamma controls the inflexion point. Recommended bounce is c(0.3, 1). The
# larger the gamma, the later the inflection point in the response curve.

## Regularization for ridge regression: Lambda is the penalty term for regularised regression.
# Lambda doesn't need manual definition from the users, because it is set to the range of
# c(0, 1) by default in hyperparameters and will be scaled to the proper altitude with
# lambda_max and lambda_min_ratio.

## Time series validation: When ts_validation = TRUE in robyn_run(), train_size defines the
# percentage of data used for training, validation and out-of-sample testing. For example,
# when train_size = 0.7, val_size and test_size will be 0.15 each. This hyperparameter is
# customizable with default range of c(0.5, 0.8) and must be between c(0.1, 1).

## 4. Set individual hyperparameter bounds. They either contain two values e.g. c(0, 0.5),
# or only one value, in which case you'd "fix" that hyperparameter.
# Run hyper_limits() to check maximum upper and lower bounds by range
hyper_limits()

# Example hyperparameters ranges for Geometric adstock
hyperparameters <- list(
  facebook_newsfeed_spend_alphas = c(0.5, 3),
  facebook_newsfeed_spend_gammas = c(0.3, 1),
  facebook_newsfeed_spend_thetas = c(0, 0.3),
  youtube_brand_spend_alphas = c(0.5, 3),
  youtube_brand_spend_gammas = c(0.3, 1),
  youtube_brand_spend_thetas = c(0.2, 0.7),
  tv_spend_alphas = c(0.5, 3),
  tv_spend_gammas = c(0.3, 1),
  tv_spend_thetas = c(0.3, 0.8),
  search_spend_alphas = c(0.5, 3),
  search_spend_gammas = c(0.3, 1),
  search_spend_thetas = c(0.1, 0.5),
  youtube_performance_spend_alphas = c(0.5, 3),
  youtube_performance_spend_gammas = c(0.3, 1),
  youtube_performance_spend_thetas = c(0, 0.3),
  newspaper_spend_alphas = c(0.5, 3),
  newspaper_spend_gammas = c(0.3, 1),
  newspaper_spend_thetas = c(0.1, 0.4),
  train_size = c(0.5, 0.8)
)

# Example hyperparameters ranges for Weibull CDF adstock
# facebook_S_alphas = c(0.5, 3)
# facebook_S_gammas = c(0.3, 1)
# facebook_S_shapes = c(0, 2)
# facebook_S_scales = c(0, 0.1)

# Example hyperparameters ranges for Weibull PDF adstock
# facebook_S_alphas = c(0.5, 3)
# facebook_S_gammas = c(0.3, 1)
# facebook_S_shapes = c(0, 10)
# facebook_S_scales = c(0, 0.1)

#### 2a-3: Third, add hyperparameters into robyn_inputs()

InputCollect <- robyn_inputs(InputCollect = InputCollect, hyperparameters = hyperparameters)
print(InputCollect)


################################################################
#### Step 2b: For known model specification, setup in one single step

## Specify hyperparameters as in 2a-2 and optionally calibration as in 2a-4 and provide them directly in robyn_inputs()

# InputCollect <- robyn_inputs(
#   dt_input = dt_simulated_weekly
#   ,dt_holidays = dt_prophet_holidays
#   ,date_var = "DATE"
#   ,dep_var = "revenue"
#   ,dep_var_type = "revenue"
#   ,prophet_vars = c("trend", "season", "holiday")
#   ,prophet_country = "DE"
#   ,context_vars = c("competitor_sales_B", "events")
#   ,paid_media_spends = c("tv_S", "ooh_S",	"print_S", "facebook_S", "search_S")
#   ,paid_media_vars = c("tv_S", "ooh_S", 	"print_S", "facebook_I", "search_clicks_P")
#   ,organic_vars = c("newsletter")
#   ,factor_vars = c("events")
#   ,window_start = "2016-11-23"
#   ,window_end = "2018-08-22"
#   ,adstock = "geometric"
#   ,hyperparameters = hyperparameters # as in 2a-2 above
#   ,calibration_input = calibration_input # as in 2a-4 above
# )

#### Check spend exposure fit if available
if (length(InputCollect$exposure_vars) > 0) {
  lapply(InputCollect$modNLS$plots, plot)
}

##### Manually save and import InputCollect as JSON file
# robyn_write(InputCollect, dir = "~/Desktop")
# InputCollect <- robyn_inputs(
#   dt_input = dt_simulated_weekly,
#   dt_holidays = dt_prophet_holidays,
#   json_file = "~/Desktop/RobynModel-inputs.json")

################################################################
#### Step 3: Build initial model

## Run all trials and iterations. Use ?robyn_run to check parameter definition
OutputModels <- robyn_run(
  InputCollect = InputCollect, # feed in all model specification
  cores = NULL, # NULL defaults to (max available - 1)
  iterations = 3000, # 2000 recommended for the dummy dataset with no calibration
  trials = 5, # 5 recommended for the dummy dataset
  ts_validation = TRUE, # 3-way-split time series for NRMSE validation.
  add_penalty_factor = FALSE # Experimental feature. Use with caution.
)
print(OutputModels)

## Check MOO (multi-objective optimization) convergence plots
# Read more about convergence rules: ?robyn_converge
OutputModels$convergence$moo_distrb_plot
OutputModels$convergence$moo_cloud_plot

## Check time-series validation plot (when ts_validation == TRUE)
# Read more and replicate results: ?ts_validation
if (OutputModels$ts_validation) OutputModels$ts_validation_plot

## Calculate Pareto fronts, cluster and export results and plots. See ?robyn_outputs
OutputCollect <- robyn_outputs(
  InputCollect, OutputModels,
  pareto_fronts = "auto", # automatically pick how many pareto-fronts to fill min_candidates (100)
  # min_candidates = 100, # top pareto models for clustering. Default to 100
  # calibration_constraint = 0.1, # range c(0.01, 0.1) & default at 0.1
  csv_out = "pareto", # "pareto", "all", or NULL (for none)
  clusters = TRUE, # Set to TRUE to cluster similar models by ROAS. See ?robyn_clusters
  export = create_files, # this will create files locally
  plot_folder = robyn_directory, # path for plots exports and files creation
  plot_pareto = create_files # Set to FALSE to deactivate plotting and saving model one-pagers
)
print(OutputCollect)

## 4 csv files are exported into the folder for further usage. Check schema here:
## https://github.com/facebookexperimental/Robyn/blob/main/demo/schema.R
# pareto_hyperparameters.csv, hyperparameters per Pareto output model
# pareto_aggregated.csv, aggregated decomposition per independent variable of all Pareto output
# pareto_media_transform_matrix.csv, all media transformation vectors
# pareto_alldecomp_matrix.csv, all decomposition vectors of independent variables


################################################################
#### Step 4: Select and save the any model

## Compare all model one-pagers and select one that mostly reflects your business reality
print(OutputCollect)
select_model <- "2_420_4" # Pick one of the models from OutputCollect to proceed

#### Version >=3.7.1: JSON export and import (faster and lighter than RDS files)
ExportedModel <- robyn_write(InputCollect, OutputCollect, select_model, export = create_files)
print(ExportedModel)

# To plot any model's one-pager:
myOnePager <- robyn_onepagers(InputCollect, OutputCollect, select_model, export = FALSE)

# To check each of the one-pager's plots
# myOnePager[[select_model]]$patches$plots[[1]]
# myOnePager[[select_model]]$patches$plots[[2]]
# myOnePager[[select_model]]$patches$plots[[3]] # ...

################################################################
#### Step 5: Get budget allocation based on the selected model above

## Budget allocation result requires further validation. Please use this recommendation with caution.
## Don't interpret budget allocation result if selected model above doesn't meet business expectation.

# Check media summary for selected model
print(ExportedModel)

# Run ?robyn_allocator to check parameter definition

# NOTE: The order of constraints should follow:
InputCollect$paid_media_spends

# Example 2: maximize response for latest 10 periods with given spend
AllocatorCollect2 <- robyn_allocator(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  date_range = "last_10", # Last 10 periods, same as c("2018-10-22", "2018-12-31")
  total_budget = 1000000, # Total budget for date_range period simulation
  channel_constr_low = c(0.7, 0.7, 0.7, 0.7, 0.7, 0.7),
  channel_constr_up = c(1.5, 1.5, 1.5, 1.5, 1.5, 1.5),
  channel_constr_multiplier = 5, # Customise bound extension for wider insights
  scenario = "max_response",
  export = create_files
)
print(AllocatorCollect2)
plot(AllocatorCollect2)
