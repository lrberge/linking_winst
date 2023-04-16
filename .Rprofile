#----------------------------------------------#
# Author: Laurent Berge
# Date creation: Mon Jan 09 21:50:34 2023
# ~: this script runs automatically at startup
#    it loads all the necessary functions
#----------------------------------------------#

# NOTA:
# - the very first time the project is opened, 
# there may be errors due to this file that is automatically loaded
# - run this file line by line to ensure everything works fine

####
#### Paths ####
####

# INFO:
# - These options define the location of the SOURCE data sets
# - By default they are located in _DATA/_RAW/, but one can change the location here

# NOTA:
# If you follow the instructions in the PDF regarding the file structure,
# you should not make changes here.

options(LOCATION_OECD = "_DATA/_RAW/OECD")
options(LOCATION_STAT_SE = "_DATA/_RAW/STAT-SE")

####
#### Packages ####
####

# INFO:
# - loads the necessary packages
# - tries to install them if they are not yet installed

# NOTA:
# - some packages may require compilation for installation, so errors may pop in the process
# - two common problems: i) lack of internet connection, ii) lack of some compilation tools
# - if problem: try to install the packages one by one and debug on the go

if(!requireNamespace("data.table", quietly = TRUE)){
  message("The package `data.table` is not installed. Trying to install it with:",
          "\n`install.packages(\"data.table\")`")
  install.packages("data.table")
}

suppressPackageStartupMessages(library(data.table))


if(!requireNamespace("fst", quietly = TRUE)){
  message("The package `fst` is not installed. Trying to install it with:",
          "\n`install.packages(\"fst\")`")
  install.packages("fst")
}

# fstcore is installed with fst
suppressPackageStartupMessages(library(fstcore))
suppressPackageStartupMessages(library(fst))


if(!requireNamespace("fixest", quietly = TRUE)){
  message("The package `fixest` is not installed. Trying to install it with:",
          "\n`install.packages(\"fixest\")`")
  install.packages("fixest")
}

# dreamerr is installed with fixest
library(dreamerr)
suppressPackageStartupMessages(library(fixest))


if(!requireNamespace("Rcpp", quietly = TRUE)){
  message("The package `Rcpp` is not installed. Trying to install it with:",
          "\n`install.packages(\"Rcpp\")`")
  install.packages("Rcpp")
}




####
#### CPP ####
####

# INFO:
# Compiles and loads C++ functions

# NOTA:
# - the very first time the project is loaded, very likely an error will pop
# - please try to run each line one by one and debug on the go

Rcpp::sourceCpp("src/string_dist.cpp", cacheDir = "_CPP_CACHE")
Rcpp::sourceCpp("src/integers.cpp", cacheDir = "_CPP_CACHE")
Rcpp::sourceCpp("src/EM_functions.cpp", cacheDir = "_CPP_CACHE")


####
#### R sources ####
####

# INFO:
# - loads functions defined in the src_* scripts
# - there should be no error here

source("src_utilities.R")
source("src_EM.R")
source("src_algo_steps.R")


####
#### dependency problems ####
####

# check_set_arg is in the dev of dreamerr
check_set_arg = dreamerr::check_arg_plus
check_set_value = dreamerr::check_value_plus



