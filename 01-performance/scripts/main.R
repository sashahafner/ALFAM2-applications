# Use application of ALFAM2 model to ALFAM2 interval-level data for performance tests

rm(list = ls())

source('packages.R')
source('functions.R')
source('load_ALFAM2.R')
source('load.R')
source('subset.R')
source('prep.R')
render('predict.Rmd', output_dir = '../logs')
source('plots.R')

devtools::install_github('ecoRoland2/ALFAM2')
library(ALFAM2)
