# Download latest ALFAM2 data from a specific release or commit and save copy

rm(list = ls())

# Set release tag for download
ghpath <- 'https://github.com/sashahafner/ALFAM2-data/raw/'
rtag <- 'v2.23'

## Alternative below to get particular commit (if not yet in release)
#ghpath <- 'https://github.com/sashahafner/ALFAM2-data/raw/9291c6fff58463e042e943559c129cb6d5a1248e/'
#rtag <- ''

source('packages.R')
source('load.R')
source('export.R')
