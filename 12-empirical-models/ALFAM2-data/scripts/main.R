# Download latest ALFAM2 data from a specific release or commit and save copy

rm(list = ls())

# Set release tag for download
ghpath <- 'https://github.com/sashahafner/ALFAM2-data/raw/'
rtag <- 'v2.59'

## Alternative below to get particular commit (if not yet in release)
## Bad practice because it is difficult to refer to and find a commit--avoid this in the released analysis!
#ghpath <- 'https://github.com/sashahafner/ALFAM2-data/raw/d4672e2efb3748dc829561a42086e5e4ec8ae41b/'
#rtag <- ''

source('packages.R')
source('load.R')
source('export.R')
