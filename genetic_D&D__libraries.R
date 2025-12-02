# Install packages and load the libraries if they have not already been installed or loaded.
# For each package you will find a comment on what it is used for.

if(!require('readxl')) {       # read_xlsx() , load .xlsx-files
  install.packages('readxl')
}
if(!require('adegenet')) {       # df2genind() , enables working with genetic data
  install.packages('adegenet')
}
if(!require('dplyr')) {       # %>% select() , simplifies working with tables
  install.packages('dplyr')
}
if(!require('tidyr')) {       # %>% unite() , simplifies working with tables
  install.packages('tidyr')
}
if(!require('poppr')) {       # poppr.amova(), poppr() , calculate AMOVA and lambda
  install.packages('poppr')
}
if(!require('permute')) {       # used to use vegan
 install.packages('permute')
}
if(!require('lattice')) {       # used to use vegan
  install.packages('lattice')
}
if(!require('vegan')) {       # taxondive(), taxa2dist() , calculate AvTD
  install.packages('vegan')
}
if(!require('corrplot')) {       # corrplot() , display correlations
  install.packages('corrplot')
}
if(!require('RLDNe')) {       # enables using NeEstimatorV2.1 (Do et al. 2014) in R & calculate effective population sizes
  install.packages("devtools")   # install devtools to use the installation link to NeEstimator
  devtools::install_github(repo="zakrobinson/RLDNe")
}
if(!require('ggplot2')) {
  install.packages('ggplot2')  # used in KBA script to display data
}
if(!require('hierfstat')) {       # calculate Dest and allelic richness
  install.packages('hierfstat')
}
