# Install packages and load the libraries if they have not already been installed or loaded.
# For each package you will find a comment on what it is used for.

if(!require('readxl')) {       # read_xlsx()
  install.packages('readxl')
}
if(!require('adegenet')) {       # df2genind()
  install.packages('adegenet')
}
if(!require('dplyr')) {       # %>% select() # be careful! An update could cause problems with the code.
  install.packages('dplyr')
}
if(!require('tidyr')) {       # %>% unite()
  install.packages('tidyr')
}
if(!require('poppr')) {       # poppr.amova(), poppr()
  install.packages('poppr')
}
if(!require('permute')) {       # used to use vegan
 install.packages('permute')
}
if(!require('lattice')) {       # used to use vegan
  install.packages('lattice')
}
if(!require('vegan')) {       # taxondive(), taxa2dist()
  install.packages('vegan')
}
if(!require('corrplot')) {       # corrplot()
  install.packages('corrplot')
}
if(!require('RLDNe')) {       # enables using NeEstimatorV2.1 (Do et al. 2014) in R 
  install.packages("devtools")   # you need to install devtools first, before you can use this installation link to NeEstimator
}