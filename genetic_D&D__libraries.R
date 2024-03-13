
# Install packages and load the libraries if they have not already been installed or loaded.
# For each package you will find a comment on what it is used for.


if(!require('readxl')) {       # read_xlsx()
  install.packages('readxl')
  suppressPackageStartupMessages(library('readxl'))
}

#if(!require('openxlsx')) {       # write.xlsx()
#  install.packages('openxlsx')
#  library('openxlsx')
#}

if(!require('adegenet')) {       # df2genind()
  install.packages('adegenet')
  suppressPackageStartupMessages(library('adegenet'))
}

if(!require('dplyr')) {       # %>% select() # be careful! An update could cause problems with the code.
  install.packages('dplyr')
  suppressPackageStartupMessages(library('dplyr'))
}

if(!require('tidyr')) {       # %>% unite()
  install.packages('tidyr')
  suppressPackageStartupMessages(library('tidyr'))
}

if(!require('poppr')) {       # poppr.amova()
  install.packages('poppr')
  suppressPackageStartupMessages(library('poppr'))
}

if(!require('permute')) {       # rarefy(), rarecurve(), rareslope(), is a precondition for vegan, needed for diversity indices ## was used to display results for me but not for public
 install.packages('permute')
 suppressPackageStartupMessages(library('permute'))
}
 
if(!require('lattice')) {       # rarefy(), rarecurve(), rareslope(), is a precondition for vegan ## was used to display results for me but not for public
  install.packages('lattice')
  suppressPackageStartupMessages(library('lattice'))
}

if(!require('vegan')) {       # rarefy(), rarecurve(), rareslope() ## was used to display results for me but not for public
  install.packages('vegan')
  suppressPackageStartupMessages(library('vegan'))
}

if(!require('ggplot2')) {       # ggplot() -> to display the confidence intervals (rare... calculations)
  install.packages('ggplot2')
  suppressPackageStartupMessages(library('ggplot2'))
}

# if(!require('viridis')) {       # viridis() is a color palette used for rarecurve(): rarecurve() is not part of the Script anymore
#   install.packages('viridis')
#   suppressPackageStartupMessages(library('viridis'))
# }

if(!require('car')) {       # ncvTest() , durbinWatsonTest()
  install.packages('car')
  suppressPackageStartupMessages(library('car'))
}

if(!require('gvlma')) {       # gvlma()
  install.packages('gvlma')
  suppressPackageStartupMessages(library('gvlma'))
}

if(!require('corrplot')) {       # corrplot()
  install.packages('corrplot')
  suppressPackageStartupMessages(library('corrplot'))
}

if(!require('RLDNe')) {       # enables using NeEstimatorV2.1 (Do et al. 2014) in R 
  install.packages("devtools")   # you need to install devtools first, before you can use this installation link to NeEstimator
  suppressPackageStartupMessages(library('devtools'))
  devtools::install_github(repo="zakrobinson/RLDNe")
  suppressPackageStartupMessages(library('RLDNe'))
}
