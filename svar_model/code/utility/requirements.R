pkgLoad <- function( packages = "requirements" ) {
  
  if( length( packages ) == 1L && packages == "requirements" ) {
  
    packages <- c( "dplyr",
                   "tidyr",
                   "readr",
                   "ggplot2",
                   "matrixcalc",
                   "lattice",
                   "cowplot",
                   "lubridate",
                   "xtable",
                   "assertthat",
                   "openxlsx",
                   "mFilter",
                   "Matrix",
                   "svars",
                   "ggfortify",
                   "renv",
                   "dotenv"
                   )
  }
  
  packagecheck <- match( packages, utils::installed.packages()[,1] )
  
  packagestoinstall <- packages[ is.na( packagecheck ) ]
  
  if( length( packagestoinstall ) > 0L ) {
    utils::install.packages( packagestoinstall,
                             repos = "https://cloud.r-project.org"
    )
  } else {
    print( "All requested packages already installed" )
  }
  
  for( package in packages ) {
    suppressPackageStartupMessages(
      library( package, character.only = TRUE, quietly = TRUE )
    )
  }
  
}
pkgLoad("renv")
renv::status()
renv::restore()

