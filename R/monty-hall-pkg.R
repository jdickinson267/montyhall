library(devtools)
has_devel()

setwd( "C:/Users/jessl/Documents" )
getwd()
usethis::create_package( "montyhall" )

getwd()
devtools::document()

setwd( ".." )
getwd()
devtools::install( "montyhall" )