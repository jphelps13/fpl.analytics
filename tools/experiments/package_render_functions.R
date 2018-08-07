
# make packge
library(devtools)
library(roxygen2)
library(sinew)
library(rmarkdown)
library(installr)

makeOxyFile(file.path(getwd(), "R"), overwrite = TRUE)

create("fpl.analytics")

devtools::use_vignette()

document()

rename_r_to_R(recursive = TRUE)

rmarkdown::render(file.path(getwd(), "vignettes/bps_tutorial.R"), 
                  output_format = "all")
