
# make packge
library(devtools)
library(roxygen2)
library(sinew)
library(installr)

makeOxyFile(file.path(getwd(), "R"), overwrite = TRUE)

create("fpl.analytics")

document()

rename_r_to_R(recursive = TRUE)

rmarkdown::render("bps_simulation.r", output_format = "all",
                  knit_root_dir = getwd())
