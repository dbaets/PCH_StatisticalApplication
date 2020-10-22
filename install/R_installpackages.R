# install the necessairy packages to deploy shiny application

# R version: R version 4.0.0 (2020-04-24) -- "Arbor Day"


# # run on computer to clone
# packages <- installed.packages()[,"Package"]
# save(packages, file="Rpackages")


# run on newly setup computer
load("Rpackages")

for(p in setdiff(packages, installed.packages()[,"Package"])){
  install.packages(p)
}