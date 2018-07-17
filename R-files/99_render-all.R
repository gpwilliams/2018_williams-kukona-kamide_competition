# load packages and helper functions
required_packages <- c("dplyr", 
                       "tidyr", 
                       "lme4", 
                       "broom", 
                       "ggplot2", 
                       "knitr", 
                       "rmarkdown"
                       )

lapply(required_packages, require, character.only = TRUE)
source("01_helper-functions.R")

# load data and render documents
render("02_semantic-results.Rmd", "html_document", output_dir = "../output/")
render("03_visual-results.Rmd", "html_document", output_dir = "../output/")
