# load packages and helper functions
required_packages <- c(
  "here",
  "lme4",
  "multcomp",
  "dplyr",
  "tidyr",
  "stringr",
  "broom",
  "ggplot2",
  "knitr",
  "rmarkdown"
)

lapply(required_packages, library, character.only = TRUE)
source(here("R-files", "00_helper-functions.R"))

# render documents
filenames <- c(
  "01_semantic-results.Rmd",
  "02_visual-results.Rmd",
  "03_additional-analyses.Rmd"
)

for (i in seq_along(filenames)) {
  render(filenames[i], output_dir = here("output"))
}