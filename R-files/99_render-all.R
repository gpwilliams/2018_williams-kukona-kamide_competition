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
  here("R-files", "01_semantic-results.Rmd"),
  here("R-files", "02_visual-results.Rmd"),
  here("R-files", "03_additional-analyses.Rmd")
)

for (i in seq_along(filenames)) {
  render(
    filenames[i], 
    c("html_document", "github_document"), 
    output_dir = here("output")
  )
}