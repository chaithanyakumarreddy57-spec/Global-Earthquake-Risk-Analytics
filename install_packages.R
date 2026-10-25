# Install every R package used by this project (run once).
#   source("install_packages.R")
# Tested with R 4.3.x. Newer R versions should also work.

cran_pkgs <- c(
  # analysis (earthquake_analysis.R, make_extra_figures.R)
  "tidyverse", "lubridate", "janitor", "skimr", "corrplot", "leaflet",
  "htmlwidgets", "maps", "caret", "rpart", "rpart.plot", "randomForest",
  "pROC", "cluster", "factoextra", "scales",
  # dashboard (app.R)
  "shiny", "bslib", "plotly", "DT",
  # report (report/earthquake_report.Rmd)
  "rmarkdown", "knitr", "kableExtra",
  # deployment (deploy_shinyapps.R)
  "rsconnect"
)

missing <- cran_pkgs[!vapply(cran_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  message("Installing: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = "https://cloud.r-project.org")
} else {
  message("All packages are already installed.")
}
