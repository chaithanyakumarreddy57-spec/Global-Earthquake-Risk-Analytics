# Deploy the dashboard to shinyapps.io (gives a public HTTPS link).
#
# One-time setup (do NOT commit your token or secret):
#   1. Create a free account at https://www.shinyapps.io
#   2. In the shinyapps.io dashboard: Account > Tokens > Show > Show Secret
#   3. Run this once in the R console (paste your own values; they are stored
#      in your user profile, not in this project):
#        rsconnect::setAccountInfo(name = "<account>", token = "<token>", secret = "<secret>")
#   4. Run:  source("deploy_shinyapps.R")
#
# Only the files the dashboard needs are uploaded.

stopifnot(requireNamespace("rsconnect", quietly = TRUE))

app_files <- c(
  "app.R",
  "data/processed/earthquakes_clean.rds",
  "outputs/tables/model_comparison.csv",
  "outputs/tables/feature_importance.csv"
)
missing <- app_files[!file.exists(app_files)]
if (length(missing)) stop("Missing files: ", paste(missing, collapse = ", "),
                          ". Run earthquake_analysis.R first.")

rsconnect::deployApp(
  appDir      = ".",
  appFiles    = app_files,
  appName     = "global-earthquake-risk-analytics",
  appTitle    = "Global Earthquake Risk Analytics",
  forceUpdate = TRUE
)
# The public URL is printed at the end, e.g.
#   https://<account>.shinyapps.io/global-earthquake-risk-analytics/
# Paste it into the README only after you have opened it and confirmed it works.
