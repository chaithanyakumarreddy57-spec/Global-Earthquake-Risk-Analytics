# =====================================================================
# Global Earthquake Risk Analytics - full analysis pipeline
# Run this file top to bottom BEFORE launching app.R
# =====================================================================

# ---------------------------------------------------------------------
# PHASE 1 - PROJECT SETUP
# ---------------------------------------------------------------------

pkgs <- c("tidyverse", "lubridate", "janitor", "skimr", "corrplot",
          "leaflet", "htmlwidgets", "maps", "caret", "rpart", "rpart.plot",
          "randomForest", "pROC", "cluster", "factoextra", "scales")
new <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(new)) install.packages(new, repos = "https://cloud.r-project.org")
invisible(lapply(pkgs, library, character.only = TRUE))

set.seed(42)

# Folder structure
dirs <- c("data/raw", "data/processed", "outputs/plots", "outputs/maps",
          "outputs/tables", "models", "report")
invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

# Locate and extract archive.zip (skipped if database.csv is already there)
csv_path <- "data/raw/database.csv"
if (!file.exists(csv_path)) {
  zip_candidates <- c("archive.zip", "~/Downloads/archive.zip", "/content/archive.zip")
  zip_path <- zip_candidates[file.exists(zip_candidates)][1]
  if (is.na(zip_path)) {
    stop("archive.zip not found. Put it in the working directory or ~/Downloads, ",
         "or place database.csv in data/raw/.")
  }
  unzip(zip_path, exdir = "data/raw")
}

# Load database.csv.
# Date and Time are forced to character: a few rows hold ISO timestamps
# ("1975-02-23T02:58:41.000Z") in both columns, and letting readr guess a
# time type would silently turn those into NA.
eq_raw <- read_csv(csv_path,
                   col_types = cols(Date = col_character(), Time = col_character()),
                   show_col_types = FALSE)
problems(eq_raw)
dim(eq_raw)
head(eq_raw)


# ---------------------------------------------------------------------
# PHASE 2 - DATA UNDERSTANDING
# ---------------------------------------------------------------------
head(eq_raw); tail(eq_raw)
cat("Rows:", nrow(eq_raw), " Columns:", ncol(eq_raw), "\n")
names(eq_raw)
glimpse(eq_raw)
summary(eq_raw)
skim(eq_raw)

num_vars <- names(eq_raw)[sapply(eq_raw, is.numeric)]
cat_vars <- names(eq_raw)[!sapply(eq_raw, is.numeric)]
cat("Numerical:", paste(num_vars, collapse = ", "), "\n")
cat("Categorical:", paste(cat_vars, collapse = ", "), "\n")



# ---------------------------------------------------------------------
# PHASE 3 - DATA CLEANING
# ---------------------------------------------------------------------
# 19. Rename columns first (snake_case) so later code is easy to read
eq <- eq_raw %>% clean_names()
names(eq)
# e.g. depth_error, magnitude_type, root_mean_square, ...

# 12. Missing values
missing_tbl <- tibble(column = names(eq),
                      n_missing = colSums(is.na(eq)),
                      pct_missing = round(100 * colSums(is.na(eq)) / nrow(eq), 2)) %>%
  arrange(desc(pct_missing))
print(missing_tbl)
write_csv(missing_tbl, "outputs/tables/missing_values.csv")

# 13-14. Duplicates
cat("Duplicate rows:", sum(duplicated(eq)), "\n")
cat("Duplicate IDs :", sum(duplicated(eq$id)), "\n")
eq <- eq %>% distinct()

# 15. Handle missing values
# Columns with >60% missing are dropped (they are quality metadata, not needed).
# The core columns (date, time, lat, lon, depth, magnitude) have no NAs.
high_missing <- missing_tbl %>% filter(pct_missing > 60) %>% pull(column)
eq <- eq %>% select(-all_of(high_missing))
eq$magnitude_type[is.na(eq$magnitude_type)] <- "Unknown"
# azimuthal_gap, root_mean_square, depth_seismic_stations: median impute
for (col in intersect(c("azimuthal_gap", "root_mean_square", "depth_seismic_stations"), names(eq))) {
  eq[[col]][is.na(eq[[col]])] <- median(eq[[col]], na.rm = TRUE)
}

# 16. Incorrect / inconsistent values
# - 'type' contains explosions and rock bursts: not natural earthquakes.
table(eq$type)
eq <- eq %>% filter(type == "Earthquake")
# - negative depth (min is -1.1): treat as 0 (surface)
eq <- eq %>% mutate(depth = pmax(depth, 0))
# - valid coordinate range
eq <- eq %>% filter(between(latitude, -90, 90), between(longitude, -180, 180))

# 18. Convert date/time.
# Most rows are "MM/DD/YYYY" + "HH:MM:SS", but 3 rows hold ISO timestamps
# ("1975-02-23T02:58:41.000Z") in BOTH columns, so parse them separately.
iso <- str_detect(eq$date, "T")
eq$datetime <- as.POSIXct(NA, tz = "UTC")
eq$datetime[iso]  <- ymd_hms(eq$date[iso], tz = "UTC")
eq$datetime[!iso] <- mdy_hms(paste(eq$date[!iso], eq$time[!iso]), tz = "UTC")
cat("Unparsed datetimes:", sum(is.na(eq$datetime)), "\n")
eq <- eq %>% filter(!is.na(datetime))

# 17. Outliers (IQR method, reported not removed: large quakes are real events)
iqr_outliers <- function(x) {
  q <- quantile(x, c(.25, .75), na.rm = TRUE); i <- diff(q)
  sum(x < q[1] - 1.5 * i | x > q[2] + 1.5 * i, na.rm = TRUE)
}
outlier_tbl <- tibble(variable = c("magnitude", "depth"),
                      n_outliers = c(iqr_outliers(eq$magnitude), iqr_outliers(eq$depth)))
print(outlier_tbl)


# ---------------------------------------------------------------------
# PHASE 4 - DATA PREPROCESSING
# ---------------------------------------------------------------------
eq <- eq %>%
  mutate(
    year  = year(datetime),
    month = month(datetime),
    month_name = month(datetime, label = TRUE, abbr = TRUE),
    day   = day(datetime),
    hour  = hour(datetime),
    magnitude_category = cut(magnitude, c(-Inf, 6, 7, 8, Inf), right = FALSE,
                             labels = c("Moderate (5.5-5.9)", "Strong (6.0-6.9)",
                                        "Major (7.0-7.9)", "Great (8.0+)")),
    depth_category = cut(depth, c(-Inf, 70, 300, Inf), right = FALSE,
                         labels = c("Shallow (<70km)", "Intermediate (70-300km)", "Deep (>300km)")),
    log_depth = log1p(depth)
  )

# 21. Encode categorical variables
eq <- eq %>%
  mutate(
    # rare types (MD, MH, MWR, ML...) are lumped so train/test splits never hit unseen levels
    magnitude_type = fct_lump_min(factor(magnitude_type), min = 100, other_level = "Other"),
    status = factor(status),
    magnitude_category_code = as.integer(magnitude_category),
    depth_category_code = as.integer(depth_category),
    status_code = as.integer(status == "Reviewed"),
    magnitude_type_code = as.integer(magnitude_type)
  )

# 22. Scale numeric variables (z-score) - used for clustering/logistic regression
scale_cols <- c("latitude", "longitude", "depth", "magnitude")
scaled <- as.data.frame(scale(eq[, scale_cols]))
names(scaled) <- paste0(scale_cols, "_scaled")
eq <- bind_cols(eq, scaled)

# 23-24. Final cleaned dataset
# (the final dataset, including risk and cluster columns, is saved in Phase 12)
cat("Clean dataset:", nrow(eq), "rows x", ncol(eq), "cols\n")

save_plot <- function(p, name, w = 8, h = 5) {
  ggsave(file.path("outputs/plots", paste0(name, ".png")), p, width = w, height = h, dpi = 150)
  print(p)
}
theme_set(theme_minimal(base_size = 12))


# ---------------------------------------------------------------------
# PHASE 5 - EXPLORATORY DATA ANALYSIS
# ---------------------------------------------------------------------
save_plot(ggplot(eq, aes(magnitude)) +geom_histogram(binwidth = 0.1, fill = "firebrick", colour = "white") +labs(title = "Magnitude distribution", x = "Magnitude", y = "Count"),"25_magnitude_distribution")

save_plot(ggplot(eq, aes(depth)) +
            geom_histogram(bins = 60, fill = "steelblue", colour = "white") +
            labs(title = "Depth distribution", x = "Depth (km)", y = "Count"),
          "26_depth_distribution")

save_plot(eq %>% count(year) %>%
            ggplot(aes(year, n)) + geom_line(colour = "firebrick") + geom_point(size = 1) +
            labs(title = "Earthquakes per year", x = "Year", y = "Count"),
          "27_earthquakes_by_year")

save_plot(eq %>% count(month_name) %>%
            ggplot(aes(month_name, n)) + geom_col(fill = "darkorange") +
            labs(title = "Earthquakes by month", x = "Month", y = "Count"),
          "28_earthquakes_by_month")

save_plot(eq %>% count(hour) %>%
            ggplot(aes(hour, n)) + geom_col(fill = "seagreen") +
            labs(title = "Earthquakes by hour (UTC)", x = "Hour", y = "Count"),
          "29_earthquakes_by_hour")

save_plot(eq %>% count(magnitude_category) %>%
            ggplot(aes(magnitude_category, n, fill = magnitude_category)) +
            geom_col(show.legend = FALSE) +
            labs(title = "Earthquakes by magnitude category", x = NULL, y = "Count"),
          "30_by_magnitude_category")

save_plot(ggplot(eq, aes(depth, magnitude)) +
            geom_point(alpha = 0.25, colour = "purple") + geom_smooth(method = "lm", colour = "black") +
            labs(title = "Magnitude vs depth", x = "Depth (km)", y = "Magnitude"),
          "31_magnitude_vs_depth")

save_plot(ggplot(eq, aes(latitude, magnitude)) +
            geom_point(alpha = 0.25, colour = "navy") + geom_smooth(colour = "red") +
            labs(title = "Latitude vs magnitude"), "32_latitude_vs_magnitude")

save_plot(ggplot(eq, aes(longitude, magnitude)) +
            geom_point(alpha = 0.25, colour = "darkgreen") + geom_smooth(colour = "red") +
            labs(title = "Longitude vs magnitude"), "33_longitude_vs_magnitude")

# 34. Correlation
cor_mat <- cor(eq %>% select(latitude, longitude, depth, magnitude, year, month, hour),
               use = "complete.obs")
png("outputs/plots/34_correlation.png", width = 800, height = 800, res = 120)
corrplot(cor_mat, method = "color", type = "upper", addCoef.col = "black", tl.col = "black")
dev.off()
corrplot(cor_mat, method = "color", type = "upper", addCoef.col = "black", tl.col = "black")

# 35. Patterns summary
patterns <- list(
  busiest_year   = eq %>% count(year, sort = TRUE) %>% slice(1),
  busiest_month  = eq %>% count(month_name, sort = TRUE) %>% slice(1),
  share_shallow  = mean(eq$depth < 70),
  share_major    = mean(eq$magnitude >= 7)
)
print(patterns)


# ---------------------------------------------------------------------
# PHASE 6 - GEOGRAPHIC ANALYSIS
# ---------------------------------------------------------------------
world <- map_data("world")

p36 <- ggplot() +
  geom_polygon(data = world, aes(long, lat, group = group), fill = "grey90", colour = "white", linewidth = 0.1) +
  geom_point(data = eq, aes(longitude, latitude), size = 0.4, alpha = 0.4, colour = "red") +
  coord_fixed(1.3) + labs(title = "Global earthquake locations", x = NULL, y = NULL)
save_plot(p36, "36_global_locations", 11, 6)

p37 <- ggplot() +
  geom_polygon(data = world, aes(long, lat, group = group), fill = "grey90", colour = "white", linewidth = 0.1) +
  geom_point(data = eq, aes(longitude, latitude, colour = magnitude, size = magnitude), alpha = 0.5) +
  scale_colour_viridis_c(option = "inferno", direction = -1) + scale_size(range = c(0.3, 4)) +
  coord_fixed(1.3) + labs(title = "Earthquakes by magnitude", x = NULL, y = NULL)
save_plot(p37, "37_magnitude_map", 11, 6)

# 38-39. Earthquake-prone regions: 10x10 degree grid cells
grid_tbl <- eq %>%
  mutate(lat_bin = floor(latitude / 10) * 10, lon_bin = floor(longitude / 10) * 10) %>%
  group_by(lat_bin, lon_bin) %>%
  summarise(n = n(), mean_mag = mean(magnitude), max_mag = max(magnitude), .groups = "drop") %>%
  arrange(desc(n))
write_csv(grid_tbl, "outputs/tables/grid_cells.csv")
print(head(grid_tbl, 10))

save_plot(ggplot(grid_tbl, aes(lon_bin + 5, lat_bin + 5, fill = n)) +
            geom_tile(width = 10, height = 10) + scale_fill_viridis_c(option = "magma", direction = -1) +
            coord_fixed() + labs(title = "Earthquake density (10 x 10 degree cells)", x = "Longitude", y = "Latitude"),
          "38_density_grid", 10, 5)

save_plot(ggplot(eq, aes(latitude)) + geom_density(fill = "tomato", alpha = 0.6) +
            labs(title = "Latitude density"), "39a_latitude_density")
save_plot(ggplot(eq, aes(longitude)) + geom_density(fill = "skyblue", alpha = 0.6) +
            labs(title = "Longitude density"), "39b_longitude_density")

# 40. Interactive map (sample of the larger events keeps the file light)
pal <- colorNumeric("YlOrRd", eq$magnitude)
map_data_sub <- eq %>% filter(magnitude >= 6)
interactive_map <- leaflet(map_data_sub) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(~longitude, ~latitude, radius = ~(magnitude - 4) * 1.5,
                   color = ~pal(magnitude), fillOpacity = 0.6, stroke = FALSE,
                   popup = ~paste0("<b>Magnitude:</b> ", magnitude,
                                   "<br><b>Depth:</b> ", depth, " km",
                                   "<br><b>Date:</b> ", as.Date(datetime))) %>%
  addLegend("bottomright", pal = pal, values = ~magnitude, title = "Magnitude")
tryCatch(
  saveWidget(interactive_map, "outputs/maps/earthquake_map.html", selfcontained = TRUE),
  error = function(e) {
    message("pandoc not available - saving map with external files instead.")
    saveWidget(interactive_map, "outputs/maps/earthquake_map.html", selfcontained = FALSE)
  })
interactive_map


# ---------------------------------------------------------------------
# PHASE 7 - RISK ANALYSIS
# ---------------------------------------------------------------------
# 41-42. Risk score (0-100): 60% magnitude energy + 40% shallowness.
# Shallow quakes cause more surface damage, so shallow depth raises risk.
mag_norm   <- (eq$magnitude - 5.5) / (max(eq$magnitude) - 5.5)
depth_norm <- 1 - pmin(eq$depth, 300) / 300
eq$risk_score <- round(100 * (0.6 * mag_norm + 0.4 * depth_norm), 2)

# 43. Risk classes
eq$risk_level <- cut(eq$risk_score, c(-Inf, 30, 42, 55, Inf),
                     labels = c("Low", "Moderate", "High", "Severe"))
print(table(eq$risk_level))

save_plot(eq %>% count(risk_level) %>%
            ggplot(aes(risk_level, n, fill = risk_level)) + geom_col(show.legend = FALSE) +
            scale_fill_manual(values = c(Low = "#4CAF50", Moderate = "#FFC107", High = "#FF7043", Severe = "#B71C1C")) +
            labs(title = "Risk level distribution", x = NULL, y = "Count"),
          "43_risk_distribution")

# 44. Risk by region (10-degree cells; a simple region label is added)
eq <- eq %>%
  mutate(region = case_when(
    longitude >= 90 & longitude <= 180 & latitude >= -15 & latitude <= 60 ~ "Western Pacific / Asia",
    longitude >= -180 & longitude < -100 & latitude > 10 ~ "North America / N. Pacific",
    longitude >= -100 & longitude < -30 & latitude >= -60 & latitude <= 15 ~ "Central & South America",
    longitude >= -30 & longitude < 90 & latitude >= 0 ~ "Europe / Middle East / Central Asia",
    longitude >= -30 & longitude < 90 & latitude < 0 ~ "Africa / Indian Ocean",
    latitude < -15 & (longitude >= 150 | longitude <= -150) ~ "Oceania / South Pacific",
    TRUE ~ "Other / Ocean ridges"))
region_risk <- eq %>% group_by(region) %>%
  summarise(n = n(), mean_risk = mean(risk_score),
            pct_high_severe = mean(risk_level %in% c("High", "Severe")) * 100) %>%
  arrange(desc(mean_risk))
print(region_risk)
write_csv(region_risk, "outputs/tables/risk_by_region.csv")
save_plot(ggplot(region_risk, aes(reorder(region, mean_risk), mean_risk)) +
            geom_col(fill = "firebrick") + coord_flip() +
            labs(title = "Mean risk score by region", x = NULL, y = "Mean risk score"),
          "44_risk_by_region")

# 45. Risk over time
risk_year <- eq %>% group_by(year) %>%
  summarise(mean_risk = mean(risk_score), n_high_severe = sum(risk_level %in% c("High", "Severe")))
save_plot(ggplot(risk_year, aes(year, mean_risk)) + geom_line(colour = "firebrick") + geom_smooth() +
            labs(title = "Mean risk score over time"), "45_risk_over_time")

# 46. High-risk geographic areas
high_risk_areas <- eq %>% filter(risk_level %in% c("High", "Severe")) %>%
  mutate(lat_bin = floor(latitude / 10) * 10, lon_bin = floor(longitude / 10) * 10) %>%
  count(lat_bin, lon_bin, sort = TRUE)
write_csv(high_risk_areas, "outputs/tables/high_risk_areas.csv")
print(head(high_risk_areas, 10))


# ---------------------------------------------------------------------
# PHASE 8A - UNSUPERVISED LEARNING (K-MEANS)
# ---------------------------------------------------------------------
# 47. Convert lat/lon to 3D unit-sphere coordinates so clusters respect the
# globe (longitude wraps at +/-180), plus scaled depth and magnitude.
rad <- pi / 180
clust_df <- tibble(
  x = cos(eq$latitude * rad) * cos(eq$longitude * rad),
  y = cos(eq$latitude * rad) * sin(eq$longitude * rad),
  z = sin(eq$latitude * rad),
  depth_s = as.numeric(scale(eq$log_depth)),
  mag_s = as.numeric(scale(eq$magnitude)))
clust_mat <- as.matrix(clust_df)

# 49. Number of clusters: elbow + silhouette on a sample for speed
samp <- clust_mat[sample(nrow(clust_mat), min(4000, nrow(clust_mat))), ]
p_elbow <- fviz_nbclust(samp, kmeans, method = "wss", k.max = 10) + labs(title = "Elbow method")
p_sil   <- fviz_nbclust(samp, kmeans, method = "silhouette", k.max = 10) + labs(title = "Silhouette method")
save_plot(p_elbow, "49_elbow"); save_plot(p_sil, "49_silhouette")

# 48. K-Means (k chosen from the plots above; 6 is a sensible default, adjust if needed)
K <- 6
km <- kmeans(clust_mat, centers = K, nstart = 25)
eq$cluster <- factor(km$cluster)

# 50. Visualise
save_plot(ggplot() +
            geom_polygon(data = world, aes(long, lat, group = group), fill = "grey92", colour = "white", linewidth = 0.1) +
            geom_point(data = eq, aes(longitude, latitude, colour = cluster), size = 0.6, alpha = 0.7) +
            coord_fixed(1.3) + labs(title = paste("K-Means clusters (k =", K, ")"), x = NULL, y = NULL),
          "50_kmeans_map", 11, 6)

# 51. Interpret
cluster_profile <- eq %>% group_by(cluster) %>%
  summarise(n = n(), lat = mean(latitude), lon = mean(longitude),
            mean_mag = mean(magnitude), mean_depth = mean(depth), mean_risk = mean(risk_score))
print(cluster_profile)
write_csv(cluster_profile, "outputs/tables/cluster_profile.csv")



# ---------------------------------------------------------------------
# PHASE 8B - SUPERVISED LEARNING
# ---------------------------------------------------------------------

LEAKY <- FALSE

# 52. Features and binary target
model_df <- eq %>%
  transmute(high_risk = factor(ifelse(risk_level %in% c("High", "Severe"), "High", "NotHigh"),
                               levels = c("High", "NotHigh")),
            latitude, longitude, year, month, hour,
            status_code, magnitude_type = magnitude_type,
            magnitude, depth)
if (!LEAKY) model_df <- model_df %>% select(-magnitude, -depth)
table(model_df$high_risk)

# 53. Train/test split (80/20, stratified)
idx <- as.vector(createDataPartition(model_df$high_risk, p = 0.8, list = FALSE))
train <- model_df[idx, ]; test <- model_df[-idx, ]

# 54-55. Train and predict
glm_fit <- glm(high_risk ~ ., data = train, family = binomial)
glm_prob <- predict(glm_fit, test, type = "response")   # P(second level = NotHigh)
glm_prob_high <- 1 - glm_prob
glm_pred <- factor(ifelse(glm_prob_high >= 0.5, "High", "NotHigh"), levels = c("High", "NotHigh"))

tree_fit <- rpart(high_risk ~ ., data = train, method = "class", cp = 0.005)
tree_prob <- predict(tree_fit, test, type = "prob")[, "High"]
tree_pred <- predict(tree_fit, test, type = "class")
png("outputs/plots/54_decision_tree.png", width = 1000, height = 700, res = 120)
rpart.plot(tree_fit, main = "Decision tree"); dev.off()

rf_fit <- randomForest(high_risk ~ ., data = train, ntree = 300, importance = TRUE)
rf_prob <- predict(rf_fit, test, type = "prob")[, "High"]
rf_pred <- predict(rf_fit, test)



# ---------------------------------------------------------------------
# PHASE 9 - MODEL EVALUATION
# ---------------------------------------------------------------------
evaluate <- function(pred, prob, name) {
  cm <- confusionMatrix(pred, test$high_risk, positive = "High")
  roc_obj <- roc(test$high_risk, prob, levels = c("NotHigh", "High"), direction = "<", quiet = TRUE)
  list(row = tibble(model = name,
                    accuracy = unname(cm$overall["Accuracy"]),
                    precision = unname(cm$byClass["Precision"]),
                    recall = unname(cm$byClass["Recall"]),
                    f1 = unname(cm$byClass["F1"]),
                    auc = as.numeric(auc(roc_obj))),
       cm = cm, roc = roc_obj)
}
res <- list(evaluate(glm_pred, glm_prob_high, "Logistic Regression"),
            evaluate(tree_pred, tree_prob, "Decision Tree"),
            evaluate(rf_pred, rf_prob, "Random Forest"))

for (r in res) { cat("\n====", r$row$model, "====\n"); print(r$cm$table) }  
comparison <- bind_rows(lapply(res, `[[`, "row"))                              
print(comparison)
write_csv(comparison, "outputs/tables/model_comparison.csv")

# 62. ROC curves
png("outputs/plots/62_roc_curves.png", width = 900, height = 700, res = 120)
plot(res[[1]]$roc, col = "blue", main = "ROC curves")
plot(res[[2]]$roc, col = "darkgreen", add = TRUE)
plot(res[[3]]$roc, col = "red", add = TRUE)
legend("bottomright", c("Logistic", "Tree", "Random Forest"), col = c("blue", "darkgreen", "red"), lwd = 2)
dev.off()

# 64. Best model by F1 (ties broken by AUC)
best_name <- comparison %>% arrange(desc(f1), desc(auc)) %>% slice(1) %>% pull(model)
cat("Best model:", best_name, "\n")



# ---------------------------------------------------------------------
# PHASE 10 - ADVANCED ANALYSIS
# ---------------------------------------------------------------------
# 65-66. Feature importance (random forest)
imp <- as.data.frame(importance(rf_fit)) %>%
  rownames_to_column("feature") %>% arrange(desc(MeanDecreaseGini))
print(imp)
write_csv(imp, "outputs/tables/feature_importance.csv")
save_plot(ggplot(imp, aes(reorder(feature, MeanDecreaseGini), MeanDecreaseGini)) +
            geom_col(fill = "steelblue") + coord_flip() +
            labs(title = "Random forest feature importance", x = NULL),
          "65_feature_importance")

# 67. Strongest earthquakes
top20 <- eq %>% arrange(desc(magnitude)) %>%
  select(datetime, latitude, longitude, magnitude, depth, risk_level) %>% head(20)
print(top20); write_csv(top20, "outputs/tables/top20_strongest.csv")

# 68. Frequency trends: linear trend in yearly counts
yearly <- eq %>% count(year)
trend_fit <- lm(n ~ year, data = yearly)
print(summary(trend_fit))

# 69. Statistical tests
# (a) Does mean magnitude differ across depth categories? Kruskal-Wallis (non-normal data)
print(kruskal.test(magnitude ~ depth_category, data = eq))
# (b) Magnitude vs depth correlation (Spearman)
print(cor.test(eq$magnitude, eq$depth, method = "spearman", exact = FALSE))
# (c) Is the number of quakes uniform across months? Chi-square (month lengths ignored)
print(chisq.test(table(eq$month)))
# (d) Is risk level related to depth category? Chi-square
print(chisq.test(table(eq$risk_level, eq$depth_category)))



# ---------------------------------------------------------------------
# PHASE 12 - FINAL OUTPUTS
# ---------------------------------------------------------------------
write_csv(eq, "data/processed/earthquakes_clean.csv")
saveRDS(eq, "data/processed/earthquakes_clean.rds")

best_model <- switch(best_name, "Logistic Regression" = glm_fit,
                     "Decision Tree" = tree_fit, "Random Forest" = rf_fit)
saveRDS(best_model, "models/best_model.rds")
saveRDS(rf_fit, "models/random_forest.rds")
saveRDS(tree_fit, "models/decision_tree.rds")
saveRDS(glm_fit, "models/logistic_regression.rds")
saveRDS(list(comparison = comparison, importance = imp, best = best_name,
             leaky = LEAKY, features = setdiff(names(train), "high_risk")),
        "models/model_info.rds")

cat("\nDone. Files are in data/processed, outputs/, and models/.\n")
