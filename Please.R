library(terra)
library(dplyr)
library(ggplot2)
library(lmtest)
library(sandwich)
library(quantreg)


#folder_path <- "Z:\\awiebler/FinalProject/South_Africa_Land_Cover (1)/"

#take all .tif files from the land use folder
#tif_files <- list.files(path = folder_path,
#                        pattern = "\\.tif$",
#                        full.names = TRUE)

#Resampling all rasters in stack to match #1
#raster_list <- lapply(tif_files, rast)
#template <- raster_list[[1]]
#raster_list_resampled <- lapply(raster_list, function(x) {
#  if(!identical(ext(x), ext(template)) | !identical(res(x), res(template))) {
#  resample(x, template, method = "near")
#} else {
#    x
#  }
#})

#create landuse stack
#landuse_stack <- rast(raster_list_resampled)

#save progress
#writeRaster(landuse_stack,
#            filename = "landuse_stack_2015_19.tif",
#            overwrite = TRUE)

#reproject so that area calculations will be accurate
landuse_stack <- rast("landuse_stack_2015_19.tif")
landuse_stack_projected <- project(landuse_stack,
                                   "EPSG:102022",
                                   method = "near")

#save progress
#writeRaster(landuse_stack_projected,
#            filename = "landuse_stack_projected.tif",
#            overwrite = TRUE,
#            gdal = c("COMPRESS=LZW"))

landuse_stack_projected <- rast("landuse_stack_projected.tif")

#move towards making cropland mask
#cropland_mask <- ifel(landuse_stack_projected == 40, 1, NA)
#names(cropland_mask) <- paste0("Cropland_", 2015:2019)

#save progress
#writeRaster(cropland_mask,
#            filename = "cropland_mask.tif",
#            overwrite = TRUE,
#            gdal = c("COMPRESS=LZW"))

cropland_mask <- rast("cropland_mask.tif")

#plot total cropland across SA per year
cell_area_km2 <- prod(res(cropland_mask)) / 1000000

cropland_counts <- global(cropland_mask, fun = "sum", na.rm = TRUE)

cropland_df <- data.frame(
  year = 2015:2019,
  area_km2 = cropland_counts$sum * cell_area_km2
)

plot(cropland_df$year, cropland_df$area_km2,
     type = "b",
     pch = 19,
     col = "darkgreen",
     lwd = 2,
     xlab = "Year",
     ylab = "Cropland_Area (km2)",
     main = "South Africa Cropland Area (2015-2019)",
     xaxt = "n")

axis(1, at = 2015:2019)
grid()

#load in and manipulate South African parks vector.
#protected_areas <- vect("Z://awiebler/FinalProject/National_Protected_Areas/National_Protected_Areas.shp")
#print(protected_areas)
#crs(protected_areas, describe = TRUE)

#protected_areas_projected <- project(protected_areas, crs(landuse_stack_projected))
#writeVector(protected_areas_projected,
#            "protected_areas_projected.shp",
#            overwrite = TRUE)
#protected_areas <- vect("protected_areas_projected.shp")

#reclassify categories within protected_areas
unique(protected_areas$CATEG)
table(protected_areas$CATEG)

protected_areas$CATEG_simple <- protected_areas$CATEG
protected_areas$CATEG_simple[protected_areas$CATEG %in%
      c("NA", "Bird Sanctuary", "National Heritage Site", "Botanical Garden", "Coast Reserve", "Other Conservation Area", "Mountain Catchment Area")] <- "Miscellaneous"
protected_areas$CATEG_simple[protected_areas$CATEG %in%
      c("National Park", "National Park(les)", "National Park(les_0)")] <- "National Parks"
protected_areas$CATEG_simple[protected_areas$CATEG %in%
      c("Provincial Nature Reserve", "Private Nature Reserve", "DWAF Forest Nature Reserve", "Game Reserve", "Nature Reserve")] <- "Reserves"
protected_areas$CATEG_simple[protected_areas$CATEG %in%
      c("Conservation Area", "Protected Natural Environment", "Game Sanctuary", "Wildlife Management Area", "Game Farm")] <- "Sanctuaries"
protected_areas$CATEG_simple[protected_areas$CATEG %in%
      c("State Land", "SANDF Property")] <- "State Lands"

table(protected_areas$CATEG_simple)

#writeVector(protected_areas,
#            "protected_areas_reclassified.shp",
#            overwrite = TRUE)
protected_areas <- vect("protected_areas_reclassified.shp")

#Look at cropland by category
#protected_areas_raster <- rasterize(protected_areas,
#                                    cropland_mask[[1]],
#                                    field = "CATEG_simp")
plot(protected_areas_raster)
unique(protected_areas_raster)

#save progress
#writeRaster(protected_areas_raster,
#            "protected_areas_raster.tif",
#            overwrite = TRUE)
protected_areas_raster <- rast("protected_areas_raster.tif")

#save progress
#cropland_in_pa <- mask(cropland_mask, protected_areas_raster)
writeRaster(cropland_in_pa,
            "cropland_in_pa.tif",
            overwrite = TRUE)
cropland_in_pa <- rast("cropland_in_pa.tif")

results <- data.frame()
for( i in 1:nlyr(cropland_in_pa)) {
  year <- 2014 + i
  zonal_sum <- zonal(cropland_in_pa[[i]], protected_areas_raster, fun = "sum", na.rm = TRUE)
  year_summary <- data.frame(
    Category = zonal_sum[,1],
    area_km2 = zonal_sum[,2] * cell_area_km2,
    year = year
  )
  year_summary <- year_summary[year_summary$area_km2 > 0, ]
  results <- rbind(results, year_summary)
}

results$Category <- as.factor(results$Category)

ggplot(results, aes(x = year, y = area_km2, color = Category, group = Category)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = 2015:2019) +
  labs(title = "Cropland Area by Protected Area Category 2015-2019",
       x = "Year",
       y = "Cropland Area (km2)",
       color = "PA Category") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "right")

#begin buffer analysis - looking at yearly change in 50km buffered area
protected_areas_buffer <- buffer(protected_areas, width = 50000)
buffer_dissolved <- aggregate(protected_areas_buffer)

#plot(buffer_dissolved, main = "Protected Areas with 50km Buffer")
#plot(protected_areas, col = "lightblue", add = TRUE)

#buffer_raster <- rasterize(buffer_dissolved, cropland_mask[[1]])

#save progress, as always
#writeRaster(buffer_raster,
#            "buffer_raster.tif",
#            overwrite = TRUE)
buffer_raster <- rast("buffer_raster.tif")

#cropland_in_buffer <- mask(cropland_mask, buffer_raster)

#SAVE progress
#writeRaster(cropland_in_buffer,
#            "cropland_buffer.tif",
#            overwrite = TRUE)
cropland_in_buffer <- rast("cropland_buffer.tif")

#plot(cropland_in_buffer[[1]], main = "Cropland in 50km Buffer (2015)")

buffer_results <- data.frame(
  year = 2015:2019,
  area_km2 = NA
)
  
for(i in 1:nlyr(cropland_in_buffer)) {
  cropland_sum <- global(cropland_in_buffer[[i]], fun = "sum", na.rm = TRUE)
  buffer_results$area_km2[i] <- cropland_sum$sum * cell_area_km2
}

plot(buffer_results$year, buffer_results$area_km2,
     type = "b",
     pch = 19,
     col = "darkgreen",
     lwd = 2,
     xlab = "Year",
     ylab = "Cropland Area (km2)",
     main = "Cropland Area in 50km Buffer Zone 2015-2019",
     xaxt = "n")

#finally, looking at percentage of each protected area category covered by cropland
#2015 percentages
year_2015 <- 1

total_cells_by_category <- freq(protected_areas_raster)
cropland_cells_2015 <- zonal(cropland_in_pa[[year_2015]],
                             protected_areas_raster,
                             fun = "sum", na.rm = TRUE)

coverage_2015 <- data.frame(
  Category = total_cells_by_category$value,
  total_cells = total_cells_by_category$count,
  cropland_cells = cropland_cells_2015[match(total_cells_by_category$value,
                                             cropland_cells_2015[,1]), 2]
)

coverage_2015$cropland_cells[is.na(coverage_2015$cropland_cells)] <- 0

coverage_2015$percent_cropland <- (coverage_2015$cropland_cells /
                                     coverage_2015$total_cells) * 100

print(coverage_2015)

#2019 percentages
year_2019 <- 5
cropland_cells_2019 <- zonal(cropland_in_pa[[year_2019]],
                             protected_areas_raster,
                             fun = "sum", na.rm = TRUE)

coverage_2019 <- data.frame(
  Category = total_cells_by_category$value,
  total_cells = total_cells_by_category$count,
  cropland_cells = cropland_cells_2019[match(total_cells_by_category$value,
                            cropland_cells_2019[,1]), 2]
)

coverage_2019$cropland_cells[is.na(coverage_2019$cropland_cells)] <- 0
coverage_2019$percent_cropland <- (coverage_2019$cropland_cells /
                                     coverage_2019$total_cells) * 100

coverage_2015$Category <- as.factor(coverage_2015$Category)

ggplot(coverage_2015, aes(x = reorder(Category, percent_cropland),
                          y = percent_cropland, fill = Category)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  labs(title = "Percentage of Protected Areas Covered by Cropland (2015)",
       x = "Protected Area Category",
       y = "Cropland Coverage (%)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

coverage_2019$Category <- as.factor(coverage_2019$Category)

ggplot(coverage_2019, aes(x = reorder(Category, percent_cropland),
                          y = percent_cropland, fill = Category)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  labs(title = "Percentage of Protected Areas Covered by Cropland (2019)",
       x = "Protected Area Category",
       y = "Cropland Coverage (%)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#begin park size/cropland area analysis
protected_areas_id_raster <- rasterize(protected_areas,
                                       cropland_mask[[1]],
                                       field = "OBJECTID")
cropland_by_pa <- mask(cropland_mask[[1]], protected_areas_id_raster)

cropland_per_pa <- zonal(cropland_by_pa, protected_areas_id_raster,
                         fun = "sum", na.rm = TRUE)

pa_cropland_df <- data.frame(
  ID = cropland_per_pa[,1],
  cropland_area_km2 = cropland_per_pa[,2] * cell_area_km2
)

pa_cropland_df$hectares <- protected_areas$HECTARES[match(pa_cropland_df$ID,
                                                          protected_areas$OBJECTID)]
pa_cropland_df$pa_size_km2 <- pa_cropland_df$hectares / 100

pa_cropland_df$Category <- protected_areas$CATEG_simp[match(pa_cropland_df$ID,
                                                            protected_areas$OBJECTID)]
pa_cropland_df <- pa_cropland_df[complete.cases(pa_cropland_df), ]

#create scatter plot for 2015
ggplot(pa_cropland_df, aes(x = pa_size_km2, y = cropland_area_km2,
                           color = Category)) +
  geom_point(alpha = 0.7, size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, color = "darkgreen") +
  labs(title = "Protected Area Size vs. Cropland Area (2015)",
       x = "Protected Area Size (km^2)",
       y = "Cropland Area (km^2)",
       color = "PA Category") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#log-scale plot for alternative interpretation
ggplot(pa_cropland_df, aes(x = pa_size_km2, y = cropland_area_km2)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "darkgreen") +
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "Protected Area Size vs. Cropland Area (2015)",
       x = "Protected Area Size (km^2, log scale)",
       y = "Cropland Area (km^2, log scale)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))



#regression analysis
model <- lm(cropland_area_km2 ~ pa_size_km2, data = pa_cropland_df)

summary(model)
#log-log model analysis
model_log <- lm(log(cropland_area_km2 + 1) ~ log(pa_size_km2 + 1),
                data = pa_cropland_df)
summary(model_log)

#testing to make sure models are trustworthy
plot(model_log, which = 2)

shapiro.test(residuals(model_log))
cat("Log-log model Shapiro Wilk p-value:", "\n")

hist(residuals(model), main = "Residuals", xlab = "Residuals")
qqnorm(residuals(model), main = "Model QQ Plot")
qqline(residuals(model))

hist(residuals(model_log), main = "Log Log Model Residuals", xlab = "Residuals")
qqnorm(residuals(model_log), main = "Log-Log Model QQ Plot")
qqline(residuals(model_log))

par(mfrow = c(1,1))

cat("\nLinear model heteroscedasticity test:\n")
bptest(model)

cat("\nLog-Log model heteroscedasticity test:\n")
bptest(model_log)

#AIC model comparison, lower AIC = better model
cat("Linear model AIC:", AIC(model), "\n")
cat("Log Log model AIC:", AIC(model_log), "\n")

#addressing heteroscedasticity
coeftest(model_log, vcov = vcovHC(model_log, type = "HC3"))

#log log model interpretation
log_coef <- coef(model_log)[2]
cat("Coefficient:", round(log_coef, 4), "\n")
cat("A 1% increase in PA size is associated with a", round(log_coef, 3), "% increase in cropland_area\n")
cat("\n0r: Doubling PA size (100% increase) is associated with a", round((2^log_coef -1) * 100, 1), "% increase in cropland\n")

model_quantile <- rq(cropland_area_km2 ~ pa_size_km2,
                     data = pa_cropland_df, tau = 0.5)
summary(model_quantile)

#final model interpretations
#log log model
sizes <- c(10, 100, 1000, 10000)
for(size in sizes) {
  predicted_cropland <- exp(coef(model_log)[1] + coef(model_log)[2] * log(size))
  percent <- (predicted_cropland / size) * 100
  cat("PA size:", size, "km2 - median cropland:",
      round(predicted_cropland, 2), "km2 (", round(percent, 2), "% of PA)\n")
}

#quantile model
for(size in sizes) {
  predicted_cropland <- exp(coef(model_quantile)[1] + coef(model_quantile)[2] * size)
  percent <- (predicted_cropland / size) * 100
  cat("PA size:", size, "km2 - median cropland:",
      round(predicted_cropland, 2), "km2 (", round(percent, 2), "% of PA)\n")
}

confint(model_log)
#confidence interval of 0.152 and 0.206, rather good, confirms ~0.18 scaling exponent figure