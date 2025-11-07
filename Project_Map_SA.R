library(terra)
library(tidyterra)
library(FedData)
library(dplyr)
library(ggplot2)
library(scales)

sa <- ne_countries(country = "South Africa", returnclass = "vect")

#extract necessary land cover data into one central list
project_files <- list.files("Z://awiebler/FinalProject/South_Africa_Land_Cover (1)/", 
                            pattern = "\\.tif$", full.names = TRUE)

#ensure extents match each other to enable layering
r_list <- lapply(project_files, rast)

#QA/QC - identify issue
sapply(r_list, crs)
sapply(r_list, res)
sapply(r_list, ext)

#correct one misaligned extent
ref <- r_list[[2]]
corrected_r_list <- lapply(r_list, 
                           \(r) resample(r, ref, method = "near"))

temp_landuse <- rast(corrected_r_list)
names(temp_landuse) <- 2015:2019

plot(temp_landuse)

#find the frequency of land use codes
freq_list <- lapply(seq_len(nlyr(temp_landuse)), function(i) {
  lyr <- temp_landuse[[i]]            # extract one layer safely
  f <- freq(lyr, digits = 0, value = NULL)
  f$year <- names(temp_landuse)[i]    # assign year name
  f
})

freq_df <- do.call(rbind, freq_list)
head(freq_df)

#prepare to plot frequency of land use code '50' (buildings/man-made structures) over time
freq_50 <- freq_df %>%
  filter(value == 50) %>%
  group_by(year) %>%
  summarise(total_pixels = sum(count), .groups = "drop")

#plot frequency of land use code '50' from 2015-2019
ggplot(freq_50, aes(x = as.numeric(year), y = total_pixels / 1000)) +
  geom_line(color = "#0072B2", linewidth = 1.2) +
  geom_point(color = "#0072B2", size = 3) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Change in Land Use Class 50 (2015–2019)",
    x = "Year",
    y = "Pixel Count (thousands)"
  ) +
  theme_minimal(base_size = 14)

