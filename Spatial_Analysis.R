
#### Install packages####

install.packages("sf")

library(sf)


install.packages(c("sf", "tmap", "dplyr"))
library(sf)
library(tmap)
library(dplyr)
#### import shape file ####

# Set the file path
#shapefile_path <- "D:\\Research\\Water Quality Buriganga\\Shape File\\salek\\shape\\dhaka.shp"


# Test reading shapefile
#water_shape <- st_read("D:/Research/Water Quality Buriganga/Shape File/salek/shape/dhaka.shp")


library(sf)
water_shape <- st_read("D:\\Research\\Water Quality Buriganga\\Shape File\\salek\\shape\\dhaka.shp")


# View structure
print(water_shape)

# Quick map
plot(water_shape)



# View features sorted by area (descending)
water_shape[order(-water_shape$Shape_Area), ]

#### Area ####
# Read shapefile
shapefile_path <- "D:/Research/Water Quality Buriganga/Shape File/salek/shape/water_dhaka.shp"
water_shape <- st_read(shapefile_path)

# Select the largest polygon assuming it's Buriganga
buriganga <- water_shape %>% filter(Shape_Area == max(Shape_Area))

# Create sampling site data
sampling_data <- data.frame(
  location = c("Hazaribagh", "Mirpur Bridge", "Kamrangir Char"),
  lat = c(23.7161, 23.8270, 23.6708),
  lon = c(90.3867, 90.3697, 90.4083)
)

# Convert to sf object
sampling_sites <- st_as_sf(sampling_data, coords = c("lon", "lat"), crs = 4326)

# Plot map
tm_shape(buriganga) +
  tm_polygons(col = "lightblue", border.col = "blue", lwd = 1.5) +
  tm_shape(sampling_sites) +
  tm_dots(col = "red", size = 0.1) +
  tm_text("location", size = 0.6, just = "top") +
  tm_layout(title = "Study Area: Buriganga River and Sampling Sites",
            legend.outside = TRUE,
            frame = FALSE)








library(sf)
library(dplyr)
library(tmap)

# Read shapefile (already done)
shapefile_path <- "D:/Research/Water Quality Buriganga/Shape File/salek/shape/water_dhaka.shp"
water_shape <- st_read(shapefile_path)

# Optional: select only the polygon representing Buriganga if multiple exist
buriganga <- water_shape %>% filter(Shape_Area == max(Shape_Area))  # or another identifier








#### Define coordinates manually####
coords <- data.frame(
  Location = c("Hazaribag", "Mirpur Bridge", "Kamrangir Char"),  # ঠিক করা হলো spelling
  lat = c(23.7161, 23.8270, 23.6708),
  lon = c(90.3867, 90.3697, 90.4083)
)







excel_data <- read.csv ("D:\\Research\\Water Quality Buriganga\\Dataset\\tidy_dataset_L1_to_L3.csv")


library(dplyr)
library(sf)

# Merge
merged_data <- left_join(excel_data, coords, by = "Location")

# NA চেক করতে চাইলে:
# merged_data %>% filter(is.na(lat) | is.na(lon))

# Convert to sf
sampling_sites <- st_as_sf(merged_data, coords = c("lon", "lat"), crs = 4326)


# যেগুলোর মধ্যে lat বা lon নেই (NA)
merged_data %>% filter(is.na(lat) | is.na(lon)) %>% select(Location) %>% distinct()





library(tmap)

tm_shape(water_shape) +
  tm_polygons(fill = "lightblue", col = "blue") +
  tm_shape(sampling_sites %>% filter(Parameter == "pH")) +
  tm_dots(col = "Value", palette = "RdYlBu", size = 0.4, title = "pH") +
  tm_text("Location", size = 0.6, just = "top") +
  tm_title("pH Levels at Sampling Sites on Buriganga River")













tm_shape(water_shape) +
  tm_polygons(fill = "lightblue", col = "blue") +
  tm_shape(sampling_sites %>% filter(Parameter == "pH")) +
  
  tm_dots(col = "Value", 
          palette = "RdYlBu", 
          style = "cont", 
          size = 1.5,             # আগের তুলনায় অনেক বড়
          title = "pH Level") +

  tm_text("Location", size = 0.6, just = "top") +
  tm_layout(title = "pH Concentration at Sampling Sites")








tmap_mode("plot")

tm_shape(water_shape) +
  tm_polygons(fill = "lightblue", col = "blue") +
  tm_shape(sampling_sites) +
  tm_dots(col = "Value", palette = "RdYlBu", style = "cont", size = 1.2) +
  tm_text("Location", size = 0.2, just = "top") +
  tm_facets(by = "Parameter") +  # Facet by parameter
  tm_layout(title = "Water Quality Parameters by Sampling Site", legend.outside = TRUE)





tmap_mode("plot")

tm_shape(water_shape) +
  tm_polygons(fill = "lightblue", col = "blue") +
  tm_shape(sampling_sites) +
  tm_dots(col = "Value", palette = "RdYlBu", style = "cont", size = 1.2) +
  tm_text("Location", size = 0.2, just = "top") +
  tm_facets(by = "Parameter") +  # Facet by parameter
  tm_layout(legend.outside = TRUE)









library(tmap)

tmap_mode("plot")

# Get unique parameters
parameters <- unique(sampling_sites$Parameter)

# Loop to create separate maps
for (param in parameters) {
  
  map_plot <- tm_shape(water_shape) +
    tm_polygons(fill = "lightblue", col = "blue") +
    tm_shape(sampling_sites[sampling_sites$Parameter == param, ]) +
    tm_dots(col = "Value", palette = "RdYlBu", style = "cont", size = 1.2) +
    tm_text("Location", size = 0.2, just = "top") +
    tm_layout(
      title = paste("Spatial Distribution of", param),
      legend.outside = TRUE
    )
  
  print(map_plot)
}








library(tmap)

# Set plotting mode
tmap_mode("plot")

# Get unique parameters (should be 10)
parameters <- unique(sampling_sites$Parameter)

# Loop to create separate maps for each parameter
for (param in parameters) {
  
  map_plot <- tm_shape(water_shape) +
    tm_polygons(fill = "lightblue", col = "blue") +
    
    tm_shape(sampling_sites[sampling_sites$Parameter == param, ]) +
    tm_dots(
      col = "Value",
      palette = "RdYlBu",
      style = "cont",
      size = 1.2
    ) +
    
    # 🔥 Increased location label size
    tm_text(
      text = "Location",
      size = 0.6,              # increased from 0.2
      just = "top",
      remove.overlap = TRUE
    ) +
    
    tm_layout(
      title = paste("Spatial Distribution of", param),
      legend.outside = TRUE,
      legend.outside.position = "right"
    )
  
  print(map_plot)
}



















library(tmap)

tmap_mode("plot")

# Get unique parameters
parameters <- unique(sampling_sites$Parameter)

# Create list to store maps
map_list <- list()

for (param in parameters) {
  
  map_list[[param]] <- 
    tm_shape(water_shape) +
    tm_polygons(fill = "lightblue", col = "blue") +
    
    tm_shape(sampling_sites[sampling_sites$Parameter == param, ]) +
    tm_dots(
      col = "Value",
      palette = "RdYlBu",
      style = "cont",
      size = 1
    ) +
    
    tm_text(
      text = "Location",
      size = 0.6,               # larger location names
      remove.overlap = TRUE
    ) +
    
    tm_layout(
      title = param,
      legend.show = FALSE       # remove individual legends
    )
}

# Combine into 2 rows × 5 columns
final_map <- tmap_arrange(
  map_list[[1]], map_list[[2]], map_list[[3]], map_list[[4]], map_list[[5]],
  map_list[[6]], map_list[[7]], map_list[[8]], map_list[[9]], map_list[[10]],
  ncol = 5,
  nrow = 2
)

# Save the combined figure
tmap_save(
  final_map,
  filename = "All_Parameters_Maps.pdf",
  width = 16,
  height = 8,
  dpi = 300
)









library(tmap)

tmap_mode("plot")

# Get unique parameters
parameters <- unique(sampling_sites$Parameter)

map_list <- list()

for (i in seq_along(parameters)) {
  
  param <- parameters[i]
  
  map_list[[i]] <- 
    tm_shape(water_shape) +
    tm_polygons(fill = "lightblue", col = "blue") +
    
    tm_shape(sampling_sites[sampling_sites$Parameter == param, ]) +
    tm_dots(
      col = "Value",
      palette = "RdYlBu",
      style = "cont",
      size = 1,
      title = "Value"
    ) +
    
    tm_text(
      text = "Location",
      size = 0.6,
      remove.overlap = TRUE
    ) +
    
    tm_layout(
      title = param,
      legend.show = ifelse(i == 1, TRUE, FALSE),  # Legend only in first map
      legend.outside = TRUE,
      legend.outside.position = "bottom",
      asp = 0
    )
}

# Arrange maps (5 columns × 2 rows)
final_map <- do.call(tmap_arrange, c(map_list, ncol = 5, nrow = 2))

# Save figure
tmap_save(
  final_map,
  filename = "All_Parameters_Maps.pdf",
  width = 18,
  height = 9,
  dpi = 300
)

# Display
final_map
library(tmap)

tmap_mode("plot")

# Get unique parameters
parameters <- unique(sampling_sites$Parameter)

map_list <- list()

for (i in seq_along(parameters)) {
  
  param <- parameters[i]
  
  map_list[[i]] <- 
    tm_shape(water_shape) +
    tm_polygons(fill = "lightblue", col = "blue") +
    
    tm_shape(sampling_sites[sampling_sites$Parameter == param, ]) +
    tm_dots(
      col = "Value",
      palette = "RdYlBu",
      style = "cont",
      size = 1,
      title = "Value"
    ) +
    
    tm_text(
      text = "Location",
      size = 0.6,
      remove.overlap = TRUE
    ) +
    
    tm_layout(
      title = param,
      legend.show = ifelse(i == 1, TRUE, FALSE),  # Legend only in first map
      legend.outside = TRUE,
      legend.outside.position = "bottom",
      asp = 0
    )
}

# Arrange maps (5 columns × 2 rows)
final_map <- do.call(tmap_arrange, c(map_list, ncol = 5, nrow = 2))

# Save figure
tmap_save(
  final_map,
  filename = "All_Parameters_Maps.pdf",
  width = 18,
  height = 9,
  dpi = 300
)

# Display
final_map












library(tmap)

tmap_mode("plot")

# Get unique parameters
parameters <- unique(sampling_sites$Parameter)

map_list <- list()

for (i in seq_along(parameters)) {
  
  param <- parameters[i]
  
  map_list[[i]] <- 
    tm_shape(water_shape) +
    tm_polygons(fill = "lightblue", col = "blue") +
    
    tm_shape(sampling_sites[sampling_sites$Parameter == param, ]) +
    tm_dots(
      col = "Value",
      palette = "RdYlBu",
      style = "cont",
      size = 1,
      title = "Value"
    ) +
    
    tm_text(
      text = "Location",
      size = 0.6,
      remove.overlap = TRUE
    ) +
    
    tm_layout(
      title = param,
      legend.show = ifelse(i == 1, TRUE, FALSE),  # Legend only in first map
      legend.outside = TRUE,
      legend.outside.position = "bottom",
      asp = 0
    )
}

# Arrange maps (5 columns × 2 rows)
final_map <- do.call(tmap_arrange, c(map_list, ncol = 5, nrow = 2))

# Save figure
tmap_save(
  final_map,
  filename = "All_Parameters_Maps.pdf",
  width = 18,
  height = 9,
  dpi = 300
)

# Display
final_map











#### actual figure ####

library(tmap)

# Set tmap mode to plot
tmap_mode("plot")

# Get unique parameters
parameters <- unique(sampling_sites$Parameter)

map_list <- list()

for (i in seq_along(parameters)) {
  
  param <- parameters[i]
  
  # Create map for each parameter
  map_list[[i]] <- 
    tm_shape(water_shape) +
    tm_polygons(fill = "lightblue", col = "blue") +
    
    tm_shape(sampling_sites[sampling_sites$Parameter == param, ]) +
    tm_dots(
      col = "Value",
      palette = "RdYlBu",
      style = "cont",
      size = 1,
      title = "Value"
    ) +
    
    tm_text(
      text = "Location",
      size = 0.6,
      remove.overlap = TRUE
    ) +
    
    tm_layout(
      title = param,                        # Individual map title
      legend.show = ifelse(i == 1, TRUE, FALSE),
      legend.outside = TRUE,
      legend.outside.position = "bottom",
      asp = 0
    )
}

# Arrange all maps with a main title
final_map <- tmap_arrange(
  grobs = map_list,
  ncol = 5,
  nrow = 2,
  outer.margins = 0.02,
  main.title = "Maps of All Water Quality Parameters"   # Main figure title
)

# Save figure
tmap_save(
  final_map,
  filename = "All_Parameters_Maps.pdf",
  width = 18,
  height = 9,
  dpi = 300
)

# Display
final_map























library(tmap)

tmap_mode("plot")

tm_shape(water_shape) +
  tm_polygons(
    fill = "lightblue",
    col = "blue"
  ) +
  
  tm_shape(sampling_sites) +
  tm_dots(
    col = "Value",        # column for coloring
    size = 1.2,
    shape = 21,
    border.col = "black",
    border.lwd = 0.5
  ) +
  
  tm_scale_continuous(
    values = "RdYlBu"     # correct argument for palette in v4
  ) +
  
  tm_text(
    "Location",
    size = 0.2,
    options = opt_tm_text(just = "top")
  ) +
  
  tm_facets(by = "Parameter") +  # Facet by parameter
  
  tm_layout(
    legend.outside = TRUE
    # title removed
  )





tmap_mode("plot")

tm_shape(water_shape) +
  tm_polygons(fill = "lightblue", col = "blue") +
  tm_shape(sampling_sites) +
  tm_dots(col = "Value", palette = "RdYlBu", style = "cont", size = 1.2) +
  tm_text("Location", size = 0.2, just = "top") +
  tm_facets(by = "Parameter") +  # Facet by parameter
  tm_layout(legend.outside = TRUE)


unique(excel_data$Parameter)











library(tmap)

tmap_mode("plot")

tm_shape(water_shape) +
  tm_polygons(fill = "lightblue", col = "blue") +
  tm_shape(sampling_sites) +
  tm_dots(col = "Value", palette = "RdYlBu", style = "cont", size = 1.5) +  # slightly bigger dots
  tm_text("Location", size = 0.5, just = "top", xmod = 0, ymod = 0.5) +  # larger and slightly above points
  tm_facets(by = "Parameter", ncol = 3) +  # make facets wider by using 4 columns
  tm_layout(
    legend.outside = TRUE,
    legend.outside.position = "right",
    inner.margins = c(0.02, 0.02, 0.02, 0.02),  # reduce margins for larger plot
    frame = TRUE,  # show frame
    outer.margins = c(0.01, 0.01, 0.01, 0.01),
    main.title.size = 1.2
  )











library(tmap)
library(dplyr)

tmap_mode("plot")

# Get all unique parameters
parameters <- unique(sampling_sites$Parameter)

# Loop over parameters and save each map
for (param in parameters) {
  
  # Filter sampling sites for this parameter
  sites_param <- sampling_sites %>% filter(Parameter == param)
  
  # Create the map
  map <- tm_shape(water_shape) +
    tm_polygons(fill = "lightblue", col = "blue") +
    tm_shape(sites_param) +
    tm_dots(col = "Value", palette = "RdYlBu", style = "cont", size = 1.2) +
    tm_text("Location", size = 0.8, just = "top") +
    tm_layout(legend.outside = TRUE)
  
  # Print or save the map
  print(map)
  # If you want to save each map as PNG:
  # tmap_save(map, filename = paste0("Map_", param, ".png"), width = 6, height = 5)
}











library(tmap)
tmap_mode("plot")

tm_shape(water_shape) +
  tm_polygons(
    col = "lightblue",
    border.col = "grey60",
    lwd = 0.5
  ) +
  
  tm_shape(sampling_sites) +
  tm_dots(
    col = "Value",
    palette = "-RdYlBu",         # Reverse to make high = red (intuitive for pollutants)
    style = "cont",
    size = 0.5,
    shape = 21,
    border.col = "black",
    border.lwd = 0.5,
    title = "Concentration"
  ) +
  
  tm_text(
    text = "Location",
    size = 0.5,
    just = "right",
    col = "black",
    shadow = TRUE
  ) +
  
  tm_facets(by = "Parameter") +
  
  tm_layout(
    title = "Water Quality Parameter Concentrations Across Sampling Sites",
    title.size = 1.2,
    title.position = c("center", "top"),
    legend.outside = TRUE,
    legend.outside.position = "right",
    legend.title.size = 0.9,
    legend.text.size = 0.7,
    frame = FALSE,
    bg.color = "white",
    panel.label.size = 1.1,
    panel.label.fontface = "bold",
    panel.label.bg.color = "white"
  )

























#### Final Figure ####

library(tmap)
library(dplyr)

tmap_mode("plot")

# Get unique parameters from your data
parameters <- unique(excel_data$Parameter)

# Loop over parameters and create maps
for (param in parameters) {
  
  # Filter sampling sites for this parameter
  sites_param <- sampling_sites %>% filter(Parameter == param)
  
  # Create the map
  map <- tm_shape(water_shape) +
    tm_polygons(fill = "lightblue", col = "blue") +
    tm_shape(sites_param) +
    tm_dots(col = "Value", palette = "RdYlBu", style = "cont", size = 1.2) +
    tm_text("Location", size = 0.8, just = "top") +  # bigger location names
    tm_layout(
      legend.outside = TRUE,
      main.title = param,            # use parameter as short caption
      main.title.size = 1.2,
      main.title.position = c("center", "top")
    )
  
  # Print the map
  print(map)
  
  # Optional: save each map as PNG
  # tmap_save(map, filename = paste0("Map_", param, ".png"), width = 6, height = 5)
}







library(tmap)
library(dplyr)

tmap_mode("plot")

# Your parameters
parameters <- unique(excel_data$Parameter)

# Create a list to store maps
map_list <- list()

# Loop through parameters to create individual maps
for (param in parameters) {
  
  # Filter sampling sites for this parameter
  sites_param <- sampling_sites %>% filter(Parameter == param)
  
  # Create the map
  map <- tm_shape(water_shape) +
    tm_polygons(fill = "lightblue", col = "blue") +
    tm_shape(sites_param) +
    tm_dots(col = "Value", palette = "RdYlBu", style = "cont", size = 1.2) +
    tm_text("Location", size = 0.5, just = "top") +
    tm_layout(
      legend.outside = TRUE,
      main.title = param,
      main.title.size = 1.2,
      main.title.position = c("center", "top")
    )
  
  # Add to list
  map_list[[param]] <- map
}

# Arrange first 9 parameters in 3x3 grid
tmap_arrange(map_list[1:9], ncol = 3)

# Plot the last (10th) parameter alone
map_list[[10]]











library(tmap)
library(dplyr)

tmap_mode("plot")

# Your parameters
parameters <- unique(excel_data$Parameter)

# Create a list to store maps
map_list <- list()

# Loop to create individual maps
for (param in parameters) {
  
  # Filter sampling sites for this parameter
  sites_param <- sampling_sites %>% filter(Parameter == param)
  
  # Create the map
  map <- tm_shape(water_shape) +
    tm_polygons(fill = "lightblue", col = "blue") +
    tm_shape(sites_param) +
    tm_dots(col = "Value", palette = "RdYlBu", style = "cont", size = 1.2) +
    tm_text("Location", size = 0.5, just = "top") +
    tm_layout(
      legend.outside = TRUE,
      main.title = param,
      main.title.size = 1.2,
      main.title.position = c("center", "top")
    )
  
  map_list[[param]] <- map
}

# -----------------------
# Arrange first 6 parameters: 2 rows × 3 columns
# -----------------------
tmap_arrange(map_list[1:6], ncol = 3)

# -----------------------
# Arrange last 4 parameters: 2 rows × 2 columns
# -----------------------
tmap_arrange(map_list[7:10], ncol = 2)








library(tmap)
library(dplyr)

tmap_mode("plot")

# Your parameters
parameters <- unique(excel_data$Parameter)

# Create a list to store maps
map_list <- list()

for (param in parameters) {
  
  # Filter sampling sites for this parameter
  sites_param <- sampling_sites %>% filter(Parameter == param)
  
  # Create map (v4 style)
  map <- tm_shape(water_shape) +
    tm_polygons(fill = "lightblue", col = "blue") +
    tm_shape(sites_param) +
    tm_dots(col = "Value", size = 1.2, shape = 21, border.col = "black", border.lwd = 0.5) +
    tm_scale_continuous(values = "RdYlBu") +   # continuous palette
    tm_text("Location", size = 0.3, options = opt_tm_text(just = "top")) +
    tm_title(param) +                           # v4 title
    tm_layout(legend.outside = TRUE)
  
  map_list[[param]] <- map
}

# -----------------------
# First figure: first 6 parameters (2 rows x 3 columns)
# -----------------------
tmap_arrange(map_list[1:6], ncol = 3)

# -----------------------
# Second figure: last 4 parameters (2 rows x 2 columns)
# -----------------------
tmap_arrange(map_list[7:10], ncol = 2)










library(tmap)
library(dplyr)

tmap_mode("plot")

# Your parameters
parameters <- unique(excel_data$Parameter)

# Create a list to store maps
map_list <- list()

for (param in parameters) {
  
  # Filter sampling sites for this parameter
  sites_param <- sampling_sites %>% filter(Parameter == param)
  
  # Create map with old colors
  map <- tm_shape(water_shape) +
    tm_polygons(fill = "lightblue", col = "blue") +
    tm_shape(sites_param) +
    tm_dots(
      col = "Value",
      size = 1.2,
      shape = 21,
      border.col = "black",
      border.lwd = 0.5,
      values = "RdYlBu"   # old palette preserved
    ) +
    tm_text("Location", size = 0.5, options = opt_tm_text(just = "top")) +
    tm_title(param) +
    tm_layout(legend.outside = TRUE)
  
  map_list[[param]] <- map
}

# -----------------------
# First figure: first 6 parameters (2 rows x 3 columns)
# -----------------------
tmap_arrange(map_list[1:6], ncol = 3)

# -----------------------
# Second figure: last 4 parameters (2 rows x 2 columns)
# -----------------------
tmap_arrange(map_list[7:10], ncol = 2)


















library(tmap)
library(dplyr)

tmap_mode("plot")

# Get unique parameters from your data
parameters <- unique(excel_data$Parameter)

# Create a list to store maps
map_list <- list()

# Loop over parameters to create individual maps
for (param in parameters) {
  
  # Filter sampling sites for this parameter
  sites_param <- sampling_sites %>% filter(Parameter == param)
  
  # Create the map with your preferred color style
  map <- tm_shape(water_shape) +
    tm_polygons(fill = "lightblue", col = "blue") +
    tm_shape(sites_param) +
    tm_dots(
      col = "Value",
      palette = "RdYlBu",   # old palette style
      style = "cont",       # continuous style
      size = 1.2,
      shape = 21,
      border.col = "black",
      border.lwd = 0.5
    ) +
    tm_text(
      "Location",
      size = 0.4,           # bigger location names
      just = "top"
    ) +
    tm_layout(
      legend.outside = TRUE
    ) +
    tm_title(param)          # parameter as caption
  
  # Store the map
  map_list[[param]] <- map
}

# -----------------------
# First figure: first 6 parameters (2 rows x 3 columns)
# -----------------------
tmap_arrange(map_list[1:6], ncol = 3)

# -----------------------
# Second figure: last 4 parameters (2 rows x 2 columns)
# -----------------------
tmap_arrange(map_list[7:10], ncol = 2)















library(tmap)
library(dplyr)

tmap_mode("plot")

# Get unique parameters from your data
parameters <- unique(excel_data$Parameter)

# Create a list to store maps
map_list <- list()

# Loop over parameters to create individual maps
for (param in parameters) {
  
  # Filter sampling sites for this parameter
  sites_param <- sampling_sites %>% filter(Parameter == param)
  
  # Create the map
  map <- tm_shape(water_shape) +
    tm_polygons(fill = "lightblue", col = "blue") +
    tm_shape(sites_param) +
    tm_dots(
      col = "Value",
      palette = "RdYlBu",   # your preferred color style
      style = "cont",       # continuous style
      size = 1.2,
      shape = 21,
      border.col = "black",
      border.lwd = 0.5
    ) +
    tm_text(
      "Location",
      size = 0.4,           # bigger location labels
      just = "top"
    ) +
    tm_layout(
      legend.outside = TRUE
    ) +
    tm_title(param)          # short caption / parameter name
  
  map_list[[param]] <- map
}

# -----------------------
# Figure 1: first 6 parameters (2 rows × 3 columns)
# -----------------------
figure1 <- tmap_arrange(map_list[1:6], ncol = 3)

# -----------------------
# Figure 2: last 4 parameters (2 rows × 2 columns)
# -----------------------
figure2 <- tmap_arrange(map_list[7:10], ncol = 2)

# Optionally print figures separately
figure1
figure2












































tmap_mode("plot")

tm_shape(water_shape) +
  tm_polygons(
    fill = "lightblue",
    col = NA        # 🔹 remove polygon border
  ) +
  tm_shape(sampling_sites) +
  tm_dots(
    col = "Value",
    palette = "-RdYlBu",  # reversed for nicer contrast
    style = "cont",
    size = 1.4,
    border.col = NA      # 🔹 remove point border
  ) +
  tm_text(
    "Location",
    size = 0.25,
    just = "top",
    col = "black"
  ) +
  tm_facets(by = "Parameter") +
  tm_layout(
    title = "Spatial Distribution of Water Quality Parameters",
    title.size = 1.2,
    title.position = c("center", "top"),
    legend.outside = TRUE,
    legend.outside.position = "right",
    frame = FALSE,        # 🔹 remove map frame
    bg.color = "white",
    panel.show = FALSE    # 🔹 remove facet panel borders
  )








tmap_mode("plot")

tm_shape(water_shape) +
  tm_polygons(
    fill = "gray95",
    col = NA
  ) +
  tm_shape(sampling_sites) +
  tm_dots(
    col = "Value",
    palette = "viridis",
    style = "quantile",   # 🔹 clear class difference
    n = 5,                # 🔹 5 easy-to-read classes
    size = 1.8,
    alpha = 0.9,
    border.col = "white"
  ) +
  tm_text(
    "Location",
    size = 0.35,          # 🔹 readable text
    just = "right"
  ) +
  tm_facets(
    by = "Parameter",
    ncol = 3              # 🔹 better layout
  ) +
  tm_layout(
    title = "Spatial Distribution of Water Quality Parameters",
    title.size = 1.3,
    legend.outside = TRUE,
    legend.text.size = 0.8,
    legend.title.size = 0.9,
    frame = FALSE,
    panel.show = FALSE,
    bg.color = "white"
  )























library(dplyr)
library(tmap)

tmap_mode("plot")

params_1 <- c("pH", "DO", "BOD", "COD", "TDS")

tm_shape(water_shape) +
  tm_polygons(fill = "gray95", col = NA) +
  tm_shape(sampling_sites %>% filter(Parameter %in% params_1)) +
  tm_dots(
    col = "Value",
    palette = "viridis",
    style = "quantile",
    n = 5,
    size = 1.8,
    alpha = 0.9,
    border.col = "white"
  ) +
  tm_text("Location", size = 0.35, just = "right") +
  tm_facets(by = "Parameter", ncol = 3) +
  tm_layout(
    title = "Spatial Distribution of Water Quality Parameters (Figure 1: pH – TDS)",
    legend.outside = TRUE,
    frame = FALSE,
    panel.show = FALSE,
    bg.color = "white"
  )









params_2 <- c("Trurbidity", "Cloride", "SS", "T-Alkainity", "EC")

tm_shape(water_shape) +
  tm_polygons(fill = "gray95", col = NA) +
  tm_shape(sampling_sites %>% filter(Parameter %in% params_2)) +
  tm_dots(
    col = "Value",
    palette = "viridis",
    style = "quantile",
    n = 5,
    size = 1.8,
    alpha = 0.9,
    border.col = "white"
  ) +
  tm_text("Location", size = 0.35, just = "right") +
  tm_facets(by = "Parameter", ncol = 3) +
  tm_layout(
    title = "Spatial Distribution of Water Quality Parameters (Figure 2: Trurbidity – EC)",
    legend.outside = TRUE,
    frame = FALSE,
    panel.show = FALSE,
    bg.color = "white"
  )










params <- c("pH", "DO", "BOD", "COD", "TDS", 
            "Trurbidity", "Cloride", "SS", "T-Alkainity", "EC")

for (p in params) {
  map <- tm_shape(water_shape) +
    tm_polygons(fill = "gray95", col = NA) +
    tm_shape(sampling_sites %>% filter(Parameter == p)) +
    tm_dots(
      col = "Value",
      palette = "viridis",
      style = "quantile",
      n = 5,
      size = 1.8,
      alpha = 0.9,
      border.col = "white"
    ) +
    tm_text("Location", size = 0.35, just = "right") +
    tm_layout(
      title = paste("Spatial Distribution of", p),
      legend.outside = TRUE,
      frame = FALSE,
      bg.color = "white"
    )
  
  print(map)
}

















