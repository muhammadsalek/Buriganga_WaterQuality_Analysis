

 #### Visualization Trend Line ####


#### Hazaribagh ####

library(dplyr)
library(ggplot2)
library(tidyr)
library(grid)  # for unit()

# Define all 10 parameters
all_parameters <- c(
  "BOD", "COD", "EC", "SS", "TDS",
  "Chloride", "DO", "pH", "T-Alkalinity", "Turbidity"
)

# Prepare dataset with all parameters
df_plot <- df_long %>%
  filter(Location == "Hazaribag") %>%
  group_by(Year, Parameter) %>%
  summarise(
    MeanValue = mean(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Ensure all 10 parameters per year, fill missing with NA
  tidyr::complete(Year, Parameter = all_parameters)

# Bright, high-IF color palette
bright_colors <- c(
  "BOD"          = "#FF4500",   # vivid orange-red
  "COD"          = "#9A32CD",   # bright purple
  "EC"           = "#FF8C00",   # dark orange
  "SS"           = "#32CD32",   # lime green
  "TDS"          = "#FFD700",   # gold
  "Chloride"     = "#A9A9A9",   # dark gray
  "DO"           = "#1E90FF",   # dodger blue
  "pH"           = "#00CED1",   # dark turquoise
  "T-Alkalinity" = "#FF1493",   # deep pink
  "Turbidity"    = "#000000"    # black
)

# Create plot
p <- ggplot(df_plot, aes(x = Year, y = MeanValue, color = Parameter, group = Parameter)) +
  
  # Lines and points
  geom_line(linewidth = 1.3, alpha = 0.9, na.rm = TRUE) +
  geom_point(size = 3.2, shape = 21, fill = "white", stroke = 0.8, na.rm = TRUE) +
  
  # Labels
  labs(
    title = "Temporal Trends of Water Quality Parameters (Hazaribag)",
    x = "Year",
    y = "Mean Concentration (mg/L)",
    color = "Parameter"
  ) +
  
  # Color palette
  scale_color_manual(values = bright_colors) +
  
  # Theme
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, margin = ggplot2::margin(b = 12)),
    
    axis.title = element_text(size = 14, face = "bold"),
    axis.text  = element_text(size = 11, color = "black"),
    
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(face = "bold", size = 12),
    legend.text  = element_text(size = 11),
    legend.key.width = unit(1.5, "cm"),
    
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    
    panel.border = element_rect(color = "black", linewidth = 0.8),
    
    plot.margin = ggplot2::margin(10, 15, 10, 15)
  )

# Show plot
p






#### Mirpur Bridge ####
library(dplyr)
library(ggplot2)
library(tidyr)
library(grid)  # for unit()

# Define all 10 parameters
all_parameters <- c(
  "BOD", "COD", "EC", "SS", "TDS",
  "Chloride", "DO", "pH", "T-Alkalinity", "Turbidity"
)

# Prepare dataset with all parameters
df_plot <- df_long %>%
  filter(Location == "Mirpur Bridge") %>%
  group_by(Year, Parameter) %>%
  summarise(
    MeanValue = mean(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Ensure all 10 parameters per year, fill missing with NA
  tidyr::complete(Year, Parameter = all_parameters)

# Bright, high-IF color palette
bright_colors <- c(
  "BOD"          = "#FF4500",   # vivid orange-red
  "COD"          = "#9A32CD",   # bright purple
  "EC"           = "#FF8C00",   # dark orange
  "SS"           = "#32CD32",   # lime green
  "TDS"          = "#FFD700",   # gold
  "Chloride"     = "#A9A9A9",   # dark gray
  "DO"           = "#1E90FF",   # dodger blue
  "pH"           = "#00CED1",   # dark turquoise
  "T-Alkalinity" = "#FF1493",   # deep pink
  "Turbidity"    = "#000000"    # black
)

# Create plot
p <- ggplot(df_plot, aes(x = Year, y = MeanValue, color = Parameter, group = Parameter)) +
  
  # Lines and points
  geom_line(linewidth = 1.3, alpha = 0.9, na.rm = TRUE) +
  geom_point(size = 3.2, shape = 21, fill = "white", stroke = 0.8, na.rm = TRUE) +
  
  # Labels
  labs(
    title = "Temporal Trends of Water Quality Parameters (Mirpur Bridge)",
    x = "Year",
    y = "Mean Concentration (mg/L)",
    color = "Parameter"
  ) +
  
  # Color palette
  scale_color_manual(values = bright_colors) +
  
  # Theme
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, margin = ggplot2::margin(b = 12)),
    
    axis.title = element_text(size = 14, face = "bold"),
    axis.text  = element_text(size = 11, color = "black"),
    
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(face = "bold", size = 12),
    legend.text  = element_text(size = 11),
    legend.key.width = unit(1.5, "cm"),
    
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    
    panel.border = element_rect(color = "black", linewidth = 0.8),
    
    plot.margin = ggplot2::margin(10, 15, 10, 15)
  )

# Show plot
p




#### kamrangir Char ####
library(dplyr)
library(ggplot2)
library(tidyr)
library(grid)  # for unit()

# Define all 10 parameters
all_parameters <- c(
  "BOD", "COD", "EC", "SS", "TDS",
  "Chloride", "DO", "pH", "T-Alkalinity", "Turbidity"
)

# Prepare dataset with all parameters
df_plot <- df_long %>%
  filter(Location == "Kamrangir Char") %>%
  group_by(Year, Parameter) %>%
  summarise(
    MeanValue = mean(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Ensure all 10 parameters per year, fill missing with NA
  tidyr::complete(Year, Parameter = all_parameters)

# Bright, high-IF color palette
bright_colors <- c(
  "BOD"          = "#FF4500",   # vivid orange-red
  "COD"          = "#9A32CD",   # bright purple
  "EC"           = "#FF8C00",   # dark orange
  "SS"           = "#32CD32",   # lime green
  "TDS"          = "#FFD700",   # gold
  "Chloride"     = "#A9A9A9",   # dark gray
  "DO"           = "#1E90FF",   # dodger blue
  "pH"           = "#00CED1",   # dark turquoise
  "T-Alkalinity" = "#FF1493",   # deep pink
  "Turbidity"    = "#000000"    # black
)

# Create plot
p <- ggplot(df_plot, aes(x = Year, y = MeanValue, color = Parameter, group = Parameter)) +
  
  # Lines and points
  geom_line(linewidth = 1.3, alpha = 0.9, na.rm = TRUE) +
  geom_point(size = 3.2, shape = 21, fill = "white", stroke = 0.8, na.rm = TRUE) +
  
  # Labels
  labs(
    title = "Temporal Trends of Water Quality Parameters (Kamrangir Char)",
    x = "Year",
    y = "Mean Concentration (mg/L)",
    color = "Parameter"
  ) +
  
  # Color palette
  scale_color_manual(values = bright_colors) +
  
  # Theme
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, margin = ggplot2::margin(b = 12)),
    
    axis.title = element_text(size = 14, face = "bold"),
    axis.text  = element_text(size = 11, color = "black"),
    
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(face = "bold", size = 12),
    legend.text  = element_text(size = 11),
    legend.key.width = unit(1.5, "cm"),
    
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    
    panel.border = element_rect(color = "black", linewidth = 0.8),
    
    plot.margin = ggplot2::margin(10, 15, 10, 15)
  )

# Show plot
p
























#### 3 Figure in One layer ####

# Install patchwork if not already installed
# install.packages("patchwork")
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(grid)  # for unit()


library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(grid)  # for unit()

bright_colors <- c(
  "BOD"          = "#FF4500",
  "COD"          = "#9A32CD",
  "EC"           = "#FF8C00",
  "SS"           = "#32CD32",
  "TDS"          = "#FFD700",
  "Chloride"     = "#A9A9A9",
  "DO"           = "#1E90FF",
  "pH"           = "#00CED1",
  "T-Alkalinity" = "#FF1493",
  "Turbidity"    = "#000000"
)

all_parameters <- c("BOD", "COD", "EC", "SS", "TDS",
                    "Chloride", "DO", "pH", "T-Alkalinity", "Turbidity")

font_size <- 11

create_plot <- function(location_name, panel_letter) {
  df_plot <- df_long %>%
    filter(Location == location_name) %>%
    group_by(Year, Parameter) %>%
    summarise(MeanValue = mean(Value, na.rm = TRUE), .groups = "drop") %>%
    tidyr::complete(Year, Parameter = all_parameters)
  
  y_min <- min(df_plot$MeanValue, na.rm = TRUE)
  y_max <- max(df_plot$MeanValue, na.rm = TRUE)
  y_range <- y_max - y_min
  
  ggplot(df_plot, aes(x = Year, y = MeanValue, color = Parameter, group = Parameter)) +
    geom_line(linewidth = 1.5, alpha = 0.9, na.rm = TRUE) +
    geom_point(size = 4, shape = 21, fill = "white", stroke = 1, na.rm = TRUE) +
    scale_color_manual(values = bright_colors) +
    labs(x = "Year", y = "Mean Concentration (mg/L)", color = "Parameter") +
    coord_cartesian(ylim = c(y_min - 0.35 * y_range, y_max)) +
    # keep only panel letter under each plot
    annotate(
      "text",
      x = mean(range(df_plot$Year, na.rm = TRUE)),
      y = y_min - 0.30 * y_range,
      label = paste0("(", tolower(panel_letter), ")"),
      hjust = 0.5, vjust = 1,
      size = font_size / 2.5
    ) +
    theme_minimal(base_size = font_size) +
    theme(
      plot.title       = element_text(size = font_size, hjust = 0.5),
      axis.title       = element_text(size = font_size),
      axis.text        = element_text(size = font_size - 1, color = "black"),
      legend.title     = element_text(size = font_size - 1),
      legend.text      = element_text(size = font_size - 2),
      legend.key.width = unit(1.5, "cm"),
      legend.position  = "top",
      panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      panel.border     = element_blank(),
      plot.margin      = margin(t = 5, r = 10, b = 15, l = 10)
    )
}

p1 <- create_plot("Hazaribag", "a")
p2 <- create_plot("Mirpur Bridge", "b")
p3 <- create_plot("Kamrangir Char", "c")

combined_plot <- p1 + p2 + p3 +
  plot_layout(ncol = 3, guides = "collect") &
  theme(
    legend.position = "top",
    panel.spacing   = unit(0.5, "lines")
  )

combined_plot <- combined_plot +
  plot_annotation(
    caption = "Figure 4: Temporal Trends of Water Quality Parameters at (a)Hazaribag;(b)Mirpur Bridge; and (c)Kamrangir Char",
    theme = theme(
      plot.caption = element_text(
        size   = font_size,
        hjust  = 0.5,
        vjust  = 0,
        margin = margin(t = -5)
      )
    )
  )

combined_plot










library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(grid)  # for unit()

# slightly more saturated / vivid colors
bright_colors <- c(
  "BOD"          = "#FF3300",  # brighter orange-red
  "COD"          = "#9933FF",  # vivid purple
  "EC"           = "#FF9900",  # bright orange
  "SS"           = "#33CC33",  # bright green
  "TDS"          = "#FFDD00",  # bright yellow
  "Chloride"     = "#666666",  # darker gray for contrast
  "DO"           = "#0088FF",  # bright blue
  "pH"           = "#00E0E0",  # bright cyan
  "T-Alkalinity" = "#FF33AA",  # vivid pink
  "Turbidity"    = "#000000"   # black
)

all_parameters <- c("BOD", "COD", "EC", "SS", "TDS",
                    "Chloride", "DO", "pH", "T-Alkalinity", "Turbidity")

font_size <- 11

create_plot <- function(location_name, panel_letter) {
  df_plot <- df_long %>%
    filter(Location == location_name) %>%
    group_by(Year, Parameter) %>%
    summarise(MeanValue = mean(Value, na.rm = TRUE), .groups = "drop") %>%
    tidyr::complete(Year, Parameter = all_parameters)
  
  y_min <- min(df_plot$MeanValue, na.rm = TRUE)
  y_max <- max(df_plot$MeanValue, na.rm = TRUE)
  y_range <- y_max - y_min
  
  ggplot(df_plot, aes(x = Year, y = MeanValue, color = Parameter, group = Parameter)) +
    geom_line(linewidth = 1.8, alpha = 1, na.rm = TRUE) +   # thicker, fully opaque
    geom_point(size = 4.2, shape = 21, fill = "white", stroke = 1.2, na.rm = TRUE) +
    scale_color_manual(values = bright_colors) +
    labs(x = "Year", y = "Mean Concentration (mg/L)", color = "Parameter") +
    coord_cartesian(ylim = c(y_min - 0.35 * y_range, y_max)) +
    annotate(
      "text",
      x = mean(range(df_plot$Year, na.rm = TRUE)),
      y = y_min - 0.30 * y_range,
      label = paste0("(", tolower(panel_letter), ")"),
      hjust = 0.5, vjust = 1,
      size = font_size / 2.5
    ) +
    theme_minimal(base_size = font_size) +
    theme(
      plot.title       = element_text(size = font_size, hjust = 0.5),
      axis.title       = element_text(size = font_size),
      axis.text        = element_text(size = font_size - 1, color = "black"),
      legend.title     = element_text(size = font_size - 1),
      legend.text      = element_text(size = font_size - 2),
      legend.key.width = unit(1.5, "cm"),
      legend.position  = "top",
      # brighter look: white panel and lighter grid
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.grid.major = element_line(color = "grey85", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      panel.border     = element_blank(),
      plot.margin      = margin(t = 5, r = 10, b = 15, l = 10)
    )
}

p1 <- create_plot("Hazaribag", "a")
p2 <- create_plot("Mirpur Bridge", "b")
p3 <- create_plot("Kamrangir Char", "c")

combined_plot <- p1 + p2 + p3 +
  plot_layout(ncol = 3, guides = "collect") &
  theme(
    legend.position = "top",
    panel.spacing   = unit(0.5, "lines")
  )

combined_plot <- combined_plot +
  plot_annotation(
    caption = "Figure 4: Temporal Trends of Water Quality Parameters at (a)Hazaribag; (b)Mirpur Bridge; and (c)Kamrangir Char",
    theme = theme(
      plot.caption = element_text(
        size   = font_size,
        hjust  = 0.5,
        vjust  = 0,
        margin = margin(t = -5)
      )
    )
  )

combined_plot















#### Overall Trend ####
# =========================================
# Water Quality Temporal Trends (3 Locations)
# Q1 Journal Style | 10 Parameters (Fixed Names)
# =========================================

library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

# ---- Define 10 parameters EXACTLY as in data ----
all_parameters <- c(
  "BOD", "Cloride", "COD",
  "DO", "EC", "pH",
  "SS", "T-Alkainity", "TDS",
  "Trurbidity"
)

# ---- Year-wise average (all 3 locations) ----
yearly_avg <- df_long %>%
  group_by(Year, Location, Parameter) %>%
  summarise(
    Avg_Value = mean(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  complete(
    Year = full_seq(Year, 1),
    Location = unique(Location),
    Parameter = all_parameters,
    fill = list(Avg_Value = NA)
  )

# ---- Factor order for facets ----
yearly_avg$Parameter <- factor(yearly_avg$Parameter, levels = all_parameters)

# ---- Location order & colors ----
yearly_avg$Location <- factor(
  yearly_avg$Location,
  levels = c("Hazaribag", "Kamrangir Char", "Mirpur Bridge")
)

location_colors <- c(
  "Hazaribag" = "#E41A1C",       # Red
  "Kamrangir Char" = "#377EB8", # Blue
  "Mirpur Bridge" = "#4DAF4A"   # Green
)

# ---- Plot (same-to-same as reference figure) ----
ggplot(
  yearly_avg,
  aes(x = Year, y = Avg_Value, color = Location, group = Location)
) +
  geom_line(linewidth = 1.2, na.rm = TRUE) +
  geom_point(
    size = 3, shape = 21, fill = "white",
    stroke = 1, na.rm = TRUE
  ) +
  facet_wrap(~ Parameter, scales = "free_y", ncol = 3) +
  scale_color_manual(values = location_colors) +
  scale_x_continuous(
    breaks = seq(min(yearly_avg$Year), max(yearly_avg$Year), by = 1)
  ) +
  labs(
    title = "Temporal Trends in Water Quality Parameters",
    x = "Year",
    y = "Average Value",
    color = "Location"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5, color = "gray30"),
    strip.text = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(color = "gray20"),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90")
  ) +
  guides(color = guide_legend(override.aes = list(size = 4)))








library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

# ---- Define 10 parameters EXACTLY as in data ----
all_parameters <- c(
  "BOD", "Cloride", "COD",
  "DO", "EC", "pH",
  "SS", "T-Alkainity", "TDS",
  "Trurbidity"
)

# ---- Year-wise average (all 3 locations) ----
yearly_avg <- df_long %>%
  group_by(Year, Location, Parameter) %>%
  summarise(
    Avg_Value = mean(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  complete(
    Year = full_seq(Year, 1),
    Location = unique(Location),
    Parameter = all_parameters,
    fill = list(Avg_Value = NA)
  )

# ---- Factor order for facets ----
yearly_avg$Parameter <- factor(yearly_avg$Parameter, levels = all_parameters)

# ---- Location order & colors ----
yearly_avg$Location <- factor(
  yearly_avg$Location,
  levels = c("Hazaribag", "Kamrangir Char", "Mirpur Bridge")
)

location_colors <- c(
  "Hazaribag" = "#E41A1C",
  "Kamrangir Char" = "#377EB8",
  "Mirpur Bridge" = "#4DAF4A"
)

# ---- Plot ----
ggplot(
  yearly_avg,
  aes(x = Year, y = Avg_Value, color = Location, group = Location)
) +
  geom_line(linewidth = 1.2, na.rm = TRUE) +
  geom_point(
    size = 3, shape = 21, fill = "white",
    stroke = 1, na.rm = TRUE
  ) +
  facet_wrap(~ Parameter, scales = "free_y", ncol = 3) +
  scale_color_manual(values = location_colors) +
  scale_x_continuous(
    breaks = seq(min(yearly_avg$Year), max(yearly_avg$Year), by = 1)
  ) +
  labs(
    x = "Year",
    y = "Average Value",
    color = "Location",
    caption = "Figure 3: Temporal trends in water quality parameters"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    
    # Bottom caption (no bold)
    plot.caption = element_text(
      size = 14,
      hjust = 0.5,
      margin = ggplot2::margin(t = 12)
    ),
    
    # Facet labels (no bold, same size family)
    strip.text = element_text(size = 12),
    
    # Axis text
    axis.text.x = element_text(size = 13, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 13),
    
    # Axis titles (same size, no bold)
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    
    # Legend clean
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90")
  ) +
  guides(color = guide_legend(override.aes = list(size = 4)))














#### Violin ####

# ============================================================
# Violin Plots of Water Quality Parameters
# Polished for Q1 journal style
# ============================================================


# ============================================================
# Violin Plots of Water Quality Parameters
# Q1 Journal Style | 10 Parameters (Clean Labels)
# ============================================================

library(dplyr)
library(ggplot2)

# ---- Raw parameter names (as in df_long) ----
raw_params <- c(
  "BOD", "Cloride", "COD",
  "DO", "EC", "pH",
  "SS", "T-Alkainity", "TDS",
  "Trurbidity"
)

# ---- Clean labels for plotting ----
param_labels <- c(
  "BOD" = "BOD",
  "Cloride" = "Chloride",
  "COD" = "COD",
  "DO" = "DO",
  "EC" = "EC",
  "pH" = "pH",
  "SS" = "SS",
  "T-Alkainity" = "T-Alkalinity",
  "TDS" = "TDS",
  "Trurbidity" = "Turbidity"
)

# ============================================================
# FIGURE 1: Mirpur Bridge – 10 Parameters
# ============================================================

mirpur_data <- df_long %>%
  filter(Location == "Mirpur Bridge",
         Parameter %in% raw_params) %>%
  mutate(
    Parameter = factor(Parameter,
                       levels = raw_params,
                       labels = param_labels)
  )

ggplot(mirpur_data, aes(x = Parameter, y = Value)) +
  geom_violin(
    fill = "#4DAF4A",
    color = "gray30",
    alpha = 0.7,
    trim = FALSE,
    na.rm = TRUE
  ) +
  geom_boxplot(
    width = 0.12,
    outlier.shape = NA,
    fill = "white",
    color = "black",
    linewidth = 0.8,
    na.rm = TRUE
  ) +
  geom_jitter(
    width = 0.08,
    size = 1.5,
    alpha = 0.6,
    color = "black",
    na.rm = TRUE
  ) +
  labs(
    title = "Distribution of Water Quality Parameters",
    subtitle = "Mirpur Bridge (2010–2023)",
    x = "Parameter",
    y = "Observed Values"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5, color = "gray30"),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90")
  )

# ============================================================
# FIGURE 2: All Locations – 30 Distributions
# ============================================================

all_data <- df_long %>%
  filter(Parameter %in% raw_params) %>%
  mutate(
    Parameter = factor(Parameter,
                       levels = raw_params,
                       labels = param_labels),
    Location = factor(Location,
                      levels = c("Hazaribag",
                                 "Kamrangir Char",
                                 "Mirpur Bridge"))
  )

location_colors <- c(
  "Hazaribag" = "#E41A1C",
  "Kamrangir Char" = "#377EB8",
  "Mirpur Bridge" = "#4DAF4A"
)

ggplot(all_data, aes(x = Location, y = Value, fill = Location)) +
  geom_violin(
    color = "gray30",
    alpha = 0.7,
    trim = FALSE,
    na.rm = TRUE
  ) +
  geom_boxplot(
    width = 0.12,
    outlier.shape = NA,
    fill = "white",
    color = "black",
    linewidth = 0.8,
    na.rm = TRUE
  ) +
  geom_jitter(
    aes(color = Location),
    width = 0.08,
    size = 1.5,
    alpha = 0.5,
    show.legend = FALSE,
    na.rm = TRUE
  ) +
  facet_wrap(~ Parameter, scales = "free_y", ncol = 3) +
  scale_fill_manual(values = location_colors) +
  scale_color_manual(values = location_colors) +
  labs(
    title = "Distribution of Water Quality Parameters Across Locations",
    x = "Location",
    y = "Observed Values",
    fill = "Location"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5, color = "gray30"),
    strip.text = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    axis.text.x = element_text(face = "bold")
  )










# ============================================================
# FIGURE 2: All Locations – 30 Distributions
# (Blue–Purple–Sky palette | Q1 safe, colorblind-friendly)
# ============================================================

all_data <- df_long %>%
  filter(Parameter %in% raw_params) %>%
  mutate(
    Parameter = factor(Parameter,
                       levels = raw_params,
                       labels = param_labels),
    Location = factor(
      Location,
      levels = c("Hazaribag",
                 "Kamrangir Char",
                 "Mirpur Bridge")
    )
  )

# ---- Journal-safe colors (no red/green) ----
location_colors <- c(
  "Hazaribag" = "#6A51A3",       # Purple
  "Kamrangir Char" = "#3182BD", # Blue
  "Mirpur Bridge" = "#9ECAE1"   # Sky blue
)

ggplot(all_data, aes(x = Location, y = Value, fill = Location)) +
  geom_violin(
    color = "gray30",
    alpha = 0.7,
    trim = FALSE,
    na.rm = TRUE
  ) +
  geom_boxplot(
    width = 0.12,
    outlier.shape = NA,
    fill = "white",
    color = "black",
    linewidth = 0.8,
    na.rm = TRUE
  ) +
  geom_jitter(
    aes(color = Location),
    width = 0.08,
    size = 1.5,
    alpha = 0.5,
    show.legend = FALSE,
    na.rm = TRUE
  ) +
  facet_wrap(~ Parameter, scales = "free_y", ncol = 3) +
  scale_fill_manual(values = location_colors) +
  scale_color_manual(values = location_colors) +
  labs(
    title = "Distribution of Water Quality Parameters Across Locations",
    x = "Location",
    y = "Observed Values",
    fill = "Location"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    strip.text = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    axis.text.x = element_text(face = "bold")
  )











# ============================================================
# FIGURE 2: All Locations – 30 Distributions
# (Blue–Purple–Sky palette | Q1 safe, colorblind-friendly)
# ============================================================

all_data <- df_long %>%
  filter(Parameter %in% raw_params) %>%
  mutate(
    Parameter = factor(Parameter,
                       levels = raw_params,
                       labels = param_labels),
    Location = factor(
      Location,
      levels = c("Hazaribag",
                 "Kamrangir Char",
                 "Mirpur Bridge")
    )
  )

# ---- Journal-safe colors (no red/green) ----
location_colors <- c(
  "Hazaribag" = "#6A51A3",       # Purple
  "Kamrangir Char" = "#3182BD", # Blue
  "Mirpur Bridge" = "#9ECAE1"   # Sky blue
)

ggplot(all_data, aes(x = Location, y = Value, fill = Location)) +
  geom_violin(
    color = "gray30",
    alpha = 0.7,
    trim = FALSE,
    na.rm = TRUE
  ) +
  geom_boxplot(
    width = 0.12,
    outlier.shape = NA,
    fill = "white",
    color = "black",
    linewidth = 0.8,
    na.rm = TRUE
  ) +
  geom_jitter(
    aes(color = Location),
    width = 0.08,
    size = 1.5,
    alpha = 0.5,
    show.legend = FALSE,
    na.rm = TRUE
  ) +
  facet_wrap(~ Parameter, scales = "free_y", ncol = 3) +
  scale_fill_manual(values = location_colors) +
  scale_color_manual(values = location_colors) +
  
  # Move title to bottom as caption
  labs(
    x = "Location",
    y = "Observed Values",
    fill = "Location",
    caption = "Figure 2: Distribution of water quality parameters across locations"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    
    # Caption bottom
    plot.caption = element_text(
      size = 14,
      hjust = 0.5,
      margin = ggplot2::margin(t = 12)
    ),
    
    # Facet labels
    strip.text = element_text(size = 12),
    
    # Axis titles & text (uniform, no bold)
    axis.title = element_text(size = 13),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    
    # Legend
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    
    # Clean grid
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90")
  )
























