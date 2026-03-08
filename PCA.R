




#### Install required packages (only once) ####
packages <- c("tidyverse","caret","xgboost","e1071","randomForest","glmnet","data.table")
installed <- rownames(installed.packages())
for(p in packages) if(!p %in% installed) install.packages(p)
lapply(packages, library, character.only = TRUE)

install.packages("readxl")      # for reading Excel files
install.packages("mice")

####  Load necessary packages ####

library(readxl)
library(tidyverse)














#### Import Excel and clean column names ####



# Import dataset
file_path <- "D:/Research/Water Quality Buriganga/Copy of Final_Data_Sheet_of_Buriganga_river(2).xlsx"
df <- read_excel(file_path)

# View dataset in RStudio
#View(df)






##############################################
### Load libraries
##############################################
library(readxl)
library(dplyr)
library(tidyr)
library(mice)

##############################################
### Import dataset
##############################################
file_path <- "D:/Research/Water Quality Buriganga/Copy of Final_Data_Sheet_of_Buriganga_river(2).xlsx"
df <- read_excel(file_path)




# Basic exploration
head(df)
tail(df)
names(df)
str(df)
summary(df)
dim(df)

# Check missing values
colSums(is.na(df))














library(dplyr)
library(tidyr)
library(readxl)

# Import dataset
file_path <- "D:/Research/Water Quality Buriganga/Copy of Final_Data_Sheet_of_Buriganga_river(2).xlsx"
df <- read_excel(file_path)

# Fill down ID variables
df <- df %>%
  fill(Location, Symbol, Parameter, .direction = "down")

# Month rename map
month_map <- c(
  "J...5"  = "January",
  "F"      = "February",
  "M...7"  = "March",
  "A...8"  = "April",
  "M...9"  = "May",
  "J...10" = "June",
  "J...11" = "July",
  "A...12" = "August",
  "S"      = "September",
  "O"      = "October",
  "N"      = "November",
  "D"      = "December"
)

# Only existing columns
existing_month_cols <- intersect(names(df), names(month_map))

# Rename safely
df <- df %>%
  rename_with(~ month_map[.x], all_of(existing_month_cols))

# Prepare new column names for pivoting
renamed_months <- month_map[existing_month_cols]

# Convert ALL month columns to numeric safely
df <- df %>%
  mutate(across(all_of(renamed_months), ~as.numeric(as.character(.x))))

# Check that all month columns are numeric
sapply(df[renamed_months], class)




# Convert all month columns to numeric safely using type.convert
df <- df %>%
  mutate(across(all_of(renamed_months), ~as.numeric(type.convert(.x, as.is = TRUE))))

# Check that all month columns are numeric
sapply(df[renamed_months], class)





# Convert all month columns to numeric safely
df <- df %>%
  mutate(across(all_of(renamed_months), ~as.numeric(as.character(.x))))

# Check that conversion worked
sapply(df[renamed_months], class)




# Convert only character month columns to numeric
df <- df %>%
  mutate(across(all_of(renamed_months), ~ if(is.character(.x)) as.numeric(.x) else .x))

# Verify all month columns are now numeric
sapply(df[renamed_months], class)



# Identify month columns that are still character
char_months <- renamed_months[sapply(df[renamed_months], class) == "character"]

# Convert them to numeric
df <- df %>%
  mutate(across(all_of(char_months), as.numeric))

# Verify again
sapply(df[renamed_months], class)



# Convert all month columns to numeric explicitly
df <- df %>%
  mutate(across(all_of(renamed_months), ~as.numeric(as.character(.))))

# Verify conversion
sapply(df[renamed_months], class)


# Force all month columns to numeric
df[renamed_months] <- lapply(df[renamed_months], function(x) as.numeric(as.character(x)))

# Verify conversion
sapply(df[renamed_months], class)




# Check which of the renamed month columns actually exist
existing_renamed_months <- intersect(names(df), renamed_months)
existing_renamed_months



# Pivot safely using only existing columns
df_long <- df %>%
  pivot_longer(
    cols = all_of(existing_renamed_months),
    names_to = "Month",
    values_to = "Value"
  )

# Check the result
head(df_long)


#### Summary statistics by Parameter and Month####
df_long %>%
  group_by(Parameter, Month) %>%
  summarise(
    Mean = mean(Value, na.rm = TRUE),
    SD   = sd(Value, na.rm = TRUE),
    n    = n()
  ) %>%
  arrange(Parameter, match(Month, month.name))

# Example: Plot trends over months for a specific Parameter


library(ggplot2)

ggplot(df_long %>% filter(Parameter == "pH"), 
       aes(x = factor(Month, levels = month.name), y = Value, group = Location, color = Location)) +
  geom_line() +
  geom_point() +
  labs(title = "Monthly pH Trend by Location", x = "Month", y = "pH Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # rotate x-axis labels for readability






#### Missing ####
library(dplyr)

# Count missing values per Parameter
df_long %>%
  group_by(Parameter) %>%
  summarise(Missing = sum(is.na(Value)),
            Total = n(),
            PercentMissing = round(Missing / Total * 100, 2))



#View(df_long)


# Show rows with missing Values
df_long %>% 
  filter(is.na(Value)) %>%
  arrange(Parameter, Location, Year, Month)


#### Summary plot ####
df_long %>%
  group_by(Parameter) %>%
  summarise(
    Min = min(Value, na.rm = TRUE),
    Q1 = quantile(Value, 0.25, na.rm = TRUE),
    Median = median(Value, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE),
    Max = max(Value, na.rm = TRUE)
  )



#### Outliers ####




library(ggplot2)

ggplot(df_long, aes(x = Parameter, y = Value)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 8) +
  labs(title = " Outliers by Parameter", x = "Parameter", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))










library(ggplot2)

ggplot(df_long, aes(x = Parameter, y = Value)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 8) +
  labs(title = "Check for Outliers by Parameter", x = "Parameter", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




is.na(df_long)

glimpse(df_long)







#### STEP 1: Long → Wide Format####




library(tidyverse)

df_wide <- df_long %>%
  select(Location, Year, Month, Parameter, Value) %>%
  pivot_wider(
    names_from = Parameter,
    values_from = Value
  )

ncol(df_wide)
sapply(df_wide, class)

ncol(df)

sapply(df, class)

View(df)








#### Heatmap ####
library(tidyverse)
library(ggcorrplot)

library(ggcorrplot)



ggcorrplot(
  cor_mat,
  type = "lower",
  hc.order = TRUE,
  lab = TRUE,
  lab_size = 3,
  method = "square",
  colors = c("#2166AC", "white", "#B2182B")
) +
  labs(
    title = "Correlation Heatmap of Water Quality Parameters",
    subtitle = "Pearson correlation (pairwise complete observations)"
  ) +
  theme_minimal()






library(ggplot2)
library(reshape2)

# Convert correlation matrix to long format
cor_df <- melt(cor_mat)

ggplot(cor_df, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(color = "white", linewidth = 0.4) +
  
  # Correlation values inside cells
  geom_text(aes(label = sprintf("%.2f", value)),
            size = 3, family = "serif") +
  
  # Color scale (blue–white–red)
  scale_fill_gradient2(
    low = "#2C3E75",
    mid = "#F7F7F7",
    high = "#B40426",
    midpoint = 0,
    limits = c(-1, 1),
    name = "Correlation Coefficient"
  ) +
  
  # Titles and labels
  labs(
    title = "Correlation Heatmap of Water Quality Parameters",
    subtitle = "Analysis of correlations between key water quality parameters",
    x = "Water Quality Parameters",
    y = "Water Quality Parameters"
  ) +
  
  # Theme styling to match your figure
  theme_minimal(base_family = "serif") +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    axis.text.y = element_text(size = 11),
    axis.title = element_text(size = 13, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    panel.grid = element_blank()
  )








library(ggplot2)
library(reshape2)

# Convert correlation matrix to long format
cor_df <- melt(cor_mat)

ggplot(cor_df, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(color = "white", linewidth = 0.5) +  # clean white borders
  
  # Correlation values inside cells
  geom_text(aes(label = sprintf("%.2f", value)),
            size = 3.5, family = "serif", color = "black") +
  
  # Softer blue-white-red gradient
  scale_fill_gradient2(
    low = "#1f77b4",   # calm blue
    mid = "#f7f7f7",   # white
    high = "#d62728",  # bright red
    midpoint = 0,
    limits = c(-1, 1),
    name = "Correlation"
  ) +
  
  # Titles and labels
  labs(
    title = "Correlation Heatmap of Water Quality Parameters",
    subtitle = "Key correlations between water quality metrics",
    x = "Parameters",
    y = "Parameters"
  ) +
  
  # Sleek minimal theme
  theme_minimal(base_family = "serif") +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, color = "#2C3E50"),
    plot.subtitle = element_text(size = 13, hjust = 0.5, color = "#34495E"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, color = "#2C3E50"),
    axis.text.y = element_text(size = 12, color = "#2C3E50"),
    axis.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#FFFFFF", color = NA)
  )















library(ggplot2)
library(reshape2)

#--------------------------------------------------
# Convert correlation matrix to long format
#--------------------------------------------------
cor_df <- melt(cor_mat)

#--------------------------------------------------
# Correlation Heatmap
#--------------------------------------------------
ggplot(cor_df, aes(x = Var2, y = Var1, fill = value)) +
  
  # Heatmap tiles
  geom_tile(color = "white", linewidth = 0.6) +
  
  # Correlation values inside cells
  geom_text(
    aes(label = sprintf("%.2f", value)),
    size = 3.8,
    family = "serif",
    color = "black"
  ) +
  
  # Blue – White – Red gradient (same as reference image)
  scale_fill_gradient2(
    low  = "#2C3E70",   # deep muted blue
    mid  = "#F2F2F2",   # light grey/white
    high = "#C0392B",   # deep red
    midpoint = 0,
    limits = c(-1, 1),
    name = "Correlation Coefficient"
  ) +
  
  # Titles and labels
  labs(
    title = "Correlation Heatmap of Water Quality Parameters",
    x = "Water Quality Parameters",
    y = "Water Quality Parameters"
  ) +
  
  # Journal-quality theme
  theme_minimal(base_family = "serif") +
  theme(
    plot.title = element_text(
      size = 22, face = "bold", hjust = 0.5
    ),
    plot.subtitle = element_text(
      size = 14, hjust = 0.5
    ),
    
    axis.text.x = element_text(
      angle = 45, hjust = 1, vjust = 1,
      size = 13, face = "bold"
    ),
    axis.text.y = element_text(
      size = 13, face = "bold"
    ),
    
    axis.title.x = element_text(
      size = 16, face = "bold",
      margin = ggplot2::margin(t = 12, unit = "pt")
    ),
    axis.title.y = element_text(
      size = 16, face = "bold",
      margin = ggplot2::margin(r = 12, unit = "pt")
    ),
    
    legend.position = "bottom",
    legend.title = element_text(size = 13, face = "bold"),
    legend.text  = element_text(size = 11),
    legend.key.width = unit(2.8, "cm"),
    
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA)
  )





ggplot(cor_df, aes(x = Var2, y = Var1, fill = value)) +
  
  # Heatmap tiles
  geom_tile(color = "white", linewidth = 0.5) +
  
  # Correlation values inside cells
  geom_text(
    aes(label = sprintf("%.2f", value)),
    size = 3.6,
    family = "serif",
    color = "black"
  ) +
  
  # Blue – White – Red gradient
  scale_fill_gradient2(
    low  = "#2C3E70",
    mid  = "#F2F2F2",
    high = "#C0392B",
    midpoint = 0,
    limits = c(-1, 1),
    name = "Correlation Coefficient"
  ) +
  
  labs(
    x = "Water Quality Parameters",
    y = "Water Quality Parameters",
    caption = "Figure 6: Correlation heatmap of water quality parameters"
  ) +
  
  theme_minimal(base_family = "serif") +
  theme(
    
    # Smaller, clean caption-style title at bottom
    plot.caption = element_text(
      size = 14,
      hjust = 0.5
    ),
    
    # Axis text (no bold)
    axis.text.x = element_text(
      angle = 45, hjust = 1, vjust = 1,
      size = 13
    ),
    axis.text.y = element_text(
      size = 13
    ),
    
    # Axis titles (no bold, same size)
    axis.title.x = element_text(
      size = 13,
      margin = ggplot2::margin(t = 10)
    ),
    axis.title.y = element_text(
      size = 13,
      margin = ggplot2::margin(r = 10)
    ),
    
    # Clean legend
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text  = element_text(size = 11),
    legend.key.width = unit(2.5, "cm"),
    
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA)
  )










ggplot(cor_df, aes(x = Var2, y = Var1, fill = value)) +
  
  # Heatmap tiles (slightly thinner border for sharp look)
  geom_tile(color = "white", linewidth = 0.4) +
  
  # Correlation values inside cells
  geom_text(
    aes(label = sprintf("%.2f", value)),
    size = 3.7,
    family = "serif",
    color = "black"
  ) +
  
  # Brighter, cleaner Blue – White – Red gradient
  scale_fill_gradient2(
    low  = "#1F4E9E",   # brighter deep blue
    mid  = "#FFFFFF",   # pure white
    high = "#D73027",   # brighter red
    midpoint = 0,
    limits = c(-1, 1),
    name = "Correlation Coefficient"
  ) +
  
  labs(
    x = "Water Quality Parameters",
    y = "Water Quality Parameters",
    caption = "Figure 6: Correlation heatmap of water quality parameters"
  ) +
  
  theme_minimal(base_family = "serif") +
  theme(
    
    # Clean caption
    plot.caption = element_text(
      size = 14,
      hjust = 0.5,
      margin = ggplot2::margin(t = 12)
    ),
    
    # Axis text
    axis.text.x = element_text(
      angle = 45, hjust = 1, vjust = 1,
      size = 13
    ),
    axis.text.y = element_text(
      size = 13
    ),
    
    # Axis titles
    axis.title.x = element_text(
      size = 13,
      margin = ggplot2::margin(t = 12)
    ),
    axis.title.y = element_text(
      size = 13,
      margin = ggplot2::margin(r = 12)
    ),
    
    # Legend cleaner spacing
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text  = element_text(size = 11),
    legend.key.width = unit(3, "cm"),
    legend.spacing.x = unit(0.4, "cm"),
    
    # Fully clean background
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )




# Numeric Variable
df_numeric <- df_wide %>%
  select(where(is.numeric))




# missing
df_numeric <- na.omit(df_numeric)






















# Standarizez
pca_res <- prcomp(df_numeric, scale. = TRUE)




# Eigen values
eigenvalues <- (pca_res$sdev)^2
eigenvalues



# Kaiser Rule 
which(eigenvalues > 1)






# Select (pH, DO, BOD, COD, TDS, Trurbidity, Cloride, SS, T-Alkainity, EC)

df_numeric <- df_wide %>%
  select(pH, DO, BOD, COD, TDS, Trurbidity, Cloride, SS, `T-Alkainity`, EC)

# missing values remove
df_numeric <- na.omit(df_numeric)

# Standardize & PCA
pca_res <- prcomp(df_numeric, scale. = TRUE)

# Eigen values
eigenvalues <- (pca_res$sdev)^2
eigenvalues

# Kaiser Rule: eigenvalue > 1
which(eigenvalues > 1)












# Scree Plot

plot(eigenvalues, type = "b",
     xlab = "Principal Component",
     ylab = "Eigenvalue",
     main = "Scree Plot")

abline(h = 1, col = "red", lty = 2)










var_explained <- eigenvalues / sum(eigenvalues) * 100

data.frame(
  Component = paste0("PC", 1:length(eigenvalues)),
  Eigenvalue = eigenvalues,
  Variance_Percent = round(var_explained, 2),
  Cumulative_Variance = round(cumsum(var_explained), 2)
)



# Eigenvalues
eigenvalues <- (pca_res$sdev)^2

# Variance explained (%)
var_explained <- eigenvalues / sum(eigenvalues) * 100

# Create data frame
pca_table <- data.frame(
  Component = paste0("PC", 1:length(eigenvalues)),
  Eigenvalue = eigenvalues,
  Variance_Percent = round(var_explained, 2),
  Cumulative_Variance = round(cumsum(var_explained), 2)
)

# Define path
path <- "D:/Research/Water Quality Buriganga/final paper for sir/Tables/PCA_Eigenvalues_Table.csv"

# Save CSV
write.csv(pca_table, path, row.names = FALSE)

# Confirm
cat("File saved to:\n", path)


#PCA Biplot (PC1 vs PC2)



library(factoextra)

fviz_pca_biplot(pca_res, 
                repel = TRUE,
                title = "PCA Biplot")





















#### Journal standard Figure ####
#1. Scree Plot (Kaiser Line + Elbow Clear)



library(ggplot2)

eigenvalues <- (pca_res$sdev)^2

scree_df <- data.frame(
  PC = 1:length(eigenvalues),
  Eigenvalue = eigenvalues
)

ggplot(scree_df, aes(x = PC, y = Eigenvalue)) +
  geom_point(size = 3) +
  geom_line() +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(
    title = "Scree Plot",
    x = "Principal Components",
    y = "Eigenvalue"
  ) +
  theme_bw()












library(ggplot2)

# Eigenvalues
eigenvalues <- (pca_res$sdev)^2

# Data frame for plotting
scree_df <- data.frame(
  PC = factor(1:length(eigenvalues)),
  Eigenvalue = eigenvalues
)

# Polished Scree Plot
ggplot(scree_df, aes(x = PC, y = Eigenvalue, group = 1)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4, shape = 21, fill = "white", stroke = 1.2) +
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 1) +
  labs(
    title = "Scree Plot of Principal Components",
    subtitle = "Kaiser Criterion (Eigenvalue > 1)",
    x = "Principal Components",
    y = "Eigenvalue"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )



# PC names
pc_names <- paste0("PC", 1:length(eigenvalues))

# Data frame for plotting
scree_df <- data.frame(
  PC = factor(pc_names, levels = pc_names),
  Eigenvalue = eigenvalues
)

# Scree Plot
ggplot(scree_df, aes(x = PC, y = Eigenvalue, group = 1)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4, shape = 21, fill = "white", stroke = 1.2) +
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 1) +
  labs(
    title = "Scree Plot of Principal Components",
    subtitle = "Kaiser Criterion (Eigenvalue > 1)",
    x = "Principal Components",
    y = "Eigenvalue"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )






# Extract loadings for PC1
pc1_loadings <- pca_res$rotation[,1]

# Create data frame for plotting
loadings_df <- data.frame(
  Parameter = names(pc1_loadings),
  Loading = pc1_loadings
)

# Plot
library(ggplot2)
ggplot(loadings_df, aes(x = Parameter, y = Loading)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "PC1 Loadings by Parameter",
       x = "Parameter",
       y = "Loading") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





#2. Variance Explained Bar Plot (PC-wise Contribution)


var_explained <- eigenvalues / sum(eigenvalues) * 100

var_df <- data.frame(
  PC = paste0("PC", 1:length(var_explained)),
  Variance = var_explained
)

ggplot(var_df, aes(x = PC, y = Variance)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Variance Explained by Principal Components",
    x = "Principal Components",
    y = "Variance Explained (%)"
  ) +
  theme_bw()




library(ggplot2)

var_explained <- eigenvalues / sum(eigenvalues) * 100

var_df <- data.frame(
  PC = factor(paste0("PC", 1:length(var_explained)), 
              levels = paste0("PC", 1:length(var_explained))),
  Variance = var_explained
)

ggplot(var_df, aes(x = PC, y = Variance, fill = Variance)) +
  
  # colored bars (journal style gradient)
  geom_bar(stat = "identity", color = "black", linewidth = 0.4) +
  scale_fill_gradient(low = "#0072B2", high = "#D55E00") +
  
  # labels & title
  labs(
    subtitle = "Percentage of total variance explained by each principal component",
    x = "Principal Components",
    y = "Variance Explained (%)",
    fill = "Variance (%)"
  ) +
  
  # clean journal theme
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 13),
    axis.title = element_text(size = 15, face = "bold"),
    axis.text = element_text(size = 12),
    
    # thin grid + no minor grid for journal look
    panel.grid.major = element_line(linetype = "dashed", color = "grey70"),
    panel.grid.minor = element_blank(),
    
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold")
  )





library(ggplot2)

var_explained <- eigenvalues / sum(eigenvalues) * 100

var_df <- data.frame(
  PC = factor(paste0("PC", 1:length(var_explained)), 
              levels = paste0("PC", 1:length(var_explained))),
  Variance = var_explained
)

ggplot(var_df, aes(x = PC, y = Variance, fill = Variance)) +
  
  geom_bar(stat = "identity", color = "black", linewidth = 0.4) +
  scale_fill_gradient(low = "#0072B2", high = "#D55E00") +
  
  labs(
    x = "Principal Components",
    y = "Variance Explained (%)",
    fill = "Variance (%)",
    caption = "Supplementary Figure S3: Prevalneces of total variance explained by each principal component"
  ) +
  
  theme_minimal(base_size = 15) +
  theme(
    
    plot.caption = element_text(
      size = 13,
      hjust = 0.5,
      margin = ggplot2::margin(t = 12)
    ),
    
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 12),
    
    panel.grid.major = element_line(linetype = "dashed", color = "grey75"),
    panel.grid.minor = element_blank(),
    
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text  = element_text(size = 11)
  )








library(ggplot2)

var_explained <- eigenvalues / sum(eigenvalues) * 100

var_df <- data.frame(
  PC = factor(paste0("PC", 1:length(var_explained)), 
              levels = paste0("PC", 1:length(var_explained))),
  Variance = var_explained
)

ggplot(var_df, aes(x = PC, y = Variance, fill = Variance)) +
  
  # Sharper clean bars
  geom_bar(stat = "identity", color = "black", linewidth = 0.35) +
  
  # Brighter, high-contrast gradient
  scale_fill_gradient(
    low = "#2C7FB8",     # cleaner blue
    high = "#E34A33",    # brighter orange-red
    name = "Variance (%)"
  ) +
  
  labs(
    x = "Principal Components",
    y = "Variance Explained (%)",
    caption = "Supplementary Figure S3: Percentage of total variance explained by each principal component"
  ) +
  
  theme_minimal(base_size = 15) +
  theme(
    
    # Clean centered caption
    plot.caption = element_text(
      size = 13,
      hjust = 0.5,
      margin = ggplot2::margin(t = 14)
    ),
    
    # Balanced axis typography
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 12),
    
    # Softer journal grid
    panel.grid.major = element_line(
      linetype = "dashed",
      color = "grey80",
      linewidth = 0.4
    ),
    panel.grid.minor = element_blank(),
    
    # Clean white background
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text  = element_text(size = 11)
  )














#3. PCA Biplot (PC1 vs PC2 – Publication Quality)



library(factoextra)

fviz_pca_biplot(
  pca_res,
  axes = c(1, 2),
  repel = TRUE,
  title = "PCA Biplot (PC1 vs PC2)"
)










library(factoextra)

# PCA Biplot with contribution colors and clean formatting
fviz_pca_biplot(
  pca_res,
  axes = c(1, 2),
  repel = TRUE,
  col.var = "contrib",                     # color variables by contributions
  gradient.cols = c("#0072B2", "#E69F00", "#D55E00"),
  col.ind = "grey30",                      # sample points color
  pointshape = 19,
  pointsize = 2,
  title = "Principal Component Analysis Biplot"
) +
  labs(
    subtitle = paste0(
      "PC1 (", round(summary(pca_res)$importance[2,1] * 100, 1), "%) ",
      "vs PC2 (", round(summary(pca_res)$importance[2,2] * 100, 1), "%)"
    ),
    caption = "Loadings calculated using PCA analysis.\nColor intensity represents variable contribution."
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(size = 11)
  )















library(factoextra)

fviz_pca_biplot(
  pca_res,
  axes = c(1, 2),
  repel = TRUE,
  
  # Variable colors based on contribution
  col.var = "contrib",
  gradient.cols = c("#0072B2", "#E69F00", "#D55E00"),
  
  # Individual sample points
  col.ind = "grey40",
  pointshape = 19,
  pointsize = 2.5,
  
  title = "Principal Component Analysis Biplot"
) +
  
  # Subtitle and caption
  labs(
    subtitle = paste0(
      "PC1 (", round(summary(pca_res)$importance[2,1] * 100, 1), "%)  •  ",
      "PC2 (", round(summary(pca_res)$importance[2,2] * 100, 1), "%)"
    ),
    caption = "Biplot showing both variable loadings and sample scores.\nColor intensity indicates variable contribution."
  ) +
  
  # Axis labels using PCA % variance
  xlab(paste0("Dim1 (", round(summary(pca_res)$importance[2,1] * 100, 1), "%)")) +
  ylab(paste0("Dim2 (", round(summary(pca_res)$importance[2,2] * 100, 1), "%)")) +
  
  # Clean, polished theme
  theme_minimal(base_size = 14) +
  theme(
    plot.title    = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 14),
    plot.caption  = element_text(size = 11, color = "grey30"),
    
    axis.title    = element_text(size = 16, face = "bold"),
    axis.text     = element_text(size = 13),
    
    # dashed grid like your previous figures
    panel.grid.major = element_line(linetype = "dashed", color = "grey75"),
    panel.grid.minor = element_blank()
  )


#PCA Individuals Plot (Only Samples)





fviz_pca_ind(
  pca_res,
  axes = c(1, 2),
  geom = "point",
  title = "PCA Score Plot"
)








library(factoextra)

fviz_pca_ind(
  pca_res,
  axes = c(1, 2),
  geom = "point",
  pointshape = 19,
  pointsize = 2.5,
  col.ind = "grey30",
  repel = TRUE,
  title = "PCA Score Plot"
) +
  labs(
    subtitle = paste0(
      "Sample distribution along principal components\n",
      "Dim1 (", round(summary(pca_res)$importance[2,1] * 100, 1), "%) vs ",
      "Dim2 (", round(summary(pca_res)$importance[2,2] * 100, 1), "%)"
    ),
    caption = "Scores represent sample projections onto PC1 and PC2."
  ) +
  
  # ---- AXIS LABELS EXACT FORMAT ----
xlab(
  paste0("Dim1 (",
         round(summary(pca_res)$importance[2,1] * 100, 1),
         "%)")
) +
  ylab(
    paste0("Dim2 (",
           round(summary(pca_res)$importance[2,2] * 100, 1),
           "%)")
  ) +
  
  # ---- MATCH VISUAL STYLE OF YOUR FIGURE ----
theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(size = 12),
    
    # dashed lines like loading plot
    panel.grid.major = element_line(linetype = "dashed", color = "grey70"),
    panel.grid.minor = element_blank(),
    
    axis.title = element_text(size = 16, face = "bold")
  )









# 5. PCA Variables / Loadings Plot (Only Parameters)


fviz_pca_var(
  pca_res,
  axes = c(1, 2),
  repel = TRUE,
  title = "PCA Loadings Plot"
)













library(factoextra)

fviz_pca_var(
  pca_res,
  axes = c(1, 2),
  repel = TRUE,
  
  # color variables by contribution (same as your figure)
  col.var = "contrib",
  gradient.cols = c("#0072B2", "#E69F00", "#D55E00"),
  
  # Draw circle like your plot
  geom = c("arrow", "text"),
  col.circle = "grey40",
  circle.size = 1
) +
  
  # ---- TITLE & SUBTITLE EXACTLY LIKE YOUR FIGURE ----
ggtitle("Principal Component Analysis Loading Plot") +
  labs(
  ) +
  
  # ---- AXIS LABELS EXACT SAME FORMAT ----
xlab(
  paste0("Dim1 (",
         round(summary(pca_res)$importance[2,1] * 100, 1),
         "%)")
) +
  ylab(
    paste0("Dim2 (",
           round(summary(pca_res)$importance[2,2] * 100, 1),
           "%)")
  ) +
  
  # ---- MATCH VISUAL STYLE ----
theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(size = 12),
    
    # dashed lines like your figure
    panel.grid.major = element_line(linetype = "dashed", color = "grey70"),
    panel.grid.minor = element_blank(),
    
    # axis title bold
    axis.title = element_text(size = 16, face = "bold")
  )








library(factoextra)

fviz_pca_var(
  pca_res,
  axes = c(1, 2),
  repel = TRUE,
  
  # color variables by contribution
  col.var = "contrib",
  gradient.cols = c("#0072B2", "#E69F00", "#D55E00"),
  
  # circle + arrows
  geom = c("arrow", "text"),
  col.circle = "grey40",
  circle.size = 1
) +
  
  # Remove top title
  labs(
    caption = "Figure 7: Principal component analysis loading plot"
  ) +
  
  # Axis labels (same format)
  xlab(
    paste0("Dim1 (",
           round(summary(pca_res)$importance[2,1] * 100, 1),
           "%)")
  ) +
  ylab(
    paste0("Dim2 (",
           round(summary(pca_res)$importance[2,2] * 100, 1),
           "%)")
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    
    # Bottom title (no bold, smaller size)
    plot.caption = element_text(
      size = 14,
      hjust = 0.5,
      margin = ggplot2::margin(t = 12)
    ),
    
    # Clean dashed grid
    panel.grid.major = element_line(
      linetype = "dashed",
      color = "grey75",
      linewidth = 0.4
    ),
    panel.grid.minor = element_blank(),
    
    # Axis titles (no bold)
    axis.title = element_text(size = 14),
    
    # Axis text clean
    axis.text = element_text(size = 12)
  )









library(factoextra)

fviz_pca_var(
  pca_res,
  axes = c(1, 2),
  repel = TRUE,
  
  # Remove default top title
  title = NULL,
  
  # color variables by contribution
  col.var = "contrib",
  gradient.cols = c("#0072B2", "#E69F00", "#D55E00"),
  
  geom = c("arrow", "text"),
  col.circle = "grey40",
  circle.size = 1
) +
  
  # Ensure no title at all
  ggtitle(NULL) +
  
  labs(
    caption = "Figure 7: Principal component analysis loading plot"
  ) +
  
  xlab(
    paste0("Dim1 (",
           round(summary(pca_res)$importance[2,1] * 100, 1),
           "%)")
  ) +
  ylab(
    paste0("Dim2 (",
           round(summary(pca_res)$importance[2,2] * 100, 1),
           "%)")
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    
    plot.title = element_blank(),   # extra safe
    
    plot.caption = element_text(
      size = 14,
      hjust = 0.5,
      margin = ggplot2::margin(t = 12)
    ),
    
    panel.grid.major = element_line(
      linetype = "dashed",
      color = "grey75",
      linewidth = 0.4
    ),
    panel.grid.minor = element_blank(),
    
    axis.title = element_text(size = 14),
    axis.text  = element_text(size = 12)
  )













#### Missing Handling ####

library(mice)

# Select only numeric columns for imputation
num_cols <- c('pH','DO','BOD','COD','TDS','Trurbidity','Cloride','SS','T-Alkainity','EC')

# Run mice with predictive mean matching
set.seed(42)
mice_mod <- mice(df_wide[, num_cols], m = 1, method = 'pmm', maxit = 5, printFlag = TRUE)

# Replace original numeric columns with imputed values
df_wide[, num_cols] <- complete(mice_mod)

# Verify no more missing values
colSums(is.na(df_wide))
any(is.na(df_wide))







#### Box plot before and after missing ####
library(ggplot2)
library(dplyr)
library(tidyr)

# 1️⃣ Convert df_long to wide for comparison
df_wide_before <- df_long %>%
  pivot_wider(names_from = Parameter, values_from = Value)

# 2️⃣ Remove outliers & create df_wide_after (already done, but let's keep for plotting)
df_wide_after <- df_wide

# 3️⃣ Melt wide data back to long for ggplot
df_long_before <- df_wide_before %>%
  pivot_longer(cols = num_cols, names_to = "Parameter", values_to = "Value")

df_long_after <- df_wide_after %>%
  pivot_longer(cols = num_cols, names_to = "Parameter", values_to = "Value")

# 4️⃣ Add a column to indicate before vs after
df_long_before$Stage <- "Before"
df_long_after$Stage <- "After"

# 5️⃣ Combine
df_long_combined <- bind_rows(df_long_before, df_long_after)

# 6️⃣ Boxplot
ggplot(df_long_combined, aes(x = Parameter, y = Value, fill = Stage)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 8, alpha = 0.6) +
  labs(title = "Boxplots Before and After Outlier Removal & Imputation",
       y = "Value", x = "Parameter") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






#### Outlier Removal & Missing Handling ####
library(dplyr)
library(mice)
library(dplyr)
library(mice)

# 1️⃣ Convert df_long back to wide format for imputation
df_wide <- df_long %>%
  pivot_wider(names_from = Parameter, values_from = Value)

# 2️⃣ Detect outliers using IQR method and replace them with NA
num_cols <- c('pH','DO','BOD','COD','TDS','Trurbidity','Cloride','SS','T-Alkainity','EC')

for(col in num_cols){
  Q1 <- quantile(df_wide[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df_wide[[col]], 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  
  # Replace outliers with NA
  df_wide[[col]][df_wide[[col]] < lower | df_wide[[col]] > upper] <- NA
}

# 3️⃣ Impute missing values (including outliers replaced by NA) using mice
set.seed(42)
mice_mod <- mice(df_wide[, num_cols], m = 1, method = 'pmm', maxit = 5, printFlag = TRUE)

# 4️⃣ Replace original numeric columns with imputed values
df_wide[, num_cols] <- complete(mice_mod)

# 5️⃣ Verify no more missing values
colSums(is.na(df_wide))
any(is.na(df_wide))









# Drop leftover original month columns
original_month_cols <- c('J...5','F','M...7','A...8','M...9','J...10','J...11','A...12','S','O','N','D')
df_wide <- df_wide %>% select(-all_of(original_month_cols))

# Check again for missing values
colSums(is.na(df_wide))
any(is.na(df_wide))



library(ggplot2)
library(dplyr)
library(tidyr)

# Select numeric columns for visualization
num_cols <- c('pH','DO','BOD','COD','TDS','Trurbidity','Cloride','SS','T-Alkainity','EC')

# 1️⃣ Prepare data for "before" boxplot
df_before <- df_wide %>%
  select(Location, all_of(num_cols)) %>%
  pivot_longer(cols = all_of(num_cols), names_to = "Parameter", values_to = "Value") %>%
  mutate(Stage = "Before")

# 2️⃣ Prepare data for "after" boxplot
# Remove outliers using IQR (same as before)
df_after <- df_wide
for(col in num_cols){
  Q1 <- quantile(df_after[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df_after[[col]], 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  
  df_after[[col]][df_after[[col]] < lower | df_after[[col]] > upper] <- NA
}

df_after <- df_after %>%
  select(Location, all_of(num_cols)) %>%
  pivot_longer(cols = all_of(num_cols), names_to = "Parameter", values_to = "Value") %>%
  mutate(Stage = "After")

# 3️⃣ Combine before and after
df_box <- bind_rows(df_before, df_after)

# 4️⃣ Plot side-by-side boxplots
ggplot(df_box, aes(x = Parameter, y = Value, fill = Stage)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 8, position = position_dodge(width = 0.8)) +
  labs(title = "Comparison of Outliers Before and After Removal",
       x = "Parameter", y = "Values") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




#### Define water quality objectives (aligned to your data)####





# Define water quality objectives
objectives <- c(
  pH = 8.5,
  Trurbidity = 5,
  Cloride = 250,
  `T-Alkainity` = 200,
  TDS = 500
)








#### Define CCME WQI computation function ####

compute_ccme_wqi <- function(df, objectives){
  params <- names(objectives)
  n <- length(params)
  
  wqi <- numeric(nrow(df))
  
  for(i in 1:nrow(df)){
    values <- as.numeric(df[i, params])
    
    # Replace NA or Inf with 0 (safety)
    values[!is.finite(values)] <- 0
    
    # 1️⃣ F1: Scope (% of variables exceeding objectives)
    f1 <- sum(values > unlist(objectives)) / n * 100
    
    # 2️⃣ F2: Frequency (% of individual tests exceeding objectives)
    # Since we have 1 observation per parameter, F2 ≈ F1
    f2 <- f1
    
    # 3️⃣ F3: Amplitude (excursions)
    excursions <- (values / unlist(objectives) - 1)
    excursions[excursions < 0] <- 0
    f3 <- sum(excursions) / n / (0.01 * sum(excursions)/n + 0.01)
    
    # 4️⃣ CCME WQI
    wqi[i] <- 100 - sqrt(f1^2 + f2^2 + f3^2) / 1.732
  }
  
  return(wqi)
}


####Compute WQI for your dataset####

df_wide$WQI <- compute_ccme_wqi(df_wide, objectives)

# Verify results

summary(df_wide$WQI)
sum(is.na(df_wide$WQI))  # Should be 0
















