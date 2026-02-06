# Econometrics with R - Final Project
# Dataset URL: https://www.kaggle.com/datasets/ahmettyilmazz/fuel-consumption?resource=download

# Loading relevant libraries
library(ggplot2)
library(corrplot)
library(GGally)
library(reshape2)
library(readxl)
library(caret)
library(dplyr)
# Importing the dataset---------------------------------------------------------

setwd("C:/Users/Lax/Downloads/אקונומטריקה עם R/עבודת סוף -קובץ מאושר עי ולדימיר")
df <- read_excel("add age of car.xlsx")

# Display basic info about the data---------------------------------------------

dim(df)
head(df)
tail(df)

#-------------------------------------------------------------------------------

unique(df$MAKE)
unique(df$TRANSMISSION)
unique(df$FUEL)

#Create a mapping from MAKE to Continent
continent_map <- list(
  America = c("ACURA", "BUICK", "CADILLAC", "CHEVROLET", "CHRYSLER", "DODGE", "FORD", "GMC",
              "HUMMER", "JEEP", "LINCOLN", "OLDSMOBILE", "PLYMOUTH", "PONTIAC", "RAM",
              "SATURN", "SCION", "SRT", "GENESIS", "HYUNDAI", "KIA", "DAEWOO"),
  Europe = c("AUDI", "BMW", "MERCEDES-BENZ", "VOLKSWAGEN", "PORSCHE", "SMART", "MINI", "SAAB",
             "VOLVO", "JAGUAR", "LAND ROVER", "ASTON MARTIN", "ROLLS-ROYCE", "BENTLEY",
             "FIAT", "ALFA ROMEO", "FERRARI", "MASERATI", "LAMBORGHINI", "BUGATTI"),
  Asia = c("HONDA", "TOYOTA", "NISSAN", "SUBARU", "SUZUKI", "ISUZU", "MITSUBISHI", "LEXUS", "INFINITI")
)
#Omit MAKE column to uppercase for matching-------------------------------------
df$MAKE <- toupper(df$MAKE)

# Assign continent to each row
df$Continent <- sapply(df$MAKE, function(make) {
  found <- sapply(names(continent_map), function(continent) {
    make %in% continent_map[[continent]]
  })
  if (any(found)) {
    names(which(found))[1]
  } else {
    "Other"
  }
})

# Convert to factor-------------------------------------------------------------

df$Continent <- as.factor(df$Continent)
table(df$Continent)

#Classifying categorical variables as factors-----------------------------------
df$MODEL = as.factor(df$MODEL)
df$`VEHICLE CLASS` = as.factor(df$`VEHICLE CLASS`)
df$TRANSMISSION = as.factor(df$TRANSMISSION)
df$FUEL = as.factor(df$FUEL)

# Create the simplified transmission type column
df$Transmission_Type <- ifelse(
  startsWith(as.character(df$TRANSMISSION), "A"), "Automatic",
  ifelse(startsWith(as.character(df$TRANSMISSION), "M"), "Manual", "Other")
)
df$Transmission_Type <- as.factor(df$Transmission_Type)

#Create summary table
trans_summary <- df %>%
  group_by(Transmission_Type) %>%
  summarise(Count = n()) %>%
  mutate(
    Percentage = round(100 * Count / sum(Count), 1),
    Label = paste0(Transmission_Type, " (", Percentage, "%)")
  )
# Detection of missing/NA values------------------------------------------------
NA_detect <- function(df) {
  if(sum(is.na(df)) == 0) {
    print("No missing values detected in the data.")
  } else {
    na_summary <- data.frame(
      Column_Name = character(),
      NA_Percentage = numeric()
    )
    for(i in 1:ncol(df)) {
      if(sum(is.na(df[,i])) > 0) {
        na_percentage <- sum(is.na(df[,i])) / nrow(df) * 100
        na_summary <- rbind(na_summary, data.frame(Column_Name = names(df)[i], NA_Percentage = na_percentage))
      }
    }
    print("Missing values detected in the following columns:")
    print(na_summary)
  }
}

NA_detect(df)

#-------------------------------------------------------------------------------
# Map fuel codes to actual fuel types
fuel_map <- c(
  "X" = "Regular Gasoline",
  "Z" = "Premium Gasoline",
  "E" = "Ethanol",
  "N" = "Natural Gas",
  "D" = "Diesel"
)

df$FUEL_TYPE <- factor(fuel_map[as.character(df$FUEL)])

#Histograms for each quantitative variable 

#Plot histogram of fuel types
ggplot(df, aes(x = FUEL_TYPE)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Fuel Types", x = "Fuel Type", y = "Count") +
  theme_minimal()

quantitative_vars <- c("Vehicle Age", "ENGINE SIZE", "CYLINDERS",
                       "FUEL CONSUMPTION", "HWY (L/100 km)", "EMISSIONS")
df_melted <- melt(df[, quantitative_vars])
ggplot(df_melted, aes(x = value)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Histograms of Quantitative Variables",
       x = "Value", y = "Count") +
  theme_minimal()

#-------------------------------------------------------------------------------
# Manually remove outliers based on logical value ranges and count how many rows were removed in total

initial_n <- nrow(df) 
df <- df[df$`Vehicle Age` >= 0 & df$`Vehicle Age` <= 25, ]
df <- df[df$`ENGINE SIZE` >= 0.5 & df$`ENGINE SIZE` <= 8, ]
df <- df[df$CYLINDERS >= 2 & df$CYLINDERS <= 16, ]
df <- df[df$`FUEL CONSUMPTION` >= 2 & df$`FUEL CONSUMPTION` <= 30, ]
df <- df[df$`HWY (L/100 km)` >= 2 & df$`HWY (L/100 km)` <= 22, ]
df <- df[df$EMISSIONS >= 100 & df$EMISSIONS <= 600, ]

# Show number of rows removed
final_n <- nrow(df)
cat("Total observations removed due to manual outlier filtering:", initial_n - final_n, "\n")

# Stats summary-----------------------------------------------------------------

summary(df)

#----------------------------------------------
# Variance and Standard Deviation (only for numeric columns)
numeric_df <- df[, sapply(df, is.numeric)]
summary_stats <- data.frame(
  Variable = colnames(numeric_df),
  Variance = apply(numeric_df, 2, var),
  Std_Dev = apply(numeric_df, 2, sd)
)
print(summary_stats)

#----------------------------------------------
# visualizations for variables

# 1. Histogram of Fuel Consumption
ggplot(df, aes(x = `FUEL CONSUMPTION`)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Fuel Consumption Distribution", x = "Fuel Consumption (L/100 km)", y = "Count") +
  theme_minimal()

# 2. Histogram of Emissions
ggplot(df, aes(x = EMISSIONS)) +
  geom_histogram(binwidth = 10, fill = "lightgreen", color = "black") +
  labs(title = "CO2 Emissions Distribution", x = "CO2 Emissions (g/km)", y = "Count") +
  theme_minimal()

# 3. Density plot of HWY (L/100 km)
ggplot(df, aes(x = `HWY (L/100 km)`)) +
  geom_density(fill = "pink", color = "black") +
  labs(title = "Highway Fuel Consumption Density", x = "HWY (L/100 km)", y = "Density") +
  theme_minimal()

# 4. Bar chart of VEHICLE CLASS
ggplot(df, aes(x = `VEHICLE CLASS`)) +
  geom_bar(fill = "gold", color = "black") +
  labs(title = "Distribution by Vehicle Class", x = "Vehicle Class", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 5. Histogram of ENGINE SIZE
ggplot(df, aes(x = `ENGINE SIZE`)) +
  geom_histogram(binwidth = 0.5, fill = "lightcoral", color = "black") +
  labs(title = "Engine Size Distribution", x = "Engine Size (L)", y = "Count") +
  theme_minimal()

# 6. Histogram of CYLINDERS
ggplot(df, aes(x = CYLINDERS)) +
  geom_bar(fill = "plum", color = "black") +
  labs(title = "Number of Cylinders", x = "Cylinders", y = "Count") +
  theme_minimal()

# 7. Histogram of Vehicle Age
ggplot(df, aes(x = `Vehicle Age`)) +
  geom_histogram(binwidth = 1, fill = "orange", color = "black") +
  labs(title = "Vehicle Age Distribution", x = "Age (Years)", y = "Count") +
  theme_minimal()

# 8. Bar chart of FUEL type
# Map fuel codes to actual fuel types
fuel_labels <- c(
  "X" = "Regular Gasoline",
  "Z" = "Premium Gasoline",
  "E" = "Ethanol (E85)",
  "N" = "Natural Gas",
  "D" = "Diesel"
)
df$Fuel_Type <- factor(df$FUEL, levels = names(fuel_labels), labels = fuel_labels)

#9 Plot fuel type distribution
ggplot(df, aes(x = Fuel_Type)) +
  geom_bar(fill = "darkseagreen2", color = "black") +
  labs(title = "Distribution of Fuel Types",
       x = "Fuel Type",
       y = "Count") +
  theme_minimal()
#10 pie chart for transmission type
ggplot(trans_summary, aes(x = "", y = Count, fill = Transmission_Type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Transmission Type Distribution") +
  theme_void() +
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Pastel1")
#------------------------------------------------------------------------------
# Correlation heatmap
correlation_matrix <- cor(numeric_df)
correlation_melted <- melt(correlation_matrix)

ggplot(correlation_melted, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +
  scale_fill_gradient(low = "cornsilk", high = "brown1") +
  labs(title = "Correlation Heatmap of Numeric Variables", x = "Variables", y = "Variables", fill = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#-------------------------------------------------------------------------------
remove_and_plot_correlation <- function(df, variable_name) {
  # Remove the specified variable
  df <- df[, !names(df) %in% variable_name]
  
  # Update numeric dataframe
  numeric_df <- df[, sapply(df, is.numeric)]
  
  # Recalculate correlation matrix
  correlation_matrix <- cor(numeric_df)
  correlation_melted <- melt(correlation_matrix)
  
  # Plot heatmap
  ggplot(correlation_melted, aes(Var1, Var2, fill = value)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(value, 2)), color = "black", size = 4) +
    scale_fill_gradient(low = "cornsilk", high = "brown1") +
    labs(title = paste("Updated Correlation Heatmap (After Removing", variable_name, ")"),
         x = "Variables", y = "Variables", fill = "Correlation") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Apply the function to each variable you want to remove and plot
for (var_to_remove in c("FUEL CONSUMPTION", "HWY (L/100 km)", "COMB (mpg)", "CYLINDERS")) {
  print(remove_and_plot_correlation(df, var_to_remove))
  df <- df[, !names(df) %in% var_to_remove]  # Update df after each removal
}
# Removing variables based on correlation analysis and modeling needs
df <- subset(df, select = -c(`year of prodaction`, `MAKE`, `MODEL`, `VEHICLE CLASS`, 
  `Fuel_Type`, `FUEL`,`TRANSMISSION`))
# Creating interaction terms between categorical and numerical variables
for (cat_var in c("Continent", "Transmission_Type", "FUEL_TYPE")) {
  if (cat_var %in% names(df)) {
    for (num_var in c("Vehicle Age", "ENGINE SIZE", "COMB (L/100 km)")) {
      if (num_var %in% names(df)) {
        interaction_col <- as.numeric(df[[cat_var]]) * df[[num_var]]
        colname <- paste0(cat_var, "_by_", gsub(" ", "_", num_var))
        df[[colname]] <- interaction_col
      }
    }
  }
}
#-------------------------------------------------------------------------------
# Finding the Optimal Regression Model: Manual Method

full_model=lm(EMISSIONS ~ .-`ENGINE SIZE`, data = df) 
summary(full_model)

model1 = lm(EMISSIONS ~ . - `ENGINE SIZE`-`Transmission_Type_by_ENGINE_SIZE`, data = df)
summary(model1)



