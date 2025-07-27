

# Load necessary libraries
library(vars)
library(ggplot2)
library(reshape2)
library(zoo)  
library(tidyverse)
library(gridExtra)

# Load data
data = Empirics_SA_

# Set seed for reproducibility
set.seed(123)

# Define variables
gdp <- data$gdp_ec
prices <- data$prices_ec
consumption <- data$consumption_ec
interest <- data$interest_ec 
spread <- data$spread_ec
trade <- data$trade_ec
investment <- data$investment_ec 
government <- data$gov_ec



soybean  <- data$Soybean   #Commodities shock 
emeralds <- data$Emeralds_X 
zinc <- data$zinc 
banana <- data$banana 
copper <- data$copper 
lithium <- data$lithium 


usint <- data$`US int. rate`   #Foreign shock
worlddemand <- data$`World Demand` 



# Create quarterly time sequence
start_date <- as.yearqtr("1996 Q2")  
time_quarters <- seq(start_date, by = 0.25, length.out = 113)

# Create dataframe (Include Prices as Dependent Variable)
data <- data.frame(Time = time_quarters, GDP = gdp,  
                   Spread = spread, Soybean = soybean, Usint = usint, Worlddemand = worlddemand)

data <- data.frame(Time = time_quarters, Prices = prices, 
                   Spread = spread, Copper = copper, Usint = usint, Worlddemand = worlddemand)

data <- data.frame(Time = time_quarters, Interest = interest, 
                   Spread = spread, Banana = banana, Usint = usint, Worlddemand = worlddemand)

# Run VAR Model with Only Shocks
VAR_model <- VAR(data[, c("GDP", "Spread", "Soybean", "Usint", "Worlddemand")], 
                 p = 2, type = "const")  

VAR_model <- VAR(data[, c("Prices", "Spread", "Soybean", "Usint", "Worlddemand")], 
                 p = 2, type = "const")  

VAR_model <- VAR(data[, c("Interest", "Spread", "Banana", "Usint", "Worlddemand")], 
                 p = 2, type = "const")  

# Get residuals (shocks)
residuals_VAR <- residuals(VAR_model)

# Initialize matrix for cumulative shock contributions
historical_decomp <- matrix(0, nrow = 113, ncol = 4)  # Spread, Soybean, Usint, Worlddemand
colnames(historical_decomp) <- c("Spread", "Banana", "Usint", "Worlddemand")

# **Reconstruct Prices using Only Shock Contributions**
for (t in 2:(nrow(residuals_VAR) + 1)) {  
  historical_decomp[t, ] <- historical_decomp[t - 1, ] + residuals_VAR[t - 1, c("Spread", "Banana", "Usint", "Worlddemand")]
}

# Convert to dataframe
historical_decomp_df <- as.data.frame(historical_decomp)
historical_decomp_df$Time <- time_quarters  
#historical_decomp_df$SumShocks <- rowSums(historical_decomp)

# Melt data for ggplot
historical_decomp_long <- melt(historical_decomp_df, id.vars = "Time", 
                               variable.name = "Shock", value.name = "Contribution")

# Create the plot
# Define custom colors for shocks
custom_colors <- c("Spread" = "darkorange", "Banana" = "steelblue", 
                   "Usint" = "firebrick", "Worlddemand" = "gray40")

# Create the plot
ggplot() +
  geom_bar(data = historical_decomp_long, aes(x = Time, y = Contribution, fill = Shock), 
           stat = "identity", position = "stack", alpha = 0.7) +
  scale_fill_manual(values = custom_colors) +  # Apply custom colors
  theme_minimal() +
  labs(title = "Historical Variance Decomposition of Prices (Shocks Only)",
       x = "Quarter", y = "Contribution to Prices") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")












# IMPORTANT!!!!!!!
# --- after you have your historical_decomp_df (with one column per shock + Time) ---

# 1) Identify your shock columns
shock_cols <- c("Spread","Banana","Usint","Worlddemand")

# 2) Compute, for each date, the sum of positives and sum of negatives
pos_sum <- apply(historical_decomp_df[, shock_cols], 1, function(x) sum(x[x > 0]))
neg_sum <- apply(historical_decomp_df[, shock_cols], 1, function(x) sum(x[x < 0]))

# 3) The midpoint is simply (top_of_bar + bottom_of_bar)/2
historical_decomp_df$midpoint <- (pos_sum + neg_sum) / 2

# 4) Melt only the shock‐columns for the bars
library(reshape2)
historical_decomp_long <- melt(
  historical_decomp_df,
  id.vars      = "Time",
  measure.vars = shock_cols,
  variable.name = "Shock",
  value.name    = "Contribution"
)

# 5) Plot: stacked bars + centre‐line
library(ggplot2)
custom_colors <- c(
  "Spread"      = "darkorange",
  "Banana"     = "steelblue",
  "Usint"       = "firebrick",
  "Worlddemand" = "gray40"
)

ggplot() +
  # bars
  geom_col(
    data     = historical_decomp_long,
    mapping  = aes(x = Time, y = Contribution, fill = Shock),
    position = "stack",
    alpha    = 0.7
  ) +
  # centre‐line
  geom_line(
    data  = historical_decomp_df,
    mapping = aes(x = Time, y = midpoint),
    color = "black",
    size  = 1
  ) +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  theme(
    axis.text.x    = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  labs(
    #title = "Historical Decomposition of Inflation",
    title = "Historical Decomposition of Interest Rates",
    x     = "Quarter",
    y     = "Contribution"
  )
