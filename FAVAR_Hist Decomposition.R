

# Load necessary libraries
library(vars)
library(tidyverse)
library(ggplot2)
library(gridExtra)


data = Empirics_SA_ 
view(data)



# ----------- BRAZIL -------------
# Step 1: Construct the variables 
gdp <- data$gdp_br
prices <- data$prices_br
consumption <- data$consumption_br
interest <- data$interest_br
spread <- data$spread_br
investment <- data$investment_br
trade <- data$trade_br
government <- data$gov_br


#soybean <- data$Soybean   #Commodities shock 
soybean <- data$`World Demand`   # I used this for the paper (To get Inflation effect look for the HSD of Consumption) 
emerals <- data$Emeralds_X 


usint <- data$`US int. rate`   #Foreign shock
#worlddemand <- data$`World Demand`      
worlddemand <- data$Soybean  # I used this for the paper (To get Inflation effect look for the HSD of Consumption)

# Step 1: Construct the VAR Dataset with Observed Shocks
VAR_data <- data.frame(gdp, prices, consumption, interest, spread, investment, government, trade, usint, worlddemand, soybean)


# Step 2: Estimate a Structural VAR Model
VAR_model <- VAR(VAR_data, p = 2, type = "const")


# Step 3: Compute Forecast Error Variance Decomposition (FEVD)
fevd_results <- fevd(VAR_model, n.ahead = 10)


# Step 4: Extract only the decomposition of GDP and Investment from SHOCKS
hd_GDP_df <- as.data.frame(fevd_results$gdp) %>%
  select(soybean, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")


hd_p_df <- as.data.frame(fevd_results$prices) %>%
  select(soybean, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")


# Consumption Decomposition
hd_c_df <- as.data.frame(fevd_results$consumption) %>%
  select(soybean, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")

# Interest Decomposition
hd_int_df <- as.data.frame(fevd_results$interest) %>%
  select(soybean, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")

# Investment Decomposition
hd_i_df <- as.data.frame(fevd_results$investment) %>%
  select(soybean, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")

# Trade Decomposition
hd_tr_df <- as.data.frame(fevd_results$trade) %>%
  select(soybean, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")

# Goverment Decomposition
hd_gov_df <- as.data.frame(fevd_results$government) %>%
  select(soybean, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")




# Step 5: Plot Historical Decomposition of GDP and Investment (Only Shocks)
p1 <- ggplot(hd_GDP_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) + theme_minimal() + ggtitle("Historical Decomposition of GDP")

p2 <- ggplot(hd_p_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) + theme_minimal() + ggtitle("Historical Decomposition of Prices")

p3 <- ggplot(hd_c_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) + theme_minimal() + ggtitle("Historical Decomposition of Consumption")

p4 <- ggplot(hd_int_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) + theme_minimal() + ggtitle("Historical Decomposition of Interest Rates")

p5 <- ggplot(hd_i_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) + theme_minimal() + ggtitle("Historical Decomposition of Investment")

p6 <- ggplot(hd_tr_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) + theme_minimal() + ggtitle("Historical Decomposition of Trade")

p7 <- ggplot(hd_gov_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) + theme_minimal() + ggtitle("Historical Decomposition of Goverment")

# Arrange plots side by side
grid.arrange(p1, p2, ncol = 2)
grid.arrange(p3, p4, ncol = 2)
grid.arrange(p5, p6,  ncol = 2)
grid.arrange(p7,  ncol = 2)







# ---------- COLOMBIA ----------- 

data = Empirics_SA_ 

# Step 1: Construct the variables 
gdp <- data$gdp_co
prices <- data$prices_co
consumption <- data$consumption_co
interest <- data$interest_co
spread <- data$spread_co
investment <- data$investment_co
trade <- data$trade_co
government <- data$gov_co


soybean <- data$Soybean   #Commodities shock 
#emeralds <- data$Emeralds_X
worlddemand <- data$Emeralds_X


#usint <- data$`US int. rate`   #Foreign shock
usint <- data$`US int. rate`   #Foreign shock
emeralds <- data$`World Demand`


# Step 1: Construct the VAR Dataset with Observed Shocks
VAR_data <- data.frame(gdp, prices, consumption, interest, spread, investment, government, trade, usint, worlddemand, emeralds)


# Step 2: Estimate a Structural VAR Model
VAR_model <- VAR(VAR_data, p = 2, type = "const")


# Step 3: Compute Forecast Error Variance Decomposition (FEVD)
fevd_results <- fevd(VAR_model, n.ahead = 10)


# Step 4: Extract only the decomposition of GDP and Investment from SHOCKS
hd_GDP_df <- as.data.frame(fevd_results$gdp) %>%
  select(emeralds, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")


hd_p_df <- as.data.frame(fevd_results$prices) %>%
  select(emeralds, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")


# Consumption Decomposition
hd_c_df <- as.data.frame(fevd_results$consumption) %>%
  select(emeralds, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")

# Interest Decomposition
hd_int_df <- as.data.frame(fevd_results$interest) %>%
  select(emeralds, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")

# Investment Decomposition
hd_i_df <- as.data.frame(fevd_results$investment) %>%
  select(emeralds, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")

# Trade Decomposition
hd_tr_df <- as.data.frame(fevd_results$trade) %>%
  select(emeralds, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")

# Goverment Decomposition
hd_gov_df <- as.data.frame(fevd_results$government) %>%
  select(emeralds, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")




# Step 5: Plot Historical Decomposition of GDP and Investment (Only Shocks)
p1 <- ggplot(hd_GDP_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) + theme_minimal() + ggtitle("Historical Decomposition of GDP")

p2 <- ggplot(hd_p_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) + theme_minimal() + ggtitle("Historical Decomposition of Prices")

p3 <- ggplot(hd_c_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) + theme_minimal() + ggtitle("Historical Decomposition of Consumption")

p4 <- ggplot(hd_int_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) + theme_minimal() + ggtitle("Historical Decomposition of Interest Rates")

p5 <- ggplot(hd_i_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) + theme_minimal() + ggtitle("Historical Decomposition of Investment")

p6 <- ggplot(hd_tr_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) + theme_minimal() + ggtitle("Historical Decomposition of Trade")

p7 <- ggplot(hd_gov_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) + theme_minimal() + ggtitle("Historical Decomposition of Goverment")

# Arrange plots side by side
grid.arrange(p1, p2, ncol = 2)
grid.arrange(p3, p4, ncol = 2)
grid.arrange(p5, p6,  ncol = 2)
grid.arrange(p7,  ncol = 2)











# ------- BOLIVIA -------------- 
# Step 1: Construct the variables 
gdp <- data$gdp_bo
prices <- data$prices_bo
consumption <- data$consumption_bo
interest <- data$interest_bo
spread <- data$spread_bo
investment <- data$investment_bo
trade <- data$trade_bo
government <- data$gov_bo


soybean <- data$Soybean   #Commodities shock 
emeralds <- data$Emeralds_X 
#lithium <- data$zinc 
usint <- data$zinc 



#usint <- data$`US int. rate`   #Foreign shock
zinc <- data$`US int. rate`   #Foreign shock
worlddemand <- data$`World Demand`


# Step 1: Construct the VAR Dataset with Observed Shocks
VAR_data <- data.frame(gdp, prices, consumption, interest, spread, investment, government, trade, usint, worlddemand, zinc)


# Step 2: Estimate a Structural VAR Model
VAR_model <- VAR(VAR_data, p = 2, type = "const")


# Step 3: Compute Forecast Error Variance Decomposition (FEVD)
fevd_results <- fevd(VAR_model, n.ahead = 10)


# zinc 
# Step 4: Extract only the decomposition of GDP and Investment from SHOCKS
hd_GDP_df <- as.data.frame(fevd_results$gdp) %>%
  select(zinc, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")


hd_p_df <- as.data.frame(fevd_results$prices) %>%
  select(zinc, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")


# Consumption Decomposition
hd_c_df <- as.data.frame(fevd_results$consumption) %>%
  select(zinc, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")

# Interest Decomposition
hd_int_df <- as.data.frame(fevd_results$interest) %>%
  select(zinc, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")

# Investment Decomposition
hd_i_df <- as.data.frame(fevd_results$investment) %>%
  select(zinc, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")

# Trade Decomposition
hd_tr_df <- as.data.frame(fevd_results$trade) %>%
  select(zinc, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")

# Goverment Decomposition
hd_gov_df <- as.data.frame(fevd_results$government) %>%
  select(zinc, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")







# FINAL VERSION 
# 0) at the top of your script
shock_levels <- c("spread", "usint", "worlddemand", "zinc")
shock_cols   <- c(
  zinc        = "#F8766D",
  spread      = "#7CAE00",
  usint       = "#00BFC4",
  worlddemand = "#C77CFF"
)



hd_int_df <- as.data.frame(fevd_results$interest) %>%
  select(zinc, usint, worlddemand, spread) %>% 
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution") %>%
  # ← force the stacking order here:
  mutate(Shock = factor(Shock, levels = shock_levels))


p4 <- ggplot(hd_int_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) +
  theme_minimal() +
  ggtitle("Historical Decomposition of Interest Rates") +
  scale_fill_manual(
    name   = "Shock",
    breaks = shock_levels,
    values = shock_cols
  )


p4






shock_levels <- c("zinc", "spread", "usint", "worlddemand")

# 0) at the top of your script
shock_levels <- c("zinc", "spread", "usint", "worlddemand")  # <- zinc first
shock_cols   <- c(
  zinc        = "#F8766D",
  spread      = "#7CAE00",
  usint       = "#00BFC4",
  worlddemand = "#C77CFF"
)

hd_int_df <- as.data.frame(fevd_results$interest) %>%
  select(zinc, usint, worlddemand, spread) %>% 
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution") %>%
  # ← force the stacking order here:
  mutate(Shock = factor(Shock, levels = shock_levels))

p4 <- ggplot(hd_int_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) +
  theme_minimal() +
  ggtitle("FEVD of Interest Rates") +
  scale_fill_manual(
    name   = "Shock",
    breaks = shock_levels,
    values = shock_cols
  )

grid.arrange(p4,  ncol = 2)







hd_p_df <- as.data.frame(fevd_results$prices) %>%
  select(zinc, usint, worlddemand, spread) %>% 
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution") %>%
  # ← force the stacking order here:
  mutate(Shock = factor(Shock, levels = shock_levels))

p5 <- ggplot(hd_p_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) +
  theme_minimal() +
  ggtitle("FEVD of Inflation") +
  scale_fill_manual(
    name   = "Shock",
    breaks = shock_levels,
    values = shock_cols
  )

grid.arrange(p5,  ncol = 2)






# ---------- CHILE ----------- 
# Step 1: Construct the variables 
gdp <- data$gdp_ch
prices <- data$prices_ch
consumption <- data$consumption_ch
interest <- data$interest_ch
spread <- data$spread_ch
investment <- data$investment_ch
trade <- data$trade_ch
government <- data$gov_ch


soybean <- data$Soybean   #Commodities shock 
emeralds <- data$Emeralds_X 
worlddemand <- data$copper


usint <- data$`US int. rate`   #Foreign shock
copper <- data$`World Demand`


# Step 1: Construct the VAR Dataset with Observed Shocks
VAR_data <- data.frame(gdp, prices, consumption, interest, spread, investment, government, trade, usint, worlddemand, copper)


# Step 2: Estimate a Structural VAR Model
VAR_model <- VAR(VAR_data, p = 2, type = "const")


# Step 3: Compute Forecast Error Variance Decomposition (FEVD)
fevd_results <- fevd(VAR_model, n.ahead = 10)


# Step 4: Extract only the decomposition of GDP and Investment from SHOCKS
hd_GDP_df <- as.data.frame(fevd_results$gdp) %>%
  select(copper, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")


hd_p_df <- as.data.frame(fevd_results$prices) %>%
  select(copper, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")


# Consumption Decomposition
hd_c_df <- as.data.frame(fevd_results$consumption) %>%
  select(copper, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")

# Interest Decomposition
hd_int_df <- as.data.frame(fevd_results$interest) %>%
  select(copper, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")

# Investment Decomposition
hd_i_df <- as.data.frame(fevd_results$investment) %>%
  select(copper, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")

# Trade Decomposition
hd_tr_df <- as.data.frame(fevd_results$trade) %>%
  select(copper, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")

# Goverment Decomposition
hd_gov_df <- as.data.frame(fevd_results$government) %>%
  select(copper, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")




# Step 5: Plot Historical Decomposition of GDP and Investment (Only Shocks)
p1 <- ggplot(hd_GDP_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) + theme_minimal() + ggtitle("Historical Decomposition of GDP")

p2 <- ggplot(hd_p_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) + theme_minimal() + ggtitle("Historical Decomposition of Prices")

p3 <- ggplot(hd_c_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) + theme_minimal() + ggtitle("Historical Decomposition of Consumption")

p4 <- ggplot(hd_int_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) + theme_minimal() + ggtitle("Historical Decomposition of Interest Rates")

p5 <- ggplot(hd_i_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) + theme_minimal() + ggtitle("Historical Decomposition of Investment")

p6 <- ggplot(hd_tr_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) + theme_minimal() + ggtitle("Historical Decomposition of Trade")

p7 <- ggplot(hd_gov_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) + theme_minimal() + ggtitle("Historical Decomposition of Goverment")

# Arrange plots side by side
grid.arrange(p1, p2, ncol = 2)
grid.arrange(p3, p4, ncol = 2)
grid.arrange(p5, p6,  ncol = 2)
grid.arrange(p7,  ncol = 2)











# ---------- PERU ----------- 
# Step 1: Construct the variables 
gdp <- data$gdp_pe
prices <- data$prices_pe
consumption <- data$consumption_pe
interest <- data$interest_pe
spread <- data$spread_pe
investment <- data$investment_pe
trade <- data$trade_pe
government <- data$gov_pe


soybean <- data$Soybean   #Commodities shock 
emeralds <- data$Emeralds_X 
#copper <- data$copper
usint <- data$copper



copper <- data$`US int. rate`   #Foreign shock
#worlddemand <- data$`World Demand`
worlddemand <- data$`World Demand`


# Step 1: Construct the VAR Dataset with Observed Shocks
VAR_data <- data.frame(gdp, prices, consumption, interest, spread, investment, government, trade, usint, worlddemand, copper)


# Step 2: Estimate a Structural VAR Model
VAR_model <- VAR(VAR_data, p = 2, type = "const")


# Step 3: Compute Forecast Error Variance Decomposition (FEVD)
fevd_results <- fevd(VAR_model, n.ahead = 10)


# Step 4: Extract only the decomposition of GDP and Investment from SHOCKS
hd_GDP_df <- as.data.frame(fevd_results$gdp) %>%
  select(copper, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")


hd_p_df <- as.data.frame(fevd_results$prices) %>%
  select(copper, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")


# Consumption Decomposition
hd_c_df <- as.data.frame(fevd_results$consumption) %>%
  select(copper, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")

# Interest Decomposition
hd_int_df <- as.data.frame(fevd_results$interest) %>%
  select(copper, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")

# Investment Decomposition
hd_i_df <- as.data.frame(fevd_results$investment) %>%
  select(copper, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")

# Trade Decomposition
hd_tr_df <- as.data.frame(fevd_results$trade) %>%
  select(copper, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")

# Goverment Decomposition
hd_gov_df <- as.data.frame(fevd_results$government) %>%
  select(copper, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")




# Step 5: Plot Historical Decomposition of GDP and Investment (Only Shocks)
p1 <- ggplot(hd_GDP_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) + theme_minimal() + ggtitle("Historical Decomposition of GDP")

p2 <- ggplot(hd_p_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) + theme_minimal() + ggtitle("Historical Decomposition of Prices")

p3 <- ggplot(hd_c_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) + theme_minimal() + ggtitle("Historical Decomposition of Consumption")

p4 <- ggplot(hd_int_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) + theme_minimal() + ggtitle("Historical Decomposition of Interest Rates")

p5 <- ggplot(hd_i_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) + theme_minimal() + ggtitle("Historical Decomposition of Investment")

p6 <- ggplot(hd_tr_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) + theme_minimal() + ggtitle("Historical Decomposition of Trade")

p7 <- ggplot(hd_gov_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) + theme_minimal() + ggtitle("Historical Decomposition of Goverment")

# Arrange plots side by side
grid.arrange(p1, p2, ncol = 2)
grid.arrange(p3, p4, ncol = 2)
grid.arrange(p5, p6,  ncol = 2)
grid.arrange(p7,  ncol = 2)







# ---------- ECUADOR ----------- 
# Step 1: Construct the variables 
gdp <- data$gdp_ec
prices <- data$prices_ec
consumption <- data$consumption_ec
interest <- data$interest_ec
spread <- data$spread_ec
investment <- data$investment_ec
trade <- data$trade_ec
government <- data$gov_ec


soybean <- data$Soybean   #Commodities shock 
emeralds <- data$Emeralds_X 
copper <- data$copper
banana <- data$banana


usint <- data$`US int. rate`   #Foreign shock
worlddemand <- data$`World Demand`


# Step 1: Construct the VAR Dataset with Observed Shocks
VAR_data <- data.frame(gdp, prices, consumption, interest, spread, investment, government, trade, usint, worlddemand, banana)


# Step 2: Estimate a Structural VAR Model
VAR_model <- VAR(VAR_data, p = 2, type = "const")


# Step 3: Compute Forecast Error Variance Decomposition (FEVD)
fevd_results <- fevd(VAR_model, n.ahead = 10)


# Step 4: Extract only the decomposition of GDP and Investment from SHOCKS
hd_GDP_df <- as.data.frame(fevd_results$gdp) %>%
  select(banana, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")


hd_p_df <- as.data.frame(fevd_results$prices) %>%
  select(banana, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")


# Consumption Decomposition
hd_c_df <- as.data.frame(fevd_results$consumption) %>%
  select(banana, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")

# Interest Decomposition
hd_int_df <- as.data.frame(fevd_results$interest) %>%
  select(banana, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")

# Investment Decomposition
hd_i_df <- as.data.frame(fevd_results$investment) %>%
  select(banana, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")

# Trade Decomposition
hd_tr_df <- as.data.frame(fevd_results$trade) %>%
  select(banana, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")

# Goverment Decomposition
hd_gov_df <- as.data.frame(fevd_results$government) %>%
  select(banana, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")




# Step 5: Plot Historical Decomposition of GDP and Investment (Only Shocks)
p1 <- ggplot(hd_GDP_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) + theme_minimal() + ggtitle("Historical Decomposition of GDP")

p2 <- ggplot(hd_p_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) + theme_minimal() + ggtitle("Historical Decomposition of Prices")

p3 <- ggplot(hd_c_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) + theme_minimal() + ggtitle("Historical Decomposition of Consumption")

p4 <- ggplot(hd_int_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) + theme_minimal() + ggtitle("Historical Decomposition of Interest Rates")

p5 <- ggplot(hd_i_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) + theme_minimal() + ggtitle("Historical Decomposition of Investment")

p6 <- ggplot(hd_tr_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) + theme_minimal() + ggtitle("Historical Decomposition of Trade")

p7 <- ggplot(hd_gov_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) + theme_minimal() + ggtitle("Historical Decomposition of Goverment")

# Arrange plots side by side
grid.arrange(p1, p2, ncol = 2)
grid.arrange(p3, p4, ncol = 2)
grid.arrange(p5, p6,  ncol = 2)
grid.arrange(p7,  ncol = 2)







# ---------- ARGENTINA ----------- 
# Step 1: Construct the variables 
gdp <- data$gdp_arg
prices <- data$prices_arg
consumption <- data$consumption_arg
interest <- data$interest_arg
spread <- data$spread_arg
investment <- data$investment_arg
trade <- data$trade_arg
government <- data$gov_arg


soybean <- data$Soybean   #Commodities shock 
emeralds <- data$Emeralds_X 
copper <- data$copper
banana <- data$banana
worlddemand <- data$lithium

usint <- data$`US int. rate`   #Foreign shock
lithium <- data$`World Demand`


# Step 1: Construct the VAR Dataset with Observed Shocks
VAR_data <- data.frame(gdp, prices, consumption, interest, spread, investment, government, trade, usint, worlddemand, lithium)


# Step 2: Estimate a Structural VAR Model
VAR_model <- VAR(VAR_data, p = 2, type = "const")


# Step 3: Compute Forecast Error Variance Decomposition (FEVD)
fevd_results <- fevd(VAR_model, n.ahead = 10)


# Step 4: Extract only the decomposition of GDP and Investment from SHOCKS
hd_GDP_df <- as.data.frame(fevd_results$gdp) %>%
  select(lithium, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")


hd_p_df <- as.data.frame(fevd_results$prices) %>%
  select(lithium, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")


# Consumption Decomposition
hd_c_df <- as.data.frame(fevd_results$consumption) %>%
  select(lithium, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")

# Interest Decomposition
hd_int_df <- as.data.frame(fevd_results$interest) %>%
  select(lithium, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")

# Investment Decomposition
hd_i_df <- as.data.frame(fevd_results$investment) %>%
  select(lithium, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")

# Trade Decomposition
hd_tr_df <- as.data.frame(fevd_results$trade) %>%
  select(lithium, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")

# Goverment Decomposition
hd_gov_df <- as.data.frame(fevd_results$government) %>%
  select(lithium, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")




# Step 5: Plot Historical Decomposition of GDP and Investment (Only Shocks)
p1 <- ggplot(hd_GDP_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) + theme_minimal() + ggtitle("Historical Decomposition of GDP")

p2 <- ggplot(hd_p_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) + theme_minimal() + ggtitle("Historical Decomposition of Prices")

p3 <- ggplot(hd_c_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) + theme_minimal() + ggtitle("Historical Decomposition of Consumption")

p4 <- ggplot(hd_int_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) + theme_minimal() + ggtitle("Historical Decomposition of Interest Rates")

p5 <- ggplot(hd_i_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) + theme_minimal() + ggtitle("Historical Decomposition of Investment")

p6 <- ggplot(hd_tr_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) + theme_minimal() + ggtitle("Historical Decomposition of Trade")

p7 <- ggplot(hd_gov_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) + theme_minimal() + ggtitle("Historical Decomposition of Goverment")

# Arrange plots side by side
grid.arrange(p1, p2, ncol = 2)
grid.arrange(p3, p4, ncol = 2)
grid.arrange(p5, p6,  ncol = 2)
grid.arrange(p7,  ncol = 2)







# ---------- PARAGUAY ----------- 
# Step 1: Construct the variables 
gdp <- data$gdp_par
prices <- data$prices_par
consumption <- data$consumption_par
interest <- data$interest_par
#spread <- data$spread_par
soybean <- data$spread_par
investment <- data$investment_par
trade <- data$trade_par
government <- data$gov_par


#soybean <- data$Soybean   #Commodities shock 
spread <- data$Soybean   #Commodities shock 
emeralds <- data$Emeralds_X 
copper <- data$copper
banana <- data$banana
lithium <- data$lithium

usint <- data$`US int. rate`   #Foreign shock
worlddemand <- data$`World Demand`
#soybean <- data$`World Demand`


# Step 1: Construct the VAR Dataset with Observed Shocks
VAR_data <- data.frame(gdp, prices, consumption, interest, spread, investment, government, trade, usint, worlddemand, soybean)


# Step 2: Estimate a Structural VAR Model
VAR_model <- VAR(VAR_data, p = 2, type = "const")


# Step 3: Compute Forecast Error Variance Decomposition (FEVD)
fevd_results <- fevd(VAR_model, n.ahead = 10)


# Step 4: Extract only the decomposition of GDP and Investment from SHOCKS
hd_GDP_df <- as.data.frame(fevd_results$gdp) %>%
  select(soybean, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")


hd_p_df <- as.data.frame(fevd_results$prices) %>%
  select(soybean, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")


# Consumption Decomposition
hd_c_df <- as.data.frame(fevd_results$consumption) %>%
  select(soybean, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")

# Interest Decomposition
hd_int_df <- as.data.frame(fevd_results$interest) %>%
  select(soybean, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")

# Investment Decomposition
hd_i_df <- as.data.frame(fevd_results$investment) %>%
  select(soybean, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")

# Trade Decomposition
hd_tr_df <- as.data.frame(fevd_results$trade) %>%
  select(soybean, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")

# Goverment Decomposition
hd_gov_df <- as.data.frame(fevd_results$government) %>%
  select(soybean, usint, worlddemand, spread) %>%  # Keep only shock variables
  mutate(Time = 1:10) %>%
  pivot_longer(-Time, names_to = "Shock", values_to = "Contribution")




# Step 5: Plot Historical Decomposition of GDP and Investment (Only Shocks)
p1 <- ggplot(hd_GDP_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) + theme_minimal() + ggtitle("FEVD of GDP")

p2 <- ggplot(hd_p_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) + theme_minimal() + ggtitle("FEVD of Prices")

p3 <- ggplot(hd_c_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) + theme_minimal() + ggtitle("FEVD of Consumption")

p4 <- ggplot(hd_int_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) + theme_minimal() + ggtitle("FEVD of Interest Rates")

p5 <- ggplot(hd_i_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) + theme_minimal() + ggtitle("FEVD of Investment")

p6 <- ggplot(hd_tr_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) + theme_minimal() + ggtitle("FEVD of Trade")

p7 <- ggplot(hd_gov_df, aes(x = Time, y = Contribution, fill = Shock)) +
  geom_area(alpha = 0.6) + theme_minimal() + ggtitle("FEVD of Goverment")

# Arrange plots side by side
grid.arrange(p1, p2, ncol = 2)
grid.arrange(p3, p4, ncol = 2)
grid.arrange(p5, p6,  ncol = 2)
grid.arrange(p7,  ncol = 2)
