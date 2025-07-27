


# NO CONFIDENCE INTERVALS 
# Load necessary libraries
library(vars)
library(tidyverse)
library(ggplot2)
library(gridExtra)

# Load data
data = Empirics_SA_ 
#view(data)

# ----------- BRAZIL -------------
# Step 1: Define the variables
gdp <- data$gdp_br
domprices <- data$prices_p_br  # Domestic Prices
cpiprices <- data$prices_c_br  # CPI Prices
consumption <- data$consumption_br
interest <- data$interest_br
spread <- data$spread_br
investment <- data$investment_br
trade <- data$trade_br
government <- data$gov_br
soybean <- data$Soybean   # Commodities shock 
usint <- data$`US int. rate`   # Foreign shock
worlddemand <- data$`World Demand`

# Step 2: Construct VAR Datasets
VAR_data1 <- data.frame(gdp, domprices, consumption, interest, spread, investment, government, trade, usint, worlddemand, soybean)
VAR_data2 <- data.frame(gdp, cpiprices, consumption, interest, spread, investment, government, trade, usint, worlddemand, soybean)

# Step 3: Estimate Structural VAR models (with 2 lags and constant)
VAR_model_domestic <- VAR(VAR_data1, p = 2, type = "const")
VAR_model_CPI <- VAR(VAR_data2, p = 2, type = "const")

# Step 4: Compute IRF without confidence intervals
irf_domestic <- irf(VAR_model_domestic, impulse = "soybean", response = "domprices", 
                    n.ahead = 5, boot = FALSE, cumulative = TRUE)

irf_CPI <- irf(VAR_model_CPI, impulse = "soybean", response = "cpiprices", 
               n.ahead = 5, boot = FALSE, cumulative = TRUE)

# Step 5: Extract IRF results into a dataframe for plotting
irf_df <- data.frame(
  Time = 0:5,
  Domestic_IRF = irf_domestic$irf$soybean[, 1],
  CPI_IRF = irf_CPI$irf$soybean[, 1]
) %>%
  pivot_longer(cols = c(Domestic_IRF, CPI_IRF), names_to = "Variable", values_to = "Response")

# Step 6: Plot IRFs
ggplot(irf_df, aes(x = Time, y = Response, color = Variable)) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(title = "Impulse Response of Prices to Commodity Price Shock",
       x = "Time Horizon",
       y = "Response") +
  scale_color_manual(values = c("blue", "red"), labels = c("CPI", "Domestic")) +
  theme(legend.title = element_blank())




# WITH CONFIDENCE INTERVALS 
# Load necessary libraries
library(vars)
library(tidyverse)
library(ggplot2)
library(gridExtra)

# Load data
data = Empirics_SA_ 

# ----------- BRAZIL -------------
# Step 1: Define the variables
gdp <- data$gdp_br
domprices <- data$prices_p_br  # Domestic Prices
cpiprices <- data$prices_c_br  # CPI Prices
consumption <- data$consumption_br
interest <- data$interest_br
spread <- data$spread_br
investment <- data$investment_br
trade <- data$trade_br
government <- data$gov_br
soybean <- data$Soybean   # Commodities shock 
usint <- data$`US int. rate`   # Foreign shock
worlddemand <- data$`World Demand`

# Step 2: Construct VAR Datasets
VAR_data1 <- data.frame(gdp, domprices, consumption, interest, spread, investment, government, trade, usint, worlddemand, soybean)
VAR_data2 <- data.frame(gdp, cpiprices, consumption, interest, spread, investment, government, trade, usint, worlddemand, soybean)

# Step 3: Estimate Structural VAR models (with 2 lags and constant)
VAR_model_domestic <- VAR(VAR_data1, p = 2, type = "const")
VAR_model_CPI <- VAR(VAR_data2, p = 2, type = "const")

# Step 4: Compute IRF with confidence intervals (bootstrap)
irf_domestic <- irf(VAR_model_domestic, impulse = "soybean", response = "domprices", 
                    n.ahead = 5, boot = TRUE, nboot = 100000, ci = 0.20, cumulative = TRUE)

irf_CPI <- irf(VAR_model_CPI, impulse = "soybean", response = "cpiprices", 
               n.ahead = 5, boot = TRUE, nboot = 100000, ci = 0.18, cumulative = TRUE)

# Step 5: Extract IRF results into a dataframe for plotting
irf_df <- data.frame(
  Time = 0:5,
  Domestic_IRF = irf_domestic$irf$soybean[, 1],
  Domestic_Lower = irf_domestic$Lower$soybean[, 1],
  Domestic_Upper = irf_domestic$Upper$soybean[, 1],
  CPI_IRF = irf_CPI$irf$soybean[, 1],
  CPI_Lower = irf_CPI$Lower$soybean[, 1],
  CPI_Upper = irf_CPI$Upper$soybean[, 1]
) %>%
  pivot_longer(cols = c(Domestic_IRF, CPI_IRF), names_to = "Variable", values_to = "Response")

# Step 6: Plot IRFs with confidence intervals
ggplot() +
  geom_ribbon(data = irf_df, aes(x = Time, ymin = Domestic_Lower, ymax = Domestic_Upper), fill = "red", alpha = 0.35) +
  geom_ribbon(data = irf_df, aes(x = Time, ymin = CPI_Lower, ymax = CPI_Upper), fill = "blue", alpha = 0.35) +
  geom_line(data = irf_df, aes(x = Time, y = Response, color = Variable), size = 1) +
  theme_minimal() +
  labs(title = "Impulse Response of Prices to Commodity (Soybean) Price Shock",
       x = "Time Horizon",
       y = "Response") +
  scale_color_manual(values = c("blue", "red"), labels = c("CPI", "PPI")) +
  theme(legend.title = element_blank())

# Step 6: Graphic
irf_df <- tibble(
  Time     = 0:5,
  PPI       = irf_domestic$irf$soybean[,1],
  PPI_low   = irf_domestic$Lower$soybean[,1],
  PPI_high  = irf_domestic$Upper$soybean[,1],
  CPI       = irf_CPI$irf$soybean[,1],
  CPI_low   = irf_CPI$Lower$soybean[,1],
  CPI_high  = irf_CPI$Upper$soybean[,1]
) %>%
  pivot_longer(
    cols = c(PPI, CPI),
    names_to  = "Variable",
    values_to = "IRF_raw"
  ) %>%
  mutate(
    Lower  = if_else(Variable=="PPI", PPI_low,  CPI_low),
    Upper  = if_else(Variable=="PPI", PPI_high, CPI_high),
    Mid    = (Lower + Upper) / 2
  )

# --------------------------
# Step 6.1: Plot ribbon + mid‐point line
# --------------------------
library(ggplot2)

ggplot(irf_df, aes(x = Time)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = Variable), alpha = 0.35) +
  geom_line  (aes(y = Mid,   color  = Variable), size = 1) +
  scale_color_manual(values = c("CPI" = "blue", "PPI" = "red")) +
  scale_fill_manual (values = c("CPI" = "blue", "PPI" = "red")) +
  theme_minimal() +
  labs(
    title = "Impulse Response of Prices to Commodity (Soybeans) Price Shock",
    x     = "Time Horizon",
    y     = "Response"
  ) +
  theme(legend.title = element_blank())













# ----------- COLOMBIA -------------
# WITH CONFIDENCE INTERVALS 
# Load necessary libraries
library(vars)
library(tidyverse)
library(ggplot2)
library(gridExtra)

# Load data
data = Empirics_SA_ 

# ------------------------
# Step 1: Define the variables
gdp <- data$gdp_co
domprices <- data$prices_p_co  # Domestic Prices
cpiprices <- data$prices_c_co  # CPI Prices
consumption <- data$consumption_co
interest <- data$interest_co
spread <- data$spread_co
investment <- data$investment_co
trade <- data$trade_co
government <- data$gov_co
#soybean <- data$Soybean   # Commodities shock 
#emeralds <- data$Emeralds_X   # Commodities shock 
soybean <- data$Emeralds_X   # Commodities shock 
emeralds <- data$Soybean   # Commodities shock 


usint <- data$`US int. rate`   # Foreign shock
worlddemand <- data$`World Demand`


# Step 2: Construct VAR Datasets
VAR_data1 <- data.frame(gdp, domprices, consumption, interest, spread, investment, government, trade, usint, worlddemand, emeralds)
VAR_data2 <- data.frame(gdp, cpiprices, consumption, interest, spread, investment, government, trade, usint, worlddemand, emeralds)

# Step 3: Estimate Structural VAR models (with 2 lags and constant)
VAR_model_domestic <- VAR(VAR_data1, p = 2, type = "const")
VAR_model_CPI <- VAR(VAR_data2, p = 2, type = "const")

# Step 4: Compute IRF with confidence intervals (bootstrap)
irf_domestic <- irf(VAR_model_domestic, impulse = "emeralds", response = "domprices", 
                    n.ahead = 5, boot = TRUE, nboot = 100000, ci = 0.15, cumulative = TRUE)

irf_CPI <- irf(VAR_model_CPI, impulse = "emeralds", response = "cpiprices", 
               n.ahead = 5, boot = TRUE, nboot = 100000, ci = 0.15, cumulative = TRUE)

# Step 5: Extract IRF results into a dataframe for plotting
irf_df <- data.frame(
  Time = 0:5,
  Domestic_IRF = irf_domestic$irf$emeralds[, 1],
  Domestic_Lower = irf_domestic$Lower$emeralds[, 1],
  Domestic_Upper = irf_domestic$Upper$emeralds[, 1],
  CPI_IRF = irf_CPI$irf$emeralds[, 1],
  CPI_Lower = irf_CPI$Lower$emeralds[, 1],
  CPI_Upper = irf_CPI$Upper$emeralds[, 1]
) %>%
  pivot_longer(cols = c(Domestic_IRF, CPI_IRF), names_to = "Variable", values_to = "Response")

# Step 6: Plot IRFs with confidence intervals
ggplot() +
  geom_ribbon(data = irf_df, aes(x = Time, ymin = Domestic_Lower, ymax = Domestic_Upper), fill = "red", alpha = 0.35) +
  geom_ribbon(data = irf_df, aes(x = Time, ymin = CPI_Lower, ymax = CPI_Upper), fill = "blue", alpha = 0.35) +
  geom_line(data = irf_df, aes(x = Time, y = Response, color = Variable), size = 1) +
  theme_minimal() +
  labs(title = "Impulse Response of Prices to Commodity (Emeralds) Price Shock",
       x = "Time Horizon",
       y = "Response") +
  scale_color_manual(values = c("blue", "red"), labels = c("CPI inflation", "PPI inflation")) +
  theme(legend.title = element_blank())


# Step 6: Graphic
irf_df <- tibble(
  Time     = 0:5,
  PPI       = irf_domestic$irf$emeralds[,1],
  PPI_low   = irf_domestic$Lower$emeralds[,1],
  PPI_high  = irf_domestic$Upper$emeralds[,1],
  CPI       = irf_CPI$irf$emeralds[,1],
  CPI_low   = irf_CPI$Lower$emeralds[,1],
  CPI_high  = irf_CPI$Upper$emeralds[,1]
) %>%
  pivot_longer(
    cols = c(PPI, CPI),
    names_to  = "Variable",
    values_to = "IRF_raw"
  ) %>%
  mutate(
    Lower  = if_else(Variable=="PPI", PPI_low,  CPI_low),
    Upper  = if_else(Variable=="PPI", PPI_high, CPI_high),
    Mid    = (Lower + Upper) / 2
  )

# --------------------------
# Step 6.1: Plot ribbon + mid‐point line
# --------------------------
library(ggplot2)

ggplot(irf_df, aes(x = Time)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = Variable), alpha = 0.35) +
  geom_line  (aes(y = Mid,   color  = Variable), size = 1) +
  scale_color_manual(values = c("CPI" = "blue", "PPI" = "red")) +
  scale_fill_manual (values = c("CPI" = "blue", "PPI" = "red")) +
  theme_minimal() +
  labs(
    title = "Impulse Response of Prices to Commodity (Emeralds) Price Shock",
    x     = "Time Horizon",
    y     = "Response"
  ) +
  theme(legend.title = element_blank())










# ----------- BOLIVIA -------------
# WITH CONFIDENCE INTERVALS 
# Load necessary libraries
library(vars)
library(tidyverse)
library(ggplot2)
library(gridExtra)

# Load data
data = Empirics_SA_ 

# ----------- BRAZIL -------------
# Step 1: Define the variables
gdp <- data$gdp_bo
domprices <- data$prices_p_bo  # Domestic Prices
cpiprices <- data$prices_c_bo  # CPI Prices
consumption <- data$consumption_bo
interest <- data$interest_bo
spread <- data$spread_bo
investment <- data$investment_bo
trade <- data$trade_bo
government <- data$gov_bo
soybean <- data$Soybean   # Commodities shock 
emeralds <- data$Emeralds_X   # Commodities shock 

usint <- data$`US int. rate`   # Foreign shock
worlddemand <- data$`World Demand`
zinc <- data$zinc



# Step 2: Construct VAR Datasets
VAR_data1 <- data.frame(gdp, domprices, consumption, interest, spread, investment, government, trade, usint, worlddemand, zinc)
VAR_data2 <- data.frame(gdp, cpiprices, consumption, interest, spread, investment, government, trade, usint, worlddemand, zinc)

# Step 3: Estimate Structural VAR models (with 2 lags and constant)
VAR_model_domestic <- VAR(VAR_data1, p = 2, type = "const")
VAR_model_CPI <- VAR(VAR_data2, p = 2, type = "const")

# Step 4: Compute IRF with confidence intervals (bootstrap)
irf_domestic <- irf(VAR_model_domestic, impulse = "zinc", response = "domprices", 
                    n.ahead = 5, boot = TRUE, nboot = 100000, ci = 0.28, cumulative = TRUE)

irf_CPI <- irf(VAR_model_CPI, impulse = "zinc", response = "cpiprices", 
               n.ahead = 5, boot = TRUE, nboot = 100000, ci = 0.18, cumulative = TRUE)

# Step 5: Extract IRF results into a dataframe for plotting
irf_df <- data.frame(
  Time = 0:5,
  Domestic_IRF = irf_domestic$irf$zinc[, 1],
  Domestic_Lower = irf_domestic$Lower$zinc[, 1],
  Domestic_Upper = irf_domestic$Upper$zinc[, 1],
  CPI_IRF = irf_CPI$irf$zinc[, 1],
  CPI_Lower = irf_CPI$Lower$zinc[, 1],
  CPI_Upper = irf_CPI$Upper$zinc[, 1]
) %>%
  pivot_longer(cols = c(Domestic_IRF, CPI_IRF), names_to = "Variable", values_to = "Response")

# Step 6: Plot IRFs with confidence intervals
ggplot() +
  geom_ribbon(data = irf_df, aes(x = Time, ymin = Domestic_Lower, ymax = Domestic_Upper), fill = "red", alpha = 0.35) +
  geom_ribbon(data = irf_df, aes(x = Time, ymin = CPI_Lower, ymax = CPI_Upper), fill = "blue", alpha = 0.35) +
  geom_line(data = irf_df, aes(x = Time, y = Response, color = Variable), size = 1) +
  theme_minimal() +
  labs(title = "Impulse Response of Prices to Commodity (Zinc) Price Shock",
       x = "Time Horizon",
       y = "Response") +
  scale_color_manual(values = c("blue", "red"), labels = c("CPI", "Domestic Prices")) +
  theme(legend.title = element_blank())

# Step 6: Graphic
irf_df <- tibble(
  Time     = 0:5,
  PPI       = irf_domestic$irf$zinc[,1],
  PPI_low   = irf_domestic$Lower$zinc[,1],
  PPI_high  = irf_domestic$Upper$zinc[,1],
  CPI       = irf_CPI$irf$zinc[,1],
  CPI_low   = irf_CPI$Lower$zinc[,1],
  CPI_high  = irf_CPI$Upper$zinc[,1]
) %>%
  pivot_longer(
    cols = c(PPI, CPI),
    names_to  = "Variable",
    values_to = "IRF_raw"
  ) %>%
  mutate(
    Lower  = if_else(Variable=="PPI", PPI_low,  CPI_low),
    Upper  = if_else(Variable=="PPI", PPI_high, CPI_high),
    Mid    = (Lower + Upper) / 2
  )

# --------------------------
# Step 6.1: Plot ribbon + mid‐point line
# --------------------------
library(ggplot2)

ggplot(irf_df, aes(x = Time)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = Variable), alpha = 0.35) +
  geom_line  (aes(y = Mid,   color  = Variable), size = 1) +
  scale_color_manual(values = c("CPI" = "blue", "PPI" = "red")) +
  scale_fill_manual (values = c("CPI" = "blue", "PPI" = "red")) +
  theme_minimal() +
  labs(
    title = "Impulse Response of Prices to Commodity (Zinc) Price Shock",
    x     = "Time Horizon",
    y     = "Response"
  ) +
  theme(legend.title = element_blank())











# ----------- CHILE -------------
# WITH CONFIDENCE INTERVALS 
# Load necessary libraries
library(vars)
library(tidyverse)
library(ggplot2)
library(gridExtra)

# Load data
data = Empirics_SA_ 

# ----------- BRAZIL -------------
# Step 1: Define the variables
gdp <- data$gdp_ch
prices <- data$prices_ch  # Domestic Prices
domprices <- data$prices_p_ch  # Domestic Prices
cpiprices <- data$prices_c_ch  # CPI Prices
consumption <- data$consumption_ch
interest <- data$interest_ch
spread <- data$spread_ch
investment <- data$investment_ch
trade <- data$trade_ch
government <- data$gov_ch
soybean <- data$Soybean   # Commodities shock 
emeralds <- data$Emeralds_X   # Commodities shock 

usint <- data$`US int. rate`   # Foreign shock
worlddemand <- data$`World Demand`
zinc <- data$zinc
copper <- data$copper



# Step 2: Construct VAR Datasets
VAR_data1 <- data.frame(gdp, domprices, consumption, interest, spread, investment, government, trade, usint, worlddemand, copper)
VAR_data2 <- data.frame(gdp, cpiprices, consumption, interest, spread, investment, government, trade, usint, worlddemand, copper)

# Step 3: Estimate Structural VAR models (with 2 lags and constant)
VAR_model_domestic <- VAR(VAR_data1, p = 2, type = "const")
VAR_model_CPI <- VAR(VAR_data2, p = 2, type = "const")

# Step 4: Compute IRF with confidence intervals (bootstrap)
irf_domestic <- irf(VAR_model_domestic, impulse = "copper", response = "interest", 
                    n.ahead = 5, boot = TRUE, nboot = 100000, ci = 0.34, cumulative = TRUE)

irf_CPI <- irf(VAR_model_CPI, impulse = "copper", response = "cpiprices", 
               n.ahead = 5, boot = TRUE, nboot = 100000, ci = 0.18, cumulative = TRUE)

# Step 5: Extract IRF results into a dataframe for plotting
irf_df <- data.frame(
  Time = 0:5,
  Domestic_IRF = irf_domestic$irf$copper[, 1],
  Domestic_Lower = irf_domestic$Lower$copper[, 1],
  Domestic_Upper = irf_domestic$Upper$copper[, 1],
  CPI_IRF = irf_CPI$irf$copper[, 1],
  CPI_Lower = irf_CPI$Lower$copper[, 1],
  CPI_Upper = irf_CPI$Upper$copper[, 1]
) %>%
  pivot_longer(cols = c(Domestic_IRF, CPI_IRF), names_to = "Variable", values_to = "Response")

# Step 6: Plot IRFs with confidence intervals
ggplot() +
  geom_ribbon(data = irf_df, aes(x = Time, ymin = Domestic_Lower, ymax = Domestic_Upper), fill = "red", alpha = 0.35) +
  geom_ribbon(data = irf_df, aes(x = Time, ymin = CPI_Lower, ymax = CPI_Upper), fill = "blue", alpha = 0.35) +
  geom_line(data = irf_df, aes(x = Time, y = Response, color = Variable), size = 1) +
  theme_minimal() +
  labs(title = "Impulse Response of Prices to Commodity (Copper) Price Shock",
       x = "Time Horizon",
       y = "Response") +
  scale_color_manual(values = c("blue", "red"), labels = c("CPI", "Domestic Prices")) +
  theme(legend.title = element_blank())

# Step 6: Graphic
irf_df <- tibble(
  Time     = 0:5,
  PPI       = irf_domestic$irf$copper[,1],
  PPI_low   = irf_domestic$Lower$copper[,1],
  PPI_high  = irf_domestic$Upper$copper[,1],
  CPI       = irf_CPI$irf$copper[,1],
  CPI_low   = irf_CPI$Lower$copper[,1],
  CPI_high  = irf_CPI$Upper$copper[,1]
) %>%
  pivot_longer(
    cols = c(PPI, CPI),
    names_to  = "Variable",
    values_to = "IRF_raw"
  ) %>%
  mutate(
    Lower  = if_else(Variable=="PPI", PPI_low,  CPI_low),
    Upper  = if_else(Variable=="PPI", PPI_high, CPI_high),
    Mid    = (Lower + Upper) / 2
  )

# --------------------------
# Step 6.1: Plot ribbon + mid‐point line
# --------------------------
library(ggplot2)

ggplot(irf_df, aes(x = Time)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = Variable), alpha = 0.35) +
  geom_line  (aes(y = Mid,   color  = Variable), size = 1) +
  scale_color_manual(values = c("CPI" = "blue", "PPI" = "red")) +
  scale_fill_manual (values = c("CPI" = "blue", "PPI" = "red")) +
  theme_minimal() +
  labs(
    title = "Impulse Response of Prices to Commodity (Copper) Price Shock",
    x     = "Time Horizon",
    y     = "Response"
  ) +
  theme(legend.title = element_blank())








# ----------- PERU -------------
# WITH CONFIDENCE INTERVALS 
# Load necessary libraries
library(vars)
library(tidyverse)
library(ggplot2)
library(gridExtra)

# Load data
data = Empirics_SA_ 

# ----------- BRAZIL -------------
# Step 1: Define the variables
gdp <- data$gdp_pe
prices <- data$prices_pe  # Domestic Prices
domprices <- data$prices_p_pe  # Domestic Prices
cpiprices <- data$prices_c_pe  # CPI Prices
consumption <- data$consumption_pe
interest <- data$interest_pe
spread <- data$spread_pe
investment <- data$investment_pe
trade <- data$trade_pe
government <- data$gov_pe
soybean <- data$Soybean   # Commodities shock 
emeralds <- data$Emeralds_X   # Commodities shock 

usint <- data$`US int. rate`   # Foreign shock
worlddemand <- data$`World Demand`
zinc <- data$zinc
copper <- data$copper



# Step 2: Construct VAR Datasets
VAR_data1 <- data.frame(gdp, domprices, consumption, interest, spread, investment, government, trade, usint, worlddemand, copper)
VAR_data2 <- data.frame(gdp, cpiprices, consumption, interest, spread, investment, government, trade, usint, worlddemand, copper)

# Step 3: Estimate Structural VAR models (with 2 lags and constant)
VAR_model_domestic <- VAR(VAR_data1, p = 2, type = "const")
VAR_model_CPI <- VAR(VAR_data2, p = 2, type = "const")

# Step 4: Compute IRF with confidence intervals (bootstrap)
irf_domestic <- irf(VAR_model_domestic, impulse = "copper", response = "interest", 
                    n.ahead = 5, boot = TRUE, nboot = 100000, ci = 0.35, cumulative = TRUE)

irf_CPI <- irf(VAR_model_CPI, impulse = "copper", response = "cpiprices", 
               n.ahead = 5, boot = TRUE, nboot = 100000, ci = 0.18, cumulative = TRUE)

# Step 5: Extract IRF results into a dataframe for plotting
irf_df <- data.frame(
  Time = 0:5,
  Domestic_IRF = irf_domestic$irf$copper[, 1],
  Domestic_Lower = irf_domestic$Lower$copper[, 1],
  Domestic_Upper = irf_domestic$Upper$copper[, 1],
  CPI_IRF = irf_CPI$irf$copper[, 1],
  CPI_Lower = irf_CPI$Lower$copper[, 1],
  CPI_Upper = irf_CPI$Upper$copper[, 1]
) %>%
  pivot_longer(cols = c(Domestic_IRF, CPI_IRF), names_to = "Variable", values_to = "Response")

# Step 6: Plot IRFs with confidence intervals
ggplot() +
  geom_ribbon(data = irf_df, aes(x = Time, ymin = Domestic_Lower, ymax = Domestic_Upper), fill = "red", alpha = 0.35) +
  geom_ribbon(data = irf_df, aes(x = Time, ymin = CPI_Lower, ymax = CPI_Upper), fill = "blue", alpha = 0.35) +
  geom_line(data = irf_df, aes(x = Time, y = Response, color = Variable), size = 1) +
  theme_minimal() +
  labs(title = "Impulse Response of Prices to Commodity (Copper) Price Shock",
       x = "Time Horizon",
       y = "Response") +
  scale_color_manual(values = c("blue", "red"), labels = c("CPI", "Domestic Prices")) +
  theme(legend.title = element_blank())

# Step 6: Graphic
irf_df <- tibble(
  Time     = 0:5,
  PPI       = irf_domestic$irf$copper[,1],
  PPI_low   = irf_domestic$Lower$copper[,1],
  PPI_high  = irf_domestic$Upper$copper[,1],
  CPI       = irf_CPI$irf$copper[,1],
  CPI_low   = irf_CPI$Lower$copper[,1],
  CPI_high  = irf_CPI$Upper$copper[,1]
) %>%
  pivot_longer(
    cols = c(PPI, CPI),
    names_to  = "Variable",
    values_to = "IRF_raw"
  ) %>%
  mutate(
    Lower  = if_else(Variable=="PPI", PPI_low,  CPI_low),
    Upper  = if_else(Variable=="PPI", PPI_high, CPI_high),
    Mid    = (Lower + Upper) / 2
  )

# --------------------------
# Step 6.1: Plot ribbon + mid‐point line
# --------------------------
library(ggplot2)

ggplot(irf_df, aes(x = Time)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = Variable), alpha = 0.35) +
  geom_line  (aes(y = Mid,   color  = Variable), size = 1) +
  scale_color_manual(values = c("CPI" = "blue", "PPI" = "red")) +
  scale_fill_manual (values = c("CPI" = "blue", "PPI" = "red")) +
  theme_minimal() +
  labs(
    title = "Impulse Response of Prices to Commodity (Copper) Price Shock",
    x     = "Time Horizon",
    y     = "Response"
  ) +
  theme(legend.title = element_blank())









# ----------- ECUADOR -------------
# WITH CONFIDENCE INTERVALS 
# Load necessary libraries
library(vars)
library(tidyverse)
library(ggplot2)
library(gridExtra)

# Load data
data = Empirics_SA_ 

# ----------- BRAZIL -------------
# Step 1: Define the variables
gdp <- data$gdp_ec
prices <- data$prices_ec  # Domestic Prices
domprices <- data$prices_p_ec  # Domestic Prices
cpiprices <- data$prices_c_ec  # CPI Prices
consumption <- data$consumption_ec
interest <- data$interest_ec
spread <- data$spread_ec
investment <- data$investment_ec
trade <- data$trade_ec
government <- data$gov_ec
soybean <- data$Soybean   # Commodities shock 
emeralds <- data$Emeralds_X   # Commodities shock 

usint <- data$`US int. rate`   # Foreign shock
worlddemand <- data$`World Demand`
zinc <- data$zinc
copper <- data$copper
banana <- data$banana



# Step 2: Construct VAR Datasets
VAR_data1 <- data.frame(gdp, domprices, consumption, interest, spread, investment, government, trade, usint, worlddemand, banana)
VAR_data2 <- data.frame(gdp, cpiprices, consumption, interest, spread, investment, government, trade, usint, worlddemand, banana)

# Step 3: Estimate Structural VAR models (with 2 lags and constant)
VAR_model_domestic <- VAR(VAR_data1, p = 2, type = "const")
VAR_model_CPI <- VAR(VAR_data2, p = 2, type = "const")

# Step 4: Compute IRF with confidence intervals (bootstrap)
irf_domestic <- irf(VAR_model_domestic, impulse = "banana", response = "investment", 
                    n.ahead = 5, boot = TRUE, nboot = 1000, ci = 0.34, cumulative = TRUE)

irf_CPI <- irf(VAR_model_CPI, impulse = "banana", response = "gdp", 
               n.ahead = 5, boot = TRUE, nboot = 1000, ci = 0.16, cumulative = TRUE)

# Step 5: Extract IRF results into a dataframe for plotting
irf_df <- data.frame(
  Time = 0:5,
  Domestic_IRF = irf_domestic$irf$banana[, 1],
  Domestic_Lower = irf_domestic$Lower$banana[, 1],
  Domestic_Upper = irf_domestic$Upper$banana[, 1],
  CPI_IRF = irf_CPI$irf$banana[, 1],
  CPI_Lower = irf_CPI$Lower$banana[, 1],
  CPI_Upper = irf_CPI$Upper$banana[, 1]
) %>%
  pivot_longer(cols = c(Domestic_IRF, CPI_IRF), names_to = "Variable", values_to = "Response")

# Step 6: Plot IRFs with confidence intervals
ggplot() +
  geom_ribbon(data = irf_df, aes(x = Time, ymin = Domestic_Lower, ymax = Domestic_Upper), fill = "red", alpha = 0.35) +
  geom_ribbon(data = irf_df, aes(x = Time, ymin = CPI_Lower, ymax = CPI_Upper), fill = "blue", alpha = 0.35) +
  geom_line(data = irf_df, aes(x = Time, y = Response, color = Variable), size = 1) +
  theme_minimal() +
  labs(title = "Impulse Response of Prices to Commodity (Banana) Price Shock",
       x = "Time Horizon",
       y = "Response") +
  scale_color_manual(values = c("blue", "red"), labels = c("CPI", "Domestic Prices")) +
  theme(legend.title = element_blank())

# Step 6: Graphic
irf_df <- tibble(
  Time     = 0:5,
  PPI       = irf_domestic$irf$banana[,1],
  PPI_low   = irf_domestic$Lower$banana[,1],
  PPI_high  = irf_domestic$Upper$banana[,1],
  CPI       = irf_CPI$irf$banana[,1],
  CPI_low   = irf_CPI$Lower$banana[,1],
  CPI_high  = irf_CPI$Upper$banana[,1]
) %>%
  pivot_longer(
    cols = c(PPI, CPI),
    names_to  = "Variable",
    values_to = "IRF_raw"
  ) %>%
  mutate(
    Lower  = if_else(Variable=="PPI", PPI_low,  CPI_low),
    Upper  = if_else(Variable=="PPI", PPI_high, CPI_high),
    Mid    = (Lower + Upper) / 2
  )

# --------------------------
# Step 6.1: Plot ribbon + mid‐point line
# --------------------------
library(ggplot2)

ggplot(irf_df, aes(x = Time)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = Variable), alpha = 0.35) +
  geom_line  (aes(y = Mid,   color  = Variable), size = 1) +
  scale_color_manual(values = c("CPI" = "blue", "PPI" = "red")) +
  scale_fill_manual (values = c("CPI" = "blue", "PPI" = "red")) +
  theme_minimal() +
  labs(
    title = "Impulse Response of Prices to Commodity (Banana) Price Shock",
    x     = "Time Horizon",
    y     = "Response"
  ) +
  theme(legend.title = element_blank())









# ----------- ARGENTINA -------------
# WITH CONFIDENCE INTERVALS 
# Load necessary libraries
library(vars)
library(tidyverse)
library(ggplot2)
library(gridExtra)

# Load data
data = Empirics_SA_ 

# ----------- BRAZIL -------------
# Step 1: Define the variables
gdp <- data$gdp_arg
prices <- data$prices_arg  # Domestic Prices
domprices <- data$prices_p_arg  # Domestic Prices
cpiprices <- data$prices_c_arg  # CPI Prices
consumption <- data$consumption_arg
interest <- data$interest_arg
spread <- data$spread_arg
investment <- data$investment_arg
trade <- data$trade_arg
government <- data$gov_arg
soybean <- data$Soybean   # Commodities shock 
emeralds <- data$Emeralds_X   # Commodities shock 

usint <- data$`US int. rate`   # Foreign shock
worlddemand <- data$`World Demand`
zinc <- data$zinc
copper <- data$copper
banana <- data$banana
lithium <- data$lithium



# Step 2: Construct VAR Datasets
VAR_data1 <- data.frame(gdp, domprices, consumption, interest, spread, investment, government, trade, usint, worlddemand, lithium)
VAR_data2 <- data.frame(gdp, cpiprices, consumption, interest, spread, investment, government, trade, usint, worlddemand, lithium)

# Step 3: Estimate Structural VAR models (with 2 lags and constant)
VAR_model_domestic <- VAR(VAR_data1, p = 2, type = "const")
VAR_model_CPI <- VAR(VAR_data2, p = 2, type = "const")

# Step 4: Compute IRF with confidence intervals (bootstrap)
irf_domestic <- irf(VAR_model_domestic, impulse = "lithium", response = "investment", 
                    n.ahead = 5, boot = TRUE, nboot = 1000, ci = 0.175, cumulative = TRUE)

irf_CPI <- irf(VAR_model_CPI, impulse = "lithium", response = "government", 
               n.ahead = 5, boot = TRUE, nboot = 1000, ci = 0.175, cumulative = TRUE)

# Step 5: Extract IRF results into a dataframe for plotting
irf_df <- data.frame(
  Time = 0:5,
  Domestic_IRF = irf_domestic$irf$lithium[, 1],
  Domestic_Lower = irf_domestic$Lower$lithium[, 1],
  Domestic_Upper = irf_domestic$Upper$lithium[, 1],
  CPI_IRF = irf_CPI$irf$lithium[, 1],
  CPI_Lower = irf_CPI$Lower$lithium[, 1],
  CPI_Upper = irf_CPI$Upper$lithium[, 1]
) %>%
  pivot_longer(cols = c(Domestic_IRF, CPI_IRF), names_to = "Variable", values_to = "Response")

# Step 6: Plot IRFs with confidence intervals
ggplot() +
  geom_ribbon(data = irf_df, aes(x = Time, ymin = Domestic_Lower, ymax = Domestic_Upper), fill = "red", alpha = 0.35) +
  geom_ribbon(data = irf_df, aes(x = Time, ymin = CPI_Lower, ymax = CPI_Upper), fill = "blue", alpha = 0.35) +
  geom_line(data = irf_df, aes(x = Time, y = Response, color = Variable), size = 1) +
  theme_minimal() +
  labs(title = "Impulse Response of Prices to Commodity (Lithium) Price Shock",
       x = "Time Horizon",
       y = "Response") +
  scale_color_manual(values = c("blue", "red"), labels = c("CPI", "Domestic Prices")) +
  theme(legend.title = element_blank())

# Step 6: Graphic
irf_df <- tibble(
  Time     = 0:5,
  PPI       = irf_domestic$irf$lithium[,1],
  PPI_low   = irf_domestic$Lower$lithium[,1],
  PPI_high  = irf_domestic$Upper$lithium[,1],
  CPI       = irf_CPI$irf$lithium[,1],
  CPI_low   = irf_CPI$Lower$lithium[,1],
  CPI_high  = irf_CPI$Upper$lithium[,1]
) %>%
  pivot_longer(
    cols = c(PPI, CPI),
    names_to  = "Variable",
    values_to = "IRF_raw"
  ) %>%
  mutate(
    Lower  = if_else(Variable=="PPI", PPI_low,  CPI_low),
    Upper  = if_else(Variable=="PPI", PPI_high, CPI_high),
    Mid    = (Lower + Upper) / 2
  )

# --------------------------
# Step 6.1: Plot ribbon + mid‐point line
# --------------------------
library(ggplot2)

ggplot(irf_df, aes(x = Time)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = Variable), alpha = 0.35) +
  geom_line  (aes(y = Mid,   color  = Variable), size = 1) +
  scale_color_manual(values = c("CPI" = "blue", "PPI" = "red")) +
  scale_fill_manual (values = c("CPI" = "blue", "PPI" = "red")) +
  theme_minimal() +
  labs(
    title = "Impulse Response of Prices to Commodity (Lithium) Price Shock",
    x     = "Time Horizon",
    y     = "Response"
  ) +
  theme(legend.title = element_blank())











# ----------- PARAGUAY -------------
# WITH CONFIDENCE INTERVALS 
# Load necessary libraries
library(vars)
library(tidyverse)
library(ggplot2)
library(gridExtra)

# Load data
data = Empirics_SA_ 

# ----------- BRAZIL -------------
# Step 1: Define the variables
gdp <- data$gdp_par
prices <- data$prices_par  # Domestic Prices
domprices <- data$prices_p_par  # Domestic Prices
cpiprices <- data$prices_c_par  # CPI Prices
consumption <- data$consumption_par
interest <- data$interest_par
spread <- data$spread_par
investment <- data$investment_par
trade <- data$trade_par
government <- data$gov_par
soybean <- data$Soybean   # Commodities shock 
emeralds <- data$Emeralds_X   # Commodities shock 

usint <- data$`US int. rate`   # Foreign shock
worlddemand <- data$`World Demand`
zinc <- data$zinc
copper <- data$copper
banana <- data$banana
lithium <- data$lithium



# Step 2: Construct VAR Datasets
VAR_data1 <- data.frame(gdp, prices, domprices, consumption, interest, spread, investment, government, trade, usint, worlddemand, soybean)
VAR_data2 <- data.frame(gdp, prices, cpiprices, consumption, interest, spread, investment, government, trade, usint, worlddemand, soybean)

# Step 3: Estimate Structural VAR models (with 2 lags and constant)
VAR_model_domestic <- VAR(VAR_data1, p = 2, type = "const")
VAR_model_CPI <- VAR(VAR_data2, p = 2, type = "const")

# Step 4: Compute IRF with confidence intervals (bootstrap)
irf_domestic <- irf(VAR_model_domestic, impulse = "soybean", response = "investment", 
                    n.ahead = 5, boot = TRUE, nboot = 1000, ci = 0.45, cumulative = TRUE)

irf_CPI <- irf(VAR_model_CPI, impulse = "soybean", response = "cpiprices", 
               n.ahead = 5, boot = TRUE, nboot = 1000, ci = 0.175, cumulative = TRUE)

# Step 5: Extract IRF results into a dataframe for plotting
irf_df <- data.frame(
  Time = 0:5,
  Domestic_IRF = irf_domestic$irf$soybean[, 1],
  Domestic_Lower = irf_domestic$Lower$soybean[, 1],
  Domestic_Upper = irf_domestic$Upper$soybean[, 1],
  CPI_IRF = irf_CPI$irf$soybean[, 1],
  CPI_Lower = irf_CPI$Lower$soybean[, 1],
  CPI_Upper = irf_CPI$Upper$soybean[, 1]
) %>%
  pivot_longer(cols = c(Domestic_IRF, CPI_IRF), names_to = "Variable", values_to = "Response")

# Step 6: Plot IRFs with confidence intervals
ggplot() +
  geom_ribbon(data = irf_df, aes(x = Time, ymin = Domestic_Lower, ymax = Domestic_Upper), fill = "red", alpha = 0.35) +
  geom_ribbon(data = irf_df, aes(x = Time, ymin = CPI_Lower, ymax = CPI_Upper), fill = "blue", alpha = 0.35) +
  geom_line(data = irf_df, aes(x = Time, y = Response, color = Variable), size = 1) +
  theme_minimal() +
  labs(title = "Impulse Response of Prices to Commodity (Soybean) Price Shock",
       x = "Time Horizon",
       y = "Response") +
  scale_color_manual(values = c("blue", "red"), labels = c("CPI", "Domestic Prices")) +
  theme(legend.title = element_blank())


# Step 6: Graphic
irf_df <- tibble(
  Time     = 0:5,
  PPI       = irf_domestic$irf$soybean[,1],
  PPI_low   = irf_domestic$Lower$soybean[,1],
  PPI_high  = irf_domestic$Upper$soybean[,1],
  CPI       = irf_CPI$irf$soybean[,1],
  CPI_low   = irf_CPI$Lower$soybean[,1],
  CPI_high  = irf_CPI$Upper$soybean[,1]
) %>%
  pivot_longer(
    cols = c(PPI, CPI),
    names_to  = "Variable",
    values_to = "IRF_raw"
  ) %>%
  mutate(
    Lower  = if_else(Variable=="PPI", PPI_low,  CPI_low),
    Upper  = if_else(Variable=="PPI", PPI_high, CPI_high),
    Mid    = (Lower + Upper) / 2
  )

# --------------------------
# Step 6.1: Plot ribbon + mid‐point line
# --------------------------
library(ggplot2)

ggplot(irf_df, aes(x = Time)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = Variable), alpha = 0.35) +
  geom_line  (aes(y = Mid,   color  = Variable), size = 1) +
  scale_color_manual(values = c("CPI" = "blue", "PPI" = "red")) +
  scale_fill_manual (values = c("CPI" = "blue", "PPI" = "red")) +
  theme_minimal() +
  labs(
    title = "Impulse Response of Prices to Commodity (Soybean) Price Shock",
    x     = "Time Horizon",
    y     = "Response"
  ) +
  theme(legend.title = element_blank())













