library(tidyverse)
library(dslabs)
data("temp_carbon")
data("greenhouse_gases")
data("historic_co2")
str(temp_carbon)

#latest year
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  max(year)
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  .$year %>%
  min()

# differenc carbon emissions between first and last year
temp_carbon %>%
  filter(year %in% c("1751","2014")) %>% 
  ggplot(aes(carbon_emissions))


temp_carbon$carbon_emissions[which(temp_carbon$year == "2014")]/temp_carbon$carbon_emissions[which(temp_carbon$year == "1751")]

y <- temp_carbon$carbon_emissions[1]
help(ind)

# temperature anomaly 
temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  .$year %>%
  max()

temp_carbon$temp_anomaly[which(temp_carbon$year == "2018")]-temp_carbon$temp_anomaly[which(temp_carbon$year == "1880")]
max(temp_carbon$temp_anomaly, na.rm = TRUE)

p <- temp_carbon %>%
  filter(!is.na(temp_anomaly), !is.na(ocean_anomaly), !is.na(land_anomaly)) %>%
  ggplot() +
  geom_line(aes(year,temp_anomaly)) + 
  geom_line(aes(year, ocean_anomaly), col = "blue") +
  geom_line(aes(year, land_anomaly), col = "green") +
  geom_hline(aes(yintercept = 0), col = "blue") +
  ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue")
p

# green house gases

greenhouse_gases %>%
  ggplot(aes(year, concentration)) +
  geom_line() +
  facet_grid(gas~., scales = "free") +
  geom_vline(aes(xintercept = 1850)) +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  ggplot(aes(year, carbon_emissions)) +
  geom_line() 

# co2 over time

co2_time <- historic_co2 %>%
  filter(!is.na(co2))%>%
  ggplot(aes(year, co2, color = source)) +
  geom_line() +
  xlim(-3000, 2018)
co2_time
help("scale_x_continuous")
