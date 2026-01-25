#Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  here,
  tidyverse,
  fixest,
  flextable,
  modelsummary,
  readxl)

###---------------------------------------------------------------------------###
### Data proccessing
###---------------------------------------------------------------------------###

###Read, clean and merge data

#Read nominal gross value added (nama_10_a10), total hours worked (nama_10_a10_e), and capital stock (nama_10_nfa_st)
#Rename columns of interest and drop the rest
df_gva <- read_csv("nama_10_a10__custom_19753486_linear.csv") %>%
  select(country = geo,
         year = TIME_PERIOD,
         sector = nace_r2,
         Y = OBS_VALUE)
df_emp <- read_csv("nama_10_a10_e__custom_19757365_linear.csv") %>%
  select(country = geo,
         year = TIME_PERIOD,
         sector = nace_r2,
         L = OBS_VALUE)
df_cap <- read_csv("nama_10_nfa_st__custom_19757768_linear.csv") %>%
  select(country = geo,
         year = TIME_PERIOD,
         sector = nace_r2,
         K = OBS_VALUE)

#Merge the three dataframes
df_outputs <- df_gva %>%
  left_join(df_emp, by = c("country", "year", "sector")) %>%
  left_join(df_cap, by = c("country", "year", "sector"))


###---------------------------------------------------------------------------###
### Compute productivity variables (TFP, TFP growth)
###---------------------------------------------------------------------------###

#Define alpha
alpha <- 0.33

#Compute TFP
df_outputs <- df_outputs %>% mutate(A = Y / (K^alpha * L^(1 - alpha)))

#Compute annual TFP growth
df_outputs <- df_outputs %>%
  arrange(country, sector, year) %>%  # Rearrange
  group_by(country, sector) %>%       # Group before calculating
  mutate(tfp_growth = log(A) - lag(log(A))) %>% #Calculate the growth rate as the log differences between a year and its lag
  ungroup()

###---------------------------------------------------------------------------###
### Plot country TFP growth and an output-weighted aggregate across countries in each year
###---------------------------------------------------------------------------###

#Calculate output-weighted aggregate
df_aggregate <- df_outputs %>% 
  filter(sector == "Total - all NACE activities") %>%
  group_by(year) %>% #Group by year and calculate dynamic Weight
  mutate(
    weight = Y / sum(Y),
    weighted_growth = tfp_growth * weight #Contribution to aggregate growth by country
  ) %>%
  summarise(
    avg_growth = sum(weighted_growth, na.rm = TRUE)) %>%  #Calculate output-weighted aggregate across countries
  ungroup()

#Calculate the mean to include it in the plot
total_mean <- mean(df_aggregate$avg_growth, na.rm = TRUE)
#Create the text label
total_label <- paste0("Aggregate TFP growth mean: ", round(total_mean * 100, 2), "%")


#Create plot
plot_tfp_growth <- ggplot() +
  #For the five countries
  geom_line(data = df_outputs %>% filter(sector == "Total - all NACE activities"), 
            aes(x = year, y = tfp_growth, color = country), 
            alpha = 0.9, linewidth = 1) +
  
  #For the aggregate
  geom_line(data = df_aggregate, 
            aes(x = year, y = avg_growth, linetype="Weighted aggregate"), 
            color = "black", linewidth = 1) +

  #Add annotation
  annotate("text", x = 2000, y = 0.05, label = total_label, 
           hjust = 0, fontface = "bold", size = 3) +
  #Format
  labs(title = "TFP growth: countries and weighted aggregate",
       subtitle = "Total economy",
       y = "Annual TFP growth (log differences)",
       x = "Year",
       color = "Country",
       linetype=""
       ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
  theme_minimal() +
  theme(legend.position = "right")+
  
  scale_x_continuous(breaks = seq(2000, 2023, by = 2)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))
#Print plot
print(plot_tfp_growth)
#Save plot
ggsave("tfpgrowth.png", plot = plot_tfp_growth, width = 10, height = 6, dpi = 600)

###------------ --------------------------------------------------------------###
### Plot country TFP growth and an output-weighted aggregate by sector across countries in each year
### Include recession data in the analysis
###---------------------------------------------------------------------------###

#Read recession file
df_rec <- read_excel("recession.xlsx")
#I use "Peak included" column to identify quarters with recessions 
#allowing to capture the turning point of the cycle
rec_periods <- df_rec %>%
  filter(Dates >= 2000 & Dates <= 2023.75) %>% #Drop the periods outside the 2000-2023 range
  arrange(Dates) %>%
  mutate(yes_rec = `Peak included` == 1,
    shade_id = cumsum(yes_rec != lag(yes_rec, default = FALSE))) %>%
  filter(yes_rec) %>%
  group_by(shade_id) %>%
  summarise(
    xmin = min(Dates),       
    xmax = max(Dates) + 0.25)
#Create vector of the sectors
target_sectors <- c(
  "Agriculture, forestry and fishing",
  "Manufacturing",
  "Construction",
  "Information and communication",
  "Financial and insurance activities",
  "Real estate activities"
)

df_aggregatesectors <- df_outputs %>%
  filter(sector %in% target_sectors) %>%
#Calculate sector weights
  group_by(year, sector) %>%
  mutate(
    sector_weight = Y / sum(Y), #Dynamic weights
    sector_weighted_growth = tfp_growth * sector_weight, #Contribution to sector's aggregate growth by country
    avg_sector_growth = sum(sector_weighted_growth, na.rm = TRUE) #Calculate output-weighted aggregate by sector across countries
  ) %>%
  ungroup()
#Calculate the mean by sector to include it in the plot
sector_means <- df_aggregatesectors %>%
  distinct(year, sector, avg_sector_growth) %>%
  group_by(sector) %>%
#Create the text label
  summarise(
    mean_label = paste0("Aggregate TFP growth mean: ", round(mean(avg_sector_growth, na.rm = TRUE) * 100, 2), "%")
  )

###Create plot
plot_tfp_sectoral_growth <- ggplot() +
  #Recession shading using 'rec_years' dataframe
  geom_rect(data = rec_periods,
            aes(xmin = xmin, xmax = xmax, 
                ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.1) +
  #For the five countries
  geom_line(data = df_aggregatesectors, 
            aes(x = year, y = tfp_growth, color = country), 
            alpha = 0.6, linewidth = 0.7) +
  #For the sectoral aggregate
  geom_line(data = df_aggregatesectors %>% distinct(year, sector, avg_sector_growth), 
            aes(x = year, y = avg_sector_growth, linetype = "Weighted aggregate"), 
            color = "black", linewidth = 0.9) +
  #Add annotation
  geom_text(data = sector_means,
            aes(x = 2001, y = 0.22, label = mean_label),
            hjust = 0,
            size = 3) +
  #Split sectors into panels
  facet_wrap(~ sector, ncol = 2) +
  #Format
  labs(title = "TFP growth: countries and weighted aggregate",
       subtitle = "Six sectors (A, C, F, J, K, L). Recession years in gray.",
       y = "Annual TFP growth (log differences)",
       x = "Year",
       color = "Country",
       linetype = ""
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
  theme_minimal() +
  theme(legend.position = "right",strip.text = element_text(face = "bold"))+
  scale_x_continuous(breaks = seq(2000, 2023, by = 4)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  coord_cartesian(ylim = c(-0.25, 0.25))

# Print plot
print(plot_tfp_sectoral_growth)

# Save plot
ggsave("tfpsectoralgrowth.png", plot = plot_tfp_sectoral_growth, width = 12, height = 8, dpi = 600)

###----------------------------------------------------------------------------###
### Compute and plot exports as share of the output
###----------------------------------------------------------------------------###

###Read, clean and merge exports data
df_exp <- read_csv("ext_tec09__custom_19760607_linear.csv") %>%
  select(country = geo,
         year = TIME_PERIOD,
         sector = nace_r2,
         X = OBS_VALUE)
#Adjust exports to million euro unit to match output units
df_exp <- df_exp %>%
  mutate(X = X / 1000)
#Merge output and exports dataframes
df_outputsplusexp <- df_outputs %>%
  filter(year >= 2012) %>%
  left_join(df_exp, by = c("country", "year", "sector")) %>%
#Create exports as a percentage of output
  mutate(X_share = (X / Y)) %>%
  mutate(X_percentage = X_share*100)
#Create stats table
stats_X_share <- df_outputsplusexp %>%
  filter(!is.na(X_percentage)) %>% #Remove missing data
  group_by(sector) %>%
  summarise(
    Mean_Exp_Share = mean(X_percentage),
    Median = median(X_percentage),
    Min = min(X_percentage),
    Max = max(X_percentage),
    SD = sd(X_percentage))
#View the table
print(stats_X_share)
#Create table object
#install.packages("flextable") #Install only if not installed previously
ft <- flextable(stats_X_share) %>% 
  #Format
  colformat_double(digits = 2) %>%
  autofit() %>%
  theme_vanilla()
#Print table
print(ft)
#Save table
save_as_docx(ft, path = "X_share_stats.docx")

###Plot share of exports against productivity

df_outputsplusexp <- df_outputsplusexp %>%
  mutate(log_tfp = log(A))
###Linear model plot (total economy)
plot_logtfp_exp <- df_outputsplusexp %>%
  filter(sector == "Total - all NACE activities") %>%
  ggplot(aes(x = X_percentage, y = log_tfp)) +
  geom_point(aes(color = country), alpha = 0.5) +
  geom_smooth(method = "lm", color = "black") +
  labs(title = "Log productivity vs exports as percentage of output (total economy)",
       x = "Exports (% of output)", y = "Log TFP") +
  theme_minimal()
#Save plot
ggsave("logtfpvsexports.png", plot = plot_logtfp_exp, width = 12, height = 8, dpi = 600)

###Linear model plot (by sector)
plot_logtfp_exp_sector <- df_outputsplusexp %>%
  filter(sector != "Total - all NACE activities") %>%
  ggplot(aes(x = X_percentage, y = log_tfp)) +
  geom_point(aes(color = country), alpha = 0.5) +
  geom_smooth(method = "lm", color = "black") +
  facet_wrap(~ sector, scales = "free") +
  labs(title = "Log productivity vs exports as percentage of output by sector",
       x = "Exports (% of output)", y = "Log TFP") +
  theme_minimal() +
  theme(legend.position = "bottom")
#Save plot
ggsave("logtfpvsexportssector.png", plot = plot_logtfp_exp_sector, width = 12, height = 8, dpi = 600)

###----------------------------------------------------------------------------###
### Econometric analysis of exports share impact on productivy
###----------------------------------------------------------------------------###

###Fixed effects panel regression
#FE with heteroskedasticiy-robust standard errors
model_fe_robust <- feols(log_tfp ~ X_percentage | country + sector + year, df_outputsplusexp %>% filter(sector != "Total - all NACE activities"), vcov = "hetero")
#FE with cluster standard erros by country
model_fe_cluster <- feols(log_tfp ~ X_percentage | country + sector + year, df_outputsplusexp %>% filter(sector != "Total - all NACE activities"), cluster=~country)
#Summary of the models
etable(model_fe_robust,model_fe_cluster)
#Format table to report
regressiontable <- modelsummary(
  list("Robust" = model_fe_robust, "Cluster" = model_fe_cluster), 
  output = "flextable",
  stars = TRUE,
  fmt = 4,
  gof_map = c("nobs", "r.squared", "FE: country", "FE: sector", "FE: year")
) %>% 
  autofit() %>% 
  theme_vanilla()
#Save to Word
save_as_docx(regressiontable, path = "regressiontable.docx")
#Quick check of model results if variables were reversed
model_fe_reversed <- feols(X_percentage ~ log_tfp | country + sector + year, df_outputsplusexp %>% filter(sector != "Total - all NACE activities"), vcov = "hetero")
etable(model_fe_reversed)

