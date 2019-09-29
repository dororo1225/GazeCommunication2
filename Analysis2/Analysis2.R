library(tidyverse)
library(rstan)
library(knitr)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# load data
df <- read_csv("Data_Analysis2.csv", col_types = "cdctdcc")
df_locomotion <- read_csv("Data_locomotion.csv", col_types = "cdcd")

# figure S3
df %>% 
  group_by(Name, AgeinMonths, DistanceCategory) %>%
  summarize(obsNum = n(),
            LocomotorStatus = first(LocomotorStatus)) %>%
  ungroup() %>%
  group_by(Name, AgeinMonths) %>%
  mutate(totalNum = sum(obsNum),
         prop = obsNum / totalNum * 100) %>% 
  ungroup() %>%
  mutate(label = str_c(LocomotorStatus, DistanceCategory, sep = ":")) -> df_bar

cols <- c(c("#ffffcc", "#a1dab4", "#41b6c4", "#225ea8"), c("#ffffd4", "#fed98e", "#fe9929", "#cc4c02"))

df_bar %>% 
  mutate(label = fct_rev(label),
         label = fct_recode(label,
                            'crawler: 0 - 0.5 m' = "crawler:0-0.5",
                            'crawler: 0.5 - 1 m' = "crawler:0.5-1.0",
                            'crawler: 1 - 1.5 m' = "crawler:1.0-1.5",
                            'crawler: 1.5 m -' = "crawler:1.5-",
                            'walker: 0 - 0.5 m' = "walker:0-0.5",
                            'walker: 0.5 - 1 m' = "walker:0.5-1.0",
                            'walker: 1 - 1.5 m' = "walker:1.0-1.5",
                            'walker: 1.5 m -' = "walker:1.5-")) %>% 
  ggplot(aes(x = AgeinMonths, y = prop, fill = label)) +
  geom_bar(color = "black", stat = "identity") +
  facet_grid(Name~.) +
  scale_x_continuous(breaks = seq(min(df$AgeinMonths), max(df$AgeinMonths), by = 1)) +
  scale_fill_manual(values = cols) +
  labs(fill = "Distance Category",
       x = "Age (months)",
       y = "Proportion (%)") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(face = "bold", size = 15),
        axis.text = element_text(color = "black", size = 12),
        strip.text = element_text(face = "bold", size = 15),
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 12))
ggsave("Analysis2/fig_sup3.jpg", dpi = 300, width = 180, height = 180, units = "mm")

# prepare data list for stan model
df %>% 
  select(-PercentWalk, -LocomotorStatus) %>% 
  complete(Name, AgeinMonths) %>%
  left_join(df_locomotion, by = c("Name", "AgeinMonths")) %>% 
  mutate(Label = case_when(
    .$DistanceCategory == "0-0.5" ~ 1,
    .$DistanceCategory == "0.5-1.0" ~ 2,
    .$DistanceCategory == "1.0-1.5" ~ 3,
    .$DistanceCategory == "1.5-" ~ 4)) -> df_stan

df_stan %>%
  mutate(ID_obs = as.numeric(as.factor(AgeinMonths)),
         ID_pair = as.numeric(as.factor(Name))) %>%
  group_by(Name, AgeinMonths) %>% 
  mutate(N = n(),
         Num_NA = sum(is.na(DistanceCategory))) %>% 
  ungroup() %>% 
  mutate(Missing = if_else(N == Num_NA, 1, 0),
         Label = if_else(Missing == 1, 1, Label)) -> df_mc

data <- list(N_total = nrow(df_mc),
             N_pair = length(unique(df_mc$ID_pair)),
             N_obs = length(unique(df_mc$ID_obs)),
             ID_pair = df_mc$ID_pair,
             ID_obs = df_mc$ID_obs,
             K = length(unique(df_mc$Label)),
             X2 = df_mc$PercentWalk,
             Miss = df_mc$Missing,
             d = df_mc$Label)

# model fitting
model <- stan_model(file = "Analysis2/model_Analysis2.stan")
fit <- sampling(model,
                data = data,
                chains = 4,
                iter = 11000,
                warmup = 1000,
                refresh = 0,
                sample_file = "Analysis2/model.csv",
                seed = 12345)

summary(fit, pars = c("beta1", "beta2", "c", "sigma_sys", "b1_over", "b2_over"))$summary %>% 
  signif(digits = 3) %>% kable()
