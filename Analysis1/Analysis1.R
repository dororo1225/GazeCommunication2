library(tidyverse)
library(knitr)
library(rstan)
library(ggmcmc)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# load data
df <- read_csv("Data_processed.csv", col_types = "cdccddcdddd") 

df %>% 
  group_by(Name, AgeinMonths) %>% 
  mutate(Session_lag = lag(SessionID, default = 0)) %>% 
  ungroup() %>% 
  mutate(NewSession = if_else(SessionID != Session_lag, 1, 0),
         ID_session = cumsum(NewSession)) %>% 
  complete(Name, AgeinMonths) %>%
  mutate(Missing = if_else(Name == "E" & AgeinMonths == 12, 1, 0),
         PercentWalk = if_else(Missing == 1, 0, PercentWalk),
         ID_pair = as.numeric(as.factor(Name)),
         ID_obs = as.numeric(as.factor(AgeinMonths))) %>% 
  select(Name, AgeinMonths, ID_pair, ID_obs, ID_session, Initiator, Distance, PercentWalk, Missing) -> df_all

df_all %>%
  filter(Missing != 1) -> df_stan

df_all %>% 
  select(Name, AgeinMonths, PercentWalk) %>% 
  distinct() %>% 
  arrange(Name, AgeinMonths) -> df_summary

# infant-led EC bout
## prepare data list for stan model
df_stan %>% 
  filter(Initiator == "infant") %>% 
  mutate(ID_session_infant = as.numeric(as.factor(ID_session))) -> df_infant

data <- list(N = nrow(df_infant),
             N_pair = length(unique(df_infant$Name)),
             N_obs = length(unique(df_infant$AgeinMonths)),
             N_session = length(unique(df_infant$ID_session_infant)),
             Y = df_infant$Distance,
             X2 = df_infant$PercentWalk,
             ID_pair = df_infant$ID_pair,
             ID_obs = df_infant$ID_obs,
             ID_session = df_infant$ID_session_infant,
             X2_pre = t(matrix(df_summary$PercentWalk,
                               length(unique(df_summary$AgeinMonths)),
                               length(unique(df_summary$Name)))))

## model fitting
model <- stan_model(file = "Analysis1/model_Analysis1.stan")

fit_I <- sampling(model,
                  data = data,
                  chains = 4,
                  iter = 11000,
                  warmup = 1000,
                  refresh = 0,
                  sample_file = "Analysis1/infant.csv",
                  seed = 12345)

summary(fit_I, pars = c("beta1", "beta2", "sigma_obs", "sigma_sys", "sigma_session", "b1_over", "b2_over"))$summary %>%
  signif(digits = 3) %>% kable()

# parent-led EC
## prepare data list for stan model
df_stan %>% 
  filter(Initiator == "parent") %>% 
  mutate(ID_session_parent = as.numeric(as.factor(ID_session))) -> df_parent

data <- list(N = nrow(df_parent),
             N_pair = length(unique(df_parent$Name)),
             N_obs = length(unique(df_parent$AgeinMonths)),
             N_session = length(unique(df_parent$ID_session_parent)),
             Y = df_parent$Distance,
             X2 = df_parent$PercentWalk,
             ID_pair = df_parent$ID_pair,
             ID_obs = df_parent$ID_obs,
             ID_session = df_parent$ID_session_parent,
             X2_pre = t(matrix(df_summary$PercentWalk,
                               length(unique(df_summary$AgeinMonths)),
                               length(unique(df_summary$Name)))))

## model fitting
model <- stan_model(file = "Analysis1/model_Analysis1.stan") # same model with infant-led EC bout

fit_P <- sampling(model,
                  data = data,
                  chains = 4,
                  iter = 11000,
                  warmup = 1000,
                  refresh = 0,
                  sample_file = "Analysis1/parent.csv",
                  seed = 12345)

summary(fit_P, pars = c("beta1", "beta2", "sigma_obs", "sigma_sys", "sigma_session", "b1_over", "b2_over"))$summary %>%
  signif(digits = 3) %>% kable()

# figure 2
ggs(fit_I) %>%
  filter(str_detect(.$Parameter, pattern = "mu")) %>%
  group_by(Parameter) %>%
  summarise(EAP = mean(value),
            q_975 = quantile(value, probs = 0.975),
            q_025 = quantile(value, probs = 0.025)) %>%
  mutate(id_pair = str_extract(.$Parameter, pattern = "[[:digit:]]+"),
         id_obs = str_replace(.$Parameter, pattern = id_pair, replacement = ""),
         id_obs = str_extract(id_obs, pattern = "[[:digit:]]+"),
         Name = unique(df_stan$Name)[as.numeric(id_pair)],
         AgeinMonths = unique(df_stan$AgeinMonths)[as.numeric(id_obs)],
         Initiator = "infant") %>%
  left_join(df_summary, by = c("Name", "AgeinMonths")) %>%
  select(-1) -> df_sample_I

ggs(fit_P) %>%
  filter(str_detect(.$Parameter, pattern = "mu")) %>%
  group_by(Parameter) %>%
  summarise(EAP = mean(value),
            q_975 = quantile(value, probs = 0.975),
            q_025 = quantile(value, probs = 0.025)) %>%
  mutate(id_pair = str_extract(.$Parameter, pattern = "[[:digit:]]+"),
         id_obs = str_replace(.$Parameter, pattern = id_pair, replacement = ""),
         id_obs = str_extract(id_obs, pattern = "[[:digit:]]+"),
         Name = unique(df_stan$Name)[as.numeric(id_pair)],
         AgeinMonths = unique(df_stan$AgeinMonths)[as.numeric(id_obs)],
         Initiator = "parent") %>%
  left_join(df_summary, by = c("Name", "AgeinMonths")) %>%
  select(-1) -> df_sample_P

df %>%
  mutate(Dist_log = log(Distance),
         Initiator = fct_recode(Initiator, 'Infant-led EC' = "infant", 'Parent-led EC' = "parent")) -> df_draw0

bind_rows(df_sample_I, df_sample_P) %>%
  mutate(Initiator = fct_recode(Initiator, 'Infant-led EC' = "infant", 'Parent-led EC' = "parent")) -> df_draw1

# figure 2
df_draw0 %>% filter(Name == "A") -> d0
df_draw1 %>% filter(Name == "A") -> d1

ggplot(d0, aes(x = AgeinMonths)) +
  geom_ribbon(data = d1, aes(ymax = q_975, ymin = q_025), fill = "grey80") +
  geom_point(aes(y = Dist_log, fill = PercentWalk), shape = 21, color = "black", alpha = 0.5) +
  geom_line(data = d1, aes(y = EAP), lwd = 1) +
  geom_point(data = d1, aes(y = EAP, fill = PercentWalk), size = 3, shape = 21, color = "black") +
  facet_grid(.~Initiator) +
  scale_fill_viridis_c(option = "magma") +
  scale_x_continuous(breaks = seq(10, 15.5, by = 1)) +
  theme_bw() +
  labs(x = "Age (months)",  y = "log-Distance", color = "% Walk", fill = "% Walk") +
  theme(panel.grid = element_blank(),
        axis.title = element_text(face = "bold", size = 12),
        axis.text = element_text(color = "black", size = 10),
        strip.text = element_text(face = "bold", size = 12),
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 8))
ggsave("Analysis1/figure2.jpg", dpi = 300, width = 180, height = 80, units = "mm")

# figure S2
ggplot(df_draw0, aes(x = AgeinMonths)) +
  geom_ribbon(data = df_draw1, aes(ymax = q_975, ymin = q_025), fill = "grey80") +
  geom_point(aes(y = Dist_log, fill = PercentWalk), shape = 21, color = "black", alpha = 0.5) +
  geom_line(data = df_draw1, aes(y = EAP), lwd = 1) +
  geom_point(data = df_draw1, aes(y = EAP, fill = PercentWalk), size = 3, shape = 21, color = "black") +
  facet_grid(Name~Initiator) +
  scale_color_viridis_c(option = "magma") +
  scale_fill_viridis_c(option = "magma") +
  scale_x_continuous(breaks = seq(10, 15.5, by = 1)) +
  theme_bw() +
  labs(x = "Age (months)",  y = "log-Distance", color = "% Walk", fill = "% Walk") +
  theme(panel.grid = element_blank(),
        axis.title = element_text(face = "bold", size = 15),
        axis.text = element_text(color = "black", size = 12),
        strip.text = element_text(face = "bold", size = 15),
        legend.title = element_text(face = "bold", size = 15),
        legend.text = element_text(size = 12))
ggsave("Analysis1/fig_sup2.jpg", dpi = 300, width = 180, height = 200, units = "mm")
