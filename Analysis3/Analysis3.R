library(tidyverse)
library(knitr)
library(rstan)
library(ggmcmc)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# load data
df <- read_csv("Data_processed.csv", col_types = "cdccddcdddd") 
df_locomotion <- read_csv("Data_locomotion.csv", col_types = "cdcd")

df %>% 
  filter(!is.na(floor), !is.na(infant)) %>% 
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
  select(Name, AgeinMonths, ID_pair, ID_obs, ID_session, Initiator, floor, infant, Distance, PercentWalk, Missing) -> df_stan

expand.grid(Distance = seq(min(df_stan$Distance, na.rm = TRUE),
                           max(df_stan$Distance, na.rm = TRUE), length = 40),
            Name = unique(df_stan$Name),
            AgeinMonths = unique(df_stan$AgeinMonths),
            stringsAsFactors = FALSE) %>% 
  as_tibble() %>% 
  left_join(df_locomotion, by = c("Name", "AgeinMonths")) %>% 
  mutate(ID_pair = as.numeric(as.factor(Name)),
         ID_obs = as.numeric(as.factor(AgeinMonths)),
         id = row_number()) -> df_predict

# the number of objects on the floor between dyad
## infant-led EC
df_stan %>% 
  filter(Initiator == "infant") %>% 
  mutate(ID_session_infant = as.numeric(as.factor(ID_session))) -> df_infant

### prepare data list for stan model
data <- list(N = nrow(df_infant),
             N_pair = length(unique(df_infant$Name)),
             N_obs = length(unique(df_infant$AgeinMonths)),
             N_session = length(unique(df_infant$ID_session_infant)),
             Y = df_infant$floor,
             X2 = df_infant$PercentWalk,
             X3 = df_infant$Distance,
             ID_pair = df_infant$ID_pair,
             ID_obs = df_infant$ID_obs,
             ID_session = df_infant$ID_session_infant,
             N_predict = nrow(df_predict),
             ID_pair_predict = df_predict$ID_pair,
             ID_obs_predict = df_predict$ID_obs,
             X2_predict = df_predict$PercentWalk,
             X3_predict = df_predict$Distance)

### model fitting
model_floor <- stan_model(file = "Analysis3/model_floor.stan")
fit_floor_I <- sampling(model_floor,
                        data = data,
                        chains = 4,
                        iter = 11000,
                        warmup = 1000,
                        sample_file = "Analysis3/floor_infant.csv",
                        seed = 123)

summary(fit_floor_I, pars = c("beta1", "beta2", "beta3", "sigma_sys", "sigma_session",
                         "b1_over", "b2_over", "b3_over"))$summary %>%
  signif(digits = 3)

## parent-led EC
df_stan %>% 
  filter(Initiator == "parent") %>% 
  mutate(ID_session_parent = as.numeric(as.factor(ID_session))) -> df_parent

# prepare data list for stan model
data <- list(N = nrow(df_parent),
             N_pair = length(unique(df_parent$Name)),
             N_obs = length(unique(df_parent$AgeinMonths)),
             N_session = length(unique(df_parent$ID_session_parent)),
             Y = df_parent$floor,
             X2 = df_parent$PercentWalk,
             X3 = df_parent$Distance,
             ID_pair = df_parent$ID_pair,
             ID_obs = df_parent$ID_obs,
             ID_session = df_parent$ID_session_parent,
             N_predict = nrow(df_predict),
             ID_pair_predict = df_predict$ID_pair,
             ID_obs_predict = df_predict$ID_obs,
             X2_predict = df_predict$PercentWalk,
             X3_predict = df_predict$Distance)

### model fitting
fit_floor_P <- sampling(model_floor,
                        data = data,
                        chains = 4,
                        iter = 11000,
                        warmup = 1000,
                        sample_file = "Analysis3/floor_parent.csv",
                        seed = 123)

summary(fit_floor_P, pars = c("beta1", "beta2", "beta3", "sigma_sys", "sigma_session",
                         "b1_over", "b2_over", "b3_over"))$summary %>%
  signif(digits = 3)

# # the number of objects in infant's hands
## infant-led EC
### prepare data list for stan model
data <- list(N = nrow(df_infant),
             N_pair = length(unique(df_infant$Name)),
             N_obs = length(unique(df_infant$AgeinMonths)),
             N_session = length(unique(df_infant$ID_session_infant)),
             Y = df_infant$infant,
             X2 = df_infant$PercentWalk,
             X3 = df_infant$Distance,
             ID_pair = df_infant$ID_pair,
             ID_obs = df_infant$ID_obs,
             ID_session = df_infant$ID_session_infant)

### model fitting
model_hands <- stan_model(file = "Analysis3/model_hands.stan")
fit_hands_I <- sampling(model_hands,
                        data = data,
                        chains = 4,
                        iter = 11000,
                        warmup = 1000,
                        sample_file = "Analysis3/hands_infant.csv",
                        seed = 123)

summary(fit_hands_I, pars = c("beta1", "beta2", "beta3", "sigma_sys", "b1_over", "b2_over", "b3_over"))$summary %>%
  signif(digits = 3)

## parent-led EC
data <- list(N = nrow(df_parent),
             N_pair = length(unique(df_parent$Name)),
             N_obs = length(unique(df_parent$AgeinMonths)),
             N_session = length(unique(df_parent$ID_session_parent)),
             Y = df_parent$infant,
             X2 = df_parent$PercentWalk,
             X3 = df_parent$Distance,
             ID_pair = df_parent$ID_pair,
             ID_obs = df_parent$ID_obs,
             ID_session = df_parent$ID_session_parent)

### model fitting
fit_hands_P <- sampling(model_hands,
                        data = data,
                        chains = 4,
                        iter = 11000,
                        warmup = 1000,
                        sample_file = "Analysis3/hands_parent.csv",
                        seed = 123)

summary(fit_hands_P, pars = c("beta1", "beta2", "beta3", "sigma_sys", "b1_over", "b2_over", "b3_over"))$summary %>%
  signif(digits = 3)

## figure 3
ggs(fit_floor_I) %>%
  filter(str_detect(.$Parameter, pattern = "lambda")) %>%
  group_by(Parameter) %>%
  summarise(EAP = mean(value),
            q_975 = quantile(value, probs = 0.975),
            q_025 = quantile(value, probs = 0.025)) %>%
  mutate(id = as.numeric(str_extract(.$Parameter, pattern = "[[:digit:]]+"))) %>%
  left_join(df_predict, by = "id") -> df_fit_I

ggs(fit_floor_P) %>%
  filter(str_detect(.$Parameter, pattern = "lambda")) %>%
  group_by(Parameter) %>%
  summarise(EAP = mean(value),
            q_975 = quantile(value, probs = 0.975),
            q_025 = quantile(value, probs = 0.025)) %>%
  mutate(id = as.numeric(str_extract(.$Parameter, pattern = "[[:digit:]]+"))) %>%
  left_join(df_predict, by = "id") -> df_fit_P

df_fit1_I %>%
  filter(Name == "A", AgeinMonths == 15.5) %>%
  select(Distance, Name, AgeinMonths, EAP, q_975, q_025) %>%
  mutate(Initiator = "Infant-led EC") -> d1

df_stan  %>%
  filter(Name == "A", AgeinMonths == 15.5) %>%
  group_by(Initiator) %>%
  summarise(Min = min(Distance),
            Max = max(Distance)) -> df_thr

thr_upr <- df_thr$Max[2]
thr_lwr <- df_thr$Min[2]

df_fit_P %>%
  filter(Name == "A", AgeinMonths == 15.5) %>%
  filter(Distance >= thr_lwr - 0.1, Distance <= thr_upr + 0.1) %>%
  select(Distance, Name, AgeinMonths, EAP, q_975, q_025) %>%
  mutate(Initiator = "Parent-led EC") -> d2

df_stan %>%
  mutate(Initiator = fct_recode(Initiator, 'Infant-led EC' = "infant", 'Parent-led EC' = "parent")) %>%
  filter(Name == "A", AgeinMonths == 15.5) %>%
  ggplot(aes(x = Distance)) +
  geom_ribbon(data = bind_rows(d1, d2), aes(ymax = q_975, ymin = q_025, fill = Initiator), alpha = 0.5) +
  geom_point(aes(y = floor, color = Initiator), size = 2, alpha = 0.75) +
  geom_line(data = bind_rows(d1, d2), aes(y = EAP, color = Initiator), lwd = 1.5) +
  facet_grid(.~Initiator) +
  scale_x_continuous(breaks = seq(0, 4, by = 0.5), limits = c(0, 4.2)) +
  scale_color_manual(values = c("deepskyblue3", "deeppink3")) +
  scale_fill_manual(values = c("deepskyblue3", "deeppink3")) +
  guides(fill = FALSE, color = FALSE) +
  labs(x = "Distance (m)", y = "Count", color = "") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(face = "bold", size = 12),
        axis.text = element_text(color = "black", size = 10),
        strip.text = element_text(face = "bold", size = 12),
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 12))
ggsave("Analysis3/figure3.jpg", dpi = 300, width = 180, height = 80, units = "mm")
