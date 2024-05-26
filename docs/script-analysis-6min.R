## analysis of 6min data


# loading basic packages 
library(tidyverse)
library(readxl)
library(jtools)
library(ggiraphExtra)
library(rstatix)
library(FSA)
library(modelr)
library(broom)
library(purrr)
library(huxtable)
library(sjPlot)
library(stargazer)
library(ggpubr)
library(car)

# Importing the different datasets needed for the analysis
session_data <- readRDS("data/derivedData/session-data-6min.RDS")
full_session_data <- readRDS("data/derivedData/session-data.RDS")

test_data  <- read_excel("data/testday1.xlsx", na = "na")
session_id <- read_excel("data/sessions-data.xlsx", na = "na")
trimp_data <- read_excel("data/trimp-data.xlsx", na = "na")
session_dataset <- read_excel("data/sessions-data.xlsx", na = "na")
trimp_data2 <- read_excel("data/diary-data.xlsx", na = "na")
hb_data <- read_excel("data/hb-data.xlsx", na = "na")
testdag2 <- read_excel("data/testday2.xlsx", na = "na")
diary_data <- read_excel("data/diary-data.xlsx", na = "na")
history_data <- read_excel("data/history-data.xlsx", na = "na")

testdag2 %>% 
  summarise(mean_t = mean(temperature, na.rm = TRUE),
            sd_t = sd(temperature, na.rm = TRUE),
            mean_h = mean(humidity, na.rm = TRUE),
            sd_h = sd(humidity, na.rm = TRUE))




session_dataset %>% 
  dplyr::group_by(id) %>% 
  summarise(mean_t = mean(temperature, na.rm = TRUE),
            sd_t = sd(temperature, na.rm = TRUE),
            mean_h = mean(humidity, na.rm = TRUE),
            sd_h = sd(humidity, na.rm = TRUE)) %>% 
  summarise(mean_t = mean(mean_t, na.rm = TRUE),
            sd_t = mean(sd_t, na.rm = TRUE),
            mean_h = mean(mean_h, na.rm = TRUE),
            sd_h = mean(sd_h, na.rm = TRUE)) 
 




test_data %>% 
  dplyr::select(id, temperature, humidity) %>% 
  pivot_longer(names_to = "variable",
               values_to = "values",
               cols = c(temperature, humidity)) %>% 
  dplyr::group_by(variable) %>% 
  summarise(mean = mean(values, na.rm = TRUE),
            sd = sd(values, na.rm = TRUE)) 
 

# Calculate the "consistency" of %VO2max

 icc_dat <- session_data %>%
  group_by(id, period, session) %>% 
  summarise(vo2.mean = mean(vo2, na.rm = TRUE),
            hr.mean = mean(hr, na.rm = TRUE)) %>%
    inner_join(test_data %>% 
                 dplyr::select(id, timepoint, vo2.max, vo2.rel.max, hr.max) %>%
                 mutate(period = gsub("T", "", timepoint)) %>%
                 filter(timepoint != "T4") %>%
                 dplyr::select(-timepoint) %>%
                 mutate(period = as.numeric(period))) %>%
    mutate(rel.vo2 = vo2.mean / vo2.max) %>%
    dplyr::filter(id != 1, id != 6, id != 8, id != 23, id != 24, id != 30, id != 36, id != 37, id != 27)

 
library(ICC)
 
ICCest(id, rel.vo2, data = icc_dat, alpha = 0.05)

 # icc model
 library(lme4)
 m <- lmer(rel.vo2 ~ 1 + (1|id), data = icc_dat)
 
summary(m)

icc_res <- data.frame(VarCorr(m))
icc_res

# Intraclass correlation is within variance / total variance

icc <- icc_res[1, 4] / sum(icc_res[,4])
 icc
## Add confidence interval
# Google: Intra class correlation confidence interval lmer boot



 # Transforming the session dataset by calculating the average VO2 across all 
# bouts and sessions for each participant and period
session_data <- session_data %>%
  group_by(id, period) %>% 
  summarise(vo2.mean = mean(vo2, na.rm = TRUE),
            hr.mean = mean(hr, na.rm = TRUE))


# Making a dataset containing each participants VO2max for each period 
test_data_id <- test_data %>% 
  dplyr::select(id, timepoint, vo2.max, vo2.rel.max, hr.max) %>%
  mutate(period = gsub("T", "", timepoint)) %>%
  filter(timepoint != "T4") %>%
  dplyr::select(-timepoint) %>%
  mutate(period = as.numeric(period))




session_trimp <- full_session_data %>% 
  inner_join(test_data_id) %>% 
  dplyr::mutate(hr_rel = hr / hr.max * 100,
                zone1 = if_else(hr_rel > 55 & hr_rel < 81, 10, 0),
                zone2 = if_else(hr_rel > 81 & hr_rel < 87, 10, 0),
                zone3 = if_else(hr_rel > 87 & hr_rel < 100, 10, 0)) %>% 
  dplyr::group_by(id) %>% 
  summarise(zone1 = sum(zone1, na.rm = TRUE) / 60,
            zone2 = sum(zone2, na.rm = TRUE) / 60,
            zone3 = sum(zone3, na.rm = TRUE) / 60) %>% 
  dplyr::mutate(zone2 = zone2 * 2,
                zone3 = zone3 * 3,
                session_trimp = zone1 + zone2 + zone3) %>% 
  dplyr::select(id, session_trimp)

  


# making a dataset containing trimp score calculated from hr
trimp_data_v2 <- trimp_data2 %>% 
  select(id:zone.5) %>% 
  filter(period == 9) %>% 
  mutate(sone.1 = zone.1 + zone.2,
         sone.2 = zone.3,
         sone.3 = zone.4 + zone.5) %>% 
  select(id, sone.1:sone.3) %>% 
  mutate(sone.2 = sone.2 * 2,
         sone.3 = sone.3 * 3) %>% 
  mutate(hr.total.trimp = sone.1 + sone.2 + sone.3) %>% 
  select(id, hr.total.trimp)



# making a dataset containing blood variables for T1 an T4
blood_data <- hb_data %>% 
  filter(timepoint == "T1" | timepoint == "T4") %>% 
  select(id, timepoint, hbmass, `hbmass/kg`, bv)


data_40tt <- testdag2 %>% 
  filter(timepoint == "T1" | timepoint == "T4") %>% 
  select(id, timepoint, w.40tt)





# Making a dataset containing id and period
session_id <- session_id %>% 
  dplyr::select(id, period) %>%
  group_by(id, period) %>%
  summarise(period = mean(period))


# making a dataset with absolute change and percent change (%) for performance index 
per_index <- test_data %>% 
  dplyr::select(id, bodymass, timepoint, w.4mmol, w.max, w.15tt) %>% 
  pivot_longer(names_to = "variable", 
               values_to = "values", 
               cols = w.4mmol:w.15tt) %>%
  dplyr::mutate(rel.values = values / bodymass) %>%
  group_by(variable) %>%
  mutate(scaled.abs = values / max(values, na.rm = TRUE),
         scaled.rel = rel.values / max(rel.values, na.rm = TRUE)) %>%  
  group_by(id, timepoint) %>%
  summarise(per.index.abs = mean(scaled.abs),
            per.index.rel = mean(scaled.rel)) %>% 
  filter(timepoint == "T1" | timepoint == "T4") %>%
  pivot_wider(names_from = timepoint,
              values_from = c(per.index.abs, per.index.rel)) %>%  
  mutate(change.per.index.abs = per.index.abs_T4 - per.index.abs_T1,
         change.per.index.rel = per.index.rel_T4 - per.index.rel_T1)
 
  

# making a dataset containing percent change for w.4mmol, vo2.max, w.max and w.15tt
change_data <- test_data %>% 
  dplyr::select(id, bodymass, timepoint, w.4mmol, vo2.rel.max, vo2.max, w.max, w.15tt, p.vo2.4mmol, ge.175, ge.225, keiser.fmax, keiser.pmax) %>%
  inner_join(testdag2) %>% 
  dplyr::select(id, bodymass, timepoint, w.4mmol, vo2.rel.max, vo2.max, w.max, w.15tt, w.40tt, p.vo2.4mmol, ge.175, ge.225, keiser.fmax, keiser.pmax) %>%
  filter(timepoint == "T1" | timepoint == "T4") %>% 
  inner_join(blood_data) %>%
  dplyr::mutate(w.4mmol.rel = w.4mmol / bodymass,
                w.max.rel = w.max / bodymass,
                w.15tt.rel = w.15tt / bodymass,
                w.40tt.rel = w.40tt / bodymass,
                bv.rel = bv / bodymass) %>%
  pivot_wider(names_from = timepoint,
              values_from = c(bodymass:bv.rel)) %>% 
  mutate(change_bodymass = bodymass_T4 - bodymass_T1,
         change_vo2 = vo2.max_T4 - vo2.max_T1,
         change_w.4mmol = w.4mmol_T4 - w.4mmol_T1,
         change_w.max = w.max_T4 - w.max_T1,
         change_w.15tt = w.15tt_T4 - w.15tt_T1,
         change_vo2.rel = vo2.rel.max_T4 - vo2.rel.max_T1,
         change_w.40tt = w.40tt_T4 - w.40tt_T1,
         change_hbmass = hbmass_T4 - hbmass_T1,
         change_bv = bv_T4 - bv_T1, 
         change_hb.rel = `hbmass/kg_T4` - `hbmass/kg_T1`,
         change_bv.rel = bv.rel_T4 - bv.rel_T1,
         change_w.4mmol.rel = w.4mmol.rel_T4 - w.4mmol.rel_T1,
         change_w.max.rel = w.max.rel_T4 - w.max.rel_T1,
         change_w.15tt.rel = w.15tt.rel_T4 - w.15tt.rel_T1,
         change_w.40tt.rel = w.40tt.rel_T4 - w.40tt.rel_T1,
         change_p.vo2.4mmol = p.vo2.4mmol_T4 - p.vo2.4mmol_T1,
         change_ge.175 = ge.175_T4 - ge.175_T1,
         change_ge.225 = ge.225_T4 - ge.225_T1,
         change_keiser.fmax = keiser.fmax_T4 - keiser.fmax_T1,
         change_keiser.pmax = keiser.pmax_T4 - keiser.pmax_T1,
         change_bodymass_percent = (bodymass_T1 - bodymass_T4) / bodymass_T4 * 100,
         change_vo2_abs_percent = (vo2.max_T1 - vo2.max_T4) / vo2.max_T4 * 100,
         change_vo2_rel_percent = (vo2.rel.max_T1 - vo2.rel.max_T4) / vo2.rel.max_T4 * 100,
         change_w.max_abs_percent = (w.max_T1 - w.max_T4) / w.max_T4 * 100,
         change_w.max_rel_percent = (w.max.rel_T1 - w.max.rel_T4) / w.max.rel_T4 * 100,
         change_w.4mmol_abs_percent = (w.4mmol_T1 - w.4mmol_T4) / w.max_T4 * 100,
         change_w.4mmol_rel_percent = (w.4mmol.rel_T1 - w.4mmol.rel_T4) / w.max.rel_T4 * 100,
         change_w.15tt_abs_percent = (w.15tt_T1 - w.15tt_T4) / w.15tt_T4 * 100,
         change_w.15tt_rel_percent = (w.15tt.rel_T1 - w.15tt.rel_T4) / w.15tt.rel_T4 * 100,
         change_w.40tt_abs_percent = (w.40tt_T1 - w.40tt_T4) / w.40tt_T4 * 100,
         change_w.40tt_rel_percent = (w.40tt.rel_T1 - w.40tt.rel_T4) / w.40tt.rel_T4 * 100,
         change_bv_abs_percent = (bv_T1 - bv_T4) / bv_T4 * 100,
         change_bv_rel_percent = (bv.rel_T1 - bv.rel_T4) / bv.rel_T4 * 100,
         change_hbmass_abs_percent = (hbmass_T1 - hbmass_T4) / hbmass_T4 * 100)



# making a dataset containing trimp score (min)
trimp_data <- trimp_data %>% 
  dplyr::select(id, total.trimp) %>% 
  mutate(trimp = total.trimp / 60) %>% 
  na.omit() %>% 
  dplyr::select(id, trimp)


# sessions dataset 
sessions <- session_dataset %>% 
  select(id, session.rpe, rpe.1:la.5) %>% 
  pivot_longer(names_to = "variable",
               values_to = "values",
               cols = session.rpe:la.5) %>% 
  group_by(id, variable) %>% 
  summarise(values = mean(values, na.rm = TRUE)) %>% 
  pivot_wider(names_from = variable,
              values_from = values) %>% 
  mutate(rpe = (rpe.1 + rpe.2 + rpe.3 + rpe.4 + rpe.5) / 5,
         la = (la.1 +la.2 + la.3 + la.4 + la.5) / 5) %>% 
  select(id, la, rpe, session.rpe)


# Combining the different datasets to one full dataset 
full_data <- session_data %>% 
  dplyr::select(id:hr.mean) %>% 
  inner_join(test_data_id) %>% 
  inner_join(session_id) %>% 
  inner_join(change_data) %>% 
  inner_join(per_index) %>% 
  inner_join(trimp_data) %>% 
  inner_join(sessions) %>% 
  inner_join(trimp_data_v2) %>%
  inner_join(session_trimp) %>% 
  mutate(rel.vo2 = 100 * (vo2.mean / vo2.max),
         rel.hr = 100 * (hr.mean / hr.max)) %>% 
  group_by(id) %>% 
  dplyr::summarise(rel.vo2 = mean(rel.vo2, na.rm = TRUE),
            rel.hr = mean(rel.hr, na.rm = TRUE),
            change_bodymass = mean(change_bodymass, na.rm = TRUE),
            change_vo2 = mean(change_vo2, na.rm = TRUE),
            change_vo2.rel = mean(change_vo2.rel, na.rm = TRUE),
            change_w.4mmol = mean(change_w.4mmol, na.rm = TRUE),
            change_w.4mmol.rel = mean(change_w.4mmol.rel, na.rm = TRUE),
            change_w.max = mean(change_w.max, na.rm = TRUE),
            change_w.max.rel = mean(change_w.max.rel, na.rm = TRUE),
            change_w.15tt = mean(change_w.15tt, na.rm = TRUE),
            change_w.15tt.rel = mean(change_w.15tt.rel, na.rm = TRUE),
            change_w.40tt = mean(change_w.40tt, na.rm = TRUE),
            change_w.40tt.rel = mean(change_w.40tt.rel, na.rm = TRUE),
            change_p.vo2.4mmol = mean(change_p.vo2.4mmol, na.rm = TRUE),
            change_ge.175 = mean(change_ge.175, na.rm = TRUE),
            change_ge.225 = mean(change_ge.225, na.rm = TRUE),
            change_keiser.fmax = mean(change_keiser.fmax, na.rm = TRUE),
            change_keiser.pmax = mean(change_keiser.pmax, na.rm = TRUE),
            change.per.index.abs = mean(change.per.index.abs, na.rm = TRUE),
            change.per.index.rel = mean(change.per.index.rel, na.rm = TRUE),
            change_hbmass = mean(change_hbmass, na.rm = TRUE),
            change_bv = mean(change_bv, na.rm = TRUE),
            change_bv.rel = mean(change_bv.rel, na.rm = TRUE),
            change_hb.rel = mean(change_hb.rel, na.rm = TRUE),
            bodymass_T1 = mean(bodymass_T1, na.rm = TRUE),
            bodymass_T4 = mean(bodymass_T4, na.rm = TRUE),
            vo2.rel.max_T1 = mean(vo2.rel.max_T1, na.rm = TRUE),
            vo2.rel.max_T4 = mean(vo2.rel.max_T4, na.rm = TRUE),
            trimp = mean(trimp, na.rm = TRUE),
            hr.total.trimp = mean(hr.total.trimp, na.rm = TRUE),
            session_trimp = mean(session_trimp, na.rm = TRUE),
            per.index.abs_T1 = mean(per.index.abs_T1, na.rm = TRUE),
            per.index.abs_T4 = mean(per.index.abs_T4, na.rm = TRUE),
            per.index.rel_T1 = mean(per.index.rel_T1, na.rm = TRUE),
            per.index.rel_T4 = mean(per.index.rel_T4, na.rm = TRUE),
            vo2.max_T1 = mean(vo2.max_T1, na.rm = TRUE),
            vo2.max_T4 = mean(vo2.max_T4, na.rm = TRUE),
            w.4mmol_T1 = mean(w.4mmol_T1, na.rm = TRUE),
            w.4mmol_T4 = mean(w.4mmol_T4, na.rm = TRUE),
            w.4mmol.rel_T1 = mean(w.4mmol.rel_T1, na.rm = TRUE),
            w.4mmol.rel_T4 = mean(w.4mmol.rel_T4, na.rm = TRUE),
            p.vo2.4mmol_T1 = mean(p.vo2.4mmol_T1, na.rm = TRUE),
            p.vo2.4mmol_T4 = mean(p.vo2.4mmol_T4, na.rm = TRUE),
            ge.175_T1 = mean(ge.175_T1, na.rm = TRUE),
            ge.175_T4 = mean(ge.175_T4, na.rm = TRUE),
            ge.225_T1 = mean(ge.225_T1, na.rm = TRUE),
            ge.225_T4 = mean(ge.225_T4, na.rm = TRUE),
            keiser.fmax_T1 = mean(keiser.fmax_T1, na.rm = TRUE),
            keiser.fmax_T4 = mean(keiser.fmax_T4, na.rm = TRUE),
            keiser.pmax_T1 = mean(keiser.pmax_T1, na.rm = TRUE),
            keiser.pmax_T4 = mean(keiser.pmax_T4, na.rm = TRUE),
            w.max_T1 = mean(w.max_T1, na.rm = TRUE),
            w.max_T4 = mean(w.max_T4, na.rm = TRUE),
            w.max.rel_T1 = mean(w.max.rel_T1, na.rm = TRUE),
            w.max.rel_T4 = mean(w.max.rel_T4, na.rm = TRUE),
            w.15tt_T1 = mean(w.15tt_T1, na.rm = TRUE),
            w.15tt_T4 = mean(w.15tt_T4, na.rm = TRUE),
            w.15tt.rel_T1 = mean(w.15tt.rel_T1, na.rm = TRUE),
            w.15tt.rel_T4 = mean(w.15tt.rel_T4, na.rm = TRUE),
            w.40tt_T1 = mean(w.40tt_T1, na.rm = TRUE),
            w.40tt_T4 = mean(w.40tt_T4, na.rm = TRUE),
            w.40tt.rel_T1 = mean(w.40tt.rel_T1, na.rm = TRUE),
            w.40tt.rel_T4 = mean(w.40tt.rel_T4, na.rm = TRUE),
            la = mean(la, na.rm = TRUE),
            rpe = mean(rpe, na.rm = TRUE),
            hbmass_T1 = mean(hbmass_T1, na.rm = TRUE),
            hbmass_T4 = mean(hbmass_T4, na.rm = TRUE),
            bv_T1 = mean(bv_T1, na.rm = TRUE),
            bv_T4 = mean(bv_T4, na.rm = TRUE),
            bv.rel_T1 = mean(bv.rel_T1, na.rm = TRUE),
            bv.rel_T4 = mean(bv.rel_T4, na.rm = TRUE),
            `hbmass/kg_T1` = mean(`hbmass/kg_T1`, na.rm = TRUE),
            `hbmass/kg_T4` = mean(`hbmass/kg_T4`, na.rm = TRUE)) %>% 
  filter(id != 37) %>% 
  filter(id != 30) %>% 
  filter(vo2.rel.max_T1 >= 60) %>% 
  filter(id != 36)
  

full_data %>% 
  ggplot(aes(rel.vo2, change_p.vo2.4mmol)) +
  geom_point() +
  theme_bw()

#################################################################################
model_p.vo2.4mmol.T1 <- lm(p.vo2.4mmol_T1 ~ rel.vo2, data = full_data)
summary(model_p.vo2.4mmol.T1)


p.vo2.4mmol.T1_label <- expression(paste(" 80% " %->%  " 90% = 7.45 %"))

plot_p.vo2.4mmol.T1 <- effect_plot(model_p.vo2.4mmol.T1, pred = rel.vo2, interval = TRUE, plot.points = TRUE, point.size = 3,
                           point.color = "Black") +
  theme_apa() +
  labs(y = expression(paste(bold("W" ["laktatterskel"]), " (% av VO" ["2maks"], ")")),
       x = expression(paste(bold("VO" ["2snitt"]), " (% av VO" ["2maks"], ")")),
       tag = expression(paste(bold("E")))) +
  annotate("text", x = 82, y = 90, label = as.character(p.vo2.4mmol.T1_label), parse = TRUE, size = 5, color = "black") +
  scale_x_continuous(breaks = c(77.5, 80, 82.5, 85, 87.5, 90, 92.5, 95)) +
  scale_y_continuous(breaks = c(72, 76, 80, 84, 88, 92)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.line = element_line(colour = "Black",
                                 size = 1.2,
                                 arrow = arrow())) +
  theme(axis.ticks = element_line(colour = "Black",
                                  size = 1.2)) +
  theme(axis.text = element_text(colour = "Black",
                                 size = "15")) +
  theme(panel.border = element_blank()) +
  theme(plot.tag = element_text(size = 20))

plot_p.vo2.4mmol.T1

##################################################################################

model_p.vo2.4mmol <- lm(change_p.vo2.4mmol ~ rel.vo2 + p.vo2.4mmol_T1, data = full_data)
summary(model_p.vo2.4mmol)
plot(model_p.vo2.4mmol)

p.vo2.4mmol_label <- expression(paste(" 80% " %->%  " 90% = 4.76 %"))

plot_p.vo2.4mmol <- effect_plot(model_p.vo2.4mmol, pred = rel.vo2, interval = TRUE, plot.points = TRUE, point.size = 3,
                                   point.color = "Black") +
  theme_apa() +
  labs(y = expression(paste(bold(Delta), bold(" W" ["laktatterskel"]), " (% av VO" ["2maks"], ")")),
       x = expression(paste(bold("VO" ["2snitt"]), " (% av VO" ["2maks"], ")")),
       tag = expression(paste(bold("D")))) +
  annotate("text", x = 82, y = 3, label = as.character(p.vo2.4mmol_label), parse = TRUE, size = 5, color = "black") +
  scale_x_continuous(breaks = c(77.5, 80, 82.5, 85, 87.5, 90, 92.5, 95)) +
  scale_y_continuous(breaks = c(-10, -7.5, -5, -2.5, 0, 2.5, 5)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.line = element_line(colour = "Black",
                                 size = 1.2,
                                 arrow = arrow())) +
  theme(axis.ticks = element_line(colour = "Black",
                                  size = 1.2)) +
  theme(axis.text = element_text(colour = "Black",
                                 size = "15")) +
  theme(panel.border = element_blank()) +
  theme(plot.tag = element_text(size = 20))

plot_p.vo2.4mmol


###################################################################################

model_ge.175 <- lm(change_ge.175 ~ rel.vo2 + ge.175_T1, data = full_data)
summary(model_ge.175)
plot(model_ge.175)

ge.175_label <- expression(paste(" 80% " %->%  " 90% = -0.16 %"))

plot_ge.175 <- effect_plot(model_ge.175, pred = rel.vo2, interval = TRUE, plot.points = TRUE, point.size = 3,
                                   point.color = "Black") +
  theme_apa() +
  labs(y = expression(paste(bold(Delta), bold(" GE 175 W"), " (%)")),
       x = expression(paste(bold("VO" ["2snitt"]), " (% av VO" ["2maks"], ")")),
       tag = expression(paste(bold("B")))) +
  annotate("text", x = 82, y = 2.5, label = as.character(ge.175_label), parse = TRUE, size = 5, color = "black") +
  scale_x_continuous(breaks = c(77.5, 80, 82.5, 85, 87.5, 90, 92.5, 95)) +
  scale_y_continuous(breaks = c(-0.4, 0.3, 1, 1.7, 2.4, 3.1)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.line = element_line(colour = "Black",
                                 size = 1.2,
                                 arrow = arrow())) +
  theme(axis.ticks = element_line(colour = "Black",
                                  size = 1.2)) +
  theme(axis.text = element_text(colour = "Black",
                                 size = "15")) +
  theme(panel.border = element_blank()) +
  theme(plot.tag = element_text(size = 20))

plot_ge.175

##################################################################################

model_ge.225 <- lm(change_ge.225 ~ rel.vo2 + ge.225_T1, data = full_data)
summary(model_ge.225)
plot(model_ge.225)

ge.225_label <- expression(paste(" 80% " %->%  " 90% = 0.49 %"))

plot_ge.225 <- effect_plot(model_ge.225, pred = rel.vo2, interval = TRUE, plot.points = TRUE, point.size = 3,
                           point.color = "Black") +
  theme_apa() +
  labs(y = expression(paste(bold(Delta), bold(" GE 225 W"), " (%)")),
       x = expression(paste(bold("VO" ["2snitt"]), " (% av VO" ["2maks"], ")")),
       tag = expression(paste(bold("C")))) +
  annotate("text", x = 82, y = 1.1, label = as.character(ge.225_label), parse = TRUE, size = 5, color = "black") +
  scale_x_continuous(breaks = c(77.5, 80, 82.5, 85, 87.5, 90, 92.5, 95)) +
  scale_y_continuous(breaks = c(-0.7, -0.2, 0.3, 0.8, 1.3)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.line = element_line(colour = "Black",
                                 size = 1.2,
                                 arrow = arrow())) +
  theme(axis.ticks = element_line(colour = "Black",
                                  size = 1.2)) +
  theme(axis.text = element_text(colour = "Black",
                                 size = "15")) +
  theme(panel.border = element_blank()) +
  theme(plot.tag = element_text(size = 20))

plot_ge.225

##################################################################################

model_bodymass <- lm(rel.vo2 ~ change_bodymass + bodymass_T1, data = full_data)
summary(model_bodymass)

model_keiser.fmax <- lm(rel.vo2 ~ change_keiser.fmax + keiser.fmax_T1, data = full_data)
summary(model_keiser.fmax)

model_keiser.pmax <- lm(rel.vo2 ~ change_keiser.pmax + keiser.pmax_T1, data = full_data)
summary(model_keiser.pmax)


# making model dataset and using log for rel.vo2 and change_per_index_percent 
model_data <- full_data %>% 
  mutate(l.rel.vo2 = log(rel.vo2),
         l.change.per.index.abs = log(change.per.index.abs))

full_data %>% 
  dplyr::select(id, vo2.rel.max_T1, w.max.rel_T1, w.max_T1) %>% 
  pivot_longer(names_to = "variable",
               values_to = "values",
               cols = c(vo2.rel.max_T1, w.max_T1, w.max.rel_T1)) %>% 
  dplyr::group_by(variable) %>% 
  summarise(mean = mean(values, na.rm = TRUE),
            sd = sd(values, na.rm = TRUE),
            min = min(values, na.rm = TRUE),
            max = max(values, na.rm = TRUE)) 


full_data %>% 
  ggplot(aes(change_vo2.rel, change_hb.rel)) +
  geom_point() +
  theme_bw()

# plotting model_data
ggplot(model_data, aes(x = rel.vo2, y = l.change.per.index.abs)) +
  geom_point() +
  theme_bw()



# making the model
model <- lm(l.change.per.index.abs ~ rel.vo2 + per.index.abs_T1, data = model_data)


model.rel.bm <- lm(change.per.index.rel ~ rel.vo2 + per.index.rel_T1 + change_bodymass, data = full_data)


model.rel <- lm(change.per.index.rel ~ rel.vo2 + per.index.rel_T1, data = full_data)


model.rel.trimp <- lm(change.per.index.rel ~ rel.vo2 + per.index.rel_T1 + hr.total.trimp + change_bodymass, data = full_data)
summary(model.rel.trimp)


# summarise the model
summary(model)

summary(model.rel.bm)

summary(model.rel)

shapiro.test(full_data$change.per.index.rel)

vif(model.rel.bm)
 
pf_label <- expression(paste(bold(Delta), " 80% " %->%  " 90% = 0.98"))

plot_pf <- effect_plot(model, pred = rel.vo2, interval = TRUE, plot.points = TRUE, point.size = 3,
            point.color = "Black") +
  theme_apa() +
  labs(y = expression(paste(bold(Delta), bold(" PI"), " (log)")),
       x = expression(paste(bold("Treningsintensitet (% av VO2" [maks]), ")")),
       tag = expression(paste(bold("A")))) +
  annotate("text", x = 82, y = -2.3, label = "italic(p) == 0.011", parse = TRUE, size = 4) +
  annotate("text", x = 82, y = -2.55, label = as.character(pf_label), parse = TRUE, size = 4)

###############################################################################

pf_label.rel <- expression(paste(bold(Delta), " 80% " %->%  " 90% = 0.046"))

plot_pf.rel <- effect_plot(model.rel, pred = rel.vo2, interval = TRUE, plot.points = TRUE, point.size = 3,
                       point.color = "Black") +
  theme_apa() +
  labs(y = expression(paste(bold(Delta), bold(" PI"))),
       x = expression(paste(bold("Treningsintensitet (% av VO2" [maks]), ")")),
       tag = expression(paste(bold("A")))) +
  annotate("text", x = 82, y = 0.095, label = "italic(p) == 0.012", parse = TRUE, size = 4) +
  annotate("text", x = 82, y = 0.08, label = as.character(pf_label.rel), parse = TRUE, size = 4)

plot_pf.rel

###############################################################################

pf_label.rel.bm <- expression(paste(" 80% " %->%  " 90% = 0.045"))

plot_pf.rel.bm <- effect_plot(model.rel.bm, pred = rel.vo2, interval = TRUE, plot.points = TRUE, point.size = 3,
                           point.color = "Black") +
  theme_apa() +
  labs(y = expression(paste(bold(Delta), bold(" Prestasjonsindeks"), " (vv)")),
       x = expression(paste(bold("VO" ["2snitt"]), " (% av VO" ["2maks"], ")")),
       tag = expression(paste(bold("A")))) +
  annotate("text", x = 82, y = 0.084, label = as.character(pf_label.rel.bm), parse = TRUE, size = 5, color = "black") +
  scale_x_continuous(breaks = c(77.5, 80, 82.5, 85, 87.5, 90, 92.5, 95)) +
  scale_y_continuous(breaks = c(-0.02, 0.00, 0.02, 0.04, 0.06, 0.08, 0.10, 0.12)) + 
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.line = element_line(colour = "Black",
                                 size = 1.2,
                                 arrow = arrow())) +
  theme(axis.ticks = element_line(colour = "Black",
                                  size = 1.2)) +
  theme(axis.text = element_text(colour = "Black",
                                 size = "15")) +
  theme(panel.border = element_blank()) +
  theme(plot.tag = element_text(size = 20))

plot_pf.rel.bm





# making the model
model_w.max <- lm(change_w.max ~ rel.vo2 + w.max_T1, data = model_data)


model_w.max.rel <- lm(change_w.max.rel ~ rel.vo2 + w.max.rel_T1, data = full_data)


model_w.max.rel.bm <- lm(change_w.max.rel ~ rel.vo2 + w.max.rel_T1 + change_bodymass, data = full_data)

# summarise the model
summary(model_w.max)

summary(model_w.max.rel)

summary(model_w.max.rel.bm)


shapiro.test(full_data$change_w.max.rel)

vif(model_w.max.rel.bm)


w.max_label <- expression(paste(bold(Delta), " 80% " %->%  " 90% = 21.67 W"))

plot_w.max <- effect_plot(model_w.max, pred = rel.vo2, interval = TRUE, plot.points = TRUE, point.size = 3,
                          point.color = "Black") +
  theme_apa() +
  labs(y = expression(paste(bold(Delta), bold(" W" [maks]))),
       x = expression(paste(bold("Treningsintensitet (% av VO2" [maks]), ")")),
       tag = expression(paste(bold("C")))) +
  annotate("text", x = 82, y = 60, label = "italic(p) == 0.029", parse = TRUE, size = 4) +
  annotate("text", x = 82, y = 54, label = as.character(w.max_label), parse = TRUE, size = 4)

###################################################################################

w.max_label.rel <- expression(paste(bold(Delta), " 80% " %->%  " 90% = 0.41 W" %.% "kg" ^-1))

plot_w.max.rel <- effect_plot(model_w.max.rel, pred = rel.vo2, interval = TRUE, plot.points = TRUE, point.size = 3,
                              point.color = "Black") +
  theme_apa() +
  labs(y = expression(paste(bold(Delta), bold(" W" [maks]), " (W" %.% "kg" ^-1, ")")),
       x = expression(paste(bold("Treningsintensitet (% av VO2" [maks]), ")")),
       tag = expression(paste(bold("C")))) +
  annotate("text", x = 82, y = 0.88, label = "italic(p) == 0.006", parse = TRUE, size = 4) +
  annotate("text", x = 82, y = 0.75, label = as.character(w.max_label.rel), parse = TRUE, size = 4)

plot_w.max.rel

###################################################################################

w.max_label.rel.bm <- expression(paste(" 80% " %->%  " 90% = 0.39 W" %.% "kg" ^-1))

plot_w.max.rel.bm <- effect_plot(model_w.max.rel.bm, pred = rel.vo2, interval = TRUE, plot.points = TRUE, point.size = 3,
                                 point.color = "Black") +
  theme_apa() +
  labs(y = expression(paste(bold(Delta), bold(" W" [maks]), " (W" %.% "kg" ^-1, ")")),
       x = expression(paste(bold("VO" ["2snitt"]), " (% av VO" ["2maks"], ")")),
       tag = expression(paste(bold("B")))) +
  annotate("text", x = 82, y = 0.75, label = as.character(w.max_label.rel.bm), parse = TRUE, size = 5, color = "black") +
  scale_x_continuous(breaks = c(77.5, 80, 82.5, 85, 87.5, 90, 92.5, 95)) +
  scale_y_continuous(breaks = c(-0.1, 0.1, 0.3, 0.5, 0.7, 0.9)) + 
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.line = element_line(colour = "Black",
                                 size = 1.2,
                                 arrow = arrow())) +
  theme(axis.ticks = element_line(colour = "Black",
                                  size = 1.2)) +
  theme(axis.text = element_text(colour = "Black",
                                 size = "15")) +
  theme(panel.border = element_blank()) +
  theme(plot.tag = element_text(size = 20))

plot_w.max.rel.bm



################################################################################


# making the model
model_w.4mmol <- lm(change_w.4mmol ~ rel.vo2 + w.4mmol_T1, data = model_data)


model_w.4mmol.rel <- lm(change_w.4mmol.rel ~ rel.vo2 + w.4mmol.rel_T1, data = full_data)


model_w.4mmol.rel.bm <- lm(change_w.4mmol.rel ~ rel.vo2 + w.4mmol.rel_T1 + change_bodymass, data = full_data)

# summarise the model
summary(model_w.4mmol)

summary(model_w.4mmol.rel)

summary(model_w.4mmol.rel.bm)


vif(model_w.4mmol.rel.bm)



w.4mmol_label <- expression(paste(bold(Delta), " 80% " %->%  " 90% = 15.96 W"))

plot_w.4mmol <- effect_plot(model_w.4mmol, pred = rel.vo2, interval = TRUE, plot.points = TRUE, point.size = 3,
                            point.color = "Black") +
  theme_apa() +
  labs(y = expression(paste(bold(Delta), bold(" W" [laktatterskel]))),
       x = expression(paste(bold("Treningsintensitet (% av VO2" [maks]), ")")),
       tag = expression(paste(bold("D")))) +
  annotate("text", x = 82, y = 40, label = "italic(p) == 0.057", parse = TRUE, size = 4) +
  annotate("text", x = 82, y = 35, label = as.character(w.4mmol_label), parse = TRUE, size = 4)

#################################################################################

w.4mmol_label.rel <- expression(paste(bold(Delta), " 80% " %->%  " 90% = 0.24 W" %.% "kg" ^-1))

plot_w.4mmol.rel <- effect_plot(model_w.4mmol.rel, pred = rel.vo2, interval = TRUE, plot.points = TRUE, point.size = 3,
                                point.color = "Black") +
  theme_apa() +
  labs(y = expression(paste(bold(Delta), bold(" W" [laktatterskel]), " (W" %.% "kg" ^-1, ")")),
       x = expression(paste(bold("Treningsintensitet (% av VO2" [maks]), ")")),
       tag = expression(paste(bold("D")))) +
  annotate("text", x = 82, y = 0.47, label = "italic(p) == 0.034", parse = TRUE, size = 4) +
  annotate("text", x = 82, y = 0.39, label = as.character(w.4mmol_label.rel), parse = TRUE, size = 4)

plot_w.4mmol.rel


#################################################################################

w.4mmol_label.rel.bm <- expression(paste(" 80% " %->%  " 90% = 0.24 W" %.% "kg" ^-1))

plot_w.4mmol.rel.bm <- effect_plot(model_w.4mmol.rel.bm, pred = rel.vo2, interval = TRUE, plot.points = TRUE, point.size = 3,
                                   point.color = "Black") +
  theme_apa() +
  labs(y = expression(paste(bold(Delta), bold(" W" [laktatterskel]), " (W" %.% "kg" ^-1, ")")),
       x = expression(paste(bold("VO" ["2snitt"]), " (% av VO" ["2maks"], ")")),
       tag = expression(paste(bold("C")))) +
  annotate("text", x = 82, y = 0.39, label = as.character(w.4mmol_label.rel.bm), parse = TRUE, size = 5, color = "black") +
  scale_x_continuous(breaks = c(77.5, 80, 82.5, 85, 87.5, 90, 92.5, 95)) +
  scale_y_continuous(breaks = c(-0.20, -0.05, 0.10, 0.25, 0.40, 0.55, 0.70)) + 
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.line = element_line(colour = "Black",
                                 size = 1.2,
                                 arrow = arrow())) +
  theme(axis.ticks = element_line(colour = "Black",
                                  size = 1.2)) +
  theme(axis.text = element_text(colour = "Black",
                                 size = "15")) +
  theme(panel.border = element_blank()) +
  theme(plot.tag = element_text(size = 20))

plot_w.4mmol.rel.bm



################################################################################


# making the model
model_w.15tt <- lm(change_w.15tt ~ rel.vo2 + w.15tt_T1, data = model_data)


model_w.15tt.rel <- lm(change_w.15tt.rel ~ rel.vo2 + w.15tt.rel_T1, data = full_data)


model_w.15tt.rel.bm <- lm(change_w.15tt.rel ~ rel.vo2 + w.15tt.rel_T1 + change_bodymass, data = full_data)

# summarise the model
summary(model_w.15tt)

summary(model_w.15tt.rel)

summary(model_w.15tt.rel.bm)


vif(model_w.15tt.rel.bm)


w.15tt_label <- expression(paste(bold(Delta), " 80% " %->%  " 90% = 7.67 W"))

plot_w.15tt <- effect_plot(model_w.15tt, pred = rel.vo2, interval = TRUE, plot.points = TRUE, point.size = 3,
                           point.color = "Black") +
  theme_apa() +
  labs(y = expression(paste(bold(Delta), bold(" W" ["15tt"]))),
       x = expression(paste(bold("Treningsintensitet: % av VO2" [maks]), ")")),
       tag = expression(paste(bold("E")))) +
  annotate("text", x = 82, y = 44, label = "italic(p) == 0.410", parse = TRUE, size = 4) +
  annotate("text", x = 82, y = 39, label = as.character(w.15tt_label), parse = TRUE, size = 4)

################################################################################

w.15tt_label.rel <- expression(paste(bold(Delta), " 80% " %->%  " 90% = 0.22 W" %.% "kg" ^-1))

plot_w.15tt.rel <- effect_plot(model_w.15tt.rel, pred = rel.vo2, interval = TRUE, plot.points = TRUE, point.size = 3,
                               point.color = "Black") +
  theme_apa() +
  labs(y = expression(paste(bold(Delta), bold(" W" ["15tt"]), " (W" %.% "kg" ^-1, ")")),
       x = expression(paste(bold("Treningsintensitet: % av VO2" [maks]), ")")),
       tag = expression(paste(bold("E")))) +
  annotate("text", x = 82, y = 0.55, label = "italic(p) == 0.167", parse = TRUE, size = 5) +
  annotate("text", x = 82, y = 0.46, label = as.character(w.15tt_label.rel), parse = TRUE, size = 5)

plot_w.15tt.rel

################################################################################

w.15tt_label.rel.bm <- expression(paste(" 80% " %->%  " 90% = 0.2 W" %.% "kg" ^-1))

plot_w.15tt.rel.bm <- effect_plot(model_w.15tt.rel.bm, pred = rel.vo2, interval = TRUE, plot.points = TRUE, point.size = 3,
                                  point.color = "Black") +
  theme_apa() +
  labs(y = expression(paste(bold(Delta), bold(" W" ["15tt"]), " (W" %.% "kg" ^-1, ")")),
       x = expression(paste(bold("VO" ["2snitt"]), " (% av VO" ["2maks"], ")")),
       tag = expression(paste(bold("D")))) +
  annotate("text", x = 82, y = 0.46, label = as.character(w.15tt_label.rel.bm), parse = TRUE, size = 5, color = "black") +
  scale_x_continuous(breaks = c(77.5, 80, 82.5, 85, 87.5, 90, 92.5, 95)) +
  scale_y_continuous(breaks = c(-0.15, 0.0, 0.15, 0.3, 0.45,0.6)) + 
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.line = element_line(colour = "Black",
                                 size = 1.2,
                                 arrow = arrow())) +
  theme(axis.ticks = element_line(colour = "Black",
                                  size = 1.2)) +
  theme(axis.text = element_text(colour = "Black",
                                 size = "15")) +
  theme(panel.border = element_blank()) +
  theme(plot.tag = element_text(size = 20))

plot_w.15tt.rel.bm



################################################################################


# making the model
model_vo2 <- lm(change_vo2 ~ rel.vo2 + vo2.max_T1, data = model_data)


model_vo2.rel <- lm(change_vo2.rel ~ rel.vo2 + vo2.rel.max_T1, data = model_data)


model_vo2.rel.bm <- lm(change_vo2.rel ~ rel.vo2 + vo2.rel.max_T1 + change_bodymass, data = full_data)

# summarise the model
summary(model_vo2)  

summary(model_vo2.rel)

summary(model_vo2.rel.bm)


vif(model_vo2.rel.bm)


vo2_label.abs <- expression(paste(bold(Delta), " 80% " %->%  " 90% = 115 mL O" ^2 %.% "min"^-1))

plot_vo2.abs <- effect_plot(model_vo2, pred = rel.vo2, interval = TRUE, plot.points = TRUE, point.size = 3,
                            point.color = "Black") +
  theme_apa() +
  labs(y = expression(paste(bold(Delta), bold(" VO2" [maks]), " (mL O" ^2 %.% "min"^-1, ")")),
       x = expression(paste(bold("Treningsintensitet (% av VO2" [maks]), ")")),
       tag = expression(paste(bold("B")))) +
  annotate("text", x = 82, y = 650, label = "italic(p) == 0.160", parse = TRUE, size = 4) +
  annotate("text", x = 82, y = 580, label = as.character(vo2_label.abs), parse = TRUE, size = 4)

plot_vo2.abs

###############################################################################

vo2_label <- expression(paste(bold(Delta), " 80% " %->%  " 90% = 2.74 mL O" ^2 %.% "kg" ^-1 %.% "min"^-1))

plot_vo2 <- effect_plot(model_vo2.rel, pred = rel.vo2, interval = TRUE, plot.points = TRUE, point.size = 3,
                        point.color = "Black") +
  theme_apa() +
  labs(y = expression(paste(bold(Delta), bold(" VO2" [maks]), " (mL O" ^2 %.% "kg" ^-1 %.% "min"^-1, ")")),
       x = expression(paste(bold("Treningsintensitet (% av VO2" [maks]), ")")),
       tag = expression(paste(bold("B")))) +
  annotate("text", x = 82, y = 9, label = "italic(p) == 0.067", parse = TRUE, size = 4) +
  annotate("text", x = 82, y = 8, label = as.character(vo2_label), parse = TRUE, size = 4)

plot_vo2

###############################################################################

vo2_label.bm <- expression(paste(" 80% " %->%  " 90% = 2.41 mL" %.% "min" ^-1 %.% "kg"^-1))

plot_vo2.bm <- effect_plot(model_vo2.rel.bm, pred = rel.vo2, interval = TRUE, plot.points = TRUE, point.size = 3,
                           point.color = "Black") +
  theme_apa() +
  labs(y = expression(paste(bold(Delta), bold(" VO" ["2maks"]), " (mL" %.% "min" ^-1 %.% "kg"^-1, ")")),
       x = expression(paste(bold("VO" ["2snitt"]), " (% av VO" ["2maks"], ")")),
       tag = expression(paste(bold("A")))) +
  annotate("text", x = 82, y = 8, label = as.character(vo2_label.bm), parse = TRUE, size = 5, color = "black") +
  scale_x_continuous(breaks = c(77.5, 80, 82.5, 85, 87.5, 90, 92.5, 95)) +
  scale_y_continuous(breaks = c(0.0, 1.5, 3, 4.5, 6, 7.5, 9)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.line = element_line(colour = "Black",
                                 size = 1.2,
                                 arrow = arrow())) +
  theme(axis.ticks = element_line(colour = "Black",
                                  size = 1.2)) +
  theme(axis.text = element_text(colour = "Black",
                                 size = "15")) +
  theme(panel.border = element_blank()) +
  theme(plot.tag = element_text(size = 20))

plot_vo2.bm


################################################################################

# log transform the hit trimp score
log_trimp <- full_data %>% 
  dplyr::mutate(l.session_trimp = log(session_trimp),
                l.rel.vo2 = log(rel.vo2)) %>% 
  dplyr::select(id, l.rel.vo2, l.session_trimp)

# inspect the relationship
log_trimp %>% 
  ggplot(aes(l.rel.vo2, l.session_trimp)) +
  geom_point() +
  theme_bw()

full_data %>% 
  ggplot(aes(rel.vo2, session_trimp)) +
  geom_point() +
  theme_bw()

full_data %>% 
  dplyr::filter(id != 9, id != 21, id != 17) %>% 
  ggplot(aes(session_trimp, change.per.index.rel)) +
  geom_point() +
  theme_bw()

full_data %>% 
  ggplot(aes(rel.vo2, rel.hr)) +
  geom_point() +
  theme_bw()

model_trimp.hr <- lm(hr.total.trimp ~ rel.vo2, data = full_data)

model_trimp_hit <- lm(session_trimp ~ rel.vo2, data = full_data)


model_trimp.change <- lm(change_vo2.rel ~ hr.total.trimp + vo2.rel.max_T1, data = full_data)

summary(model_trimp_hit)
summary(model_trimp.hr)
summary(model_trimp.change)
plot(model_trimp.hr)
plot(model_trimp_hit)

plot_trimp.hr <- effect_plot(model_trimp.hr, pred = rel.vo2, interval = TRUE, plot.points = TRUE, point.size = 3,
                             point.color = "Black") +
  theme_apa() +
  labs(y = expression(paste(bold("TRIMP" ["trening"]), " (vv)")),
       x = expression(paste(bold("VO" ["2snitt"]), " (% av VO" ["2maks"], ")")),
       tag = expression(paste(bold("A")))) +
  annotate("text", x = 82, y = 9500, label = "italic(p) == 0.009", parse = TRUE, size = 5, color = "black") +
  scale_x_continuous(breaks = c(77.5, 80, 82.5, 85, 87.5, 90, 92.5, 95)) +
  scale_y_continuous(breaks = c(4000, 5000, 6000, 7000, 8000, 9000, 10000)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.line = element_line(colour = "Black",
                                 size = 1.2,
                                 arrow = arrow())) +
  theme(axis.ticks = element_line(colour = "Black",
                                  size = 1.2)) +
  theme(axis.text = element_text(colour = "Black",
                                 size = "15")) +
  theme(panel.border = element_blank()) +
  theme(plot.tag = element_text(size = 20))


plot_trimp.hr
################################################################################ 

model_la <- lm(la ~ rel.vo2, data = full_data)


summary(model_la)

plot_la <- effect_plot(model_la, pred = rel.vo2, interval = TRUE, plot.points = TRUE, point.size = 3,
                             point.color = "Black") +
  theme_apa() +
  labs(y = expression(paste(bold("Laktat" ["snitt"]), " (mmol" %.% "L" ^"-", ")")),
       x = expression(paste(bold("VO" ["2snitt"]), " (% av VO" ["2maks"], ")")),
       tag = expression(paste(bold("A")))) +
  scale_x_continuous(breaks = c(77.5, 80, 82.5, 85, 87.5, 90, 92.5, 95)) +
  scale_y_continuous(breaks = c(3, 4, 5, 6, 7, 8)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.line = element_line(colour = "Black",
                                 size = 1.2,
                                 arrow = arrow())) +
  theme(axis.ticks = element_line(colour = "Black",
                                  size = 1.2)) +
  theme(axis.text = element_text(colour = "Black",
                                 size = "15")) +
  theme(panel.border = element_blank()) +
  theme(plot.tag = element_text(size = 20))


plot_la
################################################################################

model_rpe <- lm(rpe ~ rel.vo2, data = full_data)


summary(model_rpe)


model_hr <- lm(rel.hr ~ rel.vo2, data = full_data)

summary(model_hr)

plot_rpe <- effect_plot(model_rpe, pred = rel.vo2, interval = TRUE, plot.points = TRUE, point.size = 3,
                       point.color = "Black") +
  theme_apa() +
  labs(y = expression(paste(bold("BORG" ["snitt"]), " (6-20)")),
       x = expression(paste(bold("VO" ["2snitt"]), " (% av VO" ["2maks"], ")")),
       tag = expression(paste(bold("B")))) +
  scale_x_continuous(breaks = c(77.5, 80, 82.5, 85, 87.5, 90, 92.5, 95)) +
  scale_y_continuous(breaks = c(14.6, 15.2, 15.8, 16.4, 17, 17.6)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.line = element_line(colour = "Black",
                                 size = 1.2,
                                 arrow = arrow())) +
  theme(axis.ticks = element_line(colour = "Black",
                                  size = 1.2)) +
  theme(axis.text = element_text(colour = "Black",
                                 size = "15")) +
  theme(panel.border = element_blank()) +
  theme(plot.tag = element_text(size = 20))


plot_rpe
################################################################################

model_40tt <- lm(change_w.40tt ~ rel.vo2 + w.40tt_T1, data = full_data)


model_40tt.rel <- lm(change_w.40tt.rel ~ rel.vo2 + w.40tt.rel_T1, data = full_data)


model_40tt.rel.bm <- lm(change_w.40tt.rel ~ rel.vo2 + w.40tt.rel_T1 + change_bodymass, data = full_data)


summary(model_40tt)

summary(model_40tt.rel)

summary(model_40tt.rel.bm)


vif(model_40tt.rel.bm)


w.40tt_label <- expression(paste(bold(Delta), " 80% " %->%  " 90% = -7.26 W"))

plot_40tt <- effect_plot(model_40tt, pred = rel.vo2, interval = TRUE, plot.points = TRUE, point.size = 3,
                         point.color = "Black") +
  theme_apa() +
  labs(y = expression(paste(bold(Delta), bold(" W" ["40tt"]))),
       x = expression(paste(bold("Treningsintensitet (% av VO2" [maks]), ")")),
       tag = expression(paste(bold("F")))) +
  annotate("text", x = 82, y = 46, label = "italic(p) == 0.439", parse = TRUE, size = 4) +
  annotate("text", x = 82, y = 41, label = as.character(w.40tt_label), parse = TRUE, size = 4)

plot_40tt

################################################################################

w.40tt_label.rel <- expression(paste(bold(Delta), " 80% " %->%  " 90% = -0.03 W" %.% "kg" ^-1))

plot_40tt.rel <- effect_plot(model_40tt.rel, pred = rel.vo2, interval = TRUE, plot.points = TRUE, point.size = 3,
                             point.color = "Black") +
  theme_apa() +
  labs(y = expression(paste(bold(Delta), bold(" W" ["40tt"]), " (W" %.% "kg" ^-1, ")")),
       x = expression(paste(bold("Treningsintensitet (% av VO2" [maks]), ")")),
       tag = expression(paste(bold("F")))) +
  annotate("text", x = 82, y = 0.56, label = "italic(p) == 0.835", parse = TRUE, size = 6) +
  annotate("text", x = 82, y = 0.48, label = as.character(w.40tt_label.rel), parse = TRUE, size = 6) +
  scale_x_continuous(breaks = c(77.5, 80, 82.5, 85, 87.5, 90, 92.5, 95)) +
  scale_y_continuous(breaks = c(-0.10, 0.00, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60)) + 
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 16))

plot_40tt.rel


################################################################################

w.40tt_label.rel.bm <- expression(paste(" 80% " %->%  " 90% = -0.036 W" %.% "kg" ^-1))

plot_40tt.rel.bm <- effect_plot(model_40tt.rel.bm, pred = rel.vo2, interval = TRUE, plot.points = TRUE, point.size = 3,
                                point.color = "Black") +
  theme_apa() +
  labs(y = expression(paste(bold(Delta), bold(" W" ["40tt"]), " (W" %.% "kg" ^-1, ")")),
       x = expression(paste(bold("VO" ["2snitt"]), " (% av VO" ["2maks"], ")")),
       tag = expression(paste(bold("E")))) +
  annotate("text", x = 82, y = 0.48, label = as.character(w.40tt_label.rel.bm), parse = TRUE, size = 5, color = "black") +
  scale_x_continuous(breaks = c(77.5, 80, 82.5, 85, 87.5, 90, 92.5, 95)) +
  scale_y_continuous(breaks = c(-0.15, 0.00, 0.15, 0.30, 0.45, 0.60)) + 
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.line = element_line(colour = "Black",
                                 size = 1.2,
                                 arrow = arrow())) +
  theme(axis.ticks = element_line(colour = "Black",
                                  size = 1.2)) +
  theme(axis.text = element_text(colour = "Black",
                                 size = "15")) +
  theme(panel.border = element_blank()) +
  theme(plot.tag = element_text(size = 20)) +
  scale_linetype_manual(values = "dashed")

plot_40tt.rel.bm




#################################################################################

model_bv <- lm(change_bv ~ rel.vo2 + bv_T1, data = full_data)


model_bv.rel <- lm(change_bv.rel ~ rel.vo2 + bv.rel_T1, data = full_data)


model_bv.rel.bm <- lm(change_bv.rel ~ rel.vo2 + bv.rel_T1 + change_bodymass, data = full_data)



summary(model_bv)

summary(model_bv.rel)

summary(model_bv.rel.bm)


vif(model_bv.rel.bm)


bv_label.rel.bm <- expression(paste(" 80% " %->%  " 90% = 2.45 mL" %.% "kg" ^-1))

plot_bv.rel.bm <- effect_plot(model_bv.rel.bm, pred = rel.vo2, interval = TRUE, plot.points = TRUE, point.size = 3,
                              point.color = "Black") +
  theme_apa() +
  labs(y = expression(paste(bold(Delta), bold(" Blodvolum"), " (mL" %.% "kg" ^-1, ")")),
       x = expression(paste(bold("VO" ["2snitt"]), " (% av VO" ["2maks"], ")")),
       tag = expression(paste(bold("A")))) +
  annotate("text", x = 84, y = 5, label = as.character(bv_label.rel.bm), parse = TRUE, size = 5, color = "black") +
  scale_x_continuous(breaks = c(77.5, 80, 82.5, 85, 87.5, 90, 92.5, 95)) +
  scale_y_continuous(breaks = c(-7, -4, -1, 2, 5, 8)) + 
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.line = element_line(colour = "Black",
                                 size = 1.2,
                                 arrow = arrow())) +
  theme(axis.ticks = element_line(colour = "Black",
                                  size = 1.2)) +
  theme(axis.text = element_text(colour = "Black",
                                 size = "15")) +
  theme(panel.border = element_blank()) +
  theme(plot.tag = element_text(size = 20)) +
  scale_linetype_manual(values = "dashed")

plot_bv.rel.bm

# datasett uten FP 9 som lå utenfor cooks distance
hb_model_data <- full_data %>% 
  dplyr::filter(id != 9)


model_hb <- lm(change_hbmass ~ rel.vo2 + hbmass_T1, data = full_data)


summary(model_hb)


model_hb2 <- lm(change_hbmass ~ rel.vo2 + hbmass_T1, data = hb_model_data)


summary(model_hb2)


model_hb.rel <- lm(change_hb.rel ~ rel.vo2 + `hbmass/kg_T1`, data = full_data)


model_hb.rel.bm <- lm(change_hb.rel ~ rel.vo2 + `hbmass/kg_T1` + change_bodymass, data = full_data)




summary(model_hb.rel)

summary(model_hb.rel.bm)


vif(model_hb.rel.bm)


hb_label.rel.bm <- expression(paste(" 80% " %->%  " 90% = -0.01 g" %.% "kg" ^-1))

plot_hb.rel.bm <- effect_plot(model_hb.rel.bm, pred = rel.vo2, interval = TRUE, plot.points = TRUE, point.size = 3,
                              point.color = "Black") +
  theme_apa() +
  labs(y = expression(paste(bold(Delta), bold(" HB" ["masse"]), " (g" %.% "kg" ^-1, ")")),
       x = expression(paste(bold("VO" ["2snitt"]), " (% av VO" ["2maks"], ")")),
       tag = expression(paste(bold("B")))) +
  annotate("text", x = 84, y = 0.69, label = as.character(hb_label.rel.bm), parse = TRUE, size = 5, color = "black") +
  scale_x_continuous(breaks = c(77.5, 80, 82.5, 85, 87.5, 90, 92.5, 95)) +
  scale_y_continuous(breaks = c(-0.5, -0.2, 0.1, 0.4, 0.7, 1.0)) + 
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.line = element_line(colour = "Black",
                                 size = 1.2,
                                 arrow = arrow())) +
  theme(axis.ticks = element_line(colour = "Black",
                                  size = 1.2)) +
  theme(axis.text = element_text(colour = "Black",
                                 size = "15")) +
  theme(panel.border = element_blank()) +
  theme(plot.tag = element_text(size = 20)) +
  scale_linetype_manual(values = "dashed")

plot_hb.rel.bm

################################################################################


stargazer(model_la, model_rpe, model_p.vo2.4mmol.T1,
          type = "html",
          out = "star_model-doc",
          dep.var.labels = c("Laktat", "BORG", "Utnyttingsgrad"),
          intercept.top = T,
          intercept.bottom = F,
          single.row = F,
          p.auto = T,
          ci = T,
          ci.level = 0.95,
          dep.var.caption = "Avhengige variabler",
          model.numbers = F,
          notes.append = T,
          notes.align = "r",
          notes.label = "Merknader:",
          column.sep.width = "8pt",
          style = "all")

stargazer(model, model_vo2, model_w.max, model_w.4mmol, model_w.15tt, model_40tt,
          type = "html",
          out = "star_model2-doc",
          dep.var.caption = "Avhengige variabler",
          dep.var.labels = c("PF", "VO2maks", "Wmaks", "Wlaktatterskel", "W15tt", "W40tt"),
          intercept.bottom = F,
          intercept.top = T,
          p.auto = T,
          ci = T,
          ci.level = 0.95,
          digits = 2,
          digits.extra = 0,
          model.numbers = F,
          single.row = F,
          notes.append = T,
          notes.align = "r",
          notes.label = "Merknader:",
          column.sep.width = "6pt")

################################################################################




stargazer(model.rel, model_vo2.rel, model_w.max.rel, model_w.4mmol.rel, model_w.15tt.rel, model_40tt.rel,
          type = "html",
          out = "star_model2.rel-doc",
          dep.var.caption = "Avhengige variabler",
          dep.var.labels = c("PI", "VO2maks", "Wmaks", "Wlaktatterskel", "W15tt", "W40tt"),
          intercept.bottom = F,
          intercept.top = T,
          p.auto = T,
          ci = T,
          ci.level = 0.95,
          digits = 2,
          digits.extra = 0,
          model.numbers = F,
          single.row = F,
          notes.append = T,
          notes.align = "r",
          notes.label = "Merknader:",
          column.sep.width = "6pt")




################################################################################




stargazer(model.rel.bm, model_w.max.rel.bm, model_w.4mmol.rel.bm, model_w.15tt.rel.bm, model_40tt.rel.bm,
          type = "html",
          out = "star_model2.rel.bm-doc",
          dep.var.caption = "Avhengige variabler",
          dep.var.labels = c("PI", "Wmaks", "Wlaktatterskel", "W15tt", "W40tt"),
          intercept.bottom = F,
          intercept.top = T,
          p.auto = T,
          ci = T,
          ci.level = 0.95,
          model.numbers = F,
          single.row = F,
          notes.append = T,
          notes.align = "r",
          notes.label = "Merknader:",
          column.sep.width = "6pt",
          style = "all")


stargazer(model_vo2.rel.bm, model_ge.175, model_ge.225, model_p.vo2.4mmol, model_p.vo2.4mmol.T1,
          type = "html",
          out = "tab_uth_fys-doc",
          dep.var.caption = "Avhengige variabler",
          dep.var.labels = c("1", "2", "3", "4", "5"),
          intercept.bottom = F,
          intercept.top = T,
          p.auto = T,
          ci = T,
          ci.level = 0.95,
          model.numbers = F,
          single.row = F,
          notes.append = T,
          notes.align = "r",
          notes.label = "Merknader:",
          column.sep.width = "20pt",
          style = "all")



stargazer(model_bv.rel.bm, model_hb.rel.bm,
          type = "html",
          out = "star_model_fysiologi-doc",
          dep.var.caption = "Avhengige variabler",
          dep.var.labels = c("blodvolum", "HBmasse"),
          intercept.bottom = F,
          intercept.top = T,
          p.auto = T,
          ci = T,
          ci.level = 0.95,
          model.numbers = F,
          single.row = F,
          notes.append = T,
          notes.align = "r",
          notes.label = "Merknader:",
          column.sep.width = "6pt",
          style = "all")

# plotting graphs
multi.page <- ggarrange(plot_pf + theme(axis.title.x = element_blank()), 
          plot_vo2.abs + theme(axis.title.x = element_blank()), 
          plot_w.max, 
          plot_w.4mmol + theme(axis.title.x = element_blank()), 
          plot_w.15tt + theme(axis.title.x = element_blank()),
          plot_40tt,
          ncol = 1,
          nrow = 3)


ggexport(multi.page, filename = "multi.page.ggplot.pdf")

################################################################################
multi.page.rel <- ggarrange(plot_pf.rel + theme(axis.title.x = element_blank()), 
                        plot_vo2 + theme(axis.title.x = element_blank()), 
                        plot_w.max.rel, 
                        plot_w.4mmol.rel + theme(axis.title.x = element_blank()), 
                        plot_w.15tt.rel + theme(axis.title.x = element_blank()),
                        plot_40tt.rel,
                        ncol = 1,
                        nrow = 3)


ggexport(multi.page.rel, filename = "multi.page.rel.ggplot.pdf",
         res = 600, width = 7.7*2, height = 23*0.75)

################################################################################

multi.page.rel.bm <- ggarrange(plot_pf.rel.bm + theme(axis.title.x = element_blank()), 
                               plot_w.max.rel.bm + theme(axis.title.x = element_blank()), 
                            plot_w.4mmol.rel.bm + theme(axis.title.x = element_blank()), 
                            plot_w.15tt.rel.bm,
                            plot_40tt.rel.bm,
                            ncol = 2,
                            nrow = 3,
                            align = "v",
                            font.label = list(size = 50))

ggsave(filename = "bachelor_grafer.png",
       plot = multi.page.rel.bm,
       dpi = 600, 
       width = 7.7*2, 
       height = 23*0.75)

ggexport(multi.page.rel.bm, filename = "multi.page.rel.bm.ggplot.png",
         res = 600, width = 7.7*2, height = 23*0.75, pointsize = 30)

################################################################################

multi.page.fysiologi <- ggarrange(plot_vo2.bm + theme(axis.title.x = element_blank()), 
                               plot_ge.175 + theme(axis.title.x = element_blank()), 
                               plot_ge.225 + theme(axis.title.x = element_blank()), 
                               plot_p.vo2.4mmol,
                               plot_p.vo2.4mmol.T1,
                               ncol = 2,
                               nrow = 3,
                               align = "v",
                               font.label = list(size = 50))

ggsave(filename = "kjøør_grafer.png",
       plot = multi.page.fysiologi,
       dpi = 600, 
       width = 7.7*2, 
       height = 23*0.75)

################################################################################

blood.plots.rel.bm <- ggarrange(plot_bv.rel.bm,
                                plot_hb.rel.bm,
                                ncol = 2,
                                nrow = 1,
                                align = "v",
                                font.label = list(size = 50))

ggsave(filename = "blood_grafer.png",
       plot = blood.plots.rel.bm, 
       dpi = 600,
       width = 7.7*2,
       height = 9*0.75)

################################################################################


workload.plots <- ggarrange(plot_la,
                            plot_rpe,
                            ncol = 2,
                            nrow = 1,
                            align = "v",
                            font.label = list(size = 50))

ggsave(filename = "belastning_grafer.png",
       plot = workload.plots, 
       dpi = 600,
       width = 7.7*2,
       height = 9*0.75)

################################################################################



# filtering out participants
test_data2 <- test_data %>% 
  filter(timepoint == "T1" & vo2.rel.max >= 60 | timepoint == "T4" & vo2.rel.max >= 60) %>%
  filter(id != 30, id != 36, id != 37, id != 27)
  


# making a summarized table for T1 vs T4
tabell <- test_data2 %>% 
  dplyr::select(id, timepoint, bodymass, w.4mmol, vo2.rel.max, vo2.max, w.max, w.15tt, keiser.fmax, keiser.pmax, p.vo2.4mmol, ge.175, ge.225) %>%
  inner_join(blood_data) %>% 
  inner_join(data_40tt) %>% 
  pivot_wider(names_from = timepoint,
              values_from = c(bodymass, hbmass, `hbmass/kg`, bv, vo2.max, vo2.rel.max, w.4mmol, w.max, w.15tt, w.40tt, keiser.fmax, keiser.pmax, p.vo2.4mmol, ge.175, ge.225)) %>%  
  dplyr::select(id, bodymass_T1:ge.225_T4) %>%
  pivot_longer(names_to = "variable",
               values_to = "values",
               cols = bodymass_T1:ge.225_T4) %>%
  separate(variable, c("variable", "timepoint"), "_") %>%
  pivot_wider(names_from = timepoint,
              values_from = values) %>%
  mutate(abs_change = T4 - T1,
         percent_change = (T4 - T1) / T1 * 100) %>%
  dplyr::group_by(variable) %>% 
  summarise(T1_sd = sd(T1, na.rm = TRUE), 
            T1 = mean(T1, na.rm = TRUE),
            T4_sd = sd(T4, na.rm = TRUE),
            T4 = mean(T4, na.rm = TRUE),
            abs_change_sd = sd(abs_change, na.rm = TRUE),
            abs_change = mean(abs_change, na.rm = TRUE),
            percent_change_sd = sd(percent_change, na.rm = TRUE),
            percent_change = mean(percent_change, na.rm = TRUE)) %>% print()

 



stargazer(tabell, type = "html", out = "tab_pre-post.doc",
          summary = FALSE,
          rownames = FALSE,
          digits = 1,
          column.sep.width = "20pt")



tabell_sd <- test_data2 %>% 
  dplyr::select(id, timepoint, bodymass, w.4mmol, vo2.rel.max, vo2.max, w.max, w.15tt, keiser.fmax, keiser.pmax, p.vo2.4mmol, ge.175, ge.225) %>%
  inner_join(blood_data) %>% 
  inner_join(data_40tt) %>% 
  pivot_wider(names_from = timepoint,
              values_from = c(bodymass, hbmass, `hbmass/kg`, bv, vo2.max, vo2.rel.max, w.4mmol, w.max, w.15tt, w.40tt, keiser.fmax, keiser.pmax, p.vo2.4mmol, ge.175, ge.225)) %>%  
  dplyr::select(id, bodymass_T1:ge.225_T4) %>%
  pivot_longer(names_to = "variable",
               values_to = "values",
               cols = bodymass_T1:ge.225_T4) %>% 
  separate(variable, c("variable", "timepoint"), "_") %>%
  dplyr::group_by(variable, timepoint) %>% 
  summarise(mean = mean(values, na.rm = TRUE),
            sd = sd(values, na.rm = TRUE)) %>% 
  pivot_wider(names_from = timepoint,
              values_from = c(mean, sd)) %>% 
  relocate(sd_T1, .after = mean_T1) %>% 
  print()


full_data %>% 
  dplyr::select(id, per.index.rel_T1, per.index.rel_T4, per.index.abs_T1, per.index.abs_T4, w.max.rel_T1, w.max.rel_T4, 
                w.4mmol.rel_T1, w.4mmol.rel_T4, w.15tt.rel_T1, w.15tt.rel_T4, bv.rel_T1, bv.rel_T4,
                w.40tt.rel_T1, w.40tt.rel_T4) %>% 
  pivot_longer(names_to = "variable",
               values_to = "values",
               cols = c(per.index.rel_T1:w.40tt.rel_T4)) %>%
  separate(variable, c("variable", "timepoint"), "_") %>% 
    pivot_wider(names_from = timepoint,
                values_from = values) %>%
    mutate(abs_change = T4 - T1,
           percent_change = (T4 - T1) / T1 * 100) %>% 
    dplyr::group_by(variable) %>% 
    summarise(T1_sd = sd(T1, na.rm = TRUE), 
              T1 = mean(T1, na.rm = TRUE),
              T4_sd = sd(T4, na.rm = TRUE),
              T4 = mean(T4, na.rm = TRUE),
              abs_change_sd = sd(abs_change, na.rm = TRUE),
              abs_change = mean(abs_change, na.rm = TRUE),
              percent_change_sd = sd(percent_change, na.rm = TRUE),
              percent_change = mean(percent_change, na.rm = TRUE)) %>% print()





# Flex table for å lage tabeller

test_data3 <- test_data %>% 
  dplyr::filter(timepoint == "T1" & vo2.rel.max >= 60) %>% 
  dplyr::filter(id != 30, id != 36, id != 37) %>% 
  dplyr::select(id, age, bodyheight, bodymass) %>% 
  print()

# diary and history dataframe

characteristics <- diary_data %>% 
  dplyr::filter(period == 9) %>% 
  inner_join(test_data3) %>% 
  left_join(history_data) %>% 
  dplyr::select(id, age, bodyheight, bodymass, total.training, total.t.endurance, total.t.strength, total.n.endurance, p.t.bike, p.t.run, p.t.ski, 
                total.n.strengt, p.t.m.strength, p.t.g.strength, active.years, national.races.21, international.races.21, year.total) %>%
  data.frame()


trening <- characteristics %>% 
  dplyr::select(id, total.training, total.t.endurance, total.t.strength, active.years, national.races.21, international.races.21, year.total) %>% 
  dplyr::mutate(total.training = total.training / 60,
                total.t.endurance = total.t.endurance / 60,
                total.t.strength = total.t.strength / 60,
                year.total = year.total / 60) %>% 
  pivot_longer(names_to = "variable",
               values_to = "values",
               cols = c(total.training, total.t.endurance, total.t.strength, year.total)) %>% 
  dplyr::group_by(variable) %>% 
  summarise(mean = mean(values, na.rm = TRUE),
            sd = sd(values, na.rm = TRUE),
            min = min(values,na.rm = TRUE),
            max = max(values, na.rm = TRUE)) %>% 
  print()
  


stargazer(characteristics, type = "html", out = "karakteristikk-tabell.doc",
          digits = 1, digits.extra = 0)
  


tabell_session <- full_data %>% 
  dplyr::select(rel.vo2, rel.hr, la, rpe, hr.total.trimp) %>% 
  data.frame()


stargazer(tabell_session, type = "html", out = "sessions-tabell.doc",
          digits = 2, digits.extra = 0)  

################################################################################

test_long <- test_data2 %>% 
  dplyr::select(id,
                timepoint,
                keiser.fmax,
                keiser.pmax,
                p.vo2.4mmol,
                ge.175,
                ge.225)

test <- function(df) {
  t_test(values ~ timepoint, paired = TRUE, detailed = TRUE, data = df)
} 

wider <- function(df) {
  pivot_wider(names_from = timepoint,
              values_from = values,
              data = df)
}

shapiro <- function(df) {
  shapiro_test(data = df, differences)
}

normality <- function(df) {
  ggqqplot(data = df, "differences")
}

wilcox <- function(df) {
  wilcox_test(values ~ timepoint, data = df, paired = TRUE)
}

data_long <- full_data %>% 
  dplyr::select(id, 
                bodymass_T1:vo2.rel.max_T4, 
                per.index.rel_T1, per.index.rel_T4,
                w.4mmol.rel_T1, w.4mmol.rel_T4, 
                w.max.rel_T1, w.max.rel_T4, 
                w.15tt.rel_T1, w.15tt.rel_T4, 
                w.40tt.rel_T1, w.40tt.rel_T4,
                bv.rel_T1, bv.rel_T4,
                `hbmass/kg_T1`, `hbmass/kg_T4`) %>% 
  pivot_longer(names_to = "variable",
               values_to = "values",
               cols = c(bodymass_T1:`hbmass/kg_T4`)) %>% 
  separate(variable, c("variable", "timepoint"), "_") %>%
  pivot_wider(names_from = variable,
              values_from = values) %>% 
  arrange(timepoint) %>%
  inner_join(test_long) %>% 
  pivot_longer(names_to = "variable",
               values_to = "values",
               cols = c(bodymass:ge.225)) %>% 
  group_nest(variable) %>% 
  dplyr::mutate(test = map(data, test),
                wide = map(data, wider)) %>% 
  unnest(wide) %>% 
  dplyr::mutate(differences = T4 - T1) %>% 
  group_nest(variable) %>% 
  dplyr::mutate(shapiro = map(data, shapiro),
                qqplot = map(data, normality))

data_long$qqplot



statistics <- unnest(data_long, test) %>% 
  dplyr::mutate(p.value = format(p, scientific = FALSE))

ge.175 <- data_long %>% 
  dplyr::filter(variable == "ge.175" | variable == "bodymass") %>% 
  dplyr::select(variable, data) %>% 
  dplyr::mutate(wilcox = map(data, wilcox)) %>% 
  print()
 
  
full_data %>% 
  dplyr::select(id, rel.vo2) %>% 
  summarise(mean = mean(rel.vo2, na.rm = TRUE),
            kvantil = quantile(rel.vo2, na.rm = TRUE)) %>% 
  print()


zone_data <- diary_data %>% 
  dplyr::filter(period == 9) %>% 
  inner_join(test_data3) %>% 
  dplyr::select(id, zone.1, zone.2, zone.3, zone.4, zone.5) %>% 
  dplyr::mutate(zones.1 = zone.1 + zone.2,
                zones.2 = zone.3,
                zones.3 = zone.4 + zone.5) %>% 
  dplyr::select(id, zones.1, zones.2, zones.3) %>% 
  dplyr::mutate(zones.1 = zones.1 / 60,
                zones.2 = zones.2 / 60,
                zones.3 = zones.3 / 60) %>% 
  pivot_longer(names_to = "zones",
               values_to = "values",
               cols = c(zones.1:zones.3)) %>% 
  dplyr::group_by(zones) %>% 
  summarise(mean = mean(values, na.rm = TRUE),
            sd = sd(values, na.rm = TRUE),
            min = min(values, na.rm = TRUE),
            max = max(values, na.rm = TRUE),
            n = n()) %>% 
  print()
  
