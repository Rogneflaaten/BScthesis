## analysis of 6min data


# loading basic packages 
library(tidyverse)
library(readxl)


# Importing the different datasets needed for the analysis
session_data <- readRDS("data/derivedData/session-data-6min.RDS")
test_data  <- read_excel("data/testday1.xlsx", na = "na")
session_id <- read_excel("data/sessions-data.xlsx", na = "na")
trimp_data <- read_excel("data/trimp-data.xlsx", na = "na")


# Transforming the session dataset by calculating the average VO2 across all 
# bouts and sessions for each participant and period
session_data <- session_data %>% 
  group_by(id, period) %>% 
  summarise(vo2.mean = mean(vo2, na.rm = TRUE))


# Making a dataset containing each participants VO2max for each period 
test_data_id <- test_data %>% 
  dplyr::select(id, timepoint, vo2.max, vo2.rel.max) %>%
  mutate(period = gsub("T", "", timepoint)) %>%
  filter(timepoint != "T4") %>%
  dplyr::select(-timepoint) %>%
  mutate(period = as.numeric(period))


# Making a dataset containing id and period
session_id <- session_id %>% 
  dplyr::select(id, period) %>%
  group_by(id, period) %>%
  summarise(period = mean(period))


# making a dataset with absolute change and percent change (%) for performance index 
per_index <- test_data %>% 
  dplyr::select(id, timepoint, w.4mmol, w.max, w.15tt) %>%
  pivot_longer(names_to = "variable", 
               values_to = "values", 
               cols = w.4mmol:w.15tt) %>% 
  group_by(variable) %>%
  mutate(scaled = values / max(values, na.rm = TRUE)) %>%
  group_by(id, timepoint) %>%
  summarise(per_index = mean(scaled)) %>%
  filter(timepoint == "T1" | timepoint == "T4") %>% 
  pivot_wider(names_from = timepoint,
              values_from = per_index) %>% 
  mutate(change_per_index = T4 - T1,
         change_per_index_percent = (T4 - T1) / T1 * 100) %>% 
  dplyr::select(id, change_per_index, change_per_index_percent, T1, T4)


# making a dataset containing percent change for w.4mmol, vo2.max, w.max and w.15tt
change_data <- test_data %>% 
  dplyr::select(id, timepoint, w.4mmol, vo2.rel.max, vo2.max, w.max, w.15tt) %>% 
  filter(timepoint == "T1" | timepoint == "T4") %>% 
  pivot_wider(names_from = timepoint,
              values_from = c(vo2.max, vo2.rel.max, w.4mmol, w.max, w.15tt)) %>% 
  mutate(change_vo2 = (vo2.max_T4 - vo2.max_T1) / vo2.max_T1 * 100,
         change_w.4mmol = (w.4mmol_T4 - w.4mmol_T1) / w.4mmol_T1 * 100,
         change_w.max = (w.max_T4 - w.max_T1) / w.max_T1 * 100,
         change_w.15tt = (w.15tt_T4 - w.15tt_T1) / w.15tt_T1 * 100) %>% 
  dplyr::select(id, vo2.rel.max_T1, change_vo2:change_w.15tt)


# making a dataset containing trimp score (min)
trimp_data <- trimp_data %>% 
  dplyr::select(id, total.trimp) %>% 
  mutate(trimp = total.trimp / 60) %>% 
  na.omit() %>% 
  dplyr::select(id, trimp)



# Combining the different datasets to one full dataset 
full_data <- session_data %>% 
  dplyr::select(id:vo2.mean) %>% 
  inner_join(test_data_id) %>% 
  inner_join(session_id) %>% 
  inner_join(change_data) %>% 
  inner_join(per_index) %>%
  inner_join(trimp_data) %>% 
  mutate(rel.vo2 = 100 * (vo2.mean / vo2.max)) %>% 
  group_by(id) %>%
  summarise(rel.vo2 = mean(rel.vo2, na.rm = TRUE),
            change_vo2 = mean(change_vo2, na.rm = TRUE),
            change_w.4mmol = mean(change_w.4mmol, na.rm = TRUE),
            change_w.max = mean(change_w.max, na.rm = TRUE),
            change_w.15tt = mean(change_w.15tt, na.rm = TRUE),
            change_per_index = mean(change_per_index, na.rm = TRUE),
            change_per_index_percent = mean(change_per_index_percent, na.rm = TRUE),
            vo2.rel.max_T1 = mean(vo2.rel.max_T1, na.rm = TRUE),
            trimp = mean(trimp, na.rm = TRUE),
            T1 = mean(T1, na.rm = TRUE),
            T4 = mean(T4, na.rm = TRUE)) %>% 
  filter(id != 37) %>% 
  filter(id != 30) %>% 
  filter(vo2.rel.max_T1 >= 60) %>% 
  filter(id != 36)



# making a simple scatterplot 
full_data %>% 
  ggplot(aes(x = rel.vo2, y = change_per_index)) +
  geom_point() +
  theme_bw()



# making a simple scatterplot for percent change for performance index
full_data %>% 
  ggplot(aes(x = rel.vo2, y = change_per_index_percent)) +
  geom_point() +
  theme_bw()



# making a simple scatterplot for percent change for VO2max
full_data %>% 
  ggplot(aes(x = rel.vo2, y = change_vo2)) +
  geom_point() +
  theme_bw()



# making a simple scatterplot for change in w.4mmol 
full_data %>% 
  ggplot(aes(rel.vo2, change_w.4mmol)) +
  geom_point() +
  theme_bw()



# making a simple scatterplot for change in w.15tt
full_data %>% 
  ggplot(aes(rel.vo2, change_w.15tt)) +
  geom_point() +
  theme_bw()



# making a simple scatterplot for change in w.max
full_data %>% 
  ggplot(aes(rel.vo2, change_w.max)) +
  geom_point() +
  theme_bw()



# making a scatterplot comparing rel.vo2 and trimp
full_data %>% 
  ggplot(aes(rel.vo2, trimp)) +
  geom_point() +
  theme_bw()



# load required packages
library(modelr)



# making model dataset and using log2 for rel.vo2 and change_per_index_percent 
model_data <- full_data %>% 
  mutate(l.rel.vo2 = log(rel.vo2),
         l.change_per_index = log(change_per_index)) %>%  print()


# plotting model_data
ggplot(model_data, aes(x = l.rel.vo2, y = l.change_per_index)) +
  geom_point() +
  theme_bw()

model_nr <- lm(change_per_index ~ rel.vo2 + T1, data = model_data)
plot(model_nr)
# making the model
model <- lm(l.change_per_index ~ rel.vo2 + T1, data = model_data)
plot(model)

# Visualize the predicted values with an evenly spaced grid
grid <- model_data %>% 
  data_grid(T1, .model = model) %>% 
  add_predictions(model = model)
grid

# Visualizing the model by plotting the line for the predictions
ggplot(model_data, aes(rel.vo2, change_per_index)) + 
  geom_point() + 
  geom_line(data = grid, colour = "red", size = 1) +
  theme_bw()


# adding the residuals for the model
model_data <- model_data %>% 
  add_residuals(model, "lresid")


# plotting the residuals
ggplot(model_data, aes(l.rel.vo2, lresid)) + 
  geom_point() +
  geom_ref_line(h = 0, colour = "grey30") +
  theme_bw()


# summarise the model
summary(model)


################################################################################


# exploring the effect of vo2.rel.max_T1 on residuals from model
ggplot(model_data, aes(vo2.rel.max_T1, lresid)) +
  geom_point() +
  theme_bw()


# exploring the effect of trimp on residuals from model
ggplot(model_data, aes(trimp, lresid)) +
  geom_point() +
  theme_bw()


# making a second model that takes the individual effect of relative VO2max at T1
model2 <- lm(l.change_per_index ~ l.rel.vo2 + vo2.rel.max_T1, data = model_data)


# Visualize the predicted values for model2 with an evenly spaced grid
grid2 <- model_data %>% 
  data_grid(vo2.rel.max_T1, .model = model2) %>% 
  add_predictions(model2)
grid2


# plotting the predicted values for relative VO2max
ggplot(grid2, aes(vo2.rel.max_T1, pred)) + 
  geom_point() +
  theme_bw()


# adding residuals
model_data <- model_data %>% 
  add_residuals(model2, "lresid2")


# plotting the residuals
ggplot(model_data, aes(l.rel.vo2, lresid2)) + 
  geom_point() +
  geom_ref_line(h = 0, colour = "grey30") +
  theme_bw()


# summarise model2
summary(model2)


################################################################################


# making a smooth model with the loess() function
model_smooth <- loess(change_per_index ~ rel.vo2, data = model_data)


# Visualize the predicted values with an evenly spaced grid
grid_smooth <- model_data %>% 
  data_grid(rel.vo2 = seq_range(rel.vo2, 25)) %>% 
  add_predictions(model_smooth, "change_per_index")
grid_smooth


# Visualizing the model by plotting the line for the predictions
ggplot(model_data, aes(rel.vo2, change_per_index)) + 
  geom_point() + 
  geom_line(data = grid_smooth, colour = "red", size = 1) +
  theme_bw()


# adding the residuals for the smooth model
model_data <- model_data %>% 
  add_residuals(model_smooth, "resid_smooth")


# plotting the residuals
ggplot(model_data, aes(rel.vo2, resid_smooth)) + 
  geom_point() +
  geom_ref_line(h = 0, colour = "grey30") +
  theme_bw()


# summarise the smooth model
summary(model_smooth)


################################################################################



# making the model
model_w.max <- lm(change_w.max ~ rel.vo2, data = model_data)


# Visualize the predicted values with an evenly spaced grid
grid_w.max <- model_data %>% 
  data_grid(rel.vo2 = seq_range(rel.vo2, 20)) %>% 
  add_predictions(model_w.max, "change_w.max")
grid_w.max


# Visualizing the model by plotting the line for the predictions
ggplot(model_data, aes(rel.vo2, change_w.max)) + 
  geom_point() + 
  geom_line(data = grid_w.max, colour = "red", size = 1) +
  theme_bw()


# adding the residuals for the model
model_data <- model_data %>% 
  add_residuals(model_w.max, "resid_wmax")


# plotting the residuals
ggplot(model_data, aes(rel.vo2, resid_wmax)) + 
  geom_point() +
  geom_ref_line(h = 0, colour = "grey30") +
  theme_bw()


# summarise the model
summary(model_w.max)


################################################################################


# making the model
model_w.4mmol <- lm(change_w.4mmol ~ rel.vo2, data = model_data)


# Visualize the predicted values with an evenly spaced grid
grid_w.4mmol <- model_data %>% 
  data_grid(rel.vo2 = seq_range(rel.vo2, 20)) %>% 
  add_predictions(model_w.4mmol, "change_w.4mmol")
grid_w.4mmol


# Visualizing the model by plotting the line for the predictions
ggplot(model_data, aes(rel.vo2, change_w.4mmol)) + 
  geom_point() + 
  geom_line(data = grid_w.4mmol, colour = "red", size = 1) +
  theme_bw()


# adding the residuals for the model
model_data <- model_data %>% 
  add_residuals(model_w.4mmol, "resid_w4mmol")


# plotting the residuals
ggplot(model_data, aes(rel.vo2, resid_w4mmol)) + 
  geom_point() +
  geom_ref_line(h = 0, colour = "grey30") +
  theme_bw()


# summarise the model
summary(model_w.4mmol)


################################################################################


# making the model
model_w.15tt <- lm(change_w.15tt ~ rel.vo2, data = model_data)


# Visualize the predicted values with an evenly spaced grid
grid_w.15tt <- model_data %>% 
  data_grid(rel.vo2 = seq_range(rel.vo2, 20)) %>% 
  add_predictions(model_w.15tt, "change_w.15tt")
grid_w.15tt


# Visualizing the model by plotting the line for the predictions
ggplot(model_data, aes(rel.vo2, change_w.15tt)) + 
  geom_point() + 
  geom_line(data = grid_w.15tt, colour = "red", size = 1) +
  theme_bw()


# adding the residuals for the model
model_data <- model_data %>% 
  add_residuals(model_w.15tt, "resid_w15tt")


# plotting the residuals
ggplot(model_data, aes(rel.vo2, resid_w15tt)) + 
  geom_point() +
  geom_ref_line(h = 0, colour = "grey30") +
  theme_bw()


# summarise the model
summary(model_w.15tt)


################################################################################


# making the model
model_vo2 <- lm(change_vo2 ~ rel.vo2, data = model_data)


# Visualize the predicted values with an evenly spaced grid
grid_vo2 <- model_data %>% 
  data_grid(rel.vo2 = seq_range(rel.vo2, 20)) %>% 
  add_predictions(model_vo2, "change_vo2")
grid_vo2


# Visualizing the model by plotting the line for the predictions
ggplot(model_data, aes(rel.vo2, change_vo2)) + 
  geom_point() + 
  geom_line(data = grid_vo2, colour = "red", size = 1) +
  theme_bw()


# adding the residuals for the model
model_data <- model_data %>% 
  add_residuals(model_vo2, "resid_vo2")


# plotting the residuals
ggplot(model_data, aes(rel.vo2, resid_vo2)) + 
  geom_point() +
  geom_ref_line(h = 0, colour = "grey30") +
  theme_bw()


# summarise the model
summary(model_vo2)


################################################################################


## paired t-test for selected variables
library(rstatix)
install.packages("FSA")
library(FSA)

# filtering out participants
test_data2 <- test_data %>% 
  filter(timepoint == "T1" & vo2.rel.max >= 60 | timepoint == "T4" & vo2.rel.max >= 60) %>%
  filter(id != 30, id != 36, id != 37, id != 27)


# t.test for f.max

f.max <- test_data2 %>% 
  dplyr::select(id, timepoint, keiser.fmax) %>% 
  pivot_wider(names_from = timepoint,
              values_from = keiser.fmax) %>% 
  dplyr::select(id, T1, T4) %>% 
  print()

T1_f.max <- f.max$T1
T4_f.max <- f.max$T4

t.f.max <- t.test(T4_f.max, T1_f.max, paired = TRUE, alternative = "two.sided") 

effect_fmax <- f.max %>% 
  pivot_longer(T1:T4, names_to = "timepoint",
               values_to = "nM") %>% 
  cohens_d(nM ~ timepoint, paired = TRUE)

t.f.max$effect <- effect_fmax$effsize



# t.test for p.max
p.max <- test_data2 %>% 
  dplyr::select(id, timepoint, keiser.pmax) %>% 
  pivot_wider(names_from = timepoint,
              values_from = keiser.pmax) %>% 
  dplyr::select(id, T1, T4) %>% 
  print()

T1_p.max <- p.max$T1
T4_p.max <- p.max$T4

t.p.max <- t.test(T4_p.max, T1_p.max, paired = TRUE, alternative = "two.sided") 

effect_pmax <- p.max %>% 
  pivot_longer(T1:T4, names_to = "timepoint",
               values_to = "watt") %>% 
  cohens_d(watt ~ timepoint, paired = TRUE)

t.p.max$effect <- effect_pmax$effsize


# t.test for degree of utilization on threshold power
p.vo2.4mmol <- test_data2 %>% 
  dplyr::select(id, timepoint, p.vo2.4mmol) %>% 
  pivot_wider(names_from = timepoint,
              values_from = p.vo2.4mmol) %>% 
  dplyr::select(id, T1, T4)

T1_p.vo2.4mmol <- p.vo2.4mmol$T1
T4_p.vo2.4mmol <- p.vo2.4mmol$T4

t.p.vo2.4mmol <- t.test(T4_p.vo2.4mmol, T1_p.vo2.4mmol, paired = TRUE, alternative = "two.sided")

effect_vo2.4mmol <- p.vo2.4mmol %>% 
  pivot_longer(T1:T4, names_to = "timepoint",
               values_to = "percent") %>% 
  cohens_d(percent ~ timepoint, paired = TRUE)

t.p.vo2.4mmol$effect <- effect_vo2.4mmol$effsize


# t.test for GE 175w
ge.175 <- test_data2 %>% 
  dplyr::select(id, timepoint, ge.175) %>% 
  pivot_wider(names_from = timepoint,
              values_from = ge.175) %>% 
  dplyr::select(id, T1, T4)

T1_ge.175 <- ge.175$T1
T4_ge.175 <- ge.175$T4

t.ge.175 <- t.test(T4_ge.175, T1_ge.175, paired = TRUE, alternative = "two.sided")

effect_ge.175 <- ge.175 %>% 
  pivot_longer(T1:T4, names_to = "timepoint",
               values_to = "percent") %>% 
  cohens_d(percent ~ timepoint, paired = TRUE)

t.ge.175$effect <- effect_ge.175$effsize

# t.test for GE 225w
ge.225 <- test_data2 %>% 
  dplyr::select(id, timepoint, ge.225) %>% 
  pivot_wider(names_from = timepoint,
              values_from = ge.225) %>% 
  dplyr::select(id, T1, T4)

T1_ge.225 <- ge.225$T1
T4_ge.225 <- ge.225$T4

t.ge.225 <- t.test(T4_ge.225, T1_ge.225, paired = TRUE, alternative = "two.sided")

effect_ge.225 <- ge.225 %>% 
  pivot_longer(T1:T4, names_to = "timepoint",
               values_to = "percent") %>% 
  cohens_d(percent ~ timepoint, paired = TRUE)

t.ge.225$effect <- effect_ge.225$effsize

# t.test for threshold power
w.4mmol <- test_data2 %>% 
  dplyr::select(id, timepoint, w.4mmol) %>% 
  pivot_wider(names_from = timepoint,
              values_from = w.4mmol) %>% 
  dplyr::select(id, T1, T4)

T1_w.4mmol <- w.4mmol$T1
T4_w.4mmol <- w.4mmol$T4

t.w.4mmol <- t.test(T4_w.4mmol, T1_w.4mmol, paired = TRUE, alternative = "two.sided")

effect_w.4mmol <- w.4mmol %>% 
  pivot_longer(T1:T4, names_to = "timepoint",
               values_to = "watt") %>% 
  cohens_d(watt ~ timepoint, paired = TRUE)

t.w.4mmol$effect <- effect_w.4mmol$effsize

# t.test for w.max
w.max <- test_data2 %>% 
  dplyr::select(id, timepoint, w.max) %>% 
  pivot_wider(names_from = timepoint,
              values_from = w.max) %>% 
  dplyr::select(id, T1, T4)

T1_w.max <- w.max$T1
T4_w.max <- w.max$T4

t.w.max <- t.test(T4_w.max, T1_w.max, paired = TRUE, alternative = "two.sided")

effect_w.max <- w.max %>% 
  pivot_longer(T1:T4, names_to = "timepoint",
               values_to = "watt") %>% 
  cohens_d(watt ~ timepoint, paired = TRUE)

t.w.max$effect <- effect_w.max$effsize

# t.test for VO2max
vo2.max <- test_data2 %>% 
  dplyr::select(id, timepoint, vo2.max) %>% 
  pivot_wider(names_from = timepoint,
              values_from = vo2.max) %>% 
  dplyr::select(id, T1, T4)

T1_vo2.max <- vo2.max$T1
T4_vo2.max <- vo2.max$T4

t.vo2.max <- t.test(T4_vo2.max, T1_vo2.max, paired = TRUE, alternative = "two.sided")

effect_vo2.max <- vo2.max %>% 
  pivot_longer(T1:T4, names_to = "timepoint",
               values_to = "ml") %>% 
  cohens_d(ml ~ timepoint, paired = TRUE)

t.vo2.max$effect <- effect_vo2.max$effsize

# t.test for 15tt watt
w.15tt <- test_data2 %>% 
  dplyr::select(id, timepoint, w.15tt) %>% 
  pivot_wider(names_from = timepoint,
              values_from = w.15tt) %>% 
  dplyr::select(id, T1, T4)

T1_w.15tt <- w.15tt$T1
T4_w.15tt <- w.15tt$T4

t.15tt <- t.test(T4_w.15tt, T1_w.15tt, paired = TRUE, alternative = "two.sided")

effect_w.15tt <- w.15tt %>% 
  pivot_longer(T1:T4, names_to = "timepoint",
               values_to = "watt") %>% 
  cohens_d(watt ~ timepoint, paired = TRUE)

t.15tt$effect <- effect_w.15tt$effsize

# loading requiredpackages
library(broom)
library(purrr)


# gathered respective t.tests to one table
tab <- map_df(list(t.f.max, t.p.max, t.p.vo2.4mmol, t.ge.175, t.ge.225, t.w.4mmol,
                   t.w.max, t.vo2.max, t.15tt), tidy) %>% 
  mutate(variable = c("t.f.max", "t.p.max", "t.p.vo2.4mmol", "t.ge.175", "t.ge.225", "t.w.4mmol",
                      "t.w.max", "t.vo2.max", "t.15tt")) %>%
  relocate(variable) %>% 
  dplyr::select(variable, estimate, statistic, p.value, parameter, conf.low,
                conf.high) %>% 
  print()





