## analysis of min data


# loading basic packages 
library(tidyverse)
library(readxl)


# Importing the different datasets needed for the analysis
session_data <- readRDS("data/derivedData/session-data-4min.RDS")
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
  dplyr::select(id, change_per_index, change_per_index_percent)


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
            trimp = mean(trimp, na.rm = TRUE)) %>% 
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


# load required packages
library(modelr)

# making model dataset and using log2 for rel.vo2 and change_per_index_percent 
model_data <- full_data %>% 
  mutate(l.rel.vo2 = log2(rel.vo2),
         l.change_per_index = log2(change_per_index))


# plotting model_data
ggplot(model_data, aes(x = l.rel.vo2, y = l.change_per_index)) +
  geom_point() +
  theme_bw()


# making the model
model <- lm(l.change_per_index ~ l.rel.vo2, data = model_data)


# Visualize the predicted values with an evenly spaced grid
grid <- model_data %>% 
  data_grid(rel.vo2 = seq_range(rel.vo2, 20)) %>% 
  mutate(l.rel.vo2 = log2(rel.vo2)) %>% 
  add_predictions(model, "l.change_per_index") %>% 
  mutate(change_per_index = 2 ^ l.change_per_index)
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


