#----------------------------------- #
# Linear models in R - effective way #
#------------------------------------#

library(tidyverse)
library(ISLR)
theme_set(theme_test())

set.seed(2024)

# get and clean the data
d = Wage %>%
  filter(wage < 200 & age < 60) %>%
  group_by(education) %>%
  sample_n(100) %>%
  ungroup

# build the model
m = lm(wage ~ age + year + jobclass + education, d)

# check model performance
library(performance)
check_model(m)

# visualize model predictions
library(ggeffects)
ggeffect(m)

ggpredict(m, terms = 'age')

fancy_plot = ggeffect(m) %>%
  plot() %>%
  sjPlot::plot_grid()

fancy_plot

# get fancy table
library(gtsummary)
fancy_table = tbl_regression(
  m, 
  add_pairwise_contrasts = T,
  pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
  bold_p()

fancy_table

# variable importance
library(effectsize)
ef_size = eta_squared(m) %>%
  mutate(interpret = interpret_eta_squared((Eta2_partial)))

ef_size

# not neccessary but better than random forest plot
ef_size %>%
  ggplot(aes(x = reorder(Parameter, Eta2_partial),
             y = Eta2_partial)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = interpret),
            size = 4, hjust = -0.1, fontface = 'bold') +
  coord_flip() +
  xlab('') +
  ylim(0, 0.3)

# how good is our model fit
performance(m)


# https://www.youtube.com/watch?v=Cc4EeYIVZ44