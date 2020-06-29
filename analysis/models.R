# Load packages -----------------------------------------------------------

# Data cleansing
library(tidyverse)
library(readxl)

# Statistical modelling
library(lme4)
library(lmerTest)
library(broom.mixed) 
library(jtools)

# Plotting
library(sjPlot)
library(mvtnorm) # sjPlot dependency
library(ggeffects)
library(ggpubr) # For plotting paired data.
library(ggsci) # Plot themes


# Import data --------------------------------------------------------------
# Import the dataset used for statistical modelling. 
# The dataset includes pseudoanonymised logs from the transcription tool and learners' grades.
# Notice that this is a cleansed & aggregated dataset. For raw data, please contact the author.

phontrans_logs_cleansed <- read_excel("data/cleansed/phontrans_logs_cleansed.xlsx")
phontrans_logs_cleansed$sex <- phontrans_logs_cleansed$sex %>% factor(levels = c("F", "M"))
phontrans_logs_cleansed$group <- phontrans_logs_cleansed$group %>% factor(levels = c(7, 8, 9, 10))
phontrans_logs_cleansed$visited_at_all <- phontrans_logs_cleansed$visited_at_all %>% factor(levels = c("TRUE", "FALSE"))


# Selected models ----------------------------------------------------------

# Outcome = Midterm -------------------------------------------------------
# We will construct a linear regression model that predicts a learner’s midterm test score based on that learner’s usage of the transcription tool.

# Specify coefficient names for pretty plotting.
# coef_names_m01 <- c("Midterm score" = "midterm_score", "Written Matura score" = "matura_writ", "Oral Matura score" = "matura_oral", "Sex" = "as.factor(sex)", "Visit count" = "visit_count", "Visit count (distinct days)" = "visit_count_dist_days", "Transcription count" = "sum_input_count", "Transcription count (distinct words)" = "sum_input_count_distinct", "Mean visit duration" = "mean_visitDuration", "Visit count (7 days before assessment)" = "count_visits_7days_before_assessment", "Visit count (1 day before assessment)" = "count_visits_1day_before_assessment")

# Build model
m01 <- lme4::lmer(midterm_score ~ as.factor(sex) + matura_writ + matura_oral + as.factor(visited_at_all) + visit_count + visit_count_dist_days + sum_input_count + sum_input_count_distinct + mean_visitDuration + count_visits_7days_before_assessment + count_visits_1day_before_assessment + (1|group), data = phontrans_logs_cleansed)

# Explore model parameters
summ(m01, confint = TRUE)
glance(m01)
ranova(m01)
tidy(m01, effects = c("ran_pars", "ran_vals", "fixed"),
     scales = NULL, exponentiate = FALSE, ran_prefix = NULL,
     conf.int = TRUE, conf.level = 0.95, conf.method = "Wald")

# Model without the random variable.
# Build model
m02 <- lm(midterm_score ~ as.factor(sex) + matura_writ + matura_oral + as.factor(visited_at_all) + visit_count + visit_count_dist_days + sum_input_count + sum_input_count_distinct + mean_visitDuration + count_visits_7days_before_assessment + count_visits_1day_before_assessment, data = phontrans_logs_cleansed)

# Explore model parameters
summ(m02)
summ(m02, confint = TRUE)

# Forest-plot of fitted model estimates.
# plot_model(m01, type = "est", sort.est = TRUE, show.values = TRUE, show.p = TRUE, ci.lvl = 0.95, title = "")
plot_model(m02, type = "est", sort.est = TRUE, show.values = TRUE, show.p = TRUE, ci.lvl = 0.95, title = "")

# Below are some other exploratory figures that were not used in the paper.

# Predicted values (marginal effects) for specific model terms. 
# plot_model(m01, type = "pred", pred.type = "re", show.data = TRUE, title = "")

# Slope of coefficients for each single predictor.
# plot_model(m01, type = "slope", show.data = TRUE, show.loess = FALSE)
# plot_model(m01, type = "resid", show.data = TRUE, show.loess = FALSE)

# Plots for multicollinearity-check, QQ-plots, checks for normal distribution of residuals and homoscedasticity.
# plot_model(m01, type = "diag")


# Outcome = Phonotactics quiz ---------------------------------------------
# We will now build a linear model similar to the one in the previous section, with the difference that the outcome variable is the phonotactics quiz score, and a new variable was added indicating whether a learner entered three or more non-words over the course of using the transcription tool.
# For simplicity, the same dataset is used, i.e. the span of the data is the same as for the previous model. Arguably, a further cut off date could be used but then the models would be less comparable.

# Build model
m03 <- lm(t19_quiz_score ~ as.factor(sex) + matura_writ + matura_oral + as.factor(visited_at_all) + visit_count + visit_count_dist_days + sum_input_count + sum_input_count_distinct + mean_visitDuration + count_visits_7days_before_assessment + count_visits_1day_before_assessment + as.factor(count_nwords_3plus), data = phontrans_logs_cleansed)

# Explore model parameters
summ(m03)

# Plot model
plot_model(m03, type = "est", sort.est = TRUE, show.values = TRUE, show.p = TRUE, ci.lvl = 0.95, title = "")


# Export ------------------------------------------------------------------
# Export side-by-side comparison of all three models
export_summs(m01, m02, m03, error_format = "[{conf.low}, {conf.high}]", to.file = "html", file.name = "analysis/m01.html")