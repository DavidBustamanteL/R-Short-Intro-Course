################################################ Session Econometrics ###################################################

# deleting the memory
rm(list = ls())
gc()

# selecting our path ---> not needed if working from a cloned Git
if (.Platform$OS.type == "windows") {
    setwd("PATHSWIN")
} else {
    setwd("PATHSLINUX")
}

# English Format and Avoiding scientific notation
if (.Platform$OS.type == "windows") {
    Sys.setlocale("LC_TIME", "English")
} else {
    Sys.setlocale("LC_TIME", "en_US.utf8")
}

options(scipen = 999)

# Packages
library(dplyr)                  # data manipulation (filter, select, mutate, summarise)
library(mlogit)                 # multinomial logit models (discrete choice analysis)
library(tidyverse)              # collection of packages for data science (ggplot2, dplyr, tidyr, etc.)
library(stargazer)              # create regression tables (LaTeX, HTML, text)
library(rms)                    # regression modeling strategies (advanced modeling, validation)
library(readxl)                 # read Excel files (.xls, .xlsx)
library(magrittr)               # pipe operator %>% and functional helpers
library(broom)                  # tidy model outputs into data frames (tidy, glance, augment)
library(texreg)                 # format regression results for LaTeX/HTML
library(lmtest)                 # statistical tests for models (e.g. coeftest, hypothesis tests)
library(sandwich)               # robust / heteroskedasticity-consistent covariance matrices
library(Ecdat)                  # econometrics datasets (incl. Mroz dataset)
library(estimatr)               # fast estimation with built-in robust/clustered SEs
library(modelsummary)           # flexible regression tables and model summaries
library(giscoR)                 # geographic data from Eurostat GISCO (maps, shapefiles)
library(sf)                     # spatial data handling (simple features for GIS)
library(eurostat)               # Data from the Eurostat programm
library(EconR)                  # Allrounded Models Testing (Mine:))


# importing the data
wages = read.csv("wage1.csv", header = TRUE, sep = ",")
# wages = read.csv("FOLDERNAME/wage1.csv, header = T, sep = ","")  if working from a cloned Git

# Inspect the dataset
head(wages)
str(wages)
summary(wages)

# Number of observations
nrow(wages)

# Descriptive statistics
wages %>%
    summarise(
        mean_wage = mean(wage, na.rm = TRUE),
        sd_wage = sd(wage, na.rm = TRUE),
        mean_educ = mean(educ, na.rm = TRUE),
        mean_exper = mean(exper, na.rm = TRUE),
        female_share = mean(female, na.rm = TRUE)
    )


#### 2. Exploratory Data Analysis ####

# Wage distribution
ggplot(wages, aes(x = wage)) +
    geom_histogram(bins = 30) +
    labs(
        title = "Distribution of Hourly Wages",
        x = "Hourly Wage",
        y = "Frequency"
    ) +
    theme_minimal()

# Education and wages
ggplot(wages, aes(x = educ, y = wage)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "lm", se = TRUE) +
    labs(
        title = "Education and Hourly Wages",
        x = "Years of Education",
        y = "Hourly Wage"
    ) +
    theme_minimal()


#### 3. Ordinary Least Squares (OLS) ####

# Baseline specification
ols_reg = econ_lm(
    wage ~ educ + exper + nonwhite + female + married,
    data = wages
)

# EconR summary
summary(ols_reg)

# Tidy coefficient table
tidy(ols_reg)

# Format p-values
tidy(ols_reg, conf.int = TRUE) %>%
    mutate(p.value = formatC(p.value, format = "f", digits = 5))

# Publication-style table
modelsummary(
  ols_reg,
  stars = c(
        "*" = 0.10,
        "**" = 0.05,
        "***" = 0.01
    )
)


###### 2. Understanding the (OLS) Coefficients ####

# Extract coefficients
coef(ols_reg)

# Variance-covariance matrix
vcov(ols_reg)

# Confidence intervals
confint(ols_reg)

# Fitted values and residuals
head(fitted(ols_reg))
head(residuals(ols_reg))

# Predictions -> returns the predicted hourly wages for the first six individuals in the dataset
head(predict(ols_reg))

# Interpretation:
# educ: expected wage change for one additional year of education,
#       holding other regressors constant.
#
# exper: expected wage change for one additional year of experience.
#
# female: conditional wage difference between female = 1 and female = 0.
#
# married: conditional wage difference between married = 1 and married = 0.
#
# These coefficients describe conditional associations,
# not necessarily causal effects.


###### 3. Classical vs. Robust Standard Errors ####

# Classical OLS standard errors
model_classical = econ_lm(
    wage ~ educ + exper + nonwhite + female + married,
    data = wages,
    vcov = "classical"
)

# Heteroskedasticity-consistent standard errors
model_hc1 = econ_lm(
    wage ~ educ + exper + nonwhite + female + married,
    data = wages,
    vcov = "HC1"
)

model_hc2 = econ_lm(
    wage ~ educ + exper + nonwhite + female + married,
    data = wages,
    vcov = "HC2"
)

model_hc3 = econ_lm(
    wage ~ educ + exper + nonwhite + female + married,
    data = wages,
    vcov = "HC3"
)

# Compare standard errors
modelsummary(
    list(
        "Classical" = model_classical,
        "HC1" = model_hc1,
        "HC2" = model_hc2,
        "HC3" = model_hc3
    ),
    stars = c(
        "*" = 0.10,
        "**" = 0.05,
        "***" = 0.01
    )
)

# Important:
# Robust standard errors change statistical inference,
# not the OLS point estimates.
#
# HC0: basic heteroskedasticity-consistent covariance estimator
# HC1: degrees-of-freedom correction, common in Stata
# HC2: leverage adjustment
# HC3: stronger leverage adjustment
#
# HC0 is not inherently invalid, but can perform poorly
# in smaller samples.


###### 4. Heteroskedasticity Diagnostics ####

# Residuals versus fitted values
plot(
    fitted(model_classical),
    residuals(model_classical),
    xlab = "Fitted Wage",
    ylab = "Residual",
    main = "Residuals vs. Fitted Values"
)

abline(h = 0, col = "red", lty = 2)

# Breusch-Pagan test
lmtest::bptest(model_classical)

# Null hypothesis: homoskedasticity
# Alternative: heteroskedasticity
#
# Robust standard errors can be useful even without
# first rejecting homoskedasticity in a diagnostic test.


###### 5. Logarithmic Dependent Variable ####

# Wage is positive in the standard wage1 dataset.
# A log specification is often useful for earnings.

model_log = econ_lm(
    log(wage) ~ educ + exper + nonwhite + female + married,
    data = wages,
    vcov = "HC1"
)

summary(model_log)

# Interpretation:
# A one-unit increase in educ is associated with an
# approximate 100 * beta percent difference in wages.
#
# For a binary regressor, the exact percentage difference
# is 100 * (exp(beta) - 1).

# Exact percentage interpretation for female
100 * (exp(coef(model_log)["female"]) - 1)


###### 6. Interaction Effects ####

# Does the association between experience and wages
# differ between women and men?

model_interaction = econ_lm(
    wage ~ educ + exper * female,
    data = wages,
    vcov = "HC1"
)

summary(model_interaction)
tidy(model_interaction)

# exper * female expands to:
# exper + female + exper:female
#
# For female = 0:
#   marginal effect of exper = beta_exper
#
# For female = 1:
#   marginal effect of exper = beta_exper + beta_interaction

coef(model_interaction)["exper"]

coef(model_interaction)["exper"] +
    coef(model_interaction)["exper:female"]


###### 7. Linear Hypothesis Tests ####

# H0: The interaction coefficient is zero.
# The experience slope is equal across groups.

car::linearHypothesis(
    model_interaction,
    "exper:female = 0",
    vcov. = vcov(model_interaction),
    test = "F"
)

# H0: Both the female intercept difference
# and the interaction coefficient are zero.

car::linearHypothesis(
    model_interaction,
    c(
        "female = 0",
        "exper:female = 0"
    ),
    vcov. = vcov(model_interaction),
    test = "F"
)

# H0: The experience slope for women is zero.
# beta_exper + beta_interaction = 0

car::linearHypothesis(
    model_interaction,
    "exper + exper:female = 0",
    vcov. = vcov(model_interaction),
    test = "F"
)

# The supplied covariance matrix ensures the tests
# use the HC1 covariance estimator from EconR.


###### 8. Restricted vs. Unrestricted Models ####

# Restricted model: no experience-by-female interaction
model_restricted = econ_lm(
    wage ~ educ + exper + female,
    data = wages,
    vcov = "HC1"
)

# Unrestricted model
model_unrestricted = econ_lm(
    wage ~ educ + exper * female,
    data = wages,
    vcov = "HC1"
)

# Robust Wald test of the interaction restriction
car::linearHypothesis(
    model_unrestricted,
    "exper:female = 0",
    vcov. = vcov(model_unrestricted),
    test = "F"
)

# A classical nested-model comparison is also possible,
# but its usual F reference relies on classical assumptions.
# Do not interpret it as an HC1-robust comparison.


###### 9. Predictions ####

# Predict wages for hypothetical individuals
new_workers = data.frame(
    educ = c(12, 12, 16, 16),
    exper = c(5, 5, 10, 10),
    female = c(0, 1, 0, 1)
)

new_workers$predicted_wage = predict(
    model_interaction,
    newdata = new_workers
)

new_workers

# These are conditional predictions, not causal effects.


###### 10. Comparing Model Specifications ####

modelsummary(
    list(
        "Baseline OLS" = model_classical,
        "Robust OLS" = model_hc1,
        "Interaction" = model_interaction
    ),
    stars = c(
        "*" = 0.10,
        "**" = 0.05,
        "***" = 0.01
    ),
    gof_map = c("nobs", "r.squared", "adj.r.squared", "aic", "bic")
)

# Note:
# R-squared is meaningful for OLS models.
# Unlike ordered logit, there is a conventional OLS R-squared.


###### 11. Comparison with Base R ####

# Standard lm()
base_model = lm(
    wage ~ educ + exper + nonwhite + female + married,
    data = wages
)

# EconR with classical covariance
econ_model = econ_lm(
    wage ~ educ + exper + nonwhite + female + married,
    data = wages,
    vcov = "classical"
)

# OLS coefficients should agree
all.equal(
    unname(coef(base_model)),
    unname(coef(econ_model))
)

# Classical covariance matrices should agree
all.equal(
    unname(vcov(base_model)),
    unname(vcov(econ_model))
)

# Compare reporting
summary(base_model)
summary(econ_model)



#### 4. Logit ####
# importing the data -> female labor supply decisions MROZ
MROZ = read.csv("MROZ.csv")

# Inspect the data
head(MROZ)
str(MROZ)
summary(MROZ)

# Dependent variable:
# inlf = 1 if the woman participates in the labor force
# inlf = 0 otherwise

table(MROZ$inlf)

# Labor force participation rate
mean(MROZ$inlf, na.rm = TRUE)

# Participation by presence of young children
MROZ %>%
    group_by(kidslt6) %>%
    summarise(
        observations = n(),
        participation_rate = mean(inlf, na.rm = TRUE),
        .groups = "drop"
    )


###### 2. Estimate a Binary Logit Model ######

Logit = econ_binary(
    inlf ~ nwifeinc + educ + exper + age + kidslt6,
    link = "logit",
    vcov = "HC1",
    data = MROZ
)

# EconR regression summary
summary(Logit)

# Tidy coefficient table
tidy(Logit)

# Publication-style regression table
modelsummary(
    Logit,
    stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01)
)

# Logit coefficients are expressed in log-odds.
# They are NOT direct changes in probabilities.


###### 3. Interpret Logit Coefficients ######

# Extract coefficients
coef(Logit)

# Example: education coefficient
coef(Logit)["educ"]

# Interpretation:
# One additional year of education changes the log-odds
# of labor force participation by beta_educ,
# holding other variables constant.

# Positive beta -> higher odds of participation.
# Negative beta -> lower odds of participation.
#
# The probability change is not constant;
# it depends on the individual's characteristics.


###### 4. Odds Ratios ######

# Convert logit coefficients into odds ratios
exp(coef(Logit))

# Education odds ratio
exp(coef(Logit)["educ"])

# Percentage change in odds associated with
# one additional year of education
100 * (exp(coef(Logit)["educ"]) - 1)

# Interpretation:
# OR > 1: higher odds of labor force participation
# OR < 1: lower odds of labor force participation
# OR = 1: no change in odds

# Odds ratios are NOT probability ratios.


###### 5. Modelsummary: Log-Odds vs. Odds Ratios ######

# Standard logit coefficients
modelsummary(
    Logit,
    stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
    title = "Female Labor Force Participation: Logit"
)

# Exponentiated coefficients (odds ratios)
modelsummary(
    Logit,
    exponentiate = TRUE,
    stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
    title = "Female Labor Force Participation: Odds Ratios"
)

# With exponentiate = TRUE:
# - Coefficients are transformed into odds ratios.
# - Standard errors may be transformed using the delta method.
# - Statistical significance still tests beta = 0,
#   equivalently odds ratio = 1.


###### 6. Compare Model Specifications ######

logit1 = econ_binary(
    inlf ~ educ,
    data = MROZ,
    link = "logit",
    vcov = "HC1"
)

logit2 = econ_binary(
    inlf ~ educ + exper + age,
    data = MROZ,
    link = "logit",
    vcov = "HC1"
)

logit3 = econ_binary(
    inlf ~ nwifeinc + educ + exper + age + kidslt6,
    data = MROZ,
    link = "logit",
    vcov = "HC1"
)

modelsummary(
    list(
        "(1) Education" = logit1,
        "(2) Human Capital" = logit2,
        "(3) Full Model" = logit3
    ),
    stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
    gof_map = c("nobs", "aic", "bic")
)

# Compare:
# - How does the education coefficient change?
# - Which variables are statistically significant?
# - How does model fit change?
#
# AIC and BIC: lower is preferred for models
# estimated on comparable samples and outcomes.


###### 7. Predicted Probabilities ######

# Fitted probabilities for the first six observations
head(predict(Logit, type = "response"))

# These are probabilities, not predicted wages
# or predicted log-odds.
#
# Example:
# A predicted probability of 0.70 means that,
# conditional on the observed characteristics,
# the model estimates a 70% probability of participation.

# Inspect actual outcomes and fitted probabilities
data.frame(
    actual = MROZ$inlf,
    predicted_probability = predict(Logit, type = "response")
) %>%
    head()

# Summary of predicted probabilities
summary(predict(Logit, type = "response"))


###### 8. Predictions for Hypothetical Individuals ######

# Two otherwise identical women,
# differing in the number of young children

new_women = data.frame(
    nwifeinc = c(20, 20),
    educ = c(12, 12),
    exper = c(10, 10),
    age = c(35, 35),
    kidslt6 = c(0, 1)
)

new_women$probability = predict(
    Logit,
    newdata = new_women,
    type = "response"
)

new_women

# Difference in predicted probabilities
diff(new_women$probability)

# Interpretation:
# This is the model-implied difference in labor force
# participation probability between the two profiles.
#
# It is an association, not necessarily a causal effect.


###### 9. Marginal Effects ######

# For a continuous variable x_j:
# ME_j = beta_j * p * (1 - p)
#
# The marginal effect depends on the predicted probability.

# Marginal effect of education for each observation
p_hat = predict(Logit, type = "response")

me_educ = coef(Logit)["educ"] * p_hat * (1 - p_hat)

head(me_educ)

# Average marginal effect (AME) of education
mean(me_educ)

# Express in percentage points
100 * mean(me_educ)

# Important:
# This manual calculation illustrates the logit formula.
# It does not supply an inference standard error for the AME.


###### 10. Marginal Effects vs. Odds Ratios ######

# Odds ratio for education
exp(coef(Logit)["educ"])

# Average marginal effect for education
mean(me_educ)

# Teaching point:
#
# Odds ratio:
# Multiplicative change in odds for a one-unit increase.
#
# Average marginal effect:
# Average change in predicted probability for a
# one-unit infinitesimal increase in a continuous regressor.
#
# For binary regressors, use discrete probability differences.


###### 11. Hypothesis Testing ######

# H0: education coefficient = 0
car::linearHypothesis(
    Logit,
    "educ = 0",
    vcov. = vcov(Logit),
    test = "Chisq"
)

# H0: education and experience coefficients = 0
car::linearHypothesis(
    Logit,
    c("educ = 0", "exper = 0"),
    vcov. = vcov(Logit),
    test = "Chisq"
)

# Wald chi-square tests are natural for logit models.
# vcov(Logit) supplies the HC1 covariance matrix.


###### 12. Compare Logit and Probit ######

Probit = econ_binary(
    inlf ~ nwifeinc + educ + exper + age + kidslt6,
    link = "probit",
    vcov = "HC1",
    data = MROZ
)

summary(Probit)

modelsummary(
    list(
        "Logit" = Logit,
        "Probit" = Probit
    ),
    stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
    gof_map = c("nobs", "aic", "bic")
)

# Important:
# Logit and probit coefficients are on different scales.
# Do not compare their magnitudes directly.
#
# Instead, compare predicted probabilities or marginal effects.


###### 13. Compare Predicted Probabilities ######

comparison = data.frame(
    logit = predict(Logit, type = "response"),
    probit = predict(Probit, type = "response")
)

head(comparison)

# Correlation of predicted probabilities
cor(comparison$logit, comparison$probit)

# Scatterplot
ggplot(comparison, aes(x = logit, y = probit)) +
    geom_point(alpha = 0.4) +
    geom_abline(slope = 1, intercept = 0, lty = 2) +
    labs(
        title = "Logit vs. Probit Predicted Probabilities",
        x = "Logit probability",
        y = "Probit probability"
    ) +
    theme_minimal()


###### 14. Classification ######

# Convert probabilities into binary predictions
# using a threshold of 0.5

predicted_class = as.integer(
    predict(Logit, type = "response") >= 0.5
)

# Confusion matrix
table(
    Actual = MROZ$inlf,
    Predicted = predicted_class
)

# Classification accuracy (in-sample)
mean(predicted_class == MROZ$inlf)

# Caveat:
# Accuracy is threshold-dependent and may be misleading
# when classes are imbalanced.
#
# In-sample accuracy is not out-of-sample performance.


###### 15. Publication-Ready Table ######

modelsummary(
    list(
        "(1) Logit" = Logit,
        "(2) Probit" = Probit
    ),
    coef_map = c(
        "(Intercept)" = "Intercept",
        "nwifeinc" = "Non-wife income",
        "educ" = "Education (years)",
        "exper" = "Experience (years)",
        "age" = "Age (years)",
        "kidslt6" = "Children under age 6"
    ),
    gof_map = c("nobs", "aic", "bic"),
    estimate = "{estimate}{stars}",
    statistic = "({std.error})",
    stars = c(
        "*" = 0.10,
        "**" = 0.05,
        "***" = 0.01
    ),
    fmt = 3,
    title = "Female Labor Force Participation",
    notes = c(
        "Dependent variable: in labor force (1 = yes).",
        "HC1 heteroskedasticity-robust standard errors in parentheses.",
        "* p < 0.10, ** p < 0.05, *** p < 0.01."
    )
)



#### 5. Ordered Logit ####

# Import health data
ordered = read_csv("ordered_health.csv")

# Inspect the data
head(ordered)
str(ordered)
summary(ordered)

# Check the observed health categories
unique(ordered$healthstatus)
table(ordered$healthstatus, useNA = "ifany")

# Create an ordered factor
ordered = ordered %>%
    mutate(
        health1 = factor(
            healthstatus,
            levels = c("fair", "good", "excellent"),
            ordered = TRUE
        )
    )

# Verify the ordering
levels(ordered$health1)
is.ordered(ordered$health1)
table(ordered$health1)

# Relative frequencies
prop.table(table(ordered$health1))

# IMPORTANT:
# The ordering matters:
# fair < good < excellent
#
# The categories have a natural ranking, but the numerical
# distance between them is not assumed to be equal.


###### 2. Descriptive Analysis ######

# Health distribution
ggplot(ordered, aes(x = health1)) +
    geom_bar() +
    labs(
        title = "Self-Reported Health Status",
        x = "Health Status",
        y = "Number of Observations"
    ) +
    theme_minimal()

# Health status by age
ordered %>%
    group_by(health1) %>%
    summarise(
        n = n(),
        mean_age = mean(age, na.rm = TRUE),
        mean_income = mean(logincome, na.rm = TRUE),
        mean_diseases = mean(numberdiseases, na.rm = TRUE),
        .groups = "drop"
    )

# Discussion:
# Do older respondents tend to report worse health?
# How does income vary across health categories?
# Do respondents reporting excellent health have fewer diseases?


###### 3. Estimate Ordered Logit ######

modeltest = econ_ordered(
    health1 ~ age + logincome + numberdiseases,
    data = ordered,
    link = "logit",
    vcov = "HC1"
)

# EconR summary
summary(modeltest)

# Tidy results
broom::tidy(modeltest)

# Confidence intervals
generics::tidy(
    modeltest,
    conf.int = TRUE
)

# Publication-style regression table
modelsummary(
    modeltest,
    stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01)
)

# Interpretation:
# Positive coefficient:
# Higher values of the explanatory variable are associated
# with greater odds of reporting a higher health category.
#
# Negative coefficient:
# Higher values are associated with lower odds of reporting
# a higher health category.


###### 4. Understanding the Ordered Logit Model ######

# For three categories:
#
# 1 = fair
# 2 = good
# 3 = excellent
#
# Ordered logit models cumulative probabilities:
#
# logit[P(Y <= j | X)] = zeta_j - X'beta
#
# For j = 1 and j = 2.
#
# There are:
# - Three outcome categories
# - Two estimated thresholds
# - One coefficient per explanatory variable
#
# The thresholds divide the latent health index into
# ordered outcome categories.
#
# Unlike multinomial logit, ordered logit does not estimate
# separate slope coefficients for every category.


###### 5. Coefficients and Thresholds ######

# Regression coefficients
coef(modeltest)

# Threshold parameters
modeltest$zeta

# Full covariance matrix
vcov(modeltest)

# Standard errors for coefficients and thresholds
sqrt(diag(vcov(modeltest)))

# Important:
# Coefficients and thresholds have different roles.
#
# beta: effects of explanatory variables on the latent index.
# zeta: cutpoints between adjacent ordered categories.
#
# Thresholds are not effects of explanatory variables.


###### 6. Odds Ratios ######

# Odds ratios for regression coefficients
exp(coef(modeltest))

# Odds ratios for thresholds, following the
# higher-category convention used by rms::lrm
exp(-modeltest$zeta)

# Native EconR odds-ratio reporting
summary(
    modeltest,
    exponentiate = TRUE
)

# Tidy odds ratios with confidence intervals
generics::tidy(
    modeltest,
    exponentiate = TRUE,
    conf.int = TRUE
)

# Percentage change in cumulative odds
100 * (exp(coef(modeltest)) - 1)

# Interpretation:
#
# OR > 1:
# Higher odds of being above a given health threshold.
#
# OR < 1:
# Lower odds of being above a given health threshold.
#
# OR = 1:
# No association with the cumulative odds.
#
# Example:
# OR = 1.30 means 30% higher cumulative odds.
# OR = 0.80 means 20% lower cumulative odds.
#
# Odds ratios are NOT changes in probabilities.


###### 7. Interpret the Health Variables ######

# Age
exp(coef(modeltest)["age"])

# Income
exp(coef(modeltest)["logincome"])

# Number of diseases
exp(coef(modeltest)["numberdiseases"])

# Assuming the coefficient signs in the health example:
#
# age < 0:
# Older respondents have lower cumulative odds of reporting
# a higher health category, holding other variables constant.
#
# logincome > 0:
# Higher income is associated with greater cumulative odds
# of reporting better health.
#
# numberdiseases < 0:
# Additional diseases are associated with lower cumulative
# odds of reporting better health.
#
# These are conditional associations, not necessarily
# causal effects.


###### 8. Modelsummary: Standard Coefficient Tables ######

# Original ordered-logit coefficients
modelsummary(
    modeltest,
    estimate = "{estimate}{stars}",
    statistic = "({std.error})",
    stars = c(
        "*" = 0.10,
        "**" = 0.05,
        "***" = 0.01
    ),
    gof_map = c("nobs", "aic", "bic"),
    fmt = 3
)

# Rename and reorder coefficients
modelsummary(
    modeltest,
    coef_map = c(
        "age" = "Age",
        "logincome" = "Log income",
        "numberdiseases" = "Number of diseases",
        "fair|good" = "Fair / Good threshold",
        "good|excellent" = "Good / Excellent threshold"
    ),
    gof_map = c("nobs", "aic", "bic"),
    stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
    fmt = 3
)

# Show slope coefficients only
modelsummary(
    modeltest,
    coef_map = c(
        "age" = "Age",
        "logincome" = "Log income",
        "numberdiseases" = "Number of diseases"
    ),
    gof_map = c("nobs", "aic", "bic"),
    stars = TRUE
)

# In many applied papers, thresholds are omitted from
# the main regression table because they are not
# explanatory-variable effects.


###### 9. Modelsummary: Exponentiated Coefficients ######

# Modelsummary's own exponentiation
modelsummary(
    modeltest,
    exponentiate = TRUE,
    estimate = "{estimate}{stars}",
    statistic = "({std.error})",
    fmt = 3,
    stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
    gof_map = c("nobs", "aic", "bic")
)

# IMPORTANT:
# modelsummary exponentiates the original thresholds:
#
# exp(zeta)
#
# EconR's native odds-ratio reporting instead uses:
#
# exp(-zeta)
#
# These threshold values are reciprocals.
# They correspond to opposite cumulative-odds orientations.
#
# Slope odds ratios are the same in both approaches.


###### 10. Recommended Odds-Ratio Table ######

# For a publication table, omit thresholds when using
# modelsummary's automatic exponentiation.
#
# This avoids mixing threshold conventions and focuses
# on interpretable covariate effects.

modelsummary(
    modeltest,
    exponentiate = TRUE,
    coef_map = c(
        "age" = "Age",
        "logincome" = "Log income",
        "numberdiseases" = "Number of diseases"
    ),
    estimate = "{estimate}{stars}",
    statistic = "({std.error})",
    gof_map = c("nobs", "aic", "bic"),
    fmt = 3,
    stars = c(
        "*" = 0.10,
        "**" = 0.05,
        "***" = 0.01
    ),
    title = "Ordered Logit: Self-Reported Health",
    notes = c(
        "Dependent variable: fair < good < excellent.",
        "Odds ratios with delta-method standard errors.",
        "HC1 heteroskedasticity-robust covariance estimator.",
        "* p < 0.10, ** p < 0.05, *** p < 0.01."
    ),
    output = "ologit_econ_table.html"
)


###### 11. Predicted Category Probabilities ######

# Predict probabilities for all three health categories
probabilities = predict(
    modeltest,
    type = "probs"
)

head(probabilities)

# Each row should sum to 1
head(rowSums(probabilities))

# Example:
# fair      0.20
# good      0.50
# excellent 0.30
#
# Interpretation:
# The individual has a predicted probability of:
# 20% fair health
# 50% good health
# 30% excellent health
#
# These are mutually exclusive categories.


###### 12. Predict the Most Likely Category ######

# Predict the category with the highest probability
predicted_health = predict(
    modeltest,
    type = "class"
)

head(predicted_health)

# Compare predicted and observed categories
table(
    Actual = ordered$health1,
    Predicted = predicted_health
)

# The predicted class is the category with the
# highest predicted probability.
#
# This is not necessarily the category that contains
# the individual's conditional mean latent index.


###### 13. Predictions for Hypothetical Individuals ######

# Compare two respondents with different ages
# but identical income and disease counts.

new_people = data.frame(
    age = c(30, 70),
    logincome = c(10, 10),
    numberdiseases = c(1, 1)
)

# Predicted category probabilities
predict(
    modeltest,
    newdata = new_people,
    type = "probs"
)

# Discussion:
# How does the predicted probability of excellent health
# change between ages 30 and 70?
#
# How does the probability of fair health change?
#
# Does the probability of good health necessarily move
# in the same direction?


###### 14. Probability Effects of an Additional Disease ######

# Two otherwise identical respondents
new_disease = data.frame(
    age = c(50, 50),
    logincome = c(10, 10),
    numberdiseases = c(1, 2)
)

disease_prob = predict(
    modeltest,
    newdata = new_disease,
    type = "probs"
)

disease_prob

# Change in probability for each category
disease_prob[2, ] - disease_prob[1, ]

# Multiply by 100 to obtain percentage-point changes
100 * (disease_prob[2, ] - disease_prob[1, ])

# Interpretation:
# The probability effects differ by outcome category.
#
# An increase in the number of diseases may:
# - Increase P(fair)
# - Change P(good) in either direction
# - Decrease P(excellent)
#
# The probability changes sum to zero.


###### 15. Visualize Predicted Probabilities ######

# Construct hypothetical profiles across age
age_profiles = data.frame(
    age = seq(20, 80, by = 5),
    logincome = median(ordered$logincome, na.rm = TRUE),
    numberdiseases = median(ordered$numberdiseases, na.rm = TRUE)
)

# Predict category probabilities
age_probs = as.data.frame(
    predict(
        modeltest,
        newdata = age_profiles,
        type = "probs"
    )
)

age_probs$age = age_profiles$age

# Convert from wide to long format
age_probs_long = tidyr::pivot_longer(
    age_probs,
    cols = -age,
    names_to = "health",
    values_to = "probability"
)

# Plot predicted probabilities
ggplot(
    age_probs_long,
    aes(x = age, y = probability, color = health)
) +
    geom_line(linewidth = 1) +
    labs(
        title = "Predicted Health Probabilities by Age",
        x = "Age",
        y = "Predicted Probability",
        color = "Health Status"
    ) +
    theme_minimal()


###### 16. Proportional-Odds Assumption ######

# Ordered logit assumes the slope coefficients are
# the same across cumulative thresholds.
#
# For three categories:
#
# logit[P(Y > fair)] = X'beta - zeta_1
#
# logit[P(Y > good)] = X'beta - zeta_2
#
# The intercepts differ, but the slopes are identical.
#
# This is the proportional-odds assumption.
#
# It implies that a variable's cumulative odds ratio
# is the same across the two thresholds.
#
# For example, exp(beta_age) applies both to:
# - Odds of good/excellent versus fair
# - Odds of excellent versus fair/good
#
# If this assumption is inappropriate, a generalized
# ordered logit model may be more suitable.


###### 17. Ordered Logit vs. Ordered Probit ######

model_probit = econ_ordered(
    health1 ~ age + logincome + numberdiseases,
    data = ordered,
    link = "probit",
    vcov = "HC1"
)

summary(model_probit)

modelsummary(
    list(
        "Ordered Logit" = modeltest,
        "Ordered Probit" = model_probit
    ),
    coef_map = c(
        "age" = "Age",
        "logincome" = "Log income",
        "numberdiseases" = "Number of diseases"
    ),
    gof_map = c("nobs", "aic", "bic"),
    stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
    fmt = 3
)

# Logit and probit coefficients have different scales.
# Do not compare their magnitudes directly.
#
# Odds ratios are meaningful for logit, not probit.
#
# Compare predicted probabilities instead.


###### 18. Why Not OLS? ######

# The outcome is ordinal, not continuous.
#
# Coding fair = 1, good = 2, excellent = 3 imposes
# equal numerical distances between categories.
#
# OLS would treat:
# fair -> good
# and
# good -> excellent
# as equally spaced changes.
#
# Ordered logit respects the ranking without assuming
# equal distances between categories.
#
# It also produces valid category probabilities
# between 0 and 1 that sum to 1.
