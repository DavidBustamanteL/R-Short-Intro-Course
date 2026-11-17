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
library(magrittr)               # pipe operator %<>% and functional helpers
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
    mutate(p.value = formatC(p.value, format = "f", digits = 8))

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
confint(ols_reg, level = 0.95)

# Fitted values and residuals
head(fitted(ols_reg))
head(residuals(ols_reg))

# Predictions -> returns the predicted hourly wages for the first six individuals in the dataset, same as fitted if not used in new data
head(predict(ols_reg))

# Interpretation:
# educ: expected avg wage change for one additional year of education,
#       holding other regressors constant (c.p.).
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
model_hc0 = econ_lm(
    wage ~ educ + exper + nonwhite + female + married,
    data = wages,
    vcov = "HC0"
)

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

model_hc4 = econ_lm(
    wage ~ educ + exper + nonwhite + female + married,
    data = wages,
    vcov = "HC4"
)

model_hc4m = econ_lm(
    wage ~ educ + exper + nonwhite + female + married,
    data = wages,
    vcov = "HC4m"
)

model_hc5 = econ_lm(
    wage ~ educ + exper + nonwhite + female + married,
    data = wages,
    vcov = "HC5"
)

# Compare standard errors
modelsummary(
    list(
        "Classical" = model_classical,
        "HC0" = model_hc0,
        "HC1" = model_hc1,
        "HC2" = model_hc2,
        "HC3" = model_hc3,
        "HC4" = model_hc4,
        "HC4m" = model_hc4m,
        "HC5" = model_hc5
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
    main = "Residuals vs. Fitted Values",
    abline(h = 0, col = "red", lty = 2)
)

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

wages %<>% 
    mutate(lwage = log(wage))

model_log = econ_lm(
    lwage ~ educ + exper + nonwhite + female + married,
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

wages %<>%
    mutate(experXfemale = exper * female)

model_interaction1 = econ_lm(
    wage ~ educ + exper + female + experXfemale,
    data = wages,
    vcov = "HC1"
)

model_interaction2 = econ_lm(
    wage ~ educ + exper * female,
    data = wages,
    vcov = "HC1"
)

summary(model_interaction1)
summary(model_interaction2)

tidy(model_interaction1)%>%
    mutate(p.value = formatC(p.value, format = "f", digits = 8))

# exper * female expands to:
# exper + female + exper:female
#
# For female = 0:
#   marginal effect of exper = beta_exper
#
# For female = 1:
#   marginal effect of exper = beta_exper + beta_interaction

coef(model_interaction2)["exper"]

coef(model_interaction2)["exper"] +
    coef(model_interaction2)["exper:female"]


###### 7. Linear Hypothesis Tests ####

# H0: The interaction coefficient is zero.
# The experience slope is equal across groups.

car::linearHypothesis(
    model_interaction2,
    "exper:female = 0",
    vcov. = vcov(model_interaction2),
    test = "F"
)

# H0: Both the female intercept difference
# and the interaction coefficient are zero.

car::linearHypothesis(
    model_interaction2,
    c(
        "female = 0",
        "exper:female = 0"
    ),
    vcov. = vcov(model_interaction2),
    test = "F"
)

# H0: The experience slope for women is zero.
# beta_exper + beta_interaction = 0

car::linearHypothesis(
    model_interaction2,
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
    model_interaction2,
    newdata = new_workers
)

new_workers

# These are conditional predictions, not causal effects.


###### 10. Comparing Model Specifications ####

modelsummary(
    list(
        "Baseline OLS" = model_classical,
        "Robust OLS" = model_hc1,
        "Interaction" = model_interaction1
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

MROZ %<>% 
  mutate(sqr_age = age^2)

Logit = econ_binary(
    inlf ~ nwifeinc + educ + exper + age + sqr_age + kidslt6,
    link = "logit",
    vcov = "HC1",
    data = MROZ
)

# EconR regression summary
summary(Logit)

# Tidy coefficient table
tidy(Logit)%>%
    mutate(p.value = formatC(p.value, format = "f", digits = 8))

# Publication-style regression table
modelsummary(
    list("First<br>Specification" = Logit),
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
# of labor force participation by beta_educ units,
# holding other variables constant.
#
# Positive beta -> higher log-odds, odds, and probability.
# Negative beta -> lower log-odds, odds, and probability.
#
# Exponentiating beta gives the odds ratio (exp(beta)),
# representing the multiplicative change in odds.
#
# Neither beta nor the odds ratio directly represents
# a change in probability.
#
# The marginal effect on probability is not constant;
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

# In a logit model, coefficients represent changes in the log-odds of an outcome,
# holding other variables constant. Exponentiating a coefficient gives the odds
# ratio (OR), which measures the multiplicative change in the odds for a one-unit
# increase in the explanatory variable. For example, an OR of 1.20 indicates
# 20% higher odds, NOT a 20% higher probability. Predicted probabilities instead
# express the estimated likelihood of the outcome (between 0 and 1). Marginal
# effects (ME) measure how these probabilities change with an explanatory variable,
# expressed in percentage points when multiplied by 100. Since logit models are
# nonlinear, marginal effects vary across individuals depending on their predicted
# probabilities. The average marginal effect (AME) summarizes these individual
# effects across the sample. For continuous variables, marginal effects are
# derivatives; for binary variables, discrete probability changes are preferred.
# Unlike odds ratios, marginal effects can also be compared more directly across
# logit and probit models because they are expressed on the same probability scale.

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

# Fitted probabilities for the first six observations (same as fitted since data is the same!)
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
    predicted_probability = predict(Logit, type = "response")) %>%
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




#### 6. Multinomial Logit ####

# We simulate commuting choices for 1,500 individuals.
#
# Each individual chooses one of three transportation modes:
#
# 1. Bicycle
# 2. Public Transport
# 3. Car
#
# We assume that commuting decisions depend on:
# - Distance to work (km)
# - Monthly income (in thousands of euros)
# - Daily parking costs (euros)
# - Quality of public transport (scale from 1 to 5)
# - Rainy weather (0 = no, 1 = yes)
#
# The dependent variable is nominal:
# There is no natural ranking between the alternatives.
#
# Unlike ordered logit, multinomial logit does not assume
# that the outcome categories have an ordering.


###### 1. Simulate the Dataset ######

set.seed(2026)

n = 1500

commuters = data.frame(
    distance = round(runif(n, 1, 25), 1),
    income = round(rlnorm(n, log(2.8), 0.35), 2),
    parking = round(runif(n, 0, 12), 2),
    transit_quality = sample(
        1:5, n, replace = TRUE
    ),
    rain = rbinom(n, 1, 0.35)
)

# Define the systematic utilities.
#
# Bicycle is the reference alternative:
# V_bicycle = 0
#
# Public Transport and Car utilities are specified
# relative to Bicycle.
#
# These coefficients are chosen for simulation purposes.
# They are not estimates from real commuting data.

V_bicycle = rep(0, n)

V_transit = -0.8 +
    0.12 * commuters$distance +
    0.10 * commuters$income +
    0.45 * commuters$transit_quality +
    0.80 * commuters$rain

V_car = 0.2 +
    0.18 * commuters$distance +
    0.35 * commuters$income -
    0.20 * commuters$parking +
    0.40 * commuters$rain

# Convert utilities into choice probabilities
# using the multinomial logit formula.
#
# P(Y = j) = exp(V_j) / sum_k exp(V_k)
#
# For numerical stability, subtract the maximum
# utility from each row before exponentiating.

utilities = cbind(
    Bicycle = V_bicycle,
    Transit = V_transit,
    Car = V_car
)

utilities = utilities -
    apply(utilities, 1, max)

exp_utilities = exp(utilities)

choice_probs = exp_utilities /
    rowSums(exp_utilities)

# Verify that probabilities sum to one
head(rowSums(choice_probs))

# Simulate actual transportation choices
# according to the individual probabilities.

alternatives = c("Bicycle", "Transit", "Car")

commuters$mode = factor(
    vapply(
        seq_len(n),
        function(i) {
            sample(
                alternatives,
                size = 1,
                prob = choice_probs[i, ]
            )
        },
        character(1)
    ),
    levels = alternatives
)

# Inspect the dataset
head(commuters)
str(commuters)
summary(commuters)

# Number of observations
nrow(commuters)

# Distribution of commuting choices
table(commuters$mode)

# Relative frequencies
prop.table(table(commuters$mode))

# IMPORTANT:
# The first factor level, Bicycle, is the reference
# category in the EconR multinomial logit model.


###### 2. Exploratory Data Analysis ######

# Distribution of commuting choices
ggplot(commuters, aes(x = mode)) +
    geom_bar() +
    labs(
        title = "Commuting Transportation Choices",
        x = "Transportation Mode",
        y = "Number of Commuters"
    ) +
    theme_minimal()

# Descriptive statistics by transportation mode
commuters %>%
    group_by(mode) %>%
    summarise(
        observations = n(),
        mean_distance = mean(distance),
        mean_income = mean(income),
        mean_parking = mean(parking),
        mean_transit_quality = mean(transit_quality),
        rain_share = mean(rain),
        .groups = "drop"
    )

# Distance distribution by transportation mode
ggplot(
    commuters,
    aes(x = mode, y = distance, fill = mode)
) +
    geom_boxplot(alpha = 0.6) +
    labs(
        title = "Commuting Distance by Transportation Mode",
        x = "Transportation Mode",
        y = "Distance to Work (km)"
    ) +
    theme_minimal() +
    theme(legend.position = "none")

# Discussion:
# Are bicycle commuters traveling shorter distances?
# Do car commuters have higher incomes?
# Does better public transportation encourage transit use?
# How might parking costs influence commuting choices?


###### 3. Estimate a Multinomial Logit Model ######

MNL = econ_multinomial(
    mode ~ distance + income + parking +
        transit_quality + rain,
    data = commuters,
    link = "logit",
    vcov = "HC1"
)

# EconR regression summary
summary(MNL)

# Tidy coefficient table
tidy(MNL) %>%
    mutate(
        p.value = formatC(
            p.value,
            format = "f",
            digits = 8
        )
    )

# Publication-style regression table
modelsummary(
    MNL,
    shape = term + response ~ model,
    stars = c(
        "*" = 0.10,
        "**" = 0.05,
        "***" = 0.01
    ),
    title = "Multinomial Logit: Commuting Choices"
)

# The multinomial logit estimates separate coefficient
# vectors for each non-reference outcome.
#
# With three alternatives and Bicycle as reference:
#
# log[P(Transit) / P(Bicycle)] = X'beta_Transit
#
# log[P(Car) / P(Bicycle)] = X'beta_Car
#
# We estimate two sets of coefficients.
#
# IMPORTANT:
# Coefficients are expressed in log relative probabilities,
# not direct changes in choice probabilities.


###### 4. Understanding the Coefficients ######

# Extract all coefficients
coef(MNL)

# Distance coefficients for Transit and Car
coef(MNL)[
    c("Transit", "Car"),
    "distance"
]

# Individual coefficients
coef(MNL)["Transit", "distance"]
coef(MNL)["Car", "distance"]

# Interpretation:
#
# A positive coefficient for Car:distance means
# that an additional kilometer is associated with
# a higher log probability ratio of choosing Car
# rather than Bicycle, holding other variables constant.
#
# A negative coefficient means the corresponding
# probability ratio decreases.
#
# Coefficients are alternative-specific.
#
# For instance:
# distance can increase the relative probability
# of both Car and Transit compared with Bicycle.
#
# However, the magnitude of the coefficients does
# not directly tell us the change in probability.


###### 5. Relative Risk Ratios ######

# Exponentiate coefficients
exp(coef(MNL))

# These are Relative Risk Ratios (RRRs).
# It tells us how a one-unit increase in an explanatory variable changes the relative probability
# of choosing one outcome over a reference outcome in a multinomial logit model
#
# For alternative j relative to Bicycle:
#
# RRR = exp(beta_j)
#
# RRR > 1:
# The probability ratio P(j) / P(Bicycle) increases.
#
# RRR < 1:
# The probability ratio P(j) / P(Bicycle) decreases.
#
# RRR = 1:
# No association with that relative probability ratio.

# Example: distance and Car versus Bicycle
exp(coef(MNL)["Car", "distance"])

# Percentage change in the relative probability ratio
100 * (exp(coef(MNL)["Car", "distance"]) - 1)

# Interpretation:
#
# For each additional kilometer, the ratio of the
# probability of choosing Car to the probability
# of choosing Bicycle changes by this percentage,
# holding all other regressors constant.
#
# IMPORTANT:
# An RRR of 1.20 implies a 20% increase in
# P(Car) / P(Bicycle).
#
# It does NOT imply a 20% increase in P(Car).
#
# Relative risk ratios are not probability ratios
# across different individuals.


###### 5.1. Log-Odds and Relative Risk Ratios ######

# Multinomial logit coefficients represent log-odds
# of choosing an alternative relative to the
# reference category.
#
# Here, Bicycle is the reference category.
#
# log[P(Car) / P(Bicycle)] = X'beta_Car
#
# log[P(Transit) / P(Bicycle)] = X'beta_Transit
#
# A one-unit increase in x changes the respective
# log-odds by beta_j, holding other variables constant.


# Example: distance coefficient for Car
beta_distance_car = coef(MNL)["Car", "distance"]
beta_distance_car

# Relative risk ratio
rrr_distance_car = exp(beta_distance_car)
rrr_distance_car

# Percentage change in the relative probability ratio
100 * (rrr_distance_car - 1)

# Interpretation:
#
# An additional kilometer changes the log-odds
# of choosing Car rather than Bicycle by beta.
#
# Exponentiating beta gives the multiplicative
# change in P(Car) / P(Bicycle).
#
# For example:
# beta = 0.20
# RRR = exp(0.20) = 1.221
#
# This corresponds to approximately 22.1% higher
# relative probability of Car versus Bicycle.


###### 5.2. Log-Odds Between Two Non-Reference Categories ######

# We can also calculate the relative probability
# of choosing Car rather than Transit.
#
# log[P(Car) / P(Transit)]
# = X'(beta_Car - beta_Transit)

# Distance coefficients
beta_car = coef(MNL)["Car", "distance"]
beta_transit = coef(MNL)["Transit", "distance"]

# Difference in log-odds coefficients
log_odds_difference = beta_car - beta_transit
log_odds_difference

# Corresponding relative risk ratio
exp(log_odds_difference)

# Interpretation:
#
# The effect of distance on the Car-versus-Transit
# log-odds equals the difference between the
# corresponding coefficients.
#
# We do not need to change the reference category
# or re-estimate the model.


###### 5.3. Predicted Log-Odds and Probabilities ######

# Consider one hypothetical commuter
new_person = data.frame(
    distance = 10,
    income = 3,
    parking = 5,
    transit_quality = 3,
    rain = 0
)

# Predicted probabilities
p = predict(
    MNL,
    newdata = new_person,
    type = "probs"
)

p

# Log-odds: Car versus Bicycle
log(p["Car"] / p["Bicycle"])

# Odds: Car versus Bicycle
p["Car"] / p["Bicycle"]

# Log-odds: Transit versus Bicycle
log(p["Transit"] / p["Bicycle"])

# Log-odds: Car versus Transit
log(p["Car"] / p["Transit"])

# IMPORTANT:
#
# Log-odds may take any real value.
#
# Odds and relative risk ratios are positive.
#
# Predicted probabilities lie between 0 and 1.
#
# These quantities describe different aspects
# of the same multinomial logit model.


###### 6. Comparing Two Non-Reference Alternatives ######

# Although Bicycle is the reference category,
# we can also compare Car directly with Transit.
#
# The log relative probability is:
#
# log[P(Car) / P(Transit)]
# = X'(beta_Car - beta_Transit)
#
# Therefore, the effect of distance on the
# Car-versus-Transit log probability ratio is
# the difference between their coefficients.

beta_car_distance = coef(MNL)["Car", "distance"]

beta_transit_distance = coef(MNL)["Transit", "distance"]

# Difference in coefficients
beta_car_distance - beta_transit_distance

# Relative risk ratio: Car versus Transit
exp(beta_car_distance - beta_transit_distance)

# Interpretation:
# This RRR describes how an additional kilometer
# changes the relative probability of choosing
# Car rather than Transit.
#
# We do not need to re-estimate the model using
# a different reference category.


###### 7. Modelsummary: Relative Risk Ratios ######

# Standard multinomial logit coefficients
modelsummary(
    MNL,
    shape = response + term ~ model,
    title = "Commuting Choices: Log Relative Probabilities",
    stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
    gof_map = c("nobs", "aic", "bic")
)

# Exponentiated coefficients
modelsummary(
    MNL,
    shape = response + term ~ model,
    exponentiate = TRUE,
    title = "Commuting Choices: Relative Risk Ratios",
    stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
    gof_map = c("nobs", "aic", "bic")
)

# With exponentiate = TRUE:
# - Coefficients become relative risk ratios.
# - The null hypothesis is RRR = 1.
# - Statistical inference is based on the
#   corresponding original coefficient tests.
#
# As with binary logit, exponentiation does not
# produce marginal probability effects.


###### 8. Predicted Choice Probabilities ######

# Predict probabilities for all alternatives
predicted_probs = predict(
    MNL,
    type = "probs"
)

head(predicted_probs)

# Every individual receives three probabilities:
# P(Bicycle), P(Transit), P(Car).
#
# Example:
#
# Bicycle  Transit  Car
# 0.20     0.50     0.30
#
# Interpretation:
# 20% probability of Bicycle
# 50% probability of Transit
# 30% probability of Car

# Verify that probabilities sum to one
head(rowSums(predicted_probs))

# Average predicted probabilities
colMeans(predicted_probs)

# Compare with actual sample shares
prop.table(table(commuters$mode))

# For an unpenalized multinomial logit with
# alternative-specific intercepts, the sample
# average fitted probabilities should match
# observed shares up to numerical tolerance.


###### 9. Predict the Most Likely Transportation Mode ######

# Predict the category with the highest probability
predicted_mode = predict(
    MNL,
    type = "class"
)

head(predicted_mode)

# Confusion matrix
table(
    Actual = commuters$mode,
    Predicted = predicted_mode
)

# In-sample classification accuracy
mean(predicted_mode == commuters$mode)

# Compare with a simple baseline that always
# predicts the most common transportation mode
max(prop.table(table(commuters$mode)))

# Important:
# Classification accuracy is not a complete measure
# of model quality.
#
# The multinomial logit primarily models probabilities.
#
# The most likely category may still have a predicted
# probability substantially below 50%.
#
# These are in-sample predictions and should not
# be interpreted as out-of-sample performance.


###### 10. Predictions for Hypothetical Individuals ######

# Consider three commuters with different distances,
# but otherwise identical characteristics.

new_commuters = data.frame(
    distance = c(2, 10, 22),
    income = c(3, 3, 3),
    parking = c(5, 5, 5),
    transit_quality = c(3, 3, 3),
    rain = c(0, 0, 0)
)

# Predicted choice probabilities
new_probs = predict(
    MNL,
    newdata = new_commuters,
    type = "probs"
)

cbind(new_commuters, new_probs)

# Discussion:
# How does the probability of cycling change
# as commuting distance increases?
#
# Does the probability of taking a car increase?
#
# Which transportation mode is most likely
# for a commuter traveling 22 km?
#
# These are model-implied conditional probabilities,
# not necessarily causal predictions.


###### 11. Marginal Effects ######

# In multinomial logit, the marginal effect of
# a continuous variable x on the probability
# of alternative j is:
#
# dP_j/dx = P_j * (beta_j - sum_k P_k * beta_k)
#
# The reference category has beta = 0.
#
# IMPORTANT:
# A regressor affects all choice probabilities,
# even when its coefficient for some alternative is zero.

# Example: marginal effect of distance
# for every individual and every alternative

beta_distance = c(
    Bicycle = 0,
    Transit = coef(MNL)["Transit", "distance"],
    Car = coef(MNL)["Car", "distance"]
)

# Remove any inherited names from scalar extraction
beta_distance = unname(beta_distance)
names(beta_distance) = alternatives

# Align coefficients with the probability columns
beta_distance = beta_distance[colnames(predicted_probs)]

# Probability-weighted coefficient for each individual
weighted_beta = as.vector(
    predicted_probs %*% beta_distance
)

# Individual marginal effects
me_distance = sweep(
    predicted_probs,
    2,
    beta_distance,
    "*"
) - predicted_probs * weighted_beta

head(me_distance)

# Average marginal effects (AMEs)
ame_distance = colMeans(me_distance)

ame_distance

# Percentage-point interpretation
100 * ame_distance

# Verify that marginal effects sum to zero
sum(ame_distance)

# Interpretation:
#
# If AME for Bicycle = -0.025:
# An additional kilometer is associated with an
# approximate 2.5 percentage-point decrease in
# the probability of cycling, on average.
#
# All marginal effects across the three categories
# must sum to zero.
#
# These are point estimates of AMEs.
# This calculation does not provide standard errors.
#
# For a discrete variable such as rain, compare
# predicted probabilities at rain = 0 and rain = 1
# instead of interpreting a derivative.


###### 12. Counterfactual: Increasing Parking Costs ######

# Policy question:
# What happens to transportation choices if
# daily parking costs increase by 5 euros?
#
# We hold other observed characteristics fixed
# and use the estimated model to predict outcomes.

# Baseline predictions
baseline_probs = predict(
    MNL,
    newdata = commuters,
    type = "probs"
)

# Counterfactual dataset
commuters_policy = commuters

commuters_policy$parking =
    commuters_policy$parking + 5

# Counterfactual probabilities
policy_probs = predict(
    MNL,
    newdata = commuters_policy,
    type = "probs"
)

# Average predicted probabilities before policy
colMeans(baseline_probs)

# Average predicted probabilities after policy
colMeans(policy_probs)

# Average probability changes
policy_effect = colMeans(
    policy_probs - baseline_probs
)

# Express in percentage points
100 * policy_effect

# Verify that changes sum to zero
sum(policy_effect)

# Interpretation:
#
# The model predicts how an increase in parking
# costs changes the distribution of commuting choices.
#
# Increasing parking costs may:
# - Reduce the probability of choosing Car
# - Increase the probability of choosing Transit
# - Increase the probability of choosing Bicycle
#
# The probability changes sum to zero.
#
# IMPORTANT:
# This is a model-based counterfactual.
# It does not establish a causal policy effect.
#
# A causal interpretation would require additional
# identification assumptions.


###### 13. Visualizing the Parking-Cost Scenario ######

# Compare baseline and counterfactual choice shares

policy_comparison = data.frame(
    mode = rep(
        colnames(baseline_probs),
        times = 2
    ),
    scenario = rep(
        c("Baseline", "Parking +5 EUR"),
        each = ncol(baseline_probs)
    ),
    probability = c(
        colMeans(baseline_probs),
        colMeans(policy_probs)
    )
)

ggplot(
    policy_comparison,
    aes(
        x = mode,
        y = probability,
        fill = scenario
    )
) +
    geom_col(
        position = "dodge",
        width = 0.7
    ) +
    scale_y_continuous(
        labels = scales::percent
    ) +
    labs(
        title = "Predicted Commuting Choices",
        subtitle = "Baseline vs. Higher Parking Costs",
        x = "Transportation Mode",
        y = "Average Predicted Probability",
        fill = "Scenario"
    ) +
    theme_minimal()


###### 14. Visualize Probabilities Across Distance ######

# Construct hypothetical commuting profiles
distance_profiles = data.frame(
    distance = seq(1, 25, by = 1),
    income = median(commuters$income),
    parking = median(commuters$parking),
    transit_quality = 3,
    rain = 0
)

# Predict choice probabilities
distance_probs = as.data.frame(
    predict(
        MNL,
        newdata = distance_profiles,
        type = "probs"
    )
)

distance_probs$distance = distance_profiles$distance

# Convert from wide to long format
distance_probs_long = tidyr::pivot_longer(
    distance_probs,
    cols = -distance,
    names_to = "mode",
    values_to = "probability"
)

# Plot predicted probabilities
ggplot(
    distance_probs_long,
    aes(
        x = distance,
        y = probability,
        color = mode
    )
) +
    geom_line(linewidth = 1) +
    scale_y_continuous(
        labels = scales::percent
    ) +
    labs(
        title = "Transportation Choice by Commuting Distance",
        x = "Distance to Work (km)",
        y = "Predicted Probability",
        color = "Transportation Mode"
    ) +
    theme_minimal()

# Discussion:
#
# At which distance does cycling become less likely?
#
# Which transportation mode dominates at long distances?
#
# Do predicted probabilities change linearly?
#
# Why must the three probabilities sum to one?



###### 15. Hypothesis Testing ######

# Multinomial logit coefficients are stored as a matrix.
# car::linearHypothesis() expects a coefficient vector.
#
# We construct a named coefficient vector that matches
# the ordering of EconR's variance-covariance matrix.

# HC1 robust covariance matrix
V = vcov(MNL)

# Flatten the coefficient matrix row by row
b = as.vector(t(coef(MNL)))

# Assign the parameter names from the covariance matrix
names(b) = colnames(V)

# Verify coefficient and covariance alignment
stopifnot(
    identical(names(b), colnames(V)),
    identical(rownames(V), colnames(V)),
    length(b) == ncol(V)
)

# Inspect the coefficient vector
b


###### 15.1. Joint Significance of Distance ######

# H0: Distance has no effect on the relative
# probabilities of Transit or Car versus Bicycle.
#
# H0:
# beta_Transit,distance = 0
# beta_Car,distance = 0
#
# H1: At least one coefficient differs from zero.
#
# Two restrictions imply a chi-square test with 2 df.

# Construct the restriction matrix
R1 = matrix(
    0,
    nrow = 2,
    ncol = length(b),
    dimnames = list(
        c("Transit distance", "Car distance"),
        names(b)
    )
)

R1[1, "Transit:distance"] = 1
R1[2, "Car:distance"] = 1

# Joint robust Wald test
car::linearHypothesis(
    MNL,
    hypothesis.matrix = R1,
    coef. = b,
    vcov. = V,
    test = "Chisq"
)

# Interpretation:
#
# If p < 0.05, reject H0.
#
# Distance contributes significantly to explaining
# the relative probabilities of the transportation
# alternatives.
#
# If p >= 0.05, we fail to reject H0.
#
# This does not prove that distance has no effect.


###### 15.2. Equality of Distance Coefficients ######

# H0: The distance coefficients for Car and
# Transit are equal.
#
# beta_Car,distance - beta_Transit,distance = 0
#
# Equivalently:
#
# H0: An additional kilometer does not change
# the Car-versus-Transit relative probability ratio.
#
# One restriction implies chi-square with 1 df.

# Construct the restriction matrix
R2 = matrix(
    0,
    nrow = 1,
    ncol = length(b),
    dimnames = list(
        "Car minus Transit distance",
        names(b)
    )
)

R2[1, "Car:distance"] = 1
R2[1, "Transit:distance"] = -1

# Robust Wald test
car::linearHypothesis(
    MNL,
    hypothesis.matrix = R2,
    coef. = b,
    vcov. = V,
    test = "Chisq"
)

# Interpretation:
#
# If p < 0.05, reject H0.
#
# Distance has statistically different effects
# on the relative probabilities of Car and Transit.
#
# If p >= 0.05, the estimated difference is
# not statistically significant at the 5% level.
#
# IMPORTANT:
# The second test is NOT equivalent to testing
# both coefficients separately against zero.
#
# It tests their difference and accounts for
# the covariance between the estimates.


###### 16. Compare Model Specifications ######

# Model 1: Commuting distance only
mnl1 = econ_multinomial(
    mode ~ distance,
    data = commuters,
    link = "logit",
    vcov = "HC1"
)

# Model 2: Distance and income
mnl2 = econ_multinomial(
    mode ~ distance + income,
    data = commuters,
    link = "logit",
    vcov = "HC1"
)

# Model 3: Full specification
mnl3 = econ_multinomial(
    mode ~ distance + income + parking +
        transit_quality + rain,
    data = commuters,
    link = "logit",
    vcov = "HC1"
)

# Compare regression models
modelsummary(
    list(
        "(1) Distance" = mnl1,
        "(2) Income" = mnl2,
        "(3) Full Model" = mnl3
    ),
    shape = term + response ~ model,
    stars = c(
        "*" = 0.10,
        "**" = 0.05,
        "***" = 0.01
    ),
    gof_map = c(
        "nobs", "logLik", "aic", "bic"
    ),
    title = "Multinomial Logit: Model Comparison"
)

# Compare information criteria
AIC(mnl1, mnl2, mnl3)

BIC(mnl1, mnl2, mnl3)

# Compare:
#
# Does including income improve the model fit?
#
# How does adding parking and transit quality
# affect the estimated coefficients?
#
# Which variables are statistically significant?
#
# Which specification has the lowest AIC or BIC?
#
# IMPORTANT:
# Information criteria compare model fit while
# penalizing model complexity.
#
# AIC and BIC should be compared for models
# estimated on the same observations and outcome.


###### 17. Independence of Irrelevant Alternatives ######

# The multinomial logit relies on the
# Independence of Irrelevant Alternatives (IIA)
# property.
#
# This means that the relative probability of
# choosing alternative A rather than B does not
# depend on the systematic utility of alternative C.
#
# For example:
#
# P(Car) / P(Bicycle) = exp(V_Car - V_Bicycle)
#
# This ratio does not depend on V_Transit.
#
# Suppose a new bus service is introduced.
#
# If it is very similar to the existing public
# transport option, the multinomial logit may
# imply unrealistic substitution patterns.
#
# This is related to the famous red-bus/blue-bus
# problem.
#
# IMPORTANT:
# The IIA property is a restriction of the
# multinomial logit model.
#
# It is not automatically satisfied by real data.
#
# The suitability of IIA depends on whether the
# choice alternatives are sufficiently distinct.


###### 18. Multinomial Logit vs. Binary and Ordered Logit ######

# Binary Logit:
# - Exactly two outcome categories
# - Example: Employed vs. Unemployed
#
# Ordered Logit:
# - Three or more naturally ordered categories
# - Example: Fair, Good, Excellent health
#
# Multinomial Logit:
# - Three or more unordered categories
# - Example: Bicycle, Transit, Car
#
# In multinomial logit:
#
# 1. One category serves as the reference.
#
# 2. Each non-reference category receives its
#    own coefficient vector.
#
# 3. Coefficients describe log relative
#    probabilities.
#
# 4. Exponentiated coefficients produce
#    relative risk ratios.
#
# 5. Marginal effects depend on individual
#    characteristics and the entire set of
#    predicted choice probabilities.
#
# 6. Predicted probabilities across all
#    alternatives sum to one.
#
# 7. The model implies the IIA property.
#
# The model is appropriate when individuals
# choose between mutually exclusive,
# unordered alternatives.


#### 7. Differences betw. logits and probits ####

# Logit models assume a logistic error distribution, whereas probit models
# assume a normal error distribution. In binary regression, both estimate
# probabilities for two outcomes and generally yield similar results, but
# their coefficients differ in scale. In ordered regression, both model
# ordered categories using estimated thresholds, differing in their assumed
# error distributions. In multinomial regression, logit assumes independent
# type-I extreme-value errors, implying the Independence of Irrelevant
# Alternatives (IIA) property, whereas probit assumes jointly normal errors,
# allowing correlation across alternatives and more flexible substitution
# patterns when the error covariance matrix is sufficiently unrestricted.
