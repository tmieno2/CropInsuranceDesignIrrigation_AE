# ===================================
# Set parameters that are likely to stay across simulations
# ===================================
#--------------------------
# discount factor
#--------------------------
sigma <- 1 / (1 + 0.05)

#--------------------------
# Number of knots for Bernstein
#--------------------------
Nk <- 5

#--------------------------
# Price
#--------------------------
#--- mean and variance of the corn price ---#
# do not use ln_var and ln_mean as they are used in premium calculation
# parameters defined here are used for simulating the actual realization
# of harvest price
p_price <- 3.96 # projected price
pvf <- 0.19 # price volatility factor
ln_var_p <- log(pvf^2 + 1)
ln_mean_p <- log(p_price) - ln_var_p / 2
hp_prop <- c(ln_mean_p, sqrt(ln_var_p))

#--- non-N costs (other costs) ---#
other_cost <- 0 # ($/acre)

#--------------------------
# parameters of joint price-yield distribution
#--------------------------
rho <- -0.3 # correlation coefficient
mu <- c(0, 0) # mean
sigma <- matrix(c(1, rho, rho, 1), nrow = 2, ncol = 2) # covariance mat

#--------------------------
# Insurance parameters
#--------------------------
c_rate_now <- county_rate[year == c_year, list(reference_amount, exponent_value, reference_rate, fixed_rate)] %>%
  unlist()

c_rate_prev <- county_rate[year == c_year - 1, list(reference_amount, exponent_value, reference_rate, fixed_rate)] %>%
  unlist()

c_pars <- c(
  rate_differential_factor, py_rate_differential_factor,
  unit_residual_factor, py_unit_residual_factor
)

beta_RP <- c(
  beta_RP_0, beta_RP_1, beta_RP_2, beta_RP_3, beta_RP_4,
  beta_RP_5, beta_RP_6, beta_RP_7, beta_RP_8, beta_RP_9,
  beta_RP_10, beta_RP_11, beta_RP_12, beta_RP_13, beta_RP_14
)

beta_RPHPE <- c(
  beta_RPHPE_0, beta_RPHPE_1, beta_RPHPE_2, beta_RPHPE_3, beta_RPHPE_4,
  beta_RPHPE_5, beta_RPHPE_6, beta_RPHPE_7, beta_RPHPE_8, beta_RPHPE_9,
  beta_RPHPE_10, beta_RPHPE_11, beta_RPHPE_12, beta_RPHPE_13, beta_RPHPE_14
)

sec5 <- c(mean_quantity, sd_quantity)

sec6 <- c(
  capping_reference_yield, py_capping_reference_yield,
  capping_exponent_value, py_capping_exponent_value,
  capping_reference_rate, py_capping_reference_rate,
  capping_fixed_rate, py_capping_fixed_rate,
  capping_year
)

#--------------------------
# Subsidy Percent Lookup
#--------------------------
subsidy_table <- data.table(
  coverage_level = c(0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.90),
  subsidy_percent = c(0.67, 0.64, 0.64, 0.59, 0.59, 0.55, 0.48, 0.38, 0.28)
)

#--------------------------
# Number of years to be used to calculate APH
#--------------------------
n_window <- 10
