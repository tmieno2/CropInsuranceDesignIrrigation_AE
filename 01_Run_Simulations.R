######################################
# Run simulation for various parameters
######################################

# ===================================
# 0. Preparation
# ===================================
#--------------------------
# 0.1 Packages
#--------------------------
library(data.table)
library(magrittr)
library(Rcpp)
library(RcppArmadillo)
library(mc2d)
library(quadprog)
library(MASS)
library(parallel)
library(kableExtra)
library(mgcv)
library(scam)
library(plotly)

#--------------------------
# 0.2 Sourcing files
#--------------------------
#--- Fixed County parameters ---#
source(here("Codes/00_1_Chase_NE_parameters.R"))

#--- Other parameters ---#
source(here("Codes/00_2_Parameters.R"))

#--- Some Functions ---#
source(here("Codes/00_3_Functions.R"))

#--- Profit Generator ---#
sourceCpp(here("Codes/profit_no_premium_calc.cpp"))
sourceCpp(here("Codes/premium_calc_RP.cpp"))

#--------------------------
# 0.3 Import datasets
#--------------------------
data_for_BI <- here("Data/ProcessedData/yield_water_data_for_sim.rds") %>%
  readRDS() %>%
  .[, yield := round(yield, digits = 1)]

e_data_for_summary <- here("Data/ProcessedData/yield_water_data_for_summary.rds") %>%
  readRDS()

#--------------------------
# 0.4 Seed for simulation
#--------------------------
set.seed(123)

# /*=================================================*/
#' # 1. Simulation Preparation
# /*=================================================*/
# /*----------------------------------*/
#' ## 1.1 Define parameters to loop over
# /*----------------------------------*/
# === list of irrigation costs ===#
irc_ls <- c(6, 10, 14, 18)

# === yield substitution ratio ===#
ys_ratio_ls <- (0:5) / 5

# === Reasonable Irrigation Rule ===#
ri_ls <- c(TRUE, FALSE)

# === ysr and ri ===#
ysr_ri_ls <- 
expand.grid(
  ys_ratio = ys_ratio_ls,
  ri = ri_ls
) %>%
data.table()

# === combinations of all three ===#
ir_ys_ri_ls <- 
expand.grid(
  ir_cost = irc_ls,
  ys_ratio = ys_ratio_ls,
  ri = ri_ls
) %>%
data.table()

# /*----------------------------------*/
#' ## 1.2 Define global parameters
# /*----------------------------------*/
# === AY list ===#
#' minimum AY has to be 1, otherwise NaN produced later
AY_min <- floor(0.8 * t_yield) # yield floor
AY_step <- 3
AY_max <- (1 + ceiling(data_for_BI[, max(yield)] / AY_step)) * AY_step
AY_list_g <- seq(AY_min, AY_max, by = AY_step)

# === coverage (tau) levels ===#
tau_list <- c(0.75, 0.8, 0.85)
tau_len <- length(tau_list)

# === rate yield list ===#
#' minimum RY has to be 1, otherwise NaN produced later
RY_list <- seq(1, AY_max, by = AY_step)
RY_len <- length(RY_list)

# === AY and RY list ===#
#' this is used for profit calculation
AY_RY_ls_g <- expand.grid(AY = AY_list_g, RY = RY_list) %>%
  data.table()
AY_RY_len_g <- nrow(AY_RY_ls_g)

# === AY, RY, and tau list ===#
#' this is used for premium calculation
AY_RY_tau_ls_g <- expand.grid(AY = AY_list_g, RY = RY_list, tau = tau_list) %>%
  data.table()

# === number of weather years ===#
num_year <- data_for_BI[, year] %>%
  unique() %>%
  length()

# === other simulation related parameters ===#
ins_year <- 2017
prod_T <- 30

# /*----------------------------------*/
#' ## 1.3 Base data for t < T
# /*----------------------------------*/
# /*~~~~~~~~~~~~~~~~~~~~~~*/
#' ### introduce yield shock by hail
# /*~~~~~~~~~~~~~~~~~~~~~~*/
# === probability of hail  ===#
damage_prob <- 0.03

# === yields for the first n observations set to 0 ===#
data_for_BI[year <= damage_prob * num_year, yield := 0]

# /*~~~~~~~~~~~~~~~~~~~~~~*/
#' ### Expand on tau
# /*~~~~~~~~~~~~~~~~~~~~~~*/
base_data_d <- data_for_BI[rep(1:nrow(data_for_BI), each = tau_len), ] %>%
  .[, tau := rep(tau_list, nrow(.) / length(tau_list))] %>%
  .[, year := NULL]

# /*----------------------------------*/
#' ## 1.4 Base data for t = T (final period)
# /*----------------------------------*/
# /*~~~~~~~~~~~~~~~~~~~~~~*/
#' ### Harvest price generation
# /*~~~~~~~~~~~~~~~~~~~~~~*/
# === repetitions for each weather year ===#
rep_year <- 100

hp_seq <- qlnorm(runif(rep_year), hp_prop[1], hp_prop[2])

# === expand the data ===#
base_data_s <- base_data_d[rep(1:nrow(base_data_d), each = rep_year), ] %>%
  # === assign harvest price ===#
  .[, hp := rep(hp_seq, nrow(.) / length(hp_seq))] %>%
  # ===  ===#
  setkey(tau)

# /*=================================================*/
#' # 2. Simulation: without insurance
# /*=================================================*/
No_ins_results <- data_for_BI[rep(1:nrow(data_for_BI), each = length(irc_ls)), ] %>%
  .[, ir_cost := rep(irc_ls, nrow(.) / length(irc_ls))] %>%
  .[, u := crra_u(0.6, p_price * yield - ir_cost * ir + 50 * 30)] %>%
  .[, .(u = mean(u)), by = .(smt, ir_cost)] %>%
  .[, .SD[u == max(u)], by = ir_cost] %>%
  e_data_for_summary[., on = "smt"]

saveRDS(No_ins_results, here("Results/Sim_ni.rds"))

e_ir_threshold_data <- No_ins_results[, .(ir_cost, e_ir)]

#/*=================================================*/
#' # 3. Simulation: with insurance
#/*=================================================*/
#/*----------------------------------*/
#' ## 3.1 Find Premium
#/*----------------------------------*/
#' premium is in $1000

if (file.exists(here("Results/premiums_all.rds"))) {

	premiums <- readRDS(here("Results/premiums_all.rds"))

} else {

	premiums <- mclapply(
		1:nrow(AY_RY_tau_ls_g), 
		find_premiums, 
		mc.cores = 12
	) %>%
  rbindlist()

	saveRDS(premiums, here("Results/premiums_all.rds"))

}

#/*----------------------------------*/
#' ## 3.2 Final period simulations
#/*----------------------------------*/
#' the outcome of the final period simulation can be shared across iteration with the same irrigation cost

for (irc in irc_ls) {

	# irc <- 18

  pw_per_acre_inch <- irc

  print(paste0("===== working on irc = ", pw_per_acre_inch, " ====="))

  temp_pi_static_fname <- 
  paste0("Results/pi_static_pw_", pw_per_acre_inch, ".rds") %>% 
  here()

  if (!file.exists(temp_pi_static_fname)) {

    # === if simulation has NOT been completed previously ===#
    source(here("Codes/01_2_FinalPeriodSim.R"))

  } else {

    # === read the data ===#
    temp_pi_static <- readRDS(temp_pi_static_fname)
    num_obs_temp <- nrow(temp_pi_static)

    # === check if there are any missing observations ===#
    num_complete <- nrow(AY_RY_tau_ls_g) * data_for_BI[, length(unique(smt))]

    if (num_obs_temp < num_complete) {

      #' calculation for the missing combinations of AY and RY
      source(here("Codes/01_3_FinalPeriodSim_remaining.R"))

    }

  }

  gc()

}

# /*----------------------------------*/
#' ## 3.3 VF_T gam estimation
# /*----------------------------------*/

for (irc in irc_ls) {

  print(paste0("===== working on irc=", irc, " ====="))

  temp_file <- 
  paste0("Results/VF_T_pw_", irc, ".rds") %>% 
  here()

  if (!file.exists(temp_file)) {

    # === Value function estimation ===#
    source(here("Codes/FinalPeriod_VF_T.R"))

  }

}

#/*----------------------------------*/
#' ## 3.4 Backward Induction (Solve the dynamic optimization problems)
#/*----------------------------------*/

for (irc in irc_ls) {

  # === read the concurrent utility data ===#
  pi_static <- 
  paste0("Results/pi_static_pw_", irc, ".rds") %>% 
  here() %>% 
  readRDS()

  # === read the VF_T gam for the working irrigation cost ===#
  VF_T_gam_ls <- 
  paste0("./Results/VF_T_pw_", irc, ".rds") %>% 
	here() %>% 
	readRDS()

  # m <- 2
  for (m in 1:nrow(ysr_ri_ls)) {

    temp_ysr <- ysr_ri_ls[m, ys_ratio]
    temp_ri <- ysr_ri_ls[m, ri]

    print(paste0("working on irc=", irc, ", ysr=", temp_ysr, " ri=", temp_ri))

    # === get rid of this line after running ===#
    # source('./Codes/BackwardInduction_Faster.R')

    temp_fname <- 
    paste0("Results/opt_ir_V_pw_", irc, "_ysr_", temp_ysr, "_ri_", temp_ri, ".rds") %>% 
    here()

    if (!file.exists(temp_fname)) {

      print(paste("File does not exist. Starting Backward Induction."))

      # === Backward Induction ===#
      source(here("Codes/01_5_BackwardInduction.R"))

    } else {

      temp_results <- readRDS(temp_fname)

      # === check if the results are complete ===#
      all_t <- prod_T - 1 == temp_results[, t] %>%
        unique() %>%
        length()

      consistent_t <- 1 == temp_results[!is.na(u_tot), .N, by = t][, N] %>%
        unique() %>%
        length()

      if (all_t & consistent_t) {

        print(paste("File exists. Skipping this iteration."))

      } else {

        print(paste("File exists, but the results are incomplete. Starting BI."))

        # === Backward Induction ===#
        source(here("Codes/01_5_BackwardInduction.R"))

      }

    } # if ends here

  } # loop over m ends here

} # loop over irc ends here


# /*=================================================*/
#' # 4. Simulation based on the optimal decision making 
# /*=================================================*/

# /*----------------------------------*/
#' ## 4.1 Set up simulations
# /*----------------------------------*/
# === simulation parameters ===#
AY_start <- 200 # starting AY
RY_start <- 200 # starting RY
B <- 200000 # iterations
T <- 50 # number of simulation years
weather_year_ls <- data_for_BI[, year] %>% 
	unique()

# === starting data ===#
starting_data_store <- list()
starting_data_store[["1"]] <- 
data.table(
  sim_id = 1:B,
  AY_start = AY_start,
  RY_start = RY_start,
  period = 1
) %>%
setkey(sim_id, period)

# === weather and hp data ===#
weather_hp_data <- 
data.table(
  sim_id = rep(1:B, T),
  year = sample(weather_year_ls, (T) * B, replace = TRUE),
  hp = qlnorm(runif(B * T), hp_prop[1], hp_prop[2]),
  period = rep(1:T, each = B)
)

# /*----------------------------------*/
#' ## 4.2 Simulations
# /*----------------------------------*/
#' simulate() defined in 00_3_Functions.R

sum_results <- 
mclapply(
	1:nrow(ir_ys_ri_ls), 
	simulate, 
	mc.cores = 12
) %>%
rbindlist()

saveRDS(
	sum_results, 
	here("Results/SimResultsSummaryUpdated.rds")
)

#/*----------------------------------*/
#' ## 4.3 Simulations with trend adjustment (TA)
#/*----------------------------------*/
#' simulate_TA() defined in 00_3_Functions.R

# === list of parameters to loop over ===#
TA_ls <- -1:-4
irc_TA_ls <- expand.grid(irc = irc_ls, TA = TA_ls) %>%
  data.table()
irc_TA_len <- nrow(irc_TA_ls)

sim_TA_results <- 
mclapply(
	1:irc_TA_len, 
	simulate_TA, 
	mc.cores = 12
) %>%
rbindlist()

saveRDS(
	sim_TA_results, 
	here("Results/sim_TA_results.rds")
)

