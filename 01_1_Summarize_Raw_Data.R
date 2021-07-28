######################################
# Data Preparation
######################################
# written by Taro Mieno

# /*====================================*/
#' # Preparation
# /*====================================*/

#' Load necessary packages
library(tidyverse)
library(R.matlab)
library(here)
library(data.table)

#' import the data
yiedl_water_array <- here("Data/RawData", "AOS_SimOutputs_PS_Mar2019_Synthetic.mat") %>%
  readMat(, ) %>%
  .[[1]]

# /*====================================*/
#' # Extract and transform data by variable
# /*====================================*/
# /*-------------------------*/
#' ## Yield data
# /*-------------------------*/
#+ yield
yield_data <- yiedl_water_array[[1]] %>%
  t() %>%
  data.table() %>%
  .[, smt := 1:nrow(.)] %>%
  melt(id.var = "smt") %>%
  setnames(c("variable", "value"), c("year", "yield")) %>%
  .[, yield := yield / 0.06] %>%
  .[, year := as.numeric(gsub("V", "", year))]

#--- expected crop yield ---#
e_yield <- yield_data[, .(e_yield = mean(yield)), by = smt]

# /*-------------------------*/
#' ## Irrigation data
# /*-------------------------*/
#+ irrigation
irr_data <- yiedl_water_array[[2]] %>%
  t() %>%
  data.table() %>%
  .[, smt := 1:nrow(.)] %>%
  melt(id.var = "smt") %>%
  setnames(c("variable", "value"), c("year", "ir")) %>%
  .[, year := as.numeric(gsub("V", "", year))] %>%
  .[, ir := ir * 0.0393701] %>%
  setkey(smt, year)

#' Find expected irrigation
e_ir <- irr_data[, .(e_ir = mean(ir)), by = smt]

# /*-------------------------*/
#' ## Combine and save
# /*-------------------------*/
#--- raw data ---#
raw_data <- yield_data[irr_data, on = c("smt", "year")]

#--- expected data ---#
e_data <- e_yield[e_ir, on = "smt"]

# /*====================================*/
#' # Find soil moisture targets (SMT) that are on the yield frontier
# /*====================================*/
seq_bin <- seq(min(e_data[, e_ir]), max(e_data[, e_ir]) + 0.1, by = 0.1)
bins_len <- length(seq_bin)

e_data[, bin := 1]
for (i in 1:bins_len) {
  e_data[seq_bin[i] < e_ir & seq_bin[i + 1] > e_ir, bin := i]
}

smt_at_frontier <- e_data[, .SD[e_yield == max(e_yield), ], by = bin] %>%
  unique(by = "e_yield") %>%
  .[, smt]

# /*-------------------------*/
#' ## Extract only the smts at the frontier
# /*-------------------------*/
data_for_sim <- raw_data[smt %in% smt_at_frontier, ]
e_data_for_summary <- e_data[smt %in% smt_at_frontier, ]

# /*=================================================*/
#' # Further remove some economically irrelevant SMTs
# /*=================================================*/
#' There are SMTs that uses more water while producing an almost identical yield distribution.
#' Get rid of such SMTs as they would not be chosen under any circumstances
#' See the **smt_pattern_analysis.R** for the threshold picked

# === get the list of smts to keep ===#
e_ir_threshold <- 15
smt_ls_keep <- e_data_for_summary[e_ir <= 15, smt] %>% unique()

# === filter ===#
data_for_sim <- data_for_sim[smt %in% smt_ls_keep, ]
e_data_for_summary <- e_data_for_summary[smt %in% smt_ls_keep, ]

# /*====================================*/
#' # Save
# /*====================================*/
saveRDS(data_for_sim, here("Data/ProcessedData", "yield_water_data_for_sim.rds"))
saveRDS(e_data_for_summary, here("Data/ProcessedData", "yield_water_data_for_summary.rds"))
