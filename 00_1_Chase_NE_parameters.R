# Chase County, NE
# Year 2017
# Coverage Level 85%
#==============================
# Exogenous Parameters
#==============================

#------------------------------
#     Section 1
#------------------------------

#-----Projected Price----------
# Vary By year
proj_price <- 3.96
insured_share_percent <- 1 

#--------------------------
# section 2 
#--------------------------
optional_unit_discount_factor <- 1 
unit_structure_discount_factor <- 1

#--------------------------
# section 3
#--------------------------
rate_differential_factor <- 1.960
py_rate_differential_factor <- 1.802
unit_residual_factor <- 1.058
py_unit_residual_factor <- 1.00

#--------------------------
# section 4 
#--------------------------	

additive_optional_rate_adj_factor <- 0
multiplicative_optional_rate_adj_factor <- 1

#--------------------------
# section 5
#--------------------------

revenue_lookup_adj_factor	<- 1

#--- mean quantity and sd quantity ---#
# information available at ADM
mean_quantity <- 99.991316780 
sd_quantity <- 32.112478670

#--- price volatility factor ---#
# vary by year, set nationally
# pvf <- 0.19

#--------------------------
# section 6
#--------------------------
capping_reference_yield <- 170.00
py_capping_reference_yield <- 170.00
capping_exponent_value <- -2.050
py_capping_exponent_value <- -2.050
capping_reference_rate <- 0.0230
py_capping_reference_rate <- 0.0230
capping_fixed_rate <- 0.0150
py_capping_fixed_rate <- 0.0150
capping_year <- 2011 

#--- beta RPHPE ---#

beta_RPHPE_0 <- -0.088127700
beta_RPHPE_1 <- 1.226779000
beta_RPHPE_2 <- -0.425374700
beta_RPHPE_3 <- -0.038819970
beta_RPHPE_4 <- 0.239022700
beta_RPHPE_5 <- 0.067731340
beta_RPHPE_6 <- -0.002104320
beta_RPHPE_7 <- -0.222077000
beta_RPHPE_8 <- 0.375085300
beta_RPHPE_9 <- -0.036970050
beta_RPHPE_10 <- -0.033607650
beta_RPHPE_11 <- -0.180057000
beta_RPHPE_12 <- -0.090663480
beta_RPHPE_13 <- 0.212161100
beta_RPHPE_14 <- -0.001360380	

#--- beta RP ---#

beta_RP_0 <- -0.101368400	
beta_RP_1 <- 1.387394000
beta_RP_2 <- -0.627673000
beta_RP_3 <- -0.037645100
beta_RP_4 <- 0.263585900
beta_RP_5 <- 0.072096450
beta_RP_6 <- 0.001431780
beta_RP_7 <- -0.293226500
beta_RP_8 <- 0.265909600
beta_RP_9 <- -0.230137700
beta_RP_10 <- 0.047034840
beta_RP_11 <- 0.429848500
beta_RP_12 <- -0.108663800
beta_RP_13 <- 0.502257400
beta_RP_14 <- -0.032060080

#--------------------------
# section 9
#--------------------------
# assume fixed
experience_factor <-  1
premium_surcharge_percent <- 1
total_premium_multiplicative_optional_rate_adj_factor <- 1
multiple_commodity_adj_factor <- 1
beg_fr_subsidy_percent <- 0
native_sod_subsidy_amount <- 0

#===================================
# County-specific parameters
#===================================
county_rate <- data.table(
  year=c(2016,2017),
  reference_amount=c(183,186),
  exponent_value = c(-0.657,-0.662),
  reference_rate = c(0.0220,0.0210),
  fixed_rate = c(0.0140,0.0140) 
)

t_yield = 185
