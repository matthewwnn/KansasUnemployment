#unemployment research project
rm(list=ls())
setwd("C:/Users/matth/Desktop/ECON 641/Research Project - Unemployment")
set.seed(123)
library(data.table)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(haven)
library(rdrobust)
library(ggiraph)
library(htmltools)
library(htmlwidgets)
library(stargazer)
library(sandwich)
library(lmtest)
library(Synth)

#----------------------------------- clean original data --------------------------------------
#WAVE 1 IMPORT AND CLEAN
sipp2014w1 <- read_sas("pu2014w1.sas7bdat")

sipp2014w1c <- sipp2014w1 %>%
  select(ssuid, PNUM, spanel, swave, monthcode, EUCANY, EUC1MNYN, TUC1AMT, 
         EUC2MNYN, TUC2AMT, EUC3MNYN, TUC3AMT, EJB1_BMONTH, EJB1_EMONTH, 
         EJB1_STRTMON, EJB1_ENDWK, tst_intv, TJB1_JOBHRS3, TJB2_JOBHRS3, 
         TJB3_JOBHRS3, EJB1_CHERMN2, EJB2_CHERMN2, EJB3_CHERMN2, TAGE, 
         ESEX, ERACE, EEDUC, EMS, TNUMKIDS, tjb1_ind, tjb1_occ, EHLTSTAT, 
         TDAYSICK, TSSSAMT)

fwrite(sipp2014w1c, "sipp2014w1c.csv")


#WAVE 2 IMPORT AND CLEAN
sipp2014w2 <- read_sas("pu2014w2.sas7bdat")

sipp2014w2c <- sipp2014w2 %>%
  select(SSUID, PNUM, SPANEL, SWAVE, MONTHCODE, EUCANY, EUC1MNYN, TUC1AMT, 
         EUC2MNYN, TUC2AMT, EUC3MNYN, TUC3AMT, EJB1_BMONTH, EJB1_EMONTH, 
         EJB1_STRTMON, EJB1_ENDWK, TST_INTV, TJB1_JOBHRS3, TJB2_JOBHRS3, 
         TJB3_JOBHRS3, EJB1_CHERMN2, EJB2_CHERMN2, EJB3_CHERMN2, TAGE, 
         ESEX, ERACE, EEDUC, EMS, TNUMKIDS, TJB1_IND, TJB1_OCC, EHLTSTAT, 
         TDAYSICK, TSSSAMT)

fwrite(sipp2014w2c, "sipp2014w2c.csv")


#WAVE 3 IMPORT AND CLEAN
sipp2014w3 <- read_sas("pu2014w3.sas7bdat")

sipp2014w3c <- sipp2014w3 %>%
  select(SSUID, PNUM, SPANEL, SWAVE, MONTHCODE, EUCANY, EUC1MNYN, TUC1AMT, 
         EUC2MNYN, TUC2AMT, EUC3MNYN, TUC3AMT, EJB1_BMONTH, EJB1_EMONTH, 
         EJB1_STRTMON, EJB1_ENDWK, TST_INTV, TJB1_JOBHRS3, TJB2_JOBHRS3, 
         TJB3_JOBHRS3, EJB1_CHERMN2, EJB2_CHERMN2, EJB3_CHERMN2, TAGE, 
         ESEX, ERACE, EEDUC, EMS, TNUMKIDS, TJB1_IND, TJB1_OCC, EHLTSTAT, 
         TDAYSICK, TSSSAMT)

fwrite(sipp2014w3c, "sipp2014w3c.csv")


#WAVE 4 IMPORT AND CLEAN
sipp2014w4 <- read_sas("pu2014w4.sas7bdat")

sipp2014w4c <- sipp2014w4 %>%
  select(SSUID, PNUM, SPANEL, SWAVE, MONTHCODE, EUCANY, EUC1MNYN, TUC1AMT, 
         EUC2MNYN, TUC2AMT, EUC3MNYN, TUC3AMT, EJB1_BMONTH, EJB1_EMONTH, 
         EJB1_STRTMON, EJB1_ENDWK, TST_INTV, TJB1_JOBHRS3, TJB2_JOBHRS3, 
         TJB3_JOBHRS3, EJB1_CHERMN2, EJB2_CHERMN2, EJB3_CHERMN2, TAGE, 
         ESEX, ERACE, EEDUC, EMS, TNUMKIDS, TJB1_IND, TJB1_OCC, EHLTSTAT, 
         TDAYSICK, TSSSAMT)

fwrite(sipp2014w4c, "sipp2014w4c.csv")


#further fixing
sipp2014w1c <- fread("sipp2014w1c.csv")
sipp2014w1c <- sipp2014w1c %>%
  rename(SSUID = ssuid,
         SPANEL = spanel,
         SWAVE = swave,
         MONTHCODE = monthcode,
         TST_INTV = tst_intv,
         TJB1_IND = tjb1_ind,
         TJB1_OCC = tjb1_occ)

fwrite(sipp2014w1c, "sipp2014w1c.csv")

#----------------------------------- further cleaning data & making outcome variables -------------------
#import data & merge to one big panel
sipp2014w1c <- fread("sipp2014w1c.csv")
sipp2014w2c <- fread("sipp2014w2c.csv")
sipp2014w3c <- fread("sipp2014w3c.csv")
sipp2014w4c <- fread("sipp2014w4c.csv")

sipp_panel <- bind_rows(sipp2014w1c, sipp2014w2c, sipp2014w3c, sipp2014w4c) #merge

#fix data format
sipp_panel <- sipp_panel %>%
  mutate(across(
    c(
      PNUM, SPANEL, SWAVE, MONTHCODE, EUCANY, EUC1MNYN, TUC1AMT, 
      EUC2MNYN, TUC2AMT, EUC3MNYN, TUC3AMT, EJB1_BMONTH, EJB1_EMONTH, 
      EJB1_STRTMON, EJB1_ENDWK, TST_INTV, TJB1_JOBHRS3, TJB2_JOBHRS3, 
      TJB3_JOBHRS3, EJB1_CHERMN2, EJB2_CHERMN2, EJB3_CHERMN2, TAGE, 
      ESEX, ERACE, EEDUC, EMS, TNUMKIDS, TJB1_IND, TJB1_OCC, EHLTSTAT, 
      TDAYSICK, TSSSAMT
    ),
    ~ as.numeric(as.character(.))
  ))

sipp_panel <- sipp_panel[EUCANY == 1, ] #only keep people who have ever been unemployed 



#making the outcome variables
# 1. job length (JBLNGTH) is the difference between the month of the last job and the month of the first job
sipp_panel <- sipp_panel %>%
  mutate(JBLNGTH = (EJB1_EMONTH - EJB1_BMONTH))
# 2. total hours worked (TOTHRSWRK) is the sum of hours worked per week across all jobs
sipp_panel <- sipp_panel %>%
  mutate(TOTHRSWRK = rowSums(across(c(TJB1_JOBHRS3, TJB2_JOBHRS3, TJB3_JOBHRS3)), na.rm = TRUE))



#taking out states from the donor pool that amended their shared work programs
sipp_panel <- sipp_panel %>%
  filter(!TST_INTV %in% c(9, 19, 29, 34)) #Conneticut(9), Iowa(19), Missouri(29), New Jersey(34)



#----------------------------------- checking out the data distribution & summary stats----------
#checking out state distribution
hist(sipp_panel$TST_INTV, breaks = 61)

count_states <- function(data, var = "TST_INTV", range = 1:61) {
  for (i in range) {
    count <- sum(data[[var]] == i, na.rm = TRUE)
    cat(sprintf("State code %2d: %d\n", i, count))
  }
}

count_states(sipp_panel)

c("avg_EUC1MNYN", "avg_TUC1AMT","avg_TAGE", "avg_ESEX", "avg_ERACE",
  "avg_EEDUC", "avg_EMS", "avg_TJB1_IND", "avg_TJB1_OCC",
  "avg_EHLTSTAT")

#summary stats data & table
sumstatsd <- sipp_panel %>%
  select(MONTHCODE, EUCANY, EUC1MNYN, TUC1AMT, TAGE, ESEX, ERACE,  
         EEDUC, EMS, EHLTSTAT, JBLNGTH, TJB1_JOBHRS3, 
         TJB2_JOBHRS3, TJB3_JOBHRS3, TOTHRSWRK, TST_INTV) 

#summary stats table
stargazer(
  sumstatsd,
  type = "latex",
  title = "Summary Statistics",
  covariate.labels = c(
    "Month Code",
    "Recieved UC",
    "Received UC from Government",
    "Reg. amount of UC from Government",
    "Age",
    "Sex",
    "Race",
    "Education (Bins)",
    "Marital Status",
    "Health Status (1-5)",
    "Job Length (Months)",
    "Hours at Job 1",
    "Hours at Job 2",
    "Hours at Job 3",
    "Total Hours Worked",
    "State Code"
  ),
  summary.stat = c("n", "mean", "sd", "min", "median", "max"),
  out = "summary_stats.tex") 

#----------------------------------- preparing the data for synth GENERAL -----------------------------
#aggregate the data by state and month
sipp_agg <- sipp_panel %>%
  group_by(TST_INTV, MONTHCODE) %>%
  summarise(
    avg_EUCANY        = mean(EUCANY, na.rm = TRUE),
    avg_EUC1MNYN      = mean(EUC1MNYN, na.rm = TRUE),
    avg_TUC1AMT       = mean(TUC1AMT, na.rm = TRUE),
    avg_EUC2MNYN      = mean(EUC2MNYN, na.rm = TRUE),
    avg_TUC2AMT       = mean(TUC2AMT, na.rm = TRUE),
    avg_EUC3MNYN      = mean(EUC3MNYN, na.rm = TRUE),
    avg_TUC3AMT       = mean(TUC3AMT, na.rm = TRUE),
    avg_EJB1_BMONTH   = mean(EJB1_BMONTH, na.rm = TRUE),
    avg_EJB1_EMONTH   = mean(EJB1_EMONTH, na.rm = TRUE),
    avg_EJB1_STRTMON  = mean(EJB1_STRTMON, na.rm = TRUE),
    avg_EJB1_ENDWK    = mean(EJB1_ENDWK, na.rm = TRUE),
    avg_TJB1_JOBHRS3  = mean(TJB1_JOBHRS3, na.rm = TRUE),
    avg_TJB2_JOBHRS3  = mean(TJB2_JOBHRS3, na.rm = TRUE),
    avg_TJB3_JOBHRS3  = mean(TJB3_JOBHRS3, na.rm = TRUE),
    avg_EJB1_CHERMN2  = mean(EJB1_CHERMN2, na.rm = TRUE),
    avg_EJB2_CHERMN2  = mean(EJB2_CHERMN2, na.rm = TRUE),
    avg_EJB3_CHERMN2  = mean(EJB3_CHERMN2, na.rm = TRUE),
    avg_TAGE          = mean(TAGE, na.rm = TRUE),
    avg_ESEX          = mean(ESEX, na.rm = TRUE),
    avg_ERACE         = mean(ERACE, na.rm = TRUE),
    avg_EEDUC         = mean(EEDUC, na.rm = TRUE),
    avg_EMS           = mean(EMS, na.rm = TRUE),
    avg_TNUMKIDS      = mean(TNUMKIDS, na.rm = TRUE),
    avg_TJB1_IND      = mean(TJB1_IND, na.rm = TRUE),
    avg_TJB1_OCC      = mean(TJB1_OCC, na.rm = TRUE),
    avg_EHLTSTAT      = mean(EHLTSTAT, na.rm = TRUE),
    avg_TDAYSICK      = mean(TDAYSICK, na.rm = TRUE),
    avg_TSSSAMT       = mean(TSSSAMT, na.rm = TRUE),
    avg_JBLNGTH       = mean(JBLNGTH, na.rm = TRUE),
    avg_TOTHRSWRK     = mean(TOTHRSWRK, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(TST_INTV))


# #pivot the data to be wide
# sipp_wide <- sipp_agg %>%
#   pivot_wider(
#     names_from = MONTHCODE,
#     values_from = c(
#       avg_EUCANY, avg_EUC1MNYN, avg_TUC1AMT, avg_EUC2MNYN, avg_TUC2AMT,
#       avg_EUC3MNYN, avg_TUC3AMT, avg_EJB1_BMONTH, avg_EJB1_EMONTH,
#       avg_EJB1_STRTMON, avg_EJB1_ENDWK, avg_TJB1_JOBHRS3, avg_TJB2_JOBHRS3,
#       avg_TJB3_JOBHRS3, avg_EJB1_CHERMN2, avg_EJB2_CHERMN2,
#       avg_EJB3_CHERMN2, avg_TAGE, avg_ESEX, avg_ERACE, avg_EEDUC,
#       avg_EMS, avg_TNUMKIDS, avg_TJB1_IND, avg_TJB1_OCC,
#       avg_EHLTSTAT, avg_TDAYSICK, avg_TSSSAMT, avg_JBLNGTH,
#       avg_TOTHRSWRK
#     )
#   )



#characteristics to optimize over
predictorvars <- c("avg_EUC1MNYN", "avg_TUC1AMT","avg_TAGE", "avg_ESEX", "avg_ERACE",
                   "avg_EEDUC", "avg_EMS", "avg_TJB1_IND", "avg_TJB1_OCC",
                   "avg_EHLTSTAT")


#----------------------------------- preparing the data for synth JBLNGTH -----------------------------
#variable for states missing outcome variable (will excluse these in the synth)
problem_units_jblngth <- sipp_agg %>%
  filter(MONTHCODE %in% 1:12) %>%
  group_by(TST_INTV) %>%
  summarise(all_missing_y = any(is.na(avg_JBLNGTH))) %>%
  filter(all_missing_y == TRUE) %>%
  pull(TST_INTV)


#----------------------------TESTS RAN IN ORDER TO FIND THE PROBLEM VARIABLES
# Check which states have all NA values in any predictor during months 1–6
problem_units <- sipp_agg %>%
  filter(MONTHCODE %in% 1:6) %>%
  group_by(TST_INTV) %>%
  summarise(across(all_of(predictorvars), ~ all(is.na(.)))) %>%
  mutate(any_all_na = if_any(all_of(predictorvars), identity)) %>%
  filter(any_all_na == TRUE) %>%
  pull(TST_INTV)



# For each characteristic, count how many states have all NAs
predictor_missing_by_state <- sipp_agg %>%
  filter(MONTHCODE %in% 1:12) %>%
  group_by(TST_INTV) %>%
  summarise(across(all_of(predictorvars), ~ all(is.na(.)))) %>%
  summarise(across(all_of(predictorvars), ~ sum(.)))

# View sorted output
predictor_missing_by_state %>%
  pivot_longer(everything(), names_to = "predictor", values_to = "n_states_all_na") %>%
  arrange(desc(n_states_all_na))


predictor_missing_by_state <- sipp_agg %>%
  filter(MONTHCODE %in% 1:6) %>%
  group_by(TST_INTV) %>%
  summarise(across(all_of(predictorvars), ~ all(is.na(.))))



#--!------------------ JBLNGTH SYNTHETIC CONTROL MATCHING ------------------------
predictor_months1 <- paste0("avg_JBLNGTH_", 1:6)
outcome_months1   <- paste0("avg_JBLNGTH_", 1:12)  # to plot both pre- and post-treatment


#dataprep() function
JBLNGTHsynthdata <- dataprep(
  foo = as.data.frame(sipp_agg),
  
  predictors = predictorvars,     # All predictors across months 1–6
  predictors.op = "mean",         # Will average over time.optimize.ssr
  
  dependent = "avg_JBLNGTH",      # Outcome variable (in long format)
  unit.variable = "TST_INTV",
  time.variable = "MONTHCODE",
  
  treatment.identifier = 20,      # Kansas
  controls.identifier = setdiff(unique(sipp_agg$TST_INTV), c(20,problem_units_jblngth)),
  
  time.predictors.prior = 1:6,    # Pre-treatment months
  time.optimize.ssr = 1:6,        # Pre-treatment months
  time.plot = 1:12                # All months to graph
)

#synth
JBLNGTHscm <- synth(JBLNGTHsynthdata)


#--!------------------ JBLNGTH SYNTHETIC CONTROL MATCHING OUTPUTS ------------------------
#table
synthtable1 <- synth.tab(synth.res = JBLNGTHscm,
                        dataprep.res = JBLNGTHsynthdata,
                        round.digit = 3)

predictor_labels <- c("UC Received from Gov.",
                      "UC Amount from Gov.",
                      "Age",
                      "Sex",
                      "Race",
                      "Education Level",
                      "Marital Status",
                      "Industry Code",
                      "Occupation Code",
                      "Health Status")


#donor weights table latex
stargazer(as.data.frame(synthtable1$tab.w),
          type = "latex",
          title = "Donor Weights (W)",
          summary = FALSE,
          rownames = FALSE,
          out = "donor_weights_JBLNGTH.tex")

#predictor weights table latex
v_weights_df <- as.data.frame(synthtable1$tab.v)
v_weights_df <- v_weights_df %>%
  mutate(Predictor = predictor_labels) %>%
  select(Predictor, everything())  # move label to first column

stargazer(v_weights_df,
          type = "latex",
          title = "Predictor Weights (V)",
          summary = FALSE,
          rownames = FALSE,
          out = "predictor_weights_JBLNGTH.tex")



#path plot
path.plot(synth.res = JBLNGTHscm,
         dataprep.res = JBLNGTHsynthdata,
         Ylab = "Average Job Length (Months)",
         Xlab = "Month",
         Ylim = c(0, 12),
         Legend = c("Kansas", "Synthetic Kansas"),
         Main = "Average Job Length in Kansas vs Synthetic Kansas")



#gap plot
gaps.plot(synth.res = JBLNGTHscm,
          dataprep.res = JBLNGTHsynthdata,
          Ylab = "Average Job Length (Months)",
          Xlab = "Month",
          Ylim = c(-2, 2),
          Main = "Gap in Average Job Length in Kansas vs Synthetic Kansas")



#--!------------------ JBLNGTH SYNTHETIC CONTROL MATCHING PLACEBOS ------------------------
#grabbing all the donor weights
donor_states <- setdiff(unique(sipp_agg$TST_INTV), c(20,problem_units_jblngth))  # exclude Kansas and problem states


#running placebo tests loop
placebo_results <- list()

for (state in donor_states) {
  dp <- dataprep(
    foo = as.data.frame(sipp_agg),
    
    predictors = predictorvars,
    predictors.op = "mean",
    
    dependent = "avg_JBLNGTH",
    unit.variable = "TST_INTV",
    time.variable = "MONTHCODE",
    
    treatment.identifier = state,
    controls.identifier = setdiff(donor_states, state),
    
    time.predictors.prior = 1:6,
    time.optimize.ssr = 1:6,
    time.plot = 1:12
  )
  
  # Run synth() and store results
  sc <- tryCatch({
    synth(dp)
  }, error = function(e) return(NULL))
  
  if (!is.null(sc)) {
    placebo_results[[as.character(state)]] <- list(synth = sc, dataprep = dp)
  }
}




#--!------------------ JBLNGTH SYNTHETIC CONTROL MATCHING PLACEBOS OUTPUTS ------------------------
#gap plot
kansas_gap <- JBLNGTHsynthdata$Y1plot - (JBLNGTHsynthdata$Y0plot %*% JBLNGTHscm$solution.w)
ATE <- mean(kansas_gap[7:12], na.rm = TRUE) #average treatment effect

placebo_gaps <- lapply(placebo_results, function(x) {
  x$dataprep$Y1plot - (x$dataprep$Y0plot %*% x$synth$solution.w)
})


#plottng
gap_df <- tibble(
  Month = 1:12,
  Kansas = as.numeric(kansas_gap)
)


for (state in names(placebo_gaps)) {
  gap_df[[paste0("State_", state)]] <- as.numeric(placebo_gaps[[state]])
}


# pivot long for ggplot so code is less messy
gap_long <- gap_df %>%
  pivot_longer(-Month, names_to = "State", values_to = "Gap")


ggplot(gap_long, aes(x = Month, y = Gap, group = State)) +
  geom_line(size = 0.7, color = "gray") + 
  geom_line(data = gap_long %>% filter(State == "Kansas"), color = "black", size = 0.8) +
  geom_vline(xintercept = 7, linetype = "dotted", color = "black", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.8) +
  labs(title = "Placebo Tests Treatment Effect Gaps v.s. Kansas Effect Gap",
       y = "Gaps (Placebos & Kansas)",
       color = "State") +
  scale_x_continuous(limits=c(1,12), expand = c(0,0), breaks = c(1:12)) + ylim(-4,4) +
  theme_bw()

ggsave("placebo_tests_gap_plot.png")





#--!------------------ JBLNGTH SCM PLACEBOS MSPE -------------------------------------------------
#mspe function
mspe <- function(actual, synthetic) {
  mean((actual - synthetic)^2, na.rm = TRUE)
}

# Kansas outcome and synthetic
Y1_kansas <- JBLNGTHsynthdata$Y1plot
Y0_kansas <- JBLNGTHsynthdata$Y0plot %*% JBLNGTHscm$solution.w

# Define periods
pre_period  <- 1:6
post_period <- 7:12

# Compute Kansas MSPEs
mspe_pre_kansas  <- mspe(Y1_kansas[pre_period], Y0_kansas[pre_period])
mspe_post_kansas <- mspe(Y1_kansas[post_period], Y0_kansas[post_period])
kansas_ratio <- mspe_post_kansas / mspe_pre_kansas


# Compute MSPEs for each placebo state
placebo_mspe_ratios <- sapply(placebo_results, function(res) {
  Y1 <- res$dataprep$Y1plot
  Y0 <- res$dataprep$Y0plot %*% res$synth$solution.w
  
  pre  <- mspe(Y1[pre_period], Y0[pre_period])
  post <- mspe(Y1[post_period], Y0[post_period])
  
  if (pre == 0 || is.na(pre)) return(NA)
  post / pre
})


#combine all and compare
all_ratios <- c(Kansas = kansas_ratio, placebo_mspe_ratios)
ranked <- sort(all_ratios, decreasing = TRUE)

kansas_rank <- which(names(ranked) == "Kansas")
total_units <- length(ranked)

quasi_pvalue <- (kansas_rank) / (total_units) #0.044 so its significant to the 5% level 



#graphing it
mspe_df <- data.frame(
  State = names(all_ratios),
  MSPE_Ratio = as.numeric(all_ratios)) %>%
  mutate(Highlight = ifelse(State == "Kansas", "Kansas", "Other")) %>%
  arrange(desc(MSPE_Ratio)) %>%
  mutate(Rank = row_number())


ggplot(mspe_df, aes(x = MSPE_Ratio, fill = Highlight)) +
  geom_histogram(binwidth = 0.5, color = "white", alpha = 0.8, position = "identity") +
  scale_fill_manual(values = c("Kansas" = "black", "Other" = "gray")) +
  labs(
    title = "Distribution of MSPE Ratios Placebos v.s. Kansas",
    x = "MSPE Ratio (Post / Pre)",
    y = "Number of States",
    fill = "State") +
  scale_y_continuous(expand = c(0, 0.2)) +
  scale_x_continuous(expand = c(0.03, 0)) +
  theme_bw() + theme(legend.position = "none") 

ggsave("mspe_ratio_histogram.png")


#-FAIL---------------------------------- preparing the data for synth TOTHRSWRK ------------------------
#variable for states missing outcome variable (will excluse these in the synth)
problem_units_TOTHRSWRK <- sipp_agg %>%
  filter(MONTHCODE %in% 1:12) %>%
  group_by(TST_INTV) %>%
  summarise(all_missing_y2 = any(is.na(avg_TOTHRSWRK))) %>%
  filter(all_missing_y2 == TRUE) %>%
  pull(TST_INTV)


#----------------------------TESTS RAN IN ORDER TO FIND THE PROBLEM VARIABLES (same as above. no need to repeat)
# Check which states have all NA values in any predictor during months 1–6
problem_units <- sipp_agg %>%
  filter(MONTHCODE %in% 1:6) %>%
  group_by(TST_INTV) %>%
  summarise(across(all_of(predictorvars), ~ all(is.na(.)))) %>%
  mutate(any_all_na = if_any(all_of(predictorvars), identity)) %>%
  filter(any_all_na == TRUE) %>%
  pull(TST_INTV)



# For each characteristic, count how many states have all NAs
predictor_missing_by_state <- sipp_agg %>%
  filter(MONTHCODE %in% 1:12) %>%
  group_by(TST_INTV) %>%
  summarise(across(all_of(predictorvars), ~ all(is.na(.)))) %>%
  summarise(across(all_of(predictorvars), ~ sum(.)))

# View sorted output
predictor_missing_by_state %>%
  pivot_longer(everything(), names_to = "predictor", values_to = "n_states_all_na") %>%
  arrange(desc(n_states_all_na))


predictor_missing_by_state <- sipp_agg %>%
  filter(MONTHCODE %in% 1:6) %>%
  group_by(TST_INTV) %>%
  summarise(across(all_of(predictorvars), ~ all(is.na(.))))



#-FAIL------------------- TOTHRSWRK SYNTHETIC CONTROL MATCHING ------------------------
predictor_months2 <- paste0("avg_TOTHRSWRK_", 1:6)
outcome_months2   <- paste0("avg_TOTHRSWRK_", 1:12)  # to plot both pre- and post-treatment


#dataprep() function
TOTHRSWRKsynthdata <- dataprep(
  foo = as.data.frame(sipp_agg),
  
  predictors = predictorvars,     # All predictors across months 1–6
  predictors.op = "mean",         # Will average over time.optimize.ssr
  
  dependent = "avg_TOTHRSWRK",      # Outcome variable (in long format)
  unit.variable = "TST_INTV",
  time.variable = "MONTHCODE",
  
  treatment.identifier = 20,      # Kansas
  controls.identifier = setdiff(unique(sipp_agg$TST_INTV), c(20,problem_units_TOTHRSWRK)),
  
  time.predictors.prior = 1:6,    # Pre-treatment months
  time.optimize.ssr = 1:6,        # Pre-treatment months
  time.plot = 1:12                # All months to graph
)

#synth
TOTHRSWRKscm <- synth(TOTHRSWRKsynthdata)


#-FAIL------------------- TOTHRSWRK SYNTHETIC CONTROL MATCHING OUTPUTS ------------------------
#table
synthtable2 <- synth.tab(synth.res = TOTHRSWRKscm,
                         dataprep.res = TOTHRSWRKsynthdata,
                         round.digit = 3)

#donor weights table latex
stargazer(as.data.frame(synthtable2$tab.w),
          type = "latex",
          title = "Donor Weights (W)",
          summary = FALSE,
          rownames = FALSE,
          out = "donor_weights_TOTHRSWRK.tex")

#predictor weights table latex
stargazer(as.data.frame(synthtable2$tab.v),
          type = "latex",
          title = "Predictor Weights (V)",
          summary = FALSE,
          rownames = FALSE,
          out = "predictor_weights_TOTHRSWRK.tex")



#path plot
path.plot(synth.res = TOTHRSWRKscm,
          dataprep.res = TOTHRSWRKsynthdata,
          Ylab = "Average Hours Worked Per week",
          Xlab = "Month",
          Ylim = c(0, .0000005),
          Legend = c("Kansas", "Synthetic Kansas"),
          Main = "Average Hours Worked Per week in Kansas vs Synthetic Kansas")



#gap plot
gaps.plot(synth.res = TOTHRSWRKscm,
          dataprep.res = TOTHRSWRKsynthdata,
          Ylab = "Average Hours Worked Per week",
          Xlab = "Month",
          Ylim = c(-0.0000005, 0.00000005),
          Main = "Gap in Average Hours Worked Per week in Kansas vs Synthetic Kansas")







