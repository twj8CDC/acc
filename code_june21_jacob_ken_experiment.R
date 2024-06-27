library(MEPS)
library(dplyr)
library(survey)
# install.packages("survey")

# Function to read MEPS data
read_meps_data <- function(year, types) {
  data_list <- list()
  for (type in types) {
    data_list[[type]] <- read_MEPS(year = year, type = type)
  }
  return(data_list)
}

# Function to select conditions related to asthma (ICD-9 code 493)
asthma_conditions <- function(df) {
  if ("ICD9CODX" %in% colnames(df)) {
    df <- df %>% filter(grepl("493", ICD9CODX))
  }
  if ("ICD10CDX" %in% colnames(df)) {
    df <- df %>% filter(grepl("J45", ICD10CDX))
  }
  df %>% mutate(Condition = "Asthma")
}

# Function to merge each event file with the Condition Event Link file and then with the Condition file
merge_conditions <- function(event_df, link_df, condition_df) {
  event_df %>%
    left_join(link_df, by = c("DUPERSID", "EVNTIDX")) %>%
    left_join(condition_df, by = "CONDIDX") %>%
    asthma_conditions()
}

# Function to process data for a single year
process_year <- function(year) {
  data <- read_meps_data(year, c("RX", "DV", "IP", "ER", "OP", "OB", "HH", "COND", "CLNK"))
  
  # Rename column for RX dataset
  data$RX <- data$RX %>% rename(EVNTIDX = "LINKIDX")
  
  # Apply merge_conditions function to the relevant datasets
  event_types <- c("RX", "IP", "ER", "OP", "OB", "HH")
  merged_data <- bind_rows(lapply(event_types, function(type) {
    merge_conditions(data[[type]], data$CLNK, data$COND)
  }))
  
  # Extract unique DUPERSID for asthma-related conditions with the year
  asthma_DUPERSID <- merged_data %>%
    filter(!is.na(Condition)) %>%
    distinct(DUPERSID.y) %>%
    mutate(Year = year) %>%
    rename(DUPERSID = DUPERSID.y)
  
  return(asthma_DUPERSID)
}

# Specify the years of interest
years <- c(2013)

# Process data for all specified years and combine the results
all_years_data <- bind_rows(lapply(years, function(year) {
  process_year(year)
}))

# Function to rename weight variable and adjust weight
rename_and_adjust_weights <- function(fyc, year) {
  weight_var <- paste0("PERWT", substr(year, 3, 4), "F")
  fyc %>%
    rename(PERWTF = !!sym(weight_var)) %>%
    mutate(adjusted_PERWTF = PERWTF / length(unique(fyc$Year)))
}

# Load FYC (Full-Year Consolidated) files, pool data over the specified years, and adjust weights
fyc_data_list <- lapply(years, function(year) {
  fyc <- read_MEPS(year = year, type = "FYC")
  fyc <- fyc %>% mutate(Year = year)
  rename_and_adjust_weights(fyc, year)
})

# Combine the FYC data for all years
fyc_pooled <- bind_rows(fyc_data_list)
rm(fyc_data_list)
# Create the asthma_treated variable by performing a left join with indicator
# maybe there is a better way to do this, will look into it more later.
fyc_pooled <- fyc_pooled %>%
  left_join(all_years_data %>% mutate(asthma_treated = 1), by = c("DUPERSID", "Year")) %>%
  mutate(asthma_treated = ifelse(is.na(asthma_treated), 0, asthma_treated))


condition<-read_MEPS(year=2013, type="COND")
data <- condition %>%
  mutate(
    mi = ifelse(ICD9CODX %in% c("410", "412"), 1, 0),
    chf = ifelse(ICD9CODX %in% c("398", "402", "404", "425", "428"), 1, 0),
    pvd = ifelse(ICD9CODX %in% c("093", "437", "440", "441", "443", "447", "557", "V43"), 1, 0),
    cevd = ifelse(ICD9CODX %in% c("362", "430", "431", "432", "433", "434", "436", "437", "438"), 1, 0),
    dementia = ifelse(ICD9CODX %in% c("290", "294", "331"), 1, 0),    
    cpd = ifelse(ICD9CODX %in% c("416", "490", "491", "492", "494", "495", "496", "497", "498", "499", "500", "501", "502", "503", "504", "505", "506", "508"), 1, 0),    # removed 493
    rheum = ifelse(ICD9CODX %in% c("446", "710", "714", "725"), 1, 0),
    pud = ifelse(ICD9CODX %in% c("531", "532", "533", "534"), 1, 0),    
    mld = ifelse(ICD9CODX %in% c("070", "570", "571", "573", "V42"), 1, 0),
    diab = ifelse(ICD9CODX %in% c("250"), 1, 0),
    diabwc = ifelse(ICD9CODX %in% c("250.4", "250.5", "250.6", "250.7"), 1, 0),
    hp = ifelse(ICD9CODX %in% c("334", "342", "343", "344"), 1, 0),
    rend = ifelse(ICD9CODX %in% c("403", "404", "582", "583", "585", "586", "V42", "V45", "V56"), 1, 0),    
    canc = ifelse(ICD9CODX %in% c("140", "141", "142", "143", "144", "145", "146", "147", "148", "149", "150", "151", "152", "153", "154", "155", "156", "157", "158", "159", "160", "161", "162", "163", "164", "165", "166", "167", "168", "169", "170", "171", "172", "174", "175", "176", "177", "178", "179", "180", "181", "182", "183", "184", "185", "186", "187", "188", "189", "190", "191", "192", "193", "194", "195", "200", "201", "202", "203", "204", "205", "206", "207", "208", "238"), 1, 0),    
    msld = ifelse(ICD9CODX %in% c("456", "572"), 1, 0),
    metacanc = ifelse(ICD9CODX %in% c("196", "197", "198", "199"), 1, 0),
    aids = ifelse(ICD9CODX %in% c("042", "043", "044"), 1, 0)
  ) %>%
  group_by(DUPERSID) %>%
  summarise(
    CCI = sum(mi, chf, pvd, cevd, dementia, rheum, cpd, pud, mld, diab) + 
      2 * sum(diabwc, hp, rend, canc) +
      3 * sum(msld) +
      6 * sum(metacanc, aids),
    mi = max(mi),
    chf = max(chf),
    pvd = max(pvd),
    cevd = max(cevd),
    dementia = max(dementia),
    cpd = max(cpd),
    rheum = max(rheum),
    pud = max(pud),
    mld_liver = max(mld),
    diab = max(diab),
    diabwc = max(diabwc),
    hemiplegia = max(hp),
    rend = max(rend),
    cancer = max(canc),
    ms_liver = max(msld),
    metacanc = max(metacanc),
    aids = max(aids),
    
    .groups = 'drop'
  )


small_fyc<-fyc_pooled%>%
  mutate(
    expend=TOTEXP13,
    age=AGE13X,
    sex=SEX,
    race=RACETHX,
    married=ifelse(MARRY13X==1,1,0),
    income=POVCAT13,
    region=REGION13,
    insurance=case_when(
      MCAID13X == 1 ~ "Medicaid",
      MCARE13X == 1 ~ "Medicare",
      INSCOV13 == 1 ~ "Private",
      INSCOV13 == 3 ~ "Uninsured",
      TRUE~ NA_character_),
    educ=case_when(
      EDUYRDG %in% c(1,2) ~ "LessHS",
      EDUYRDG %in% c(3,4) ~ "HS",
      EDUYRDG %in% c(5,6,7,8) ~ "College",
      EDUYRDG == 9 ~ "Advanced",
      EDUYRDG == 10 ~ "Child5",
      TRUE~ NA_character_)
  ) %>% 
  select(DUPERSID,asthma_treated,VARSTR,PERWTF,expend,age,sex,race,married,income,region,insurance,educ)

small<-small_fyc%>%left_join(data)
small<-small[,-c(15:31)]

small %>% glimpse

write.csv(small, "meps_2013.csv")
library(twopartm)

data<-small

# Filter out missing values in the relevant column
data <- data[!is.na(data$expend), ]
data$agesq<-(data$age)^2
data<-data%>%mutate(across(c(sex,race,married,income,region,insurance,educ), as.factor))
data<-na.omit(data)

write.csv(data, "data_clean_2013.csv")
# Fit the two-part model using the tpm function
# tp_model <- tpm(
#   expend ~ asthma_treated + sex + age + agesq + income + educ + race + insurance + CCI + married + region,
#   data = data,
#   link_part1 = "logit",
#   family_part2 = Gamma(link = "log")
# )

# weighted
tp_model <- tpm(
  expend ~ asthma_treated + sex + age + agesq + income + educ + race + insurance + married + region, 
  data = data, 
  link_part1 = "logit", 
  family_part2 = Gamma(link = "log"),
  weights=data$PERWTF)


# Calculate the incremental effect of asthma_treated
base_data <- subset(data,asthma_treated==1)
counterfactual_data <- base_data
counterfactual_data$asthma_treated <- 0

base_prediction <- predict(tp_model, newdata = base_data, se.fit=T)
base_prediction
counterfactual_prediction <- predict(tp_model, newdata = counterfactual_data, se.fit=T)
str(counterfactual_prediction)
# str(counterfactual_prediction)

# Calculate incremental cost
# mae <- mean(base_prediction - counterfactual_prediction)
mae <- mean(base_prediction$fit - counterfactual_prediction$fit)
# mae
mse <- mean(base_prediction$se.fit - counterfactual_prediction$se.fit)
mse


# Print the results
cat('incremental cost:', round(mae), ' ', 'se: ', round(mse), "\n")

# Linear regression for comparison
lfit <- lm(expend ~ asthma_treated + sex + age + agesq + income + educ + race + insurance + CCI + married + region, data = data)
cat('Linear regression estimate:', round(coef(lfit)['asthma_treated']), '\n')

##### LET'S CHECK AGAINST MARGINS COMMAND IN STATA
mgn = margin(tp_model,term = "asthma_treated",value = list(asthma_treated = c(0,1)))
str(mgn)
# mgn$Pred_marg[1]- 
mgn$Pred_marg$Margin[2] - mgn$Pred_marg$Margin[1] 
mgn$Pred_marg$Margin[2] - mgn$Pred_marg$Margin[1] 
