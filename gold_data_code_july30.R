library(MEPS)
library(dplyr)
library(survey)
library(twopartm)
library(tidyr)

# Function to read MEPS data
read_meps_data <- function(year, types) {
  data_list <- list()
  for (type in types) {
    data_list[[type]] <- read_MEPS(year = year, type = type)
  }
  return(data_list)
}

# Function to select conditions related to asthma (ICD-9 code 493 or ICD-10 code J45)
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
years <- c(2016:2021)

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
fyc_pooled <- fyc_pooled %>%
  left_join(all_years_data %>% mutate(asthma_treated = 1), by = c("DUPERSID", "Year")) %>%
  mutate(asthma_treated = ifelse(is.na(asthma_treated), 0, asthma_treated))

# Function to process yearly data
process_yearly_data <- function(data, year) {
  year_suffix <- substr(year, 3, 4)  # Get the last two digits of the year
  
  data %>%
    mutate(
      missed_work= get(paste0("DDNWRK", year_suffix)),
      expend = get(paste0("TOTEXP", year_suffix)),
      sex = SEX,
      race = RACETHX,
      age = get(paste0("AGE", year_suffix, "X")),
      married = ifelse(get(paste0("MARRY", year_suffix, "X")) == 1, 1, 0),
      income = get(paste0("POVCAT", year_suffix)),
      region = get(paste0("REGION", year_suffix)),
      insurance = case_when(
        get(paste0("MCAID", year_suffix, "X")) == 1 ~ "Medicaid",
        get(paste0("MCARE", year_suffix, "X")) == 1 ~ "Medicare",
        get(paste0("INSCOV", year_suffix)) == 1 ~ "Private",
        get(paste0("INSCOV", year_suffix)) == 3 ~ "Uninsured",
        TRUE ~ NA_character_
      ),
      educ = if (year <= 2014) {
        case_when(
          EDUYRDG %in% c(1, 2) ~ "LessHS",
          EDUYRDG %in% c(3, 4) ~ "HS",
          EDUYRDG %in% c(5, 6, 7, 8) ~ "College",
          EDUYRDG == 9 ~ "Advanced",
          EDUYRDG == 10 ~ "Child5",
          TRUE ~ NA_character_
        )
      } else {
        case_when(
          HIDEG == 1 ~ "LessHS",
          HIDEG %in% c(2, 3) ~ "HS",
          HIDEG %in% c(4, 7) ~ "College",
          HIDEG %in% c(5, 6) ~ "Advanced",
          HIDEG == 8 ~ "Child16",
          TRUE ~ NA_character_
        )
      }
    ) %>%
    select(DUPERSID, Year, asthma_treated, VARSTR, PERWTF, expend, age, sex, race, married, income, region, REGION31,REGION42,REGION53, insurance, educ,missed_work)
}

# Apply the process_yearly_data function to each year in the pooled FYC data
small_fyc <- bind_rows(lapply(years, function(year) {
  process_yearly_data(fyc_pooled %>% filter(Year == year), year)
}))

# Process the condition data
condition_list <- lapply(years, function(year) {
  condition <- read_MEPS(year = year, type = "COND")
  
  if (year < 2016) {
    condition <- condition %>%
      mutate(
        Year = year,
        mi = ifelse(ICD9CODX %in% c("410", "412"), 1, 0),
        chf = ifelse(ICD9CODX %in% c("398", "402", "404", "425", "428"), 1, 0),
        pvd = ifelse(ICD9CODX %in% c("093", "437", "440", "441", "443", "447", "557", "V43"), 1, 0),
        cevd = ifelse(ICD9CODX %in% c("362", "430", "431", "432", "433", "434", "436", "437", "438"), 1, 0),
        dementia = ifelse(ICD9CODX %in% c("290", "294", "331"), 1, 0),    
        cpd = ifelse(ICD9CODX %in% c("416", "490", "491", "492", "494", "495", "496", "497", "498", "499", "500", "501", "502", "503", "504", "505", "506", "508"), 1, 0), 
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
      )
  } else {
    condition <- condition %>%
      mutate(
        Year = year,
        mi = ifelse(ICD10CDX %in% c("I21", "I22", "I25"), 1, 0),
        chf = ifelse(ICD10CDX %in% c("I09", "I11", "I13", "I25", "I42", "I43", "I50", "P29"), 1, 0),
        pvd = ifelse(ICD10CDX %in% c("I70", "I71", "I73", "I77", "I79", "K55", "Z95"), 1, 0),
        cevd = ifelse(ICD10CDX %in% c("G45", "G46", "H34", "I60", "I61", "I62", "I63", "I64", "I65", "I66", "I67", "I68", "I69"), 1, 0),
        dementia = ifelse(ICD10CDX %in% c("F00", "F01", "F02", "F03", "F05", "G30", "G31"), 1, 0),    
        cpd = ifelse(ICD10CDX %in% c("I27", "J40", "J41", "J42", "J43", "J44", "J46", "J47", "J60", "J61", "J62", "J63", "J64", "J65", "J66", "J67", "J68", "J70"), 1, 0),    
        rheum = ifelse(ICD10CDX %in% c("M05", "M06", "M31", "M32", "M33", "M34", "M35", "M36"), 1, 0),
        pud = ifelse(ICD10CDX %in% c("K25", "K26", "K27", "K28"), 1, 0),    
        mld = ifelse(ICD10CDX %in% c("B18", "K70", "K71", "K73", "K74", "K76", "Z94"), 1, 0),
        diab = ifelse(ICD10CDX %in% c("E10", "E11", "E12", "E13", "E14"), 1, 0),
        diabwc = ifelse(ICD10CDX %in% c("E10.2", "E10.3", "E10.4", "E10.5", "E10.7", "E11.2", "E11.3", "E11.4", "E11.5", "E11.7", "E12.2", "E12.3", "E12.4", "E12.5", "E12.7", "E13.2", "E13.3", "E13.4", "E13.5", "E13.7", "E14.2", "E14.3", "E14.4", "E14.5", "E14.7"), 1, 0),
        hp = ifelse(ICD10CDX %in% c("G04", "G11", "G80", "G81", "G82", "G83"), 1, 0),
        rend = ifelse(ICD10CDX %in% c("I12", "I13", "N03", "N05", "N18", "N19", "N25", "Z49", "Z94", "Z99"), 1, 0),    
        canc = ifelse(ICD10CDX %in% c("C00", "C01", "C02", "C03", "C04", "C05", "C06", "C07", "C08", "C09", "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20", "C21", "C22", "C23", "C24", "C25", "C26", "C30", "C31", "C32", "C33", "C34", "C37", "C38", "C39", "C40", "C41", "C43", "C45", "C46", "C47", "C48", "C49", "C50", "C51", "C52", "C53", "C54", "C55", "C56", "C57", "C58", "C60", "C61", "C62", "C63", "C64", "C65", "C66", "C67", "C68", "C69", "C70", "C71", "C72", "C73", "C74", "C75", "C76", "C80", "C81", "C82", "C83", "C84", "C85", "C88", "C90", "C91", "C92", "C93", "C94", "C95", "C96", "C97"), 1, 0),    
        msld = ifelse(ICD10CDX %in% c("I85.0", "I85.9", "I86.4", "I98.2", "K70.4", "K71.1", "K72.1", "K72.9", "K76.5a", "K76.6", "K76.7"), 1, 0),
        metacanc = ifelse(ICD10CDX %in% c("C77", "C78", "C79", "C80"), 1, 0),
        aids = ifelse(ICD10CDX %in% c("B20", "B21", "B22", "B24"), 1, 0)
      )
  }
  
  condition %>%
    group_by(DUPERSID, Year) %>%
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
})

condition <- bind_rows(condition_list)

# Prepare the final dataset for modeling
small <- small_fyc %>% left_join(condition, by = c("DUPERSID", "Year"))

library(tidyr) #for replace_na

data<-small
data<-data%>%replace_na(list(CCI=0))
data <- data[!is.na(data$expend), ]

data$region <- apply(data[, c("region", "REGION31", "REGION42", "REGION53")], 1, function(x) {
  x[x != -1][1] })

data$agesq<-(data$age)^2
data$year<-data$Year
data<-data%>%mutate(across(c(Year,sex,race,married,income,region,insurance,educ), as.factor))
data<-data%>%replace_na(list(mi=0,chf=0,pvd=0,cevd=0,dementia=0,cpd=0,rheum=0,pud=0,mld_liver=0,diab=0,
                             diabwc=0,hemiplegia=0,rend=0,cancer=0,ms_liver=0,metacanc=0,aids=0))
data<-data%>%filter(!is.na(insurance),!is.na(educ))

cpi <- c(98.349, 100, 101.745, 103.227, 105.419, 107.549)
cpi_data <- data.frame(
  year = 2016:2021,
  CPI = cpi
)

# Merge the data with the CPI values
data <- merge(data, cpi_data, by = "year")
# Calculate the adjusted expenditure for 2021 dollars
data$expend <- data$expend * (cpi_data$CPI[6] / data$CPI)




