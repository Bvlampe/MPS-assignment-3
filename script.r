library(haven)
library(lme4)
library(nlme)
library(multilevel)
library(beepr)

data_raw = haven::read_sav("ESS9e03_1.sav")

# The full dataset from the ESS' website does not load due to an invalid timestamp
# I did not find a fix in the 2 minutes I spent looking and decided to manually
# add the country-level data instead
# data_full = haven::read_dta("ESS9MDWe03.1_F1.dta")

# Only 24 of 27 EU members represented: Greece, Malta, and Romania are missing
data = data_raw[!data_raw$cntry %in% c("CH", "GB", "IS", "ME", "NO", "RS"),]
data = data[, c("cntry", "vteurmmb", "imbgeco", "psppsgva", "hincfel", "trstun",
                "atchctr", "brncntr", "eisced")]

# Manually create dataframe with country-level data:
# Money spent on and received from the EU budget in 2018, as % of GNI
cntry = c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", "FR", "HR",
          "HU", "IE", "IT", "LT", "LV", "NL", "PL", "PT", "SE", "SI", "SK")
cntry_received = c(0.51, 1.86, 3.91, 1.31, 2.1, 0.35, 0.46, 3.02, 1.02, 0.64, 0.62, 2.22,
             4.97, 0.81, 0.58, 4.8, 4.14, 0.32, 3.43, 2.55, 0.38, 2.04, 2.78)
cntry_spent = c(0.91, 1.3, 1.05, 1.01, 1.01, 0.85, 0.94, 0.97, 0.98, 0.93, 0.92, 0.96,
          1.0, 1.03, 0.96, 1.04, 0.94, 0.94, 0.99, 0.95, 0.8, 1.01, 0.97)
cntry_df = data.frame(cntry, cntry_received, cntry_spent)
cntry_df$cntry_diff = cntry_df$cntry_received - cntry_df$cntry_spent

# Add country-level data to main dataset
data = merge(data, cntry_df, by="cntry")

# drop missing values
# Estonia gets dropped entirely due to no DV values
cases_before = length(data$vteurmmb)
data = data[(data$vteurmmb == 1 | data$vteurmmb == 2),]
data = data[!is.na(data$vteurmmb),]
data = data[!is.na(data$imbgeco),]
data = data[!is.na(data$psppsgva),]
data = data[!is.na(data$hincfel),]
data = data[!is.na(data$trstun),]
data = data[!is.na(data$atchctr),]
data = data[!is.na(data$brncntr),]
data = data[!is.na(data$eisced),]
data = data[!data$eisced == 55,] # "Other" removed
diff = cases_before - length(data$vteurmmb)

# Recode EU stance DV to 0-1
data[data$vteurmmb == 1, "vteurmmb"] = 0
data[data$vteurmmb == 2, "vteurmmb"] = 1

# Recode "born in country" from 2-1 to 0-1 (no-yes)
data[data$brncntr == 2, "brncntr"] = 0

# summary(data)
# table(data$cntry)

# Rename columns
names(data)[names(data) == "vteurmmb"] = "EU_exit"
names(data)[names(data) ==  "agea"] = "age"
names(data)[names(data) == "imbgeco"] = "immigrants_eco"
names(data)[names(data) ==  "psppsgva"] = "say_in_politics"
names(data)[names(data) == "hincfel"] = "econ_difficulty"
names(data)[names(data) == "trstun"] = "trust_UN"
names(data)[names(data) == "atchctr"] = "attachment_cntry"
names(data)[names(data) == "brncntr"] = "born_cntry"
names(data)[names(data) == "eisced"] = "education"

# Descriptives
summary(data)
table(data$cntry)

for(var in colnames(data)[names(data) != "cntry"]){
  print(var)
  print(sd(data[[var]]))
}

# Game on

rm(cntry, cntry_spent, cntry_received, diff)

attach(data)

# Intraclass correlation coefficient
multilevel::ICC1(aov(EUexit ~ cntry, data))

# Model
model_diff = lme4::glmer(EUexit ~ immigrants_eco + say_in_politics + econ_difficulty + trust_UN +
                     attachment_cntry + born_cntry + education + cntry_diff + (1 | cntry),
                     data, binomial("logit"))
beepr::beep(3)
summary(model_diff)
