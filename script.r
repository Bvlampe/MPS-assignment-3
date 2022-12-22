library(haven)

data_raw = haven::read_sav("ESS9e03_1.sav")

table(data_raw$cntry)
data = data_raw[!data_raw$cntry %in% c("CH", "GB", "IS", "ME", "NO", "RS"),]
table(data$cntry)

# Only 24 of 27 EU members represented: Croatia, Malta, and Romania are missing

# drop missing values for EU stance, recode to 0-1
cases_before = length(data$vteurmmb)
data = data[(data$vteurmmb == 1 | data$vteurmmb == 2),]
data = data[!is.na(data$vteurmmb),]
diff = cases_before - length(data$vteurmmb)
data[data$vteurmmb == 1, "vteurmmb"] = 0
data[data$vteurmmb == 2, "vteurmmb"] = 1
