library(readxl)

# Read data from Excel file, skip the first row with column names
data <- read_excel("JSTdatasetR6.xlsx", sheet = "Data", skip = 1)

colnames(data)
colnames(data) <- c("year", "country", "iso", "ifs", "pop", "rgdpmad", "rgdpbarro", "rconsbarro", "gdp", "iy", "cpi", "ca", "imports", "exports", "narrowm", "money", "stir", "ltrate", "hpnom", "unemp", "wage", "debtgdp", "revenue", "expenditure", "xrusd", "tloans", "tmort", "thh", "tbus", "bdebt", "lev", "ltd", "noncore", "crisisJST", "crisisJST_old", "peg", "peg_strict", "peg_type", "peg_base", "JSTtrilemmaIV", "eq_tr", "housing_tr", "bond_tr", "bill_rate", "rent_ipolated", "housing_capgain_ipolated", "housing_capgain", "housing_rent_rtn", "housing_rent_yd", "eq_capgain", "eq_dp", "eq_capgain_interp", "eq_tr_interp", "eq_dp_interp", "bond_rate", "eq_div_rtn", "capital_tr", "risky_tr", "safe_tr")

colnames(data)
subset_data <- data[, c("year", "country", "gdp", "cpi", "imports", "exports", "unemp", "debtgdp")]

summary(subset_data)

ols_model <- lm(log(GDP) ~ Inflation + Imports + Exports + Unemployment + Debt_GDP_Ratio, data = subset_data)
summary(ols_model)

log(GDP) ~ cpi + imports + exports + unemp + debtgdp
ols_model <- lm(log(gdp) ~ cpi + imports + exports + unemp + debtgdp, data = subset_data)
summary(ols_model)

library(plm)
panel_data <- pdata.frame(subset_data, index = c("country", "year"))
pfe_model <- plm(log(gdp) ~ cpi + imports + exports + unemp + debtgdp, data = panel_data, model = "within")
summary(pfe_model)





panel_data$year <- as.numeric(panel_data$year)
panel_data_pre1945 <- na.omit(subset(panel_data, year <= 1945))
# Check for missing values
sum(is.na(panel_data_pre1945))
sum(is.na(panel_data_post1945))


panel_data_pre1945 <- subset(panel_data, year <= 1945)
panel_data_post1945 <- subset(panel_data, year >= 1946)
pfe_model_pre1945 <- plm(log(gdp) ~ cpi + imports + exports + unemp + debtgdp, data = panel_data_pre1945, model = "within")
pfe_model_post1945 <- plm(log(gdp) ~ cpi + imports + exports + unemp + debtgdp, data = panel_data_post1945, model = "within")
summary(pfe_model_pre1945)
summary(pfe_model_post1945)

panel_data_excludeUS <- subset(panel_data, country != "United States")
panel_data_excludeAUS <- subset(panel_data, country != "Australia")
panel_data_excludeJPN <- subset(panel_data, country != "Japan")

pfe_model_excludeUS <- plm(log(gdp) ~ cpi + imports + exports + unemp + debtgdp, data = panel_data_excludeUS, model = "within")
pfe_model_excludeAUS <- plm(log(gdp) ~ cpi + imports + exports + unemp + debtgdp, data = panel_data_excludeAUS, model = "within")
pfe_model_excludeJPN <- plm(log(gdp) ~ cpi + imports + exports + unemp + debtgdp, data = panel_data_excludeJPN, model = "within")

summary(pfe_model_excludeUS)
summary(pfe_model_excludeAUS)
summary(pfe_model_excludeJPN)


