
library(tidyverse)
library(data.table)


url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
path <- "data/stormdata.bz2"

if (!file.exists("data")) {
        dir.create("data")
}

if (!file.exists(path)) {
        download.file(url, path, method = "curl")
}

storm_data <- fread("data/stormdata.bz2", stringsAsFactors = FALSE)
str(storm_data)
dim(storm_data)

#Across the United States, which types of events (as indicated in the #EVTYPE are most harmful with respect to population health?

harmful_ph <- storm_data %>% 
        group_by(EVTYPE) %>% 
        summarize(
                Deaths = sum(FATALITIES, na.rm = FALSE),
                Injuries = sum(INJURIES, na.rm = FALSE)
        ) %>% 
        mutate(
                Death_Pct = (Deaths / sum(Deaths)) * 100,
                Injury_Pct = (Injuries / sum(Injuries)) * 100) %>%
        arrange(desc(Deaths), desc(Injuries))

head(harmful_ph, 5)

#Across the United States, which types of events have the greatest economic consequences?
unique(storm_data$PROPDMGEXP)
unique(storm_data$CROPDMGEXP)

eco_conseq <- storm_data %>% 
        select(EVTYPE, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP) %>% 
        mutate(
                prop_exp_clean = toupper(trimws(PROPDMGEXP)),
                crop_exp_clean = toupper(trimws(CROPDMGEXP)),
                
                prop_mult = case_when(
                        prop_exp_clean == "K" ~ 10^3,
                        prop_exp_clean == "M" ~ 10^6,
                        prop_exp_clean == "B" ~ 10^9,
                        prop_exp_clean == "H" ~ 10^2,
                        prop_exp_clean %in% as.character(0:8) ~ 10^as.numeric(prop_exp_clean),
                        TRUE ~ 1
                ),
                crop_mult = case_when(
                        crop_exp_clean == "K" ~ 10^3,
                        crop_exp_clean == "M" ~ 10^6,
                        crop_exp_clean == "B" ~ 10^9,
                        crop_exp_clean == "H" ~ 10^2,
                        crop_exp_clean %in% as.character(0:8) ~ 10^as.numeric(crop_exp_clean),
                        TRUE ~ 1
                ),
                
                prop_dmg = PROPDMG * prop_mult,
                crop_dmg = CROPDMG * crop_mult,
                total = prop_dmg + crop_dmg
        )

eco_summary <- eco_conseq %>%
        group_by(EVTYPE) %>% 
        summarise(
                PropertyLossInMil = sum(prop_dmg, na.rm = TRUE)/ 10^6,
                CropLossInMil = sum(crop_dmg, na.rm = TRUE)/ 10^6,
                TotalinMil = sum(total, na.rm = TRUE)/ 10^6
        ) %>% 
        arrange(desc(TotalinMil))
head(eco_summary, 5)


