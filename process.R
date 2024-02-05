library(tidyverse) #for data manipulation
library(readxl)    #for reading excel file
library(MASS)      #for fitting ordinal regression  
library(formatR)   #for better formatting of RMarkdown


#load dataset
dd <- read_excel('data/Cdn_fishery_attributes_current.xlsx', sheet="2022 Fishery attributes")
dd$gear_old <- dd$gear
dd$gear     <- dd$gear_current

cs <- read_csv('data/Climate sensitivity_6 Oct.csv')

dd <- left_join(dd,cs,by="stock_id")

#aggregate poorly represented groups
dd <- dd %>% mutate(dfo_region = ifelse(dfo_region=="Arctic"|dfo_region=="NCR", 'Central', dfo_region),
                    gear       = ifelse(gear=="SOK"|gear=="Hand", "Hand gathered", gear),
                    gear       = ifelse(gear=="Midwater trawl"|gear=="Purse seine", "Pelagic nets", gear),
                    gear       = ifelse(gear=="Longline"|gear=="Rod-and-reel"|gear=="Troll", "Pelagic lines", gear),
                    gear       = ifelse(gear=="Bottom trawl"|gear=="Dredge", "Demersal nets", gear),
                    gear       = ifelse(gear=="Weir"|gear=="Trap","Trap",gear),
                    taxa       = ifelse(taxa=="Anoplopomatidae"|taxa=="Lophiidae"|taxa=="Myxinidae"|taxa=="Osmeridae"|taxa=="Xiphiidae"|taxa=="Scombridae", "Other", taxa))

#standardize continuous variables for plotting
dd <- dd %>% mutate(fishing_vulnerability = scale(fishing_vulnerability),
                    trophic_level         = scale(trophic_level),
                    log10value            = scale(log10(`value_$` + 1)),
                    ref_points            = scale(ref_points),
                    crib_sens             = scale(crib_sens))


#Remove *Uncertain* category and store *Critical*, *Cautious*, and *Healthy* as an ordered factor 
d  <- dd %>% filter(status!='Uncertain')
d$status <- factor(d$status, levels=c("Critical", "Cautious", "Healthy"), ordered=TRUE)


#Define categorical variables of interest as factor variables
d <- d %>% mutate(dfo_region     = factor(dfo_region),
                  gear           = factor(d$gear),
                  msc_status     = factor(d$msc_status),
                  sea_monitoring = factor(d$sea_monitoring), 
                  taxa           = factor(d$taxa))
#d$ref_points     <- factor(d$ref_points)  #this is considered as a continuous variable below


#For each categorical variable, we will set the reference category equal to the approximate middle category to ease interpretation of coefficients
d <- d %>% mutate(sea_monitoring = relevel(sea_monitoring, ref="Partial"),
                  dfo_region = relevel(dfo_region, ref="Quebec"),
                  msc_status = relevel(msc_status, ref="None"),
                  gear = relevel(gear, ref="Pelagic nets"),
                  taxa = relevel(taxa, ref="Pleuronectidae"))
