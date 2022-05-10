source(here::here('script', 'packages.R'))
source(here::here('script', 'functions.R'))

#Lipno#####
#Data from the database#####
con <- dbConnect(PostgreSQL(), dbname = "fishecu_complex-survey_db", user = "fishecuuser", host = "172.21.3.20", password = "f1sh3cuus3r!")

#catches
specs <- data.table(dbGetQuery(conn = con, statement =  "SELECT * FROM fishecu.species;"))
catch_db <- data.table(dbGetQuery(conn = con, statement =  "SELECT * FROM fishecu.catch_merged;"))

#gillnet deployments####
#db call
all_gillnet_depl <- data.table(dbGetQuery(conn = con, statement = "SELECT * FROM fishecu.gillnet_sampling_merged_wide"))
# all_gillnet_12 <- data.table(dbGetQuery(conn = con, statement = "SELECT * FROM fishecu.gillnet_sampling"))
# all_gillnet_12 <- all_gillnet_12[gi_deployment_type %in% c("triplicates", "randomised")]
# all_gillnet_12 <- all_gillnet_12[dl_depthlayerid == "0-3-pelagic"]

#disconnect from db
dbDisconnect(con)

#lipno campain
all_gillnet_lip <- all_gillnet_depl[ca_campaignid %in% c("F2021LIP01")]

#gear effort
setnames(x = all_gillnet_lip, old = c("gearsize"),new = c("Effort"))

#exclude unmatching nets
all_gillnet_lip <- all_gillnet_lip[depthlayerid %in% c("0-3-benthic", "0-3-pelagic", "3-6-benthic")]

#separating the random from triplicates
all_gillnet_lip[sa_samplingid %in% c("F2021LIP01-G1-1_F2021LIP01-G2-1", "F2021LIP01-G13-1_F2021LIP01-G14-1", "F2021LIP01-G15-1_F2021LIP01-G16-1",
                                      'F2021LIP01-G17-1_F2021LIP01-G18-1', "F2021LIP01-G3-1_F2021LIP01-G4-1", "F2021LIP01-G35-1_F2021LIP01-G36-1",
                                      "F2021LIP01-G37-1_F2021LIP01-G38-1", "F2021LIP01-G39-1_F2021LIP01-G40-1", "F2021LIP01-G5-1_F2021LIP01-G6-1"), gi_deployment_type := "triplicates"]
all_gillnet_lip$gi_deployment_type[is.na(all_gillnet_lip$gi_deployment_type)] <- "randomised"


#filter the catch####
#add the net type
catch_lip <- merge(all_gillnet_lip[, c("sa_samplingid", "gi_deployment_type", "depthlayerid")], catch_db, by = "sa_samplingid")

#add latin names
catch_lip <- merge(catch_lip, specs[,c("sp_speciesid", "sp_scientificname")], by = "sp_speciesid")

#removing the eel attack
catch_lip <- catch_lip[!sp_speciesid == 'EA']

#remove YOY from catch
catch_lip <- catch_lip[!ct_agegroup == 'YOY']
setDT(catch_lip)

#Calcule BPUE
bpue_txr_lip <- getVPUE(samplings = all_gillnet_lip, catch = catch_lip, 
                    split.factors.samplings = c("sa_samplingid", "gi_deployment_type", "depthlayerid"),
                    split.factors.catch = c("ct_agegroup"),
                    id.colname = 'sa_samplingid', value.var = "ct_weightstar")
#Calcule CPUE
cpue_txr_lip <- getVPUE(samplings = all_gillnet_lip, catch = catch_lip, 
                    split.factors.samplings = c("sa_samplingid", "gi_deployment_type", "depthlayerid"),
                    split.factors.catch = c("ct_agegroup"),
                    id.colname = 'sa_samplingid', value.var = "ct_abundancestar")

vpue_txr_lip <- merge(cpue_txr_lip, bpue_txr_lip)
#changing the name of variables
setnames(x = vpue_txr_lip, old = c('ct_weightstar.mean','ct_weightstar.se', 'ct_abundancestar.mean','ct_abundancestar.se'),new = c('bpue_mean','bpue_se', 'cpue_mean','cpue_se'))#rename the outputs
#tranforming 1000m? per net
vpue_txr_lip[, ':='(cpue_mean = cpue_mean*1000, bpue_mean = bpue_mean*1000)]

#Calcule BPUE SP
bpue_txr_sp_lip <- getVPUE(samplings = all_gillnet_lip, catch = catch_lip, 
                    split.factors.samplings = c("sa_samplingid", "gi_deployment_type", "depthlayerid"),
                    split.factors.catch = c("sp_scientificname"),
                    id.colname = 'sa_samplingid', value.var = "ct_weightstar")
#Calcule CPUE SP
cpue_txr_sp_lip <- getVPUE(samplings = all_gillnet_lip, catch = catch_lip, 
                    split.factors.samplings = c("sa_samplingid", "gi_deployment_type", "depthlayerid"),
                    split.factors.catch = c("sp_scientificname"),
                    id.colname = 'sa_samplingid', value.var = "ct_abundancestar")

vpue_txr_sp_lip <- merge(cpue_txr_sp_lip, bpue_txr_sp_lip)
#changing the name of variables
setnames(x = vpue_txr_sp_lip, old = c('ct_weightstar.mean','ct_weightstar.se', 'ct_abundancestar.mean','ct_abundancestar.se'),new = c('bpue_mean','bpue_se', 'cpue_mean','cpue_se'))#rename the outputs
#tranforming 1000m? per net
vpue_txr_sp_lip[, ':='(cpue_mean = cpue_mean*1000, bpue_mean = bpue_mean*1000)]
#write.xlsx(vpue_txr_sp_lip, here::here("data", "vpue_txr_sp_lip.xlsx"))

#size
lip_stats_sum_size <- catch_lip[!ct_sl == 0,.(Mean = round(mean(ct_sl), 2),
                                           Se = round(plotrix::std.error(ct_sl), 2), 
                                           Max = round(max(ct_sl), 2),
                                           Min = round(min(ct_sl), 2)),
                                by = .(sp_scientificname, gi_deployment_type, depthlayerid)]
write.xlsx(lip_stats_sum_size, here::here("data", "lip_stats_sum_size.xlsx"))







# #Rimov####
#Rimov campain
all_gillnet_rim <- all_gillnet_depl[ca_campaignid %in% c("F2021RIM04")]

#gear effort
setnames(x = all_gillnet_rim, old = c("gearsize"),new = c("Effort"))

#separating the random from triplicates
all_gillnet_rim[sa_samplingid %in% c("F2021RIM04-G117-1_F2021RIM04-G118-1", "F2021RIM04-G119-1_F2021RIM04-G120-1", "F2021RIM04-G121-1_F2021RIM04-G122-1",
                                     "F2021RIM04-G127-1_F2021RIM04-G128-1", "F2021RIM04-G129-1_F2021RIM04-G130-1", "F2021RIM04-G131-1_F2021RIM04-G132-1",
                                     "F2021RIM04-G143-1_F2021RIM04-G144-1", "F2021RIM04-G145-1_F2021RIM04-G146-1", "F2021RIM04-G147-1_F2021RIM04-G148-1",
                                     "F2021RIM04-G35-1_F2021RIM04-G36-1", "F2021RIM04-G37-1_F2021RIM04-G38-1", "F2021RIM04-G39-1_F2021RIM04-G40-1",
                                     "F2021RIM04-G149-1_F2021RIM04-G150-1", "F2021RIM04-G151-1_F2021RIM04-G152-1", "F2021RIM04-G153-1_F2021RIM04-G154-1",
                                     "F2021RIM04-G155-1_F2021RIM04-G156-1", "F2021RIM04-G157-1_F2021RIM04-G158-1", "F2021RIM04-G159-1_F2021RIM04-G160-1",
                                     "F2021RIM04-G161-1_F2021RIM04-G162-1", "F2021RIM04-G163-1_F2021RIM04-G164-1", "F2021RIM04-G165-1_F2021RIM04-G166-1",
                                     "F2021RIM04-G167-1_F2021RIM04-G168-1", "F2021RIM04-G169-1_F2021RIM04-G170-1", "F2021RIM04-G171-1_F2021RIM04-G172-1",
                                     "F2021RIM04-G63-1_F2021RIM04-G64-1", "F2021RIM04-G65-1_F2021RIM04-G66-1", "F2021RIM04-G67-1_F2021RIM04-G68-1",
                                     "F2021RIM04-G73-1_F2021RIM04-G74-1", "F2021RIM04-G75-1_F2021RIM04-G76-1", "F2021RIM04-G77-1_F2021RIM04-G78-1",
                                     "F2021RIM04-G93-1_F2021RIM04-G94-1", "F2021RIM04-G95-1_F2021RIM04-G96-1", "F2021RIM04-G97-1_F2021RIM04-G98-1"), gi_deployment_type := "triplicates"]
all_gillnet_rim$gi_deployment_type[is.na(all_gillnet_rim$gi_deployment_type)] <- "randomised"

#exclude unmatching nets
all_gillnet_rim <- all_gillnet_rim[depthlayerid %in% c("0-3-benthic", "3-6-benthic")]

#filter the catch####
#add the net type
catch_rim <- merge(all_gillnet_rim[, c("sa_samplingid", "gi_deployment_type", "depthlayerid")], catch_db, by = "sa_samplingid")

#add latin names
catch_rim <- merge(catch_rim, specs[,c("sp_speciesid", "sp_scientificname")], by = "sp_speciesid")
catch_rim <- catch_rim[sp_speciesid == 'jeseter rusky', sp_scientificname := "Acipenser gueldenstaedtii"]

#removing the eel attack
catch_rim <- catch_rim[!sp_speciesid == 'EA']

#remove YOY from catch
catch_rim <- catch_rim[!ct_agegroup == 'YOY']
setDT(catch_rim)

#Calcule BPUE
bpue_txr_rim <- getVPUE(samplings = all_gillnet_rim, catch = catch_rim, 
                    split.factors.samplings = c("sa_samplingid", "gi_deployment_type", "depthlayerid", "locality"),
                    split.factors.catch = c("ct_agegroup"),
                    id.colname = 'sa_samplingid', value.var = "ct_weightstar")
#Calcule CPUE
cpue_txr_rim <- getVPUE(samplings = all_gillnet_rim, catch = catch_rim, 
                    split.factors.samplings = c("sa_samplingid", "gi_deployment_type", "depthlayerid", "locality"),
                    split.factors.catch = c("ct_agegroup"),
                    id.colname = 'sa_samplingid', value.var = "ct_abundancestar")

vpue_txr_rim <- merge(cpue_txr_rim, bpue_txr_rim)
#changing the name of variables
setnames(x = vpue_txr_rim, old = c('ct_weightstar.mean','ct_weightstar.se', 'ct_abundancestar.mean','ct_abundancestar.se'),new = c('bpue_mean','bpue_se', 'cpue_mean','cpue_se'))#rename the outputs
#tranforming 1000m? per net
vpue_txr_rim[, ':='(cpue_mean = cpue_mean*1000, bpue_mean = bpue_mean*1000)]

#Calcule BPUE
bpue_txr_sp_rim <- getVPUE(samplings = all_gillnet_rim, catch = catch_rim, 
                       split.factors.samplings = c("sa_samplingid", "gi_deployment_type", "depthlayerid"),
                       split.factors.catch = c("sp_scientificname"),
                       id.colname = 'sa_samplingid', value.var = "ct_weightstar")
#Calcule CPUE
cpue_txr_sp_rim <- getVPUE(samplings = all_gillnet_rim, catch = catch_rim, 
                       split.factors.samplings = c("sa_samplingid", "gi_deployment_type", "depthlayerid"),
                       split.factors.catch = c("sp_scientificname"),
                       id.colname = 'sa_samplingid', value.var = "ct_abundancestar")

vpue_txr_sp_rim <- merge(cpue_txr_sp_rim, bpue_txr_sp_rim)
#changing the name of variables
setnames(x = vpue_txr_sp_rim, old = c('ct_weightstar.mean','ct_weightstar.se', 'ct_abundancestar.mean','ct_abundancestar.se'),new = c('bpue_mean','bpue_se', 'cpue_mean','cpue_se'))#rename the outputs
#tranforming 1000m? per net
vpue_txr_sp_rim[, ':='(cpue_mean = cpue_mean*1000, bpue_mean = bpue_mean*1000)]
#write.xlsx(vpue_txr_sp_rim, here::here("data", "vpue_txr_sp_rim.xlsx"))
#size
#size
rim_stats_sum_size <- catch_rim[!ct_sl == 0,.(Mean = round(mean(ct_sl), 2),
                                                Se = round(plotrix::std.error(ct_sl), 2), 
                                                Max = round(max(ct_sl), 2),
                                                Min = round(min(ct_sl), 2)),
                                  by = .(sp_scientificname, gi_deployment_type, depthlayerid)]
write.xlsx(rim_stats_sum_size, here::here("data", "rim_stats_sum_size.xlsx"))
