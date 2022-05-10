#vpue total analises 
#lipno
#normallity test 
shapiro.test(vpue_txr_lip$cpue_mean)
shapiro.test(vpue_txr_lip$bpue_mean)


#homogenity test lipno
bartlett.test(cpue_mean ~ gi_deployment_type, data = vpue_txr_lip)
bartlett.test(bpue_mean ~ gi_deployment_type, data = vpue_txr_lip)

histogram(vpue_txr_lip$cpue_mean)

#model cpue
set.seed(3333)
lm_cpue_lip_tt <- glm(cpue_mean ~ gi_deployment_type + depthlayerid, data = vpue_txr_lip)
summary(lm_cpue_lip_tt)

#model bpue
set.seed(3333)
lm_bpue_lip_tt <- glm(bpue_mean ~ gi_deployment_type + depthlayerid, data = vpue_txr_lip)
summary(lm_bpue_lip_tt)

#model size
set.seed(3333)
lm_size_lip_tt <- glm(ct_sl ~ gi_deployment_type + depthlayerid, data = catch_lip)
summary(lm_size_lip_tt)




#Rimov####
#normallity test Rimov
shapiro.test(vpue_txr_rim$cpue_mean)
shapiro.test(vpue_txr_rim$bpue_mean)

#homogenity test Rimov
bartlett.test(cpue_mean ~ gi_deployment_type, data = vpue_txr_rim)
bartlett.test(bpue_mean ~ gi_deployment_type, data = vpue_txr_rim)

histogram(vpue_txr_rim$cpue_mean)

#model cpue
set.seed(3333)
lm_cpue_rim_tt <- glm(cpue_mean ~ gi_deployment_type + depthlayerid, data = vpue_txr_rim)
summary(lm_cpue_rim_tt)

#model bpue
set.seed(3333)
lm_bpue_rim_tt <- glm(bpue_mean ~ gi_deployment_type + depthlayerid, data = vpue_txr_rim)
summary(lm_bpue_rim_tt)

#model size
set.seed(3333)
lm_size_rim_tt <- glm(ct_sl ~ gi_deployment_type + depthlayerid, data = catch_rim)
summary(lm_size_rim_tt)




#vpue sp analises Lipno####
#Dcasting cpue
vpue_txr_sp_lip <- setDT(vpue_txr_sp_lip)
dcast_txr_cpue_sp_lip <- dcast(data = vpue_txr_sp_lip, formula = gi_deployment_type + depthlayerid + sa_samplingid ~ sp_scientificname,
                            value.var = "cpue_mean")
dcast_txr_cpue_sp_lip <- setDT(dcast_txr_cpue_sp_lip)
dcast_txr_cpue_sp_lip[is.na(dcast_txr_cpue_sp_lip)] <- 0
lapply(dcast_txr_cpue_sp_lip[,c(4:14)], function(x) shapiro.test(x))
lapply(dcast_txr_cpue_sp_lip[,c(4:14)], function(x) bartlett.test(x ~ gi_deployment_type, data = dcast_txr_cpue_sp_lip))

#model cpue
set.seed(3333)
lm_cpue_net_sp <- lapply(dcast_txr_cpue_sp_lip[,c(4:14)], function(x) glm(x ~ gi_deployment_type + depthlayerid, data = dcast_txr_cpue_sp_lip))
summary(lm_cpue_net_sp$`Abramis brama`)
summary(lm_cpue_net_sp$`Abramis brama x Rutilus rutilus`)
summary(lm_cpue_net_sp$`Alburnus alburnus`)
summary(lm_cpue_net_sp$`Blicca bjoerkna`)
summary(lm_cpue_net_sp$`Cyprinus carpio`)
summary(lm_cpue_net_sp$`Esox lucius`)
summary(lm_cpue_net_sp$`Gymnocephalus cernua`)
summary(lm_cpue_net_sp$`Leuciscus aspius`)
summary(lm_cpue_net_sp$`Perca fluviatilis`)
summary(lm_cpue_net_sp$`Rutilus rutilus`)
summary(lm_cpue_net_sp$`Sander lucioperca`)

#Dcasting bpue
dcast_txr_bpue_sp_lip <- dcast(data = vpue_txr_sp_lip, formula = gi_deployment_type + depthlayerid + sa_samplingid ~ sp_scientificname,
                           value.var = "bpue_mean")
dcast_txr_bpue_sp_lip <- setDT(dcast_txr_bpue_sp_lip)
dcast_txr_bpue_sp_lip[is.na(dcast_txr_bpue_sp_lip)] <- 0
lapply(dcast_txr_bpue_sp_lip[,c(4:14)], function(x) shapiro.test(x))
lapply(dcast_txr_bpue_sp_lip[,c(4:14)], function(x) bartlett.test(x ~ gi_deployment_type, data = dcast_txr_bpue_sp_lip))

#model cpue
set.seed(3333)
lm_bpue_net_sp <- lapply(dcast_txr_bpue_sp_lip[,c(4:14)], function(x) glm(x ~ gi_deployment_type + depthlayerid, data = dcast_txr_bpue_sp_lip))
summary(lm_bpue_net_sp$`Abramis brama`)
summary(lm_bpue_net_sp$`Abramis brama x Rutilus rutilus`)
summary(lm_bpue_net_sp$`Alburnus alburnus`)
summary(lm_bpue_net_sp$`Blicca bjoerkna`)
summary(lm_bpue_net_sp$`Cyprinus carpio`)
summary(lm_bpue_net_sp$`Esox lucius`)
summary(lm_bpue_net_sp$`Gymnocephalus cernua`)
summary(lm_bpue_net_sp$`Leuciscus aspius`)
summary(lm_bpue_net_sp$`Perca fluviatilis`)
summary(lm_bpue_net_sp$`Rutilus rutilus`)
summary(lm_bpue_net_sp$`Sander lucioperca`)

#vpue sp analises Rimov####
#Dcasting cpue
vpue_txr_sp_rim <- setDT(vpue_txr_sp_rim)
dcast_txr_cpue_sp_rim <- dcast(data = vpue_txr_sp_rim, formula = gi_deployment_type + depthlayerid + sa_samplingid ~ sp_scientificname,
                           value.var = "cpue_mean")
dcast_txr_cpue_sp_rim <- setDT(dcast_txr_cpue_sp_rim)
dcast_txr_cpue_sp_rim[is.na(dcast_txr_cpue_sp_rim)] <- 0
lapply(dcast_txr_cpue_sp_rim[,c(4:19)], function(x) shapiro.test(x))
lapply(dcast_txr_cpue_sp_rim[,c(4:19)], function(x) bartlett.test(x ~ gi_deployment_type, data = dcast_txr_cpue_sp_rim))

#model cpue
set.seed(3333)
lm_cpue_net_sp_rim <- lapply(dcast_txr_cpue_sp_rim[,c(4:19)], function(x) glm(x ~ gi_deployment_type + depthlayerid, data = dcast_txr_cpue_sp_rim))
summary(lm_cpue_net_sp_rim$`Abramis brama`)
summary(lm_cpue_net_sp_rim$`Abramis brama x Rutilus rutilus`)
summary(lm_cpue_net_sp_rim$`Alburnus alburnus`)
summary(lm_cpue_net_sp_rim$`Blicca bjoerkna`)
summary(lm_cpue_net_sp_rim$`Cyprinus carpio`)
summary(lm_cpue_net_sp_rim$`Esox lucius`)
summary(lm_cpue_net_sp_rim$`Gymnocephalus cernua`)
summary(lm_cpue_net_sp_rim$`Leuciscus aspius`)
summary(lm_cpue_net_sp_rim$`Perca fluviatilis`)
summary(lm_cpue_net_sp_rim$`Rutilus rutilus`)
summary(lm_cpue_net_sp_rim$`Sander lucioperca`)
summary(lm_cpue_net_sp_rim$`Acipenser gueldenstaedtii`)
summary(lm_cpue_net_sp_rim$`Carassius gibelio`)
summary(lm_cpue_net_sp_rim$`Gobio gobio`)
summary(lm_cpue_net_sp_rim$`Vimba vimba`)

#Dcasting bpue
dcast_txr_bpue_sp_rim <- dcast(data = vpue_txr_sp_rim, formula = gi_deployment_type + depthlayerid + sa_samplingid ~ sp_scientificname,
                           value.var = "bpue_mean")
dcast_txr_bpue_sp_rim <- setDT(dcast_txr_bpue_sp_rim)
dcast_txr_bpue_sp_rim[is.na(dcast_txr_bpue_sp_rim)] <- 0
lapply(dcast_txr_bpue_sp_rim[,c(4:19)], function(x) shapiro.test(x))
lapply(dcast_txr_bpue_sp_rim[,c(4:19)], function(x) bartlett.test(x ~ gi_deployment_type, data = dcast_txr_bpue_sp_rim))

#model cpue
set.seed(3333)
lm_bpue_net_sp_rim <- lapply(dcast_txr_bpue_sp_rim[,c(4:19)], function(x) glm(x ~ gi_deployment_type + depthlayerid, data = dcast_txr_bpue_sp_rim))
summary(lm_bpue_net_sp_rim$`Abramis brama`)
summary(lm_bpue_net_sp_rim$`Abramis brama x Rutilus rutilus`)
summary(lm_bpue_net_sp_rim$`Alburnus alburnus`)
summary(lm_bpue_net_sp_rim$`Blicca bjoerkna`)
summary(lm_bpue_net_sp_rim$`Cyprinus carpio`)
summary(lm_bpue_net_sp_rim$`Esox lucius`)
summary(lm_bpue_net_sp_rim$`Gymnocephalus cernua`)
summary(lm_bpue_net_sp_rim$`Leuciscus aspius`)
summary(lm_bpue_net_sp_rim$`Perca fluviatilis`)
summary(lm_bpue_net_sp_rim$`Rutilus rutilus`)
summary(lm_bpue_net_sp_rim$`Sander lucioperca`)
summary(lm_bpue_net_sp_rim$`Acipenser gueldenstaedtii`)
summary(lm_bpue_net_sp_rim$`Carassius gibelio`)
summary(lm_bpue_net_sp_rim$`Gobio gobio`)
summary(lm_bpue_net_sp_rim$`Vimba vimba`)

