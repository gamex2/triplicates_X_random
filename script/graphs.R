#lipno
ggplot(vpue_txr_lip, #cpue tt 
       aes(x = gi_deployment_type, y = cpue_mean, fill = gi_deployment_type)) +
  geom_boxplot(width = 0.5,
               position = position_dodge(0.9), outlier.shape = NA) + 
  geom_jitter(alpha = 0.6, position=position_jitter(width=0.1, height=0.1)) +
  scale_fill_viridis_d(option = 'C')+
  facet_wrap(~depthlayerid, scale =  "free_y", ncol = 3) + 
  theme(strip.text = element_text(face = "italic")) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 5, col = 'black', position = position_dodge(.9)) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, col = 'white', position = position_dodge(.9)) +
  labs(x = "Deployment type", y = 'CPUE in units per 1000m² net', title = "Lipno")+
  theme(plot.title = element_text(size = 28, face = "bold"),
        axis.text.x = element_text(size = 24, angle = 45, hjust = 1, vjust = 1),
        axis.text.y = element_text(size = 24), 
        strip.text = element_text(size = 24),
        axis.title.x = element_text(size = 24),
        axis.title.y = element_text(size = 18),
        legend.title = element_text(size = 24),
        legend.text = element_text(size = 24), 
        legend.position="bottom") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  guides(fill = F)

ggplot(vpue_txr_lip, #bpue tt
       aes(x = gi_deployment_type, y = bpue_mean/1000, fill = gi_deployment_type)) +
  geom_boxplot(width = 0.5,
               position = position_dodge(0.9), outlier.shape = NA) + 
  geom_jitter(alpha = 0.6, position=position_jitter(width=0.1, height=0.1)) +
  scale_fill_viridis_d(option = 'C')+
  facet_wrap(~depthlayerid, scale =  "free_y", ncol = 3) + 
  theme(strip.text = element_text(face = "italic")) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 5, col = 'black', position = position_dodge(.9)) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, col = 'white', position = position_dodge(.9)) +
  labs(x = "Deployment type", y = 'BPUE in kilos per 1000m² net', title = "Lipno")+
  theme(plot.title = element_text(size = 28, face = "bold"),
        axis.text.x = element_text(size = 24, angle = 45, hjust = 1, vjust = 1),
        axis.text.y = element_text(size = 24), 
        strip.text = element_text(size = 24),
        axis.title.x = element_text(size = 24),
        axis.title.y = element_text(size = 18),
        legend.title = element_text(size = 24),
        legend.text = element_text(size = 24), 
        legend.position="bottom") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  guides(fill = F)


catch_lip[, sp_grouped := fct_lump(f = sp_scientificname, prop = 0.05, w = ct_abundancestar)]
ggplot(data = catch_lip, 
       aes(x = ct_sl, fill = gi_deployment_type)) + 
  geom_histogram(position = "dodge", col = "black", bins = 40) +
  facet_wrap(~sp_grouped, ncol = 3) +
  labs(x = 'Body length (mm)', y = "Fish N",
       fill = "Net deployment:", color = NULL, title = "Lipno") +
  guides(color = F) + 
  scale_fill_viridis_d(option = 'C')+
  theme(plot.title = element_text(size = 28, face = "bold"),
        axis.text.x = element_text(size = 26,angle = 0, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(size = 26, face = "italic"), 
        strip.text = element_text(size = 28),
        axis.title.x = element_text(size = 28),
        axis.title.y = element_text(size = 28),
        legend.title = element_text(size= 28),
        legend.text = element_text(size = 26, face = "italic")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="bottom")  




#rimov####
ggplot(vpue_txr_rim, #cpue tt 
       aes(x = gi_deployment_type, y = cpue_mean, fill = gi_deployment_type)) +
  geom_boxplot(width = 0.5,
               position = position_dodge(0.9), outlier.shape = NA) + 
  geom_jitter(alpha = 0.6, position=position_jitter(width=0.1, height=0.1)) +
  scale_fill_viridis_d(option = 'C')+
  facet_wrap(~depthlayerid, scale =  "free_y", ncol = 3) + 
  theme(strip.text = element_text(face = "italic")) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 5, col = 'black', position = position_dodge(.9)) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, col = 'white', position = position_dodge(.9)) +
  labs(x = "Deployment type", y = 'CPUE in units per 1000m² net', title = "Římov")+
  theme(plot.title = element_text(size = 28, face = "bold"),
        axis.text.x = element_text(size = 24, angle = 45, hjust = 1, vjust = 1),
        axis.text.y = element_text(size = 24), 
        strip.text = element_text(size = 24),
        axis.title.x = element_text(size = 24),
        axis.title.y = element_text(size = 18),
        legend.title = element_text(size = 24),
        legend.text = element_text(size = 24), 
        legend.position="bottom") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  guides(fill = F)

ggplot(vpue_txr_rim, #bpue tt
       aes(x = gi_deployment_type, y = bpue_mean/1000, fill = gi_deployment_type)) +
  geom_boxplot(width = 0.5,
               position = position_dodge(0.9), outlier.shape = NA) + 
  geom_jitter(alpha = 0.6, position=position_jitter(width=0.1, height=0.1)) +
  scale_fill_viridis_d(option = 'C')+
  facet_wrap(~depthlayerid, scale =  "free_y", ncol = 3) + 
  theme(strip.text = element_text(face = "italic")) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 5, col = 'black', position = position_dodge(.9)) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, col = 'white', position = position_dodge(.9)) +
  labs(x = "Deployment type", y = 'BPUE in kilos per 1000m² net', title = "Římov")+
  theme(plot.title = element_text(size = 28, face = "bold"),
        axis.text.x = element_text(size = 24, angle = 45, hjust = 1, vjust = 1),
        axis.text.y = element_text(size = 24), 
        strip.text = element_text(size = 24),
        axis.title.x = element_text(size = 24),
        axis.title.y = element_text(size = 18),
        legend.title = element_text(size = 24),
        legend.text = element_text(size = 24), 
        legend.position="bottom") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  guides(fill = F)


catch_rim[, sp_grouped := fct_lump(f = sp_scientificname, prop = 0.05, w = ct_abundancestar)]

ggplot(data = catch_rim, 
       aes(x = ct_sl, fill = gi_deployment_type)) + 
  geom_histogram(position = "dodge", col = "black", bins = 40) +
  facet_wrap(~sp_grouped, ncol = 3) +
  labs(x = 'Body length (mm)', y = "Fish N",
       fill = "Net deployment:", color = NULL, title = "Římov") +
  guides(color = F) + 
  scale_fill_viridis_d(option = 'C')+
  theme(plot.title = element_text(size = 28, face = "bold"),
        axis.text.x = element_text(size = 26,angle = 0, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(size = 26, face = "italic"), 
        strip.text = element_text(size = 28),
        axis.title.x = element_text(size = 28),
        axis.title.y = element_text(size = 28),
        legend.title = element_text(size= 28),
        legend.text = element_text(size = 26, face = "italic")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="bottom") 
