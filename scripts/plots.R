library(ggpubr)
library(tidyverse)

df$month <- factor(df$month, levels = c("April", "May","June",
                                        "July","August","September", "October"),
                   ordered = T)
                   

df%>%
  filter(Fish == "Parachana")%>%
  drop_na()%>%
  pivot_longer(cols = 4:ncol(.), names_to = "Heavy metals", values_to = "Concentration")%>%
  group_by(month, `Heavy metals`)%>%
  summarise_if(is.numeric, mean)%>%
  ggplot(aes(month, Concentration, group = `Heavy metals`, col = `Heavy metals`))+
  geom_line()+
  geom_point()+
  theme_pubr()+
  theme(legend.position = "bottom")
  
df%>%
  filter(Fish == "Parachana")%>%
  drop_na()%>%
  pivot_longer(cols = 4:ncol(.), names_to = "Heavy metals", values_to = "Concentration")%>%
  ggplot(aes(`Heavy metals`, Concentration, fill = `Heavy metals`))+
  geom_col(position = "dodge")+
  theme_pubclean()+
  facet_wrap(~month, scales = "free", strip.position = "bottom")+
  guides(fill = "none")

df%>%
  filter(Fish == "Parachana")%>%
  drop_na()%>%
  pivot_longer(cols = 4:ncol(.), names_to = "Heavy metals", values_to = "Concentration")%>%
  ggplot(aes(`Heavy metals`, Concentration, fill = Treatment))+
  geom_col(position = "dodge")+
  theme_pubclean()+
  facet_wrap(~`Heavy metals`, scales = "free")+
  scale_fill_discrete(labels = c("Gill", "Intestine", "Trunk"))

###############################################
df%>%
  filter(Fish == "African pike")%>%
  drop_na()%>%
  pivot_longer(cols = 4:ncol(.), names_to = "Heavy metals", values_to = "Concentration")%>%
  ggplot(aes(`Heavy metals`, Concentration, fill = Treatment))+
  geom_col(position = "dodge")+
  labs(y = "Concentration (mg/kg)")+
  theme_pubclean()+
  scale_fill_discrete(labels = c("Gill", "Intestine", "Trunk"))+
  facet_grid(~month, scales = "free")+
  theme(legend.position = "right", legend.title = element_blank())+
  theme(axis.text = element_text(face = "bold",colour = "black", size = rel(0.75)),
        axis.title = element_text(face = "bold",colour = "black", size = rel(1)))

library(ggpubr)
df%>%
  filter(Fish == "African pike")%>%
  drop_na()%>%
  pivot_longer(cols = 4:ncol(.), names_to = "Heavy metals", values_to = "Concentration")%>%
  ggplot(aes(`Heavy metals`, Concentration, fill = `Heavy metals`))+
  geom_col(position = "dodge")+
  theme_pubclean()+
  guides(fill = "none")

df%>%
  filter(Fish == "African pike")%>%
  drop_na()%>%
  pivot_longer(cols = 4:ncol(.), names_to = "Heavy metals", values_to = "Concentration")%>%
  ggplot(aes(`Heavy metals`, Concentration, fill = Treatment))+
  geom_col(position = "dodge")+
  theme_pubclean()+
  facet_wrap(~`Heavy metals`, scales = "free")+
  scale_fill_discrete(labels = c("Gill", "Intestine", "Trunk"))
