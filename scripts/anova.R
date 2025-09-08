library(tidyverse)
library(agricolae)


df_init <- read_csv("Heavy metals.csv")

df <- df_init%>%
  group_by(month, Treatment, Fish)%>%
  summarise_all(mean)

df$Treatment <- gsub("[1-6]+","",df$Treatment)

df

df_agg <- df%>%
  group_by(month, Treatment, Fish)%>%
  summarise(across(c("Cu":"Pb"),
                   list(~mean(.x, na.rm = T),
                        ~plotrix::std.error(.x, na.rm = T))))%>%
  mutate(across(where(is.numeric), ~round(.x, 2)))

df_p <- df_agg%>%
  filter(Fish == "Parachana")%>%
  select(-Fish)%>%
  unite("Cu", starts_with("Cu"), sep = plus_minus)%>%
  unite("Cd", starts_with("Cd"), sep = plus_minus)%>%
  unite("Fe", starts_with("Fe"), sep = plus_minus)%>%
  unite("Mn", starts_with("Mn"), sep = plus_minus)%>%
  unite("Ni", starts_with("Ni"), sep = plus_minus)%>%
  unite("Pb", starts_with("Pb"), sep = plus_minus)
  
df_aov <- df%>%
  janitor::clean_names()

formulae <- lapply(colnames(df_aov)[-1],
                   function(x) as.formula(paste0(x, "~african_pike")))


res <- lapply(formulae, function(x) summary.aov(aov(x, data = df_aov)))

names(res) <- format(formulae)
res


cd <- duncan.test(aov(Cd ~ `African Pike`, data = df), "African Pike")

plot(cd, main = "", xlab = "Treatment", ylab = "Concentration")


mn <- duncan.test(aov(Mn ~ `African Pike`, data = df), "African Pike")

plot(mn, main = "", xlab = "Treatment", ylab = "Crude protein")

df_agg <- df_agg%>%
  unite("Cu", starts_with("Cu"), sep = plus_minus)%>%
  unite("Cd", starts_with("Cd"), sep = plus_minus)%>%
  unite("Fe", starts_with("F"), sep = plus_minus)%>%
  unite("Mn", starts_with("M"), sep = plus_minus)%>%
  unite("Ni", starts_with("N"), sep = plus_minus)%>%
  unite("Pb", starts_with("P"), sep = plus_minus)

df_agg$fi_g <- c("a","a","b")
df_agg$mi_g <- c("b","a","a")

df_agg <- df_agg%>%
  unite("Cd", c("Cd", "fi_g"), sep = "")%>%
  unite("Mn", c("Mn", "mi_g"), sep = "")%>%
  pivot_longer(cols = "Cu":"Pb", names_to = "elements",values_to = "conc")%>%
  pivot_wider(id_cols = "elements", names_from = `African Pike`, values_from = conc)

write.csv(df_agg,"anova table pike.csv", row.names = F)
