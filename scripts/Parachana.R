########################### monthly
df_mn_p <- df%>%
  filter(Fish == "Parachana")%>%
  group_by(month)%>%
  summarise(across(c("Cu":"Pb"),
                   list(~mean(.x, na.rm = T),
                        ~plotrix::std.error(.x, na.rm = T))))%>%
  mutate(across(where(is.numeric), ~round(.x, 2)))
##########
df_mn_p <- df_mn_p%>%
  unite("Cu", starts_with("Cu"), sep = plus_minus)%>%
  unite("Cd", starts_with("Cd"), sep = plus_minus)%>%
  unite("Fe", starts_with("Fe"), sep = plus_minus)%>%
  unite("Mn", starts_with("Mn"), sep = plus_minus)%>%
  unite("Ni", starts_with("Ni"), sep = plus_minus)%>%
  unite("Pb", starts_with("Pb"), sep = plus_minus)
############
df_aov <- df%>%
  filter(Fish == "Parachana")%>%
  janitor::clean_names()

formulae <- lapply(colnames(df_aov)[4:ncol(df_aov)],
                   function(x) as.formula(paste0(x, "~month")))


lapply(formulae, function(x) plot(HSD.test(aov(x, data = df_aov),"month")))
#############
df_mn_p$a <- c("a","a","a","a","a","a")
df_mn_p$b <- c("a","a","a","a","a","a")
df_mn_p$c <- c("a","a","a","a","a","a")
df_mn_p$d <- c("a","a","a","a","a","a")
df_mn_p$e <- c("a","a","a","a","a","a")
df_mn_p$f <- c("a","a","a","a","a","a")

df_mn_p%>%
  unite("Cu", c("Cu", "a"), sep = "")%>%
  unite("Cd", c("Cd","b"), sep = "")%>%
  unite("Fe", c("Fe", "c"), sep = "")%>%
  unite("Mn", c("Mn", "d"), sep = "")%>%
  unite("Ni", c("Ni", "e"), sep = "")%>%
  unite("Pb", c("Pb", "f"), sep = "")%>%
  write.csv(., "Parachana monthly.csv")

################################## Body parts
df_trt_p <- df%>%
  filter(Fish == "Parachana")%>%
  group_by(Treatment)%>%
  summarise(across(c("Cu":"Pb"),
                   list(~mean(.x, na.rm = T),
                        ~plotrix::std.error(.x, na.rm = T))))%>%
  mutate(across(where(is.numeric), ~round(.x, 2)))

df_trt_p <- df_trt_p%>%
  unite("Cu", starts_with("Cu"), sep = plus_minus)%>%
  unite("Cd", starts_with("Cd"), sep = plus_minus)%>%
  unite("Fe", starts_with("Fe"), sep = plus_minus)%>%
  unite("Mn", starts_with("Mn"), sep = plus_minus)%>%
  unite("Ni", starts_with("Ni"), sep = plus_minus)%>%
  unite("Pb", starts_with("Pb"), sep = plus_minus)
#####################################################
formulae <- lapply(colnames(df_aov)[4:ncol(df_aov)],
                   function(x) as.formula(paste0(x, "~treatment")))


lapply(formulae, function(x) plot(HSD.test(aov(x, data = df_aov),"treatment")))
#############
df_trt_p$a <- c("ab","a","b")
df_trt_p$b <- c("a","a","a")
df_trt_p$c <- c("a","a","b")
df_trt_p$d <- c("a","a","b")
df_trt_p$e <- c("a","a","a")
df_trt_p$f <- c("a","a","a")

df_trt_p%>%
  unite("Cu", c("Cu", "a"), sep = "")%>%
  unite("Cd", c("Cd","b"), sep = "")%>%
  unite("Fe", c("Fe", "c"), sep = "")%>%
  unite("Mn", c("Mn", "d"), sep = "")%>%
  unite("Ni", c("Ni", "e"), sep = "")%>%
  unite("Pb", c("Pb", "f"), sep = "")%>%
  write.csv(., "Parachana per body part.csv")
