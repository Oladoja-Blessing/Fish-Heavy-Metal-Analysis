########################### monthly
df_mn_a <- df%>%
  filter(Fish == "African pike")%>%
  group_by(month)%>%
  summarise(across(c("Cu":"Pb"),
                   list(~mean(.x, na.rm = T),
                        ~plotrix::std.error(.x, na.rm = T))))%>%
  mutate(across(where(is.numeric), ~round(.x, 2)))
##########
df_mn_a <- df_mn_a%>%
  unite("Cu", starts_with("Cu"), sep = plus_minus)%>%
  unite("Cd", starts_with("Cd"), sep = plus_minus)%>%
  unite("Fe", starts_with("Fe"), sep = plus_minus)%>%
  unite("Mn", starts_with("Mn"), sep = plus_minus)%>%
  unite("Ni", starts_with("Ni"), sep = plus_minus)%>%
  unite("Pb", starts_with("Pb"), sep = plus_minus)
############
df_aov <- df%>%
  filter(Fish == "African pike")%>%
  janitor::clean_names()

formulae <- lapply(colnames(df_aov)[4:ncol(df_aov)],
                   function(x) as.formula(paste0(x, "~month")))


lapply(formulae, function(x) plot(HSD.test(aov(x, data = df_aov),"month")))
#############
df_mn_a$a <- c("a","a","a","a","a","a")
df_mn_a$b <- c("a","a","a","a","a","a")
df_mn_a$c <- c("a","a","a","a","a","a")
df_mn_a$d <- c("a","a","a","a","a","a")
df_mn_a$e <- c("a","a","a","a","a","a")
df_mn_a$f <- c("a","a","a","a","a","a")

df_mn_a%>%
  unite("Cu", c("Cu", "a"), sep = "")%>%
  unite("Cd", c("Cd","b"), sep = "")%>%
  unite("Fe", c("Fe", "c"), sep = "")%>%
  unite("Mn", c("Mn", "d"), sep = "")%>%
  unite("Ni", c("Ni", "e"), sep = "")%>%
  unite("Pb", c("Pb", "f"), sep = "")%>%
  write.csv(., "Pike monthly.csv")

################################## Body parts
df_trt_a <- df%>%
  filter(Fish == "African pike")%>%
  group_by(Treatment)%>%
  summarise(across(c("Cu":"Pb"),
                   list(~mean(.x, na.rm = T),
                        ~plotrix::std.error(.x, na.rm = T))))%>%
  mutate(across(where(is.numeric), ~round(.x, 2)))

df_trt_a <- df_trt_a%>%
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
df_trt_a$a <- c("a","a","a")
df_trt_a$b <- c("a","a","a")
df_trt_a$c <- c("a","a","b")
df_trt_a$d <- c("a","a","a")
df_trt_a$e <- c("a","a","a")
df_trt_a$f <- c("a","a","a")

df_trt_a%>%
  unite("Cu", c("Cu", "a"), sep = "")%>%
  unite("Cd", c("Cd","b"), sep = "")%>%
  unite("Fe", c("Fe", "c"), sep = "")%>%
  unite("Mn", c("Mn", "d"), sep = "")%>%
  unite("Ni", c("Ni", "e"), sep = "")%>%
  unite("Pb", c("Pb", "f"), sep = "")%>%
  write.csv(., "Pike per body part.csv")
