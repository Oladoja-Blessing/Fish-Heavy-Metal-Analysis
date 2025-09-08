library(tidyverse)

df%>%
  filter(Fish == "Parachana")


df_cor_p <- df%>%
  filter(Fish == "Parachana")%>%
  corrr::correlate()
df_cor_p


df_cor_a <- df%>%
  filter(Fish == "African pike")%>%
  corrr::correlate()
df_cor_a

df <- read_csv("corr data.csv")

df%>%
  filter(Fish == "African pike")%>%
  corrr::correlate()%>%
  corrr::fashion()
  write.csv(.,"corr results.csv", row.names = F)





write.csv(df_cor_p,"corelate pike.csv", row.names = F)
write.csv(df_cor_a,"corelate snakehead.csv", row.names = F)
