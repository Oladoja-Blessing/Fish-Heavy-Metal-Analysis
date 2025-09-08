library(tidyverse)
library(readxl)
water <- read_excel("water.xlsx")
View(water)

df <- read_excel("water.xlsx", sheet = "Sheet4")

library(corrr)

calc_p_value <- function(vec_a, vec_b){
  test_res <- cor.test(vec_a, vec_b)
  sig <- if (test_res$p.value < 0.05) { "*"} 
else if (test_res$p.value < 0.01) {
    "**"
  }
  else if (test_res$p.value < 0.001) {
    "***"
  }
  else("")
  paste0(round(cor.test(vec_a, vec_b)$estimate, 2), sig)
}

colpair_map(df, calc_p_value)%>%
  shave()%>%
  fashion()%>%
  write.csv(., "new correlation result.csv", row.names = F)

  
