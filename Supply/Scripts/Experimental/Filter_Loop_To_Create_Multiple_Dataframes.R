df <- data.frame(topic = c("xxx", "xxx", "yyy", "yyy", "yyy", "zzz", "zzz"), 
                 high = c(52L, 27L, 89L, 99L, 43L, 21L, 90L), 
                 low = c(56L, 98L, 101L, 21L, 98L, 40L, 43L), 
                 stringsAsFactors = FALSE)

library(dplyr)


for (variable in unique(df$topic)) {
  assign(variable, df %>% filter (topic == variable), envir = .GlobalEnv)
}
