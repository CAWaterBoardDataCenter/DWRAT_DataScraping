df <- data.frame("mytext" = as.character(row.names(mtcars)))
head(df)

#number of columns after splitting by space
ncols <- max(stringr::str_count(df$mytext, " ")) + 1
ncols

#generate necessary column names
colmn <- paste("col", 1:ncols)
colmn

df <-
  tidyr::separate(
    data = df,
    col = mytext,
    sep = " ",
    into = colmn,
    remove = FALSE
  )

require(dplyr) 
head(df) %>% knitr::kable()