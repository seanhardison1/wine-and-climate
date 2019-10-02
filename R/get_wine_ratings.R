library(tidyverse)
library(rvest)
library(svMisc)


wine_ratings <- NULL
for (i in 1:500){
  progress(i, progress.bar = TRUE)
  url <- paste0("https://www.winemag.com/wine-ratings/?s=&drink_type=wine&page=",i,"&sort_by=pub_date_web&sort_dir=desc")
  wine_html <- read_html(url)
  
  titles <- 
    wine_html %>% 
    html_node('body') %>% 
    html_nodes(".title") %>% 
    html_text() %>% 
    unique()
  
  years <- 
    str_extract(titles, "\\d{4}")
  
  ratings <- 
    wine_html %>% 
    html_node('body') %>% 
    html_nodes(".rating ") %>% 
    html_text() 
  
  wine_df <- data.frame(
    titles = titles,
    years = years,
    ratings = ratings
  )
  
  assign("wine_ratings", rbind(wine_ratings,wine_df))

  date_time<-Sys.time()
  
  while((as.numeric(Sys.time()) - as.numeric(date_time))<1.5){}
}
