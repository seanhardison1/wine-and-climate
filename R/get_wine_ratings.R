library(tidyverse)
library(rvest)
library(svMisc)


wine_ratings <- NULL
for (i in 1:79657){
  message(i)
  progress(i, progress.bar = TRUE)
  url <- paste0("https://www.winemag.com/wine-ratings/",i,"/?s=&country=US&state=California&page=",i)
  wine_html <- read_html(url)
  
  titles <- 
    wine_html %>% 
    html_node('body') %>% 
    html_nodes(".title") %>% 
    html_text() %>% 
    as.data.frame() %>% 
    mutate(index = rep(c(1,2),length(.)/2)) %>% 
    filter(index == 1) %>% 
    dplyr::rename(titles = 1) %>% 
    pull(titles) %>% 
    as.character()

  years <- NULL
  for (j in 1:length(titles)){
    if (str_detect(titles[j], "\\d{4}")){
      years[j] <- 
        str_extract(titles[j], "\\d{4}")
    } else {
      years[j] <- NA
    }
  }
  
  
  ratings <- 
    wine_html %>% 
    html_node('body') %>% 
    html_nodes(".rating ") %>% 
    html_text() 
  
  location <- 
    wine_html %>% 
    html_node('body') %>% 
    html_nodes(".appellation") %>% 
    html_text()

  if (length(location) != length(titles)) location <- location[c(T,F)]
  
  if (length(ratings) == length(location) & length(location) == length(titles) & length(titles) == length(years)){
    
    wine_df <- data.frame(
      titles = titles,
      years = years,
      ratings = ratings,
      location = location
    )
    
    assign("wine_ratings", rbind(wine_ratings,wine_df))
    
  } else {
    print(paste(length(ratings),
                length(location) ,
                length(titles),
                length(years), "skipping to next page"))
    next
  }
  
  #Do some waiting around so you don't accidentally crash their servers
  # if (i %% 100 == 0 & i != 1){
    date_time<-Sys.time()
    while((as.numeric(Sys.time()) - as.numeric(date_time))<2){}
  #   message("Waiting...")
  # 
  # }
  
}
