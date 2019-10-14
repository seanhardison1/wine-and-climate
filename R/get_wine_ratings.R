library(tidyverse)
library(rvest)
library(svMisc)
library(stringi)

scrape_ratings <- function(init){
  
  out <- tryCatch({
    
    message(init)
    # Sys.sleep(2)
    url <- paste0("https://www.winemag.com/?s=California&drink_type=wine&page=",init,"&search_type=reviews")
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
    
  buying_guide <-
    str_to_lower(titles) %>% 
    str_remove_all("[^A-Za-z0-9|\\s+|-|è|é|ń|ñ|ê]") %>% 
    str_replace_all("\\s+", "-") %>%
    paste0("https://www.winemag.com/buying-guide/",.)
    
  variety <- NULL
  for (j in 1:length(buying_guide)){
    bg_url <- stringi::stri_trans_general(buying_guide[j],"Latin-ASCII")
    info_html <- read_html(bg_url)
    
   bg_variety =  info_html %>% 
      html_node('body') %>% 
      html_nodes('.info-section , span') %>% 
      html_text()
   
   inter <- bg_variety[which(bg_variety == "Variety") + 1]
   
     if (!identical(inter, character(0))){
       variety[j] <- inter
      } else {
       variety[j] <- bg_url
     }
  }
  
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

      
      return(
        data.frame(
          titles = titles,
          years = years,
          ratings = ratings,
          location = location,
          variety = variety
        )
      )
      
    } else {
      print(paste(length(ratings),
                  length(location) ,
                  length(titles),
                  length(years), "skipping to next page"))
      stop("Scraped components must be of equal length")
    }
    
  },
  error = function(cond) {
    message("Scraper broken!")
    return(init)
  })
  
  return(out)
}

#get total number of pages
wine_html <- read_html("https://www.winemag.com/?s=California&drink_type=wine&page1&search_type=reviews")

pages <- 
  wine_html %>% 
    html_node('body') %>% 
    html_nodes(".pagination") %>% 
    html_text() %>% 
    stringr::str_extract("\\d{4}") %>% 
    stringr::str_remove(",") %>% 
    as.numeric()

ratings1 <- NULL
ratings2 <- NULL
ratings3 <- NULL
ratings4 <- NULL
ratings5 <- NULL
ratings6 <- NULL
ratings7 <- NULL
ratings8 <- NULL
ratings9 <- NULL
ratings10 <- NULL

vec <- seq(1, pages, pages/10)
vec[11] <- pages
vec <- floor(vec)

for (i in 6:10){
  
  df <- lapply(vec[i]:vec[i + 1], scrape_ratings)
  df <- do.call(rbind.data.frame, df)
  assign(paste0("ratings",i),rbind(get(paste0("ratings",i)), df))
  Sys.sleep(10)

}

cal_wines <- bind_rows(ratings1, ratings2, ratings3, ratings4,ratings5, ratings6, ratings7) %>%
  mutate(titles = as.character(titles),
         years = as.numeric(years),
         ratings = as.numeric(str_extract(ratings, "\\d{2}"))) %>% 
  filter(!is.na(title))

save(cal_wines, file = here::here('data/cal_wines_beta.rdata'))
