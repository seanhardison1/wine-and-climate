library(tidyverse)
library(rvest)
library(svMisc)

scrape_ratings <- function(init){
  
  out <- tryCatch({
    
    message(init)
    Sys.sleep(2)
    url <- paste0("https://www.winemag.com/wine-ratings/",init,"/?s=&country=US&state=California&page=",init)
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

      
      return(
        data.frame(
          titles = titles,
          years = years,
          ratings = ratings,
          location = location
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
wine_html <- read_html("https://www.winemag.com/wine-ratings/1/?s=&country=US&state=California&page=1")

pages <- 
  wine_html %>% 
    html_node('body') %>% 
    html_nodes(".results-count") %>% 
    html_text() %>% 
    stringr::str_extract("\\d{3},\\d{3}") %>% 
    stringr::str_remove(",") %>% 
    as.numeric()

ratings <- lapply(1:pages, scrape_ratings)


