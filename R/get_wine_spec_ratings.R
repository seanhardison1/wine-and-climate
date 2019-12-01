library(tidyverse)
library(rvest)
library(svMisc)
library(stringi)

#get wine rating from wine spectator
library(RSelenium)
remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4567L,
  browserName = "safari"
)
remDr$open()

url <- "https://www.winespectator.com/auth/login?returnpage=https://www.winespectator.com/wine/search/recall/yes/page/1/sort_by/vintage/sort_dir/asc"
remDr$navigate(url)

username <- remDr$findElement("id","userid")
username$sendKeysToElement(list("seanhardison@gmail.com"))

pss <- remDr$findElement("id","passwd")
pss$sendKeysToElement(list("vir6rtfpa", key = "enter"))
remDr$addCookie(name = "mycookie", value = "1")
url2 <- "https://www.winespectator.com/wine/search?submitted=Y&page=1&winery=central%20coast&text_search_flag=wine_plus_vintage&search_by=all&scorelow=-1&scorehigh=-1&pricelow=-1&pricehigh=-1&case_prod=null_case_prod&taste_date=&issue_date=&issue_year=&varietal%5B%5D=null_varietal&regions%5B%5D=null_regions&vintage%5B%5D=&size=15&sort_by=score&sort_dir=desc"
remDr$navigate(url2)


search1 <- remDr$findElement(using = "css selector", "input")
search1$highlightElement()
search1$clickElement()
search1$sendKeysToElement(list("central coast"))


  wine_html %>%
      html_node("article") %>% 
      html_node("form") %>% 
      html_text()
  
wine_html %>% 
    html_node("body") %>%
    html_node("main") %>%
    html_node("article") %>%
    html_nodes("form")

rD <- rsDriver()
remDr <- rD[["client"]]
remDr$navigate("http://www.google.com/ncr")
