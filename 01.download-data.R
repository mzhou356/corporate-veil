# load required libraries
library(checkmate)
library(dplyr)
library(httr)
library(jsonlite)
library(textclean)
library(varhandle)

# specify token, username, and password
username <- "dcbarnard"
password <- "9KjvHViweMbyLRT9kukl"

# set global httr configuration
set_config(authenticate(username,password),override = FALSE)

# specify baseURL for use in searching for the relevant cases
subStr1 <- "https://www.courtlistener.com/api/rest/v3/search/?q="
subStr2 <- "(%22personal+injury%22+OR+death)+AND"
subStr3 <- "(%22pierc*+veil%22~3+OR+%22disregard*+corpor*%22~5)"
subStr4 <- "&type=o&order_by=score+desc&stat_Precedential=on"
baseURL <- paste0(subStr1,subStr2,subStr3,subStr4)

# download metadata regarding the identified cases
i <- 1
pages <- list()
newURL <- paste0(baseURL,"&page=","1")
while(testNull(newURL) != TRUE){
  httpResponse <- GET(newURL,content_type_json())
  newdata <- fromJSON(content(httpResponse,"text"))
  pages[[i]] <- newdata$results
  i <- i + 1
  newURL <- newdata$`next`
}
metadata <- rbind_pages(pages)

# download the text of the relevant opinions
id <- metadata$id
count <- length(id)
text <- vector(length = count)
url1 <- "https://www.courtlistener.com/api/rest/v3/opinions/"
url3 <- "/?format=json"
for (i in 1:count){
  url2 <- id[i]
  newURL <- paste0(url1,url2,url3)
  httpResponse <- GET(newURL,content_type_json())
  newdata <- fromJSON(content(httpResponse,"text"))
  htmlOpinion <- newdata$html_with_citations
  textOpinion <- newdata$plain_text
  if (textOpinion != "") text[i] <- textOpinion else text[i] <- htmlOpinion
}
opinions <- data_frame(doc_id = id, text = text)

# remove html from the downloaded opinions
opinions$text <- replace_html(opinions$text)

# clean up the workspace
rm.all.but(c("metadata", "opinions"))

# save the workspace
save.image("~/OneDrive/Projects/corporate-veil/data/download-data.RData")