# ----- Initialization -----
# Loading required libraries
require(rvest)
# Creation of objects required 
euractiv <- data.frame()
end <- FALSE
i <- 1

# ----- Webscraping data -----
while (end == FALSE) {
  url <- paste(c("https://www.euractiv.com/page/", i, "/?s=democra+eu+reform&year=2014&orderby=post_date&order=ASC"), collapse = "")
  pagina <- try(read_html(url))
  
  # Continuity test
  node <- ".text-danger"
  test <- tryCatch(pagina %>% html_nodes(node) %>% html_text(),
                               warning = function(w) {print("Fail in 1"); return(NA)})
    if (length(test) != 0) { end <- TRUE; break }
  
  # headline
  node <- "h3 a"
  headline <- tryCatch(pagina %>% html_nodes(node) %>% html_text(),
                       warning = function(w) {print("Fail in 1"); return(NA)})
  # content type
  node <- ".clearfix a:nth-child(1)"
  content.type <- tryCatch(pagina %>% html_nodes(node) %>% html_text(),
                           warning = function(w) {print("Fail in 1"); return(NA)})
  # topic
  node <- "#main_container a+ a"
  topic <- tryCatch(pagina %>% html_nodes(node) %>% html_text(),
                    warning = function(w) {print("Fail in 1"); return(NA)})
  # date
  node <- ".ea-dateformat"
  date <- tryCatch(pagina %>% html_nodes(node) %>% html_text(),
                   warning = function(w) {print("Fail in 1"); return(NA)})
  # summary
  node <- ".excerpt p"
  summary <- tryCatch(pagina %>% html_nodes(node) %>% html_text(),
                      warning = function(w) {print("Fail in 1"); return(NA)})
  # hyperlink
  node <- "h3 a"
  hyperlink <- tryCatch(pagina %>% html_nodes(node) %>% html_attr("href"),
                      warning = function(w) {print("Fail in 1"); return(NA)})
  
  subset <- cbind(headline, content.type, topic, date, summary, hyperlink)
  subset <- as.data.frame(subset, stringsAsFactors = FALSE)
  
  euractiv <- rbind(euractiv, subset)

  i <- i+1
}
closeAllConnections()
# Removing entries without hyperlink
euractiv <- euractiv[!is.na(euractiv$hyperlink), ]
euractiv <- euractiv[!duplicated(euractiv$headline),]

# filtering 
table(euractiv$content.type)
index <- grep(pattern = 'video', x = euractiv$content.type, ignore.case = TRUE)
euractiv$hyperlink[index]


table(euractiv$topic)
euractiv <- subset(x = euractiv, subset = topic != 'Transport')
euractiv <- subset(x = euractiv, subset = topic != 'Health')
euractiv <- subset(x = euractiv, subset = topic != 'Agrifood')


# ----- individual extraction -----
euractiv$source <- NA
euractiv$full.text <- NA
euractiv$promoted <- NA
# Extracting especific data
for (i in 1:nrow(euractiv)) {
  url <- euractiv$hyperlink[i]
  pagina <- try(read_html(url))
  # source
  node <- ".ea-article-header .ea-article-meta"
  euractiv$source[i] <- tryCatch(pagina %>% html_nodes(node) %>% html_text(),
                                            warning = function(w) {print("Fail in 1"); return(NA)})
  # full.text
  node <- ".ea-article-body-content"
  euractiv$full.text[i] <- tryCatch(pagina %>% html_nodes(node) %>% html_text(),
                                         warning = function(w) {print("Fail in 1"); return(NA)})
  # Is promoted
  node <- '#main_container b'
  temp <- tryCatch(pagina %>% html_nodes(node) %>% html_text(),
                                   warning = function(w) {print("Fail in 1"); return(NA)})
  if (length(temp) != 0) {
    euractiv$promoted[i] <- temp[1]
  }
}

# ----- Cleaning entries -----
### Date
# Removing dobuble spaces
index <- grep(pattern = "[[:space:]]+", x = euractiv$date)
euractiv$date[index] <-  trimws(gsub(pattern = "[[:space:]]+", replacement = " ", x = euractiv$date[index]), which = "both")

### Source
# Removing dobuble spaces
index <- grep(pattern = "[[:space:]]+", x = euractiv$source)
euractiv$source[index] <-  trimws(gsub(pattern = "[[:space:]]+", replacement = " ", x = euractiv$source[index]), which = "both")

### Full text
# Removing dobuble spaces
euractiv$clean.text <- euractiv$full.text
index <- grep(pattern = "[[:space:]]+", x = euractiv$clean.text)
euractiv$clean.text[index] <-  trimws(gsub(pattern = "[[:space:]]+", replacement = " ", x = euractiv$clean.text[index]), which = "both")

# Promoted content
index <- grep(pattern = 'promoted', x = euractiv$promoted, ignore.case = TRUE)
euractiv$promoted <- FALSE
euractiv$promoted[index] <- TRUE

# test

# Here is a change

# ----- Closing project -----
# Removing objects
rm(pagina)
rm(subset)
rm(content.type)
rm(date)
rm(end)
rm(headline)
rm(hyperlink)
rm(i)
rm(index)
rm(node)
rm(summary)
rm(test)
rm(topic)
rm(url)

# Storing results
save(file = "output/1_euractiv_14.Rdata", list = "euractiv")
write.csv(x = euractiv, file = "output/euractiv_14.csv")

euractiv$id <- 1:nrow(euractiv)

for (u in 1:nrow(euractiv)) {
  name <- paste(c('output/ind/art_', euractiv$id[u], '.txt'), collapse = '')
  write(x = euractiv$full.text[u], file = name)
}
