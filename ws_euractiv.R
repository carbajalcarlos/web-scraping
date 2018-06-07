# ----- Initialization -----
# Loading required libraries
require(rvest)

# Creation of objects required 
euractiv.dataset <- data.frame()
end <- FALSE
i <- 1

# Webscraping data
while (end == FALSE) {
  url <- paste(c("https://www.euractiv.com/page/", i, "/?s=democracy+eu&year=2018&orderby=post_date&order=ASC"), collapse = "")
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
  
  euractiv.dataset <- rbind(euractiv.dataset, subset)

  i <- i+1
}
closeAllConnections()

# Removing entries without hyperlink
euractiv.dataset <- euractiv.dataset[!is.na(euractiv.dataset$hyperlink), ]

# Extracting full text
euractiv.dataset$source <- NA
euractiv.dataset$full.text <- NA

# Extracting especific data
for (i in 1:nrow(euractiv.dataset)) {
  url <- euractiv.dataset$hyperlink[i]
  pagina <- try(read_html(url))
  # source
  node <- ".ea-article-header .ea-article-meta"
  euractiv.dataset$source[i] <- tryCatch(pagina %>% html_nodes(node) %>% html_text(),
                                            warning = function(w) {print("Fail in 1"); return(NA)})
  # full.text
  node <- ".ea-article-body-content"
  euractiv.dataset$full.text[i] <- tryCatch(pagina %>% html_nodes(node) %>% html_text(),
                                         warning = function(w) {print("Fail in 1"); return(NA)})
}

# Storing results
save(file = "output/1_euractiv.Rdata", list = "euractiv.dataset")
write.csv(x = euractiv.dataset, file = "output/euractiv.csv")
