# ===== 0_Initialization =====
# Loading required libraries
require(rvest)
# Creation of objects required 
politico <- data.frame()

# ===== 1_Webscraping headlines =====
# Extracting number of pages
year_sta <- 2014 # From January 01
year_end <- 2014 # Until December 31.

url <- paste(c("https://www.politico.eu/page/1/?s=EU&sp%5Bf%5D=01%2F01%2F", 
               year_sta, "&sp%5Bt%5D=12%2F31%2F", year_end, 
               "&orderby=date&order=asc"), collapse = "")
pagina <- try(read_html(url))
node <- "li:nth-child(3) .page-numbers"
num_pag <- as.integer(tryCatch(pagina %>% html_nodes(node) %>% html_text(),
                               warning = function(w) {print("Fail in 1"); return(NA)}))

# Headlines and summary extracting sequence
for (i in 1:num_pag) {
  # website access
  url <- paste(c("https://www.politico.eu/page/", i, "/?s=EU&sp%5Bf%5D=01%2F01%2F", 
                 year_sta, "&sp%5Bt%5D=12%2F31%2F", year_end, 
                 "&orderby=date&order=asc"), collapse = "")
  pagina <- try(read_html(url))
  
  # headline
  node <- "h3 a"
  headline <- tryCatch(pagina %>% html_nodes(node) %>% html_text(),
                       warning = function(w) {print("Fail in 1"); return(NA)})
  # date
  node <- "time"
  date <- tryCatch(pagina %>% html_nodes(node) %>% html_text(),
                   warning = function(w) {print("Fail in 1"); return(NA)})
  date <- tail(x = date, n = length(headline))
  
  # summary
  node <- ".format-ml .tease p"
  summary <- tryCatch(pagina %>% html_nodes(node) %>% html_text(),
                      warning = function(w) {print("Fail in 1"); return(NA)})
  # hyperlink
  node <- "h3 a"
  hyperlink <- tryCatch(pagina %>% html_nodes(node) %>% html_attr("href"),
                        warning = function(w) {print("Fail in 1"); return(NA)})
  
  subset <- cbind(headline, date, summary, hyperlink)
  subset <- as.data.frame(subset, stringsAsFactors = FALSE)
  
  politico <- rbind(politico, subset)
  
  # Printing progress
  if (i == 1) {
    print(paste("Number of pages:", num_pag))
  }
  if (i%%5 == 0) {
    print(paste(c("Progress: ", i, "/", num_pag), collapse = ""))
  }
}

# Security feature
closeAllConnections()

# Removing entries without hyperlink
politico <- politico[!is.na(politico$hyperlink), ]
# Removing duplicated entries
politico <- politico[!duplicated(politico$headline),]

# ===== 2_Filtering =====

#===> This section requires to filter the articles that you would like to extract
#===> From the dataset available at the moment, you can search in the headline or
#===> the summary for the keywords that you mentioned before.

# Your text message:  Basically, I need all articles containing EU and 
#                     democra* or reform* or institut* or future 

# This is what I did with euractiv website:

# table(euractiv$content.type)
# index <- grep(pattern = 'video', x = euractiv$content.type, ignore.case = TRUE)
# euractiv$hyperlink[index]
# 
# 
# table(euractiv$topic)
# euractiv <- subset(x = euractiv, subset = topic != 'Transport')
# euractiv <- subset(x = euractiv, subset = topic != 'Health')
# euractiv <- subset(x = euractiv, subset = topic != 'Agrifood')

# ===== 3_Individual entries extraction =====
politico$source <- NA
politico$author <- NA
politico$full.text <- NA
politico$categories <- NA

# Extracting especific data
for (i in 1:nrow(politico)) {
  url <- politico$hyperlink[i]
  pagina <- try(read_html(url))
  # Source
  node <- "p.byline"
  ws <- tryCatch(pagina %>% html_nodes(node) %>% html_text(),
                                 warning = function(w) {print("Fail in 1"); return(NA)})
  politico$source[i] <- tail(x = ws, n = 1)
  
  # Author
  node <- ".credits-author .fn"
  ws <- tryCatch(pagina %>% html_nodes(node) %>% html_text(),
                                warning = function(w) {print("Fail in 1"); return(NA)})
  if (length(ws) > 0) {
    politico$author[i] <- tail(x = ws, n = 1)
  }
  
  # Full-text
  node <- ".has-sidebar > p"
  ws <- tryCatch(pagina %>% html_nodes(node) %>% html_text(),
                                    warning = function(w) {print("Fail in 1"); return(NA)})
  politico$full.text[i] <- paste(ws, collapse  = " ")
  
  # Categories
  node <- ".categories-list a"
  ws <- tryCatch(pagina %>% html_nodes(node) %>% html_text(),
                                            warning = function(w) {print("Fail in 1"); return(NA)})
  politico$categories[i] <- paste(ws, collapse  = ";   ")
 
   # Printing progress
  if (i == 1) {
    print(paste("Number of entries:", nrow(politico)))
  }
  if (i%%10 == 0) {
    print(paste(c("Progress: ", i, "/", nrow(politico)), collapse = ""))
  }
}

# Security feature
closeAllConnections()

# ===== 4_Cleaning entries =====
### Date
# Removing dobuble spaces
index <- grep(pattern = "[[:space:]]+", x = politico$date)
politico$date[index] <-  trimws(gsub(pattern = "[[:space:]]+", replacement = " ", x = politico$date[index]), which = "both")

### Full text
# Removing dobuble spaces
politico$clean.text <- politico$full.text
index <- grep(pattern = "[[:space:]]+", x = politico$clean.text)
politico$clean.text[index] <-  trimws(gsub(pattern = "[[:space:]]+", replacement = " ", x = politico$clean.text[index]), which = "both")

# Generating entries ID
politico$id <- 1:nrow(politico)

# ===== 5_Closing project =====
# Removing objects
rm(pagina)
rm(subset)
rm(date)
rm(headline)
rm(hyperlink)
rm(i)
rm(index)
rm(node)
rm(num_pag)
rm(summary)
rm(test)
rm(url)
rm(ws)

# Storing results change the name accordingly
name <- paste(c("output/politico_", year_sta, "-", year_end, ".Rdata"), 
              collapse = "")
save(file = name, list = "politico")
name <- paste(c("output/politico_", year_sta, "-", year_end, ".csv"), 
              collapse = "")
write.csv(x = politico, file = name)

#Extracting entries in individual text files.
for (u in 1:nrow(politico)) {
  name <- paste(c('output/ind_pol/art_', politico$id[u], '.txt'), collapse = '')
  write(x = politico$full.text[u], file = name)
}
