# Loading required libraries
require(rvest)

url <- "http://jcr.incites.thomsonreuters.com/JCRJournalHomeAction.action"
pagina <- try(read_html(url))

# headline
node <- ".x-grid-cell-gridcolumn-1011"
jcr <- tryCatch(pagina %>% html_nodes(node) %>% html_text(),
                     warning = function(w) {print("Fail in 1"); return(NA)})
