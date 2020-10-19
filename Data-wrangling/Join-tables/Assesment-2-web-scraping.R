library(rvest)
library(tidyverse)
library(dplyr)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)

nodes <- html_nodes(h, "table")
html_text(nodes[[8]])
html_table(nodes[[8]])

tab1 <- html_table(nodes[[1]])
tab2 <- html_table(nodes[[2]])
tab3 <- html_table(nodes[[3]])
tab4 <- html_table(nodes[[4]])

tab1
tab2
tab3
tab4

tab19 <- html_table(nodes[[19]])
tab20 <- html_table(nodes[[20]])
tab21 <- html_table(nodes[[21]])

head(tab19)
head(tab20)
head(tab21)
class(tab19)

tab_1 <- html_table(nodes[[10]])
tab_2 <- html_table(nodes[[19]])
head(tab_1)
head(tab_2)

class(tab_1)

tab_1 <- tab_1[-1,-1]
colnames(tab_1) <- c("Team", "Payroll", "Average")
tab_2 <- tab_2[-1,]
colnames(tab_2) <- c("Team", "Payroll", "Average")
tab_comb <- full_join(tab_1,tab_2, by = "Team")

nrow(tab_comb)

url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
h <- read_html(url)

tab <- read_html(url) %>% html_nodes("table")
length(tab)

head(html_table(tab[[5]],fill = T))
