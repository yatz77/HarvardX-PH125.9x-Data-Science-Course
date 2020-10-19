library(tidyverse)
library(pdftools)
options(digits = 3) # report 3 significant digits
library(dslabs)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package = "dslabs")
system("cmd.exe", input = paste("start", fn))

txt <- pdf_text(fn)
head(txt)

x <- txt[9] %>% str_split("\n")
class(x)
x

s <- x[[1]]
s <- str_trim(s)
s[1]

header_index <- str_which(s, "2015")[1]
header_index

header <- s[header_index] %>% str_split("\\s+", simplify = T)
month <- header[1]
header <- header[-1]
month
header

tail_index <- str_which(s, "Total")
tail_index

n <- str_count(s, pattern = "\\d+")
sum(n == 1)
p <- str_which(n, "1")
st <- s[-(tail_index:40)]
st <- st[-str_which(n, "1")[2]]
st <- st[-str_which(n, "1")[1]]
st <- st[-(header_index)]
st

out <- c(1:header_index, which(n==1), tail_index:length(s))
s <- s[-out]
length(s)

s <- str_remove_all(s, "[^\\d\\s]")
s <- str_split_fixed(s, "\\s+", n = 6)[,1:5]

colnames(s) <- c("day",header)
tab <- cbind(s, c(replicate(30,month)))                                  
tab
class(s[1,1])
s <- mapply(s, FUN = as.numeric)
s <- matrix(data=s, ncol=5, nrow=30)

mean(s[20:30,4])

tab <- s %>% 
  as_data_frame() %>% 
  setNames(c("day", header)) %>%
  mutate_all(as.numeric)

tab <- tab %>% gather(year, deaths, -day) %>%
  mutate(deaths = as.numeric(deaths))

tab %>% filter(year != 2018) %>%
  ggplot(aes(day, deaths, color = year)) +
  geom_line() +
  geom_vline(xintercept = 20) +
  geom_point()
