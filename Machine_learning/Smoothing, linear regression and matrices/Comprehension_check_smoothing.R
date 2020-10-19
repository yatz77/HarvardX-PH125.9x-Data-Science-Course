library(tidyverse)
library(lubridate)
library(purrr)
library(pdftools)
?str_which
fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
  s[-out] %>%
    str_remove_all("[^\\d\\s]") %>%
    str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>%
    .[,1:5] %>%
    as_data_frame() %>% 
    setNames(c("day", header)) %>%
    mutate(month = month,
           day = as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths = as.numeric(deaths))
}) %>%
  mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                        "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
  mutate(date = make_date(year, month, day)) %>%
  filter(date <= "2018-05-01")
dat <- na.omit(dat)

z <- max(dat$date)-min(dat$date)
span_2m <- as.numeric(z)/365*12
x <- as.numeric(dat$date)
y <- dat$deaths
class(x)
class(y)
anyNA(dat$deaths)
fit <- loess(y ~ x, degree = 1, span = 0.05)
qplot(dat$date, fit$fitted)
dat  %>%
  mutate(smooth = fit$fitted) %>%
  ggplot(aes(date, deaths)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(date, smooth), color="red")


# smooth loess through ggplot
dat %>% ggplot(aes(date, deaths)) +
  geom_point() +
  geom_smooth(color="red", span = 0.05, method.args = list(degree=1))

#loess fit for each year
dat %>% 
  mutate(smooth = predict(fit, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
  ggplot(aes(day, smooth, col = year)) +
  geom_line(lwd = 2)


# predictive power of second covariate for 2 or 7 digital reader
library(broom)
mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy()
qplot(x_2, y, data = mnist_27$train)

fit <-  mnist_27$train %>% loess(as.numeric(y) ~ x_2, span = 0.2, degree = 1, data = .)
mnist_27$train %>% mutate(smooth = predict(fit, x_2)) %>%
  ggplot() +
  geom_point(aes(x_2, y)) +
  geom_line(aes(x_2, smooth), lwd = 2, col = "red")

mnist_27$train %>% 
  mutate(y = ifelse(y=="7", 1, 0)) %>%
  ggplot(aes(x_2, y)) + 
  geom_smooth(method = "loess")
