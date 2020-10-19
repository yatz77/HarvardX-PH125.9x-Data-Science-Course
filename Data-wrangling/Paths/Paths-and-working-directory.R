# see working directory
getwd()

# change your working directory 
setwd("C:/Users/yanni/R-projects/Data-wrangling/Paths")


# set path to the location for raw data files in the dslabs package and list files
path <- system.file("extdata", package="dslabs")
list.files(path)

# generate a full path to a file
filename <- "murders.csv"
fullpath <- file.path(path, filename)

# copy file from dslabs package to your working directory
file.copy(fullpath, getwd())

# check if file exists
file.exists(filename)

library(dslabs)
library(tidyverse) # includes readr
library(readxl)

# inspect the first 3 lines
read_lines("murders.csv", n_max = 3)

# read file in CSV-format
dat <- read_csv(filename)

# rad using full path
dat <- read_csv(fullpath)
filename
head(dat)

#Ex:
path <- system.file("extdata", package = "dslabs")
files <- list.files(path)
files

filename <- "murders.csv"
filename1 <- "life-expectancy-and-fertility-two-countries-example.csv"
filename2 <- "fertility-two-countries-example.csv"
dat = read.csv(file.path(path, filename))
dat1 = read.csv(file.path(path, filename1))
dat2 = read.csv(file.path(path, filename2))

# filename is defined in the previous video
# read.csv converts strings to factors
dat2 <- read.csv(filename)
class(dat2$abb)
class(dat2$region)

url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"
dat <- read_csv(url)
download.file(url, "murders.csv")
tempfile()
tmp_filename <- tempfile()
download.file(url, tmp_filename)
dat <- read_csv(tmp_filename)
file.remove(tmp_filename)
head(dat)

read_csv("http://mlr.cs.umass.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data")

dat4<-read_csv("http://mlr.cs.umass.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data", col_names = F)
nrows(dat4)
dat4
