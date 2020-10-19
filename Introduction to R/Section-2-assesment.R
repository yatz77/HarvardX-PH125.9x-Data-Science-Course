x <- c(2, 43, 27, 96, 18)
order(x)
rank(x)
sort(x)
min(x)

name <- c("Mandi", "Amy", "Nicole", "Olivia")
distance <- c(0.8, 3.1, 2.8, 4.0)
time <- c(10, 30, 40, 50)
time <- time/60
speed <- distance / time
time[4]
speed[1]
speeds <- data.frame(names = name, distances = distance, time = time, speed = speed)
speeds
