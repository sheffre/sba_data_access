#plot

library(ggplot2)

ggplot(data = df, mapping = aes(x = timestamp,
                                y = co2_partial_pressure)) +
  geom_point() +
  geom_line()


df$min <- format(as.POSIXct(df$timestamp), format = "%M")
df$hour <- format(as.POSIXct(df$timestamp), format = "%H")

avg_minute <- data.frame()

for (j in levels(as.factor(df$hour))) {
  sub <- subset(df, hour == j)
  for (i in levels(as.factor(sub$min))) {
    sub2 <- subset(sub, min == i)
    avg_minute[j,i] <- as.numeric(mean(sub2$co2_partial_pressure))
  }
}

avg_5_min <- matrix() 

for(k in c(1:12)) {
  for (j in c(1:nrow(avg_minute))) {
    for (i in seq(1, 60, by = 5)) {
      temp <- avg_minute[j, c(i, i+4)]
      avg_5_min[j,k] <- mean(as.numeric(temp, na.rm = T)
    }
  }
}

# sub <- subset(df, hour == 15)
# sub2 <- subset(sub, min == i)
# mean(sub2$co2_partial_pressure)
mean(as.numeric(avg_minute[2, c(1:5)]))

