#plot

library(ggplot2)

ggplot(data = df, mapping = aes(x = timestamp,
                                y = co2_partial_pressure)) +
  geom_point() +
  geom_line()

df <- getter(con)
df$timestamp <- as.POSIXct(df$timestamp, tz = "Europe/Moscow")
df$timestamp_char <- as.character(df$timestamp)
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

colnames(avg_minute)

df$timestamp_rounded = round_date(df$timestamp, "5 minutes")
result <- aggregate(co2_partial_pressure ~ timestamp_rounded, data = df, FUN = mean)

df %>%
  mutate(year = year(timestamp), month = month(timestamp), day = day(timestamp),
         hour = hour(timestamp), minute = minute(timestamp)) %>%
  group_by(year, month, day, hour, minute) %>%
  summarise(mean_var = mean(var))


ggplot(data = result, mapping = aes(x = timestamp_rounded,
                                y = co2_partial_pressure)) +
  geom_point() +
  geom_line()



# sub <- subset(df, hour == 15)
# sub2 <- subset(sub, min == i)
# mean(sub2$co2_partial_pressure)
mean(as.numeric(avg_minute[2, c(1:5)]))

