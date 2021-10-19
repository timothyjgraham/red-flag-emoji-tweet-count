# analyse tweet frequency counts
library(stringr)
require(ggplot2)

# I collected the data in terminal using twarc2 and a command of this format:
# twarc2 counts 🚩 --text --granularity hour > red_flag_tweet_counts_per_hour_v2.csv
# (where the red flag emoji is the search term)

df_freq_redflag_emojis <- read.table("/Users/grahamtj/Documents/Research/2021/Miscellaneous_analysis/red_flag_emoji/red_flag_tweet_counts_per_hour_v2.csv", stringsAsFactors = F, header = F, sep=" ", fill = T)

View(df_freq_redflag_emojis)

# remove first two columns
df_freq_redflag_emojis <- df_freq_redflag_emojis[,-c(1:2)]

# delete all rows including and after the "Total" row
# toDel <- which(df_freq_redflag_emojis$V3=="Total")
toDel <- which(nchar(df_freq_redflag_emojis$V3) < 20) # we find which row isn't a valid timestamp because it's too short

toDel <- c(toDel:nrow(df_freq_redflag_emojis))

df_freq_redflag_emojis <- df_freq_redflag_emojis[-toDel,]

# process the date column
df_freq_redflag_emojis$V3 <- gsub("T"," ",df_freq_redflag_emojis$V3)
df_freq_redflag_emojis$V3 <- gsub("Z:","",df_freq_redflag_emojis$V3)
df_freq_redflag_emojis$V3 <- gsub("\\.000","",df_freq_redflag_emojis$V3)

df_freq_redflag_emojis$V3 <- as.POSIXct(df_freq_redflag_emojis$V3)

colnames(df_freq_redflag_emojis) <- c("Time","Freq")
View(df_freq_redflag_emojis)

# process the frequency counts column
df_freq_redflag_emojis$Freq <- gsub(",","",df_freq_redflag_emojis$Freq)
df_freq_redflag_emojis$Freq <- as.numeric(df_freq_redflag_emojis$Freq)

# optionally remove the final row, which is often an incomplete observation for that unit of time
# (e.g. it might only be 17 mins of an hour)

df_freq_redflag_emojis <- df_freq_redflag_emojis[-nrow(df_freq_redflag_emojis),]

# visualise time series

p <- ggplot(df_freq_redflag_emojis, aes(x=Time, y=Freq)) +
  geom_line() + 
  xlab("") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))
p

p + scale_x_datetime( breaks = "12 hours", 
                      # labels = date_format("%a-%d\n%H:%M", tz="CET"), 
                      expand = c(0,0)) +
  labs(title = paste0("Number of red flag emoji tweets per hour (",range(as.Date(df_freq_redflag_emojis$Time))[1]," to ",range(as.Date(df_freq_redflag_emojis$Time))[2],")"), 
       x = "Time of day", 
       y = "Number of tweets")

