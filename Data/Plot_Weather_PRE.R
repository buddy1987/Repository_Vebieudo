# Preprocessing & summarizing data
library(dplyr)
library(tidyr)

# Visualizatin development
library(ggplot2)

df = read.csv(file.choose(),header=T)

# rename variables
names(df) <- c("Year", "Month", "Day","Stage")

# create dataframe that represents 1995-2013 historical data
Past <- df %>%
  group_by(Year, Month) %>%
  arrange(Day) %>%
  ungroup() %>%
  group_by(Year) %>%
  mutate(newDay = seq(1, length(Day))) %>%   # label days as 1:365 (will represent x-axis)         
  ungroup() %>%
  filter(Year!= 2016) %>%     # filter out missing data (identified with '-99' value) & current year data
  group_by(newDay) %>%
  mutate(upper = max(Stage), # identify max value for each day
         lower = min(Stage), # identify min value for each day
         avg = mean(Stage),  # calculate mean value for each day
         se = sd(Stage)/sqrt(length(Stage))) %>%  # calculate standard error of mean
  mutate(avg_upper = avg+(2.101*se),  # calculate 95% CI for mean
         avg_lower = avg-(2.101*se)) %>%  # calculate 95% CI for mean
  ungroup()

# tao bang gia tri trung binh
dfav = aggregate(Past$Stage, by = list(Past$newDay),mean)
names(dfav) = c("newDay","ave")
         
# create dataframe that represents current year data
Present <- df %>%
  group_by(Year, Month) %>%
  arrange(Day) %>%
  ungroup() %>%
  group_by(Year) %>%
  mutate(newDay = seq(1, length(Day))) %>%  # create matching x-axis as historical data
  ungroup() %>%
  filter(Year == 2016)  # filter out missing data & select current year data

# tach them nam 2015 ve xem the nao
Present1 <- df %>%
  group_by(Year, Month) %>%
  arrange(Day) %>%
  ungroup() %>%
  group_by(Year) %>%
  mutate(newDay = seq(1, length(Day))) %>%  # create matching x-axis as historical data
  ungroup() %>%
  filter(Year == 2015)  # filter out missing data & select current year data

# tach them nam 2015 ve xem the nao
Year98 <- df %>%
  group_by(Year, Month) %>%
  arrange(Day) %>%
  ungroup() %>%
  group_by(Year) %>%
  mutate(newDay = seq(1, length(Day))) %>%  # create matching x-axis as historical data
  ungroup() %>%
  filter(Year == 1998)  # filter out missing data & select current year data

# create dataframe that represents the lowest temp for each day for the historical data
PastLows <- Past %>%
  group_by(newDay) %>%
  summarise(Pastlow = min(Stage)) # identify lowest temp for each day from 1995-2013

# create dataframe that identifies the days in 2014 in which the temps were lower than all previous 19 years
PresentLows <- Present %>%
  left_join(PastLows) %>%  # merge historical lows to current year low data
  mutate(record = ifelse(Stage<Pastlow, "Y", "N")) %>% # identifies if current year was record low
  filter(record == "Y")  # filter for days that represent current year record lows

# create dataframe that represents the highest temp for each day for the historical data
PastHighs <- Past %>%
  group_by(newDay) %>%
  summarise(Pasthigh = max(Stage))  # identify highest temp for each day from 1995-2013

# create dataframe that identifies the days in 2014 in which the temps were higher than all previous 19 years
PresentHighs <- Present %>%
  left_join(PastHighs) %>%  # merge historical highs to current year low data
  mutate(record = ifelse(Stage>Pasthigh, "Y", "N")) %>% # identifies if current year was record high
  filter(record == "Y")  # filter for days that represent current year record highs



# create y-axis variable
#a <- dgr_fmt(seq(4,24, by= 2))

# create a small dataframe to represent legend symbol for 2014 Temperature
tbinh <- data.frame(x=seq(35,42),y=rnorm(8,6,0.1))
nam1998 <- data.frame(x=seq(35,42),y=rnorm(8,7,0.1))
nam2015 <- data.frame(x=seq(35,42),y=rnorm(8,8,0.1))
nam2016 <- data.frame(x=seq(35,42),y=rnorm(8,9,0.1))

p <- ggplot(Past, aes(newDay, Stage)) +
  theme(plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        #axis.text = element_blank(),  
        axis.title = element_blank()) +
  geom_linerange(Past, mapping=aes(x=newDay, ymin=lower, ymax=upper), colour = "wheat2", alpha=.1)
 print(p)

 p <- p + geom_linerange(Past, mapping=aes(x=newDay, ymin=avg_lower, ymax=avg_upper), colour = "wheat4")
 
 #print(p)
# ve nam 2016 tu bang Present
 p <- p + 
   geom_line(Present, mapping=aes(x=newDay, y= Stage, group=1)) +
   geom_vline(xintercept = 0, colour = "wheat4", linetype=1, size=1)
 
# ve nam 2015
 p <- p + 
   geom_line(Present1, mapping=aes(x=newDay, y= Stage, group=1), colour = "green", size = 1.0) +
   geom_vline(xintercept = 0, colour = "wheat4", linetype= 1, size= 1)
 
 # Ve duong trung binh
 p <- p + 
   geom_line(dfav, mapping=aes(x=newDay, y= ave, group=1), colour = "red", size = 1.0) +
   geom_vline(xintercept = 0, colour = "wheat4", linetype= 1, size= 1)
 
 # Ve duong trung binh
 p <- p + 
   geom_line(Year98, mapping=aes(x=newDay, y= Stage, group=1), colour = "blue", size = 1.0) +
   geom_vline(xintercept = 0, colour = "wheat4", linetype= 1, size= 1)
 #print(p)
# ad cot ngang trong chart
 p <- p + 
   geom_hline(yintercept = 0, colour = "wheat3", linetype=3) +
   geom_hline(yintercept = 1, colour = "wheat3", linetype=3) +
   geom_hline(yintercept = 2, colour = "wheat3", linetype=3) +
   geom_hline(yintercept = 3, colour = "wheat3", linetype=3) +
   geom_hline(yintercept = 4, colour = "wheat3", linetype=3) +
   geom_hline(yintercept = 5, colour = "wheat3", linetype=3) +
   geom_hline(yintercept = 6, colour = "wheat3", linetype=3) +
   geom_hline(yintercept = 7, colour = "wheat3", linetype=3) +
   geom_hline(yintercept = 8, colour = "wheat3", linetype=3) +
   geom_hline(yintercept = 9, colour = "wheat3", linetype=3) +
   geom_hline(yintercept = 10, colour = "wheat3", linetype=3)+ 
   geom_hline(yintercept = 11, colour = "wheat3", linetype=3)
 print(p)
 
 # add cot doc
p <- p + 
   geom_vline(xintercept = 31, colour = "wheat4", linetype=3, size=.5) +
   geom_vline(xintercept = 59, colour = "wheat4", linetype=3, size=.5) +
   geom_vline(xintercept = 90, colour = "wheat4", linetype=3, size=.5) +
   geom_vline(xintercept = 120, colour = "wheat4", linetype=3, size=.5) +
   geom_vline(xintercept = 151, colour = "wheat4", linetype=3, size=.5) +
   geom_vline(xintercept = 181, colour = "wheat4", linetype=3, size=.5) +
   geom_vline(xintercept = 212, colour = "wheat4", linetype=3, size=.5) +
   geom_vline(xintercept = 243, colour = "wheat4", linetype=3, size=.5) +
   geom_vline(xintercept = 273, colour = "wheat4", linetype=3, size=.5) +
   geom_vline(xintercept = 304, colour = "wheat4", linetype=3, size=.5) +
   geom_vline(xintercept = 334, colour = "wheat4", linetype=3, size=.5) +
   geom_vline(xintercept = 365, colour = "wheat4", linetype=3, size=.5) 

 p <- p +
   coord_cartesian(ylim = c(0,11)) +
   scale_y_continuous(breaks = seq(0,11, by= 1)) +
   scale_x_continuous(expand = c(0, 0), 
                      breaks = c(15,45,75,105,135,165,195,228,258,288,320,350),
                      labels = c("January", "February", "March", "April",
                                 "May", "June", "July", "August", "September",
                                 "October", "November", "December"))
 
 #print(p)

 p <- p +
   ggtitle("Variation of water level at PreakDam") +
   theme(plot.title=element_text(face="bold",hjust=.012,vjust=.8,colour="#3C3C3C",size=16)) +
   annotate("text", x = 30, y = 11, label = "Water level(m)", size = 4, fontface="bold")
 
 #print(p)

# gan chu thich cho chart; o day can chu y co the gan la doan; la text hay la box deu duoc
 ## luu y ve vi tri x va y tren bieu do
 p <- p +
   annotate("segment", x = 80, xend = 80, y = 6, yend = 9, colour = "wheat2", size=3) +
   annotate("segment", x = 80, xend = 80, y = 7, yend = 8, colour = "wheat4", size=3) +
   geom_line(data=tbinh, aes(x=x,y=y), colour = "red") +
   geom_line(data=nam1998, aes(x=x,y=y),colour = "blue") +
   geom_line(data=nam2015, aes(x=x,y=y),colour = "green") +
   geom_line(data=nam2016, aes(x=x,y=y),colour = "black") +
   annotate("text", x = 80, y = 7.5, label = "NORMAL RANGE", size= 2.5, colour="gray30") +
   annotate("text", x = 24, y = 9, label = "Year 2016", size= 3, colour="gray30") +
   annotate("text", x = 24, y = 8, label = "Year 2015", size= 3, colour="gray30") +
   annotate("text", x = 24, y = 7, label = "Year 1998", size= 3, colour="gray30") +
   annotate("text", x = 24, y = 6, label = "Average", size= 3, colour="gray30") +
   annotate("text", x = 87, y = 9, label = "High record", size= 3, colour="gray30") +
   annotate("text", x = 87, y = 6, label = "Low record", size= 3, colour="gray30")
 
 print(p)



