install.packages("skimr")
library(skimr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(janitor)
library(readxl)
library(writexl)
library(fs)
install.packages("glimpse")
library(glimpse)
install.packages("plotrix")
library(plotrix)
library(RColorBrewer)


#============================
#STEP 1 COLLECT AND READ DATA
#============================
df1 <- read_xlsx("~/Desktop/Coursera/Capstone/Cyclistic/workbook/202106.xlsx")
df2 <- read_xlsx("~/Desktop/Coursera/Capstone/Cyclistic/workbook/202107.xlsx")
df3 <- read_xlsx("~/Desktop/Coursera/Capstone/Cyclistic/workbook/202108.xlsx")
df4 <- read_xlsx("~/Desktop/Coursera/Capstone/Cyclistic/workbook/202109.xlsx")
df5 <- read_xlsx("~/Desktop/Coursera/Capstone/Cyclistic/workbook/202110.xlsx")
df6 <- read_xlsx("~/Desktop/Coursera/Capstone/Cyclistic/workbook/202111.xlsx")
df7 <- read_xlsx("~/Desktop/Coursera/Capstone/Cyclistic/workbook/202112.xlsx")
df8 <- read_xlsx("~/Desktop/Coursera/Capstone/Cyclistic/workbook/202201.xlsx")
df9 <- read_xlsx("~/Desktop/Coursera/Capstone/Cyclistic/workbook/202202.xlsx")
df10 <- read_xlsx("~/Desktop/Coursera/Capstone/Cyclistic/workbook/202203.xlsx")
df11 <- read_xlsx("~/Desktop/Coursera/Capstone/Cyclistic/workbook/202204.xlsx")
df12 <- read_xlsx("~/Desktop/Coursera/Capstone/Cyclistic/workbook/202205.xlsx")


#=================================
#STEP 2 - WRANGLE AND COMBINE DATA
#=================================
all_trips <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)


colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.  
tail(all_trips)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics
glimpse(all_trips)

#=================================================
# STEP 3: CLEAN DATA AND PREPARE DATA FOR ANALYSIS
#=================================================
sum(is.null(all_trips)) #Check for missing values
sum(is.na(all_trips)) #Check for variables designated not available 

str(all_trips$rideable_type)
str(all_trips$member_casual)

table(all_trips$member_casual)
table(all_trips$rideable_type)
table(all_trips$start_station_name)
table(all_trips$end_station_name)
table(all_trips$ride_id)

####Adding Columns
all_trips$duration <- seconds_to_period(all_trips$ended_at - all_trips$started_at) #Calculate the Ride Length in h,m,s
all_trips$ride_length <- all_trips$ended_at - all_trips$started_at
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

summary(all_trips$duration) #Look at duration 
summary(all_trips$ride_length)#Look at Ride Length



#Looking at the duration of ride lengths that are negative and longer than one day
cat("No of Rows with ride duration < 0 seconds = ", sum(all_trips$ride_length < 0),'\n')
cat("No of Rows with ride duration > 1 day =", sum(all_trips$ride_length > 86400),'\n')
cat("No of Rows with ride duration > 3 day =", sum(all_trips$ride_length > 259200),'\n')
cat("No of Rows with ride duration > 5 day =", sum(all_trips$ride_length > 432000),'\n')
cat("No of Rows with ride duration > 10 day =", sum(all_trips$ride_length > 864000),'\n')


#####DATA REMOVAL
all_trips_v2 <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng,)) #Remove columns

#Removing data for negative ride lengths
cat("Original no of Rows = ",nrow(all_trips),'\n')
all_trips_v3 <- all_trips_v2[!(all_trips_v2$ride_length < 0),]
cat("Updated no of Rows = ", nrow(all_trips_v3))

all_trips_v4 <- drop_na(all_trips_v3)

all_trips_v5 <- (filter(all_trips_v4, !(start_station_name == "WATSON TESTING - DIVVY" | start_station_name == "HUBBARD ST BIKE CHECKING (LBS-WH-TEST)" | start_station_name =="hubbard_test_lws" | start_station_name =="")))


is.factor(all_trips_v5$ride_length)
all_trips_v5$ride_length <- as.numeric(as.character(all_trips_v5$ride_length))
is.numeric(all_trips_v5$ride_length)

all_trips_stations <- all_trips_v5[,c(5)]
all_trips_stations <- all_trips_stations[!duplicated(all_trips_stations$start_station_name),]
NROW(unique(all_trips_stations))



#Create a new column with route on the casual riders data frame
all_trips_v5 <- all_trips_v5 %>%
  mutate(route = paste(start_station_name, "To", sep=" ", end_station_name))


#Member's top ten popular routes
popular_ride_route_member <- all_trips_v5 %>% 
  group_by(member_casual, route) %>%
  filter(member_casual == "member") %>%
  summarise(number_of_rides  = n(), average_duration_minutes = mean(ride_length)) %>% 
  arrange(member_casual, route, number_of_rides, average_duration_minutes)

popular_ride_route_member <- head(arrange(popular_ride_route_member, desc(number_of_rides)),10)
head(popular_ride_route_member, 10)
write_csv(popular_ride_route_member, "popular_ride_route_member.csv") 

NROW(filter(all_trips_v5, member_casual=="member", start_station_name == "Ellis Ave & 60th St" & end_station_name == "University Ave & 57th St")) #verify work

#Casual's top ten popular routes
popular_ride_route_casual <- all_trips_v5 %>% 
  group_by(member_casual, route) %>%
  filter(member_casual == "casual") %>%
  summarise(number_of_rides  = n(), average_duration_minutes = mean(ride_length)) %>% 
  arrange(member_casual, route, number_of_rides, average_duration_minutes)

popular_ride_route_casual <- head(arrange(popular_ride_route_casual, desc(number_of_rides)),10)
head(popular_ride_route_casual, 10)
write_csv(popular_ride_route_casual, "popular_ride_route_casual.csv")

NROW(filter(all_trips_v5, member_casual=="casual", start_station_name == "Streeter Dr & Grand Ave" & end_station_name == "Streeter Dr & Grand Ave"))#verify work

#=====================
# STEP 4: ANALYZE DATA
#=====================
##Customer's Rides Per Day
#day_of_ride<- all_trips_v3 %>% 
#  group_by(day_of_week, member_casual)%>%
#  select(member_casual, day_of_week) %>% 
#  summarize(number = table(day_of_week))
#day_of_ride_casual<- day_of_ride %>% 
#  filter(member_casual =="casual" & number >0)
#day_of_ride_member<- day_of_ride %>% 
#  filter(member_casual =="member" & number >0)
#customer_daily_ride <- rbind(day_of_ride_casual, day_of_ride_member)
#did this better below 

aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual, FUN = mean)
aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual, FUN = median)
aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual, FUN = max)
aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual, FUN = min)


all_trips_v5$day_of_week <- ordered(all_trips_v5$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
aggregate(all_trips_v5$ride_length ~ all_trips_v5$member_casual + all_trips_v5$day_of_week, FUN = mean)
all_trips_v5$month <- ordered(all_trips_v5$month, levels=c("06", "07", "08", "09", "10", "11", "12", "01", "02", "03", "04", "05"))

#Customers monthly ride lengths average
summary_monthly_ride <- all_trips_v5%>% 
  mutate(month = month(started_at, label = TRUE), format = "%m")%>% 
  mutate(year, format = "%y")%>%
  group_by(month, year, member_casual) %>%  
  summarise(number_of_rides = n()
            ,average_duration_in_minutes = (mean(ride_length)/60)) %>%    
  arrange(year,month,member_casual)
write_csv(summary_monthly_ride, "summary_monthly_ride.csv")
#Customer's monthly ride lengths average by bike type
summary_monthly_ride_type <- all_trips_v5%>% 
  mutate(month = month(started_at, label = TRUE), format = "%m")%>%
  mutate(year, format = "%y")%>%  
  group_by(month,year,member_casual, rideable_type) %>%  
  summarise(number_of_rides = n()
            ,average_duration_in_minutes = (mean(ride_length)/60)) %>%    
  arrange(year, month, member_casual, rideable_type)
write_csv(summary_monthly_ride_type, "summary_monthly_ride_type.csv")

str(summary_monthly_ride_type)

#Customer's daily ride lengths average
summary_daily_ride <- all_trips_v5 %>%
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, day_of_week)
  write_csv(summary_daily_ride, "summary_daily_ride.csv") 

#Customer's daily ride lengths average by bike type
summary_daily_ride_type <- all_trips_v5%>% 
  group_by(day_of_week, member_casual, rideable_type) %>%  
  summarise(number_of_rides = n()
            ,average_duration_in_minutes = (mean(ride_length)/60)) %>%    
  arrange(day_of_week, member_casual, rideable_type)
  write_csv(summary_daily_ride_type, "summary_daily_ride_type.csv")
  
#=====================
# STEP 5: SHARE
#=====================
#####Visualizations####  
#####Reccomendations####

##Rides by Customer  
ggp1 <- all_trips_v5 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()) %>% 		# calculates the number of rides
  arrange(member_casual, weekday)	 %>% #arranges by member type and weekday
  ggplot(aes(x = weekday, y = number_of_rides, group = member_casual, color = member_casual)) +
  geom_line()  + scale_color_brewer(palette="Paired")
  ggp1 + labs(title = "Customer's Number of Rides Per Day", x = "Day of the Week"
       ,y = "Number of Rides", color = "Customer Type")
  options(scipen=999999)
  
##Average Daily Customer Ride Duration 
ggp2 <- all_trips_v5 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length/60)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) + 
  geom_col(position = "dodge") + scale_fill_brewer(palette="Paired")
  ggp2 + labs(x="Day of Week", y="Avg Ride Duration (in mins)", fill="Customer Type", title = "Customer's Average Daily Ride Duration")

#####Obseration- 
  #Members are consistant bread and butter for Cyclistic. They are riding with consistant demand and duration. 
  #Casual riders come in with stronger demand on weekends and their rides are consistantly double that of a member.  
#####Reccomendation-
  #Better perks for weekend riders on the membership might be of use to convert casual riders. OR Special Weekend Membership/Pass options may be something to look into.
  
##Customer's Number of Rides Per Month
ggp3 <- all_trips_v5 %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n(), ) %>%
  arrange(member_casual, month) %>%
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) + geom_col() + scale_fill_brewer(palette="Paired")
  ggp3 + labs(x="Month", y="Number of Rides", fill="Customer Type", title="Customer's Number of Rides Per Month") 
#####Observations-
  #Cyclistic's Riders in general all drop significantly during the winter months of Nov-April.
  #Want to look into Monthly Rides by bike type and customer type
#####Reccomendation-
  #Look into Membership options of 6 and 12 months duration. This may help customer retention during the winter months.
  

##Customer's Bike Selections
ggp4 <- all_trips_v5 %>% 
  group_by(member_casual, rideable_type) %>% 
  summarise(number_of_rides = n(), .groups = "drop") %>%
  arrange(member_casual, rideable_type) %>%
  ggplot(aes(x = rideable_type, y = number_of_rides, fill = member_casual)) + geom_bar(position="dodge", stat="identity") + scale_fill_brewer(palette="Paired")
  ggp4 + labs(x="Bike type", y="Number of Rides", fill="Customer Type", title="Customer's Bike Selections") 


#####Observations-
#Does membership limit to classic and electric bikes only? 
#####Reccomendation-
#Docked bikes durations by casual riders needs to be looked into- might be maintenance. 

  
##Customer's Average Bike Ride 
ggp5 <- all_trips_v5 %>% 
  group_by(member_casual, rideable_type) %>% 
  summarise(average_duration = mean(ride_length/60), .groups = "drop") %>%
  arrange(member_casual, rideable_type) %>%
  ggplot(aes(x = rideable_type, y = as.numeric(average_duration), fill = member_casual)) + geom_bar(position="dodge", stat="identity") + scale_fill_brewer(palette="Paired")
  ggp5 + labs(x="Bike type", y="Avg Ride Duration (in mins)", fill="Customer Type", title="Customer's Average Ride with Bike") 
  
##Monthly Bike Rides   
ggp6 <- all_trips_v5 %>%
  group_by(rideable_type, month,) %>%
  summarise(number_of_rides = n(), ) %>%
  arrange(rideable_type, month) %>%
  ggplot(aes(x = month, y = number_of_rides, fill = rideable_type)) + geom_col(position = "dodge") + scale_fill_manual(values = c(classic_bike="#A0CBE8", electric_bike="#0072B2", docked_bike="#8CD17D")) + coord_flip()
ggp6 + labs(x="Month", y="Number of Rides", fill="Bike Type", title="Monthly Bike Rides") 

##Monthly Bike Rides By Casual Riders
ggp7 <- all_trips_v5 %>%
  group_by(rideable_type, month,) %>%
  filter(member_casual == "casual") %>%
  summarise(number_of_rides = n(), ) %>%
  arrange(rideable_type, month) %>%
  ggplot(aes(x = month, y = number_of_rides, group = rideable_type, color = rideable_type)) + geom_line() + scale_color_brewer(palette = "Paired")
ggp7 + labs(x="Month", y="Number of Rides", color="Bike Type", title="Casual Rider's Monthly Ride Trend") 
##Monthly Bike Rides by Members
ggp8 <- all_trips_v5 %>%
  group_by(rideable_type, month,) %>%
  filter(member_casual == "member") %>%
  summarise(number_of_rides = n(), ) %>%
  arrange(rideable_type, month) %>%
  ggplot(aes(x = month, y = number_of_rides, group = rideable_type, color = rideable_type)) + geom_line() +  scale_color_brewer(palette = "Paired")
ggp8 + labs(x="Month", y="Number of Rides", color="Bike Type", title="Member's Monthly Ride Trend") 


#Customer Pie Chart
total_members <- NROW(filter(all_trips_v5, member_casual == "member"))
total_members

total_casuals <- NROW(filter(all_trips_v5, member_casual == "casual"))
total_casuals

labs <- c("Members", "Casual Riders")
slices_customers <- c(total_members, total_casuals)
piepercent <- round(100 * slices_customers / sum(slices_customers), 1)
lbls <- paste(labs, piepercent)
lbls_customers <- paste(lbls, "%", sep="")

lbls_customers

ggpie <- pie3D(slices_customers, labels = lbls_customers, explode = 0.1, col = hcl.colors(length(slices_customers), palette = "Teal"), main = "Cyclistic Customers")



write_csv(all_trips_v5, "all_trips_v5.csv")

#####Observations-
  #Members and casual riders have a pretty consistent demand for electric bikes May-October.
  #Winter slump effects both customers November-April.
#####Reccomendations-
  #6/12 options may help increase customer retention during winter slump for members and may convert some casual riders.
  #Membership option for "weekend" riders may be a way to convert consistent casual weekend riders.
  #Focus marketing on riders top 10 routes and increase bike availability in these areas during peak season.
##Notes
#Analysis into customer frequency and demographics would probably show some great input as well into the needs/wants of casual riders for better targeting.



