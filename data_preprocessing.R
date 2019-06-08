########################################################################################
#           Project Name: IoT_Analytics- Energy-Consumption                            #
#                         (Deep Analytics, Visualitzation and Modelling)               #
#                                                                                      #
#           Project Description: The goal of the project is to justify that by         #
#           installing sub-metering devices in households, the home developer would    #
#			add value by helping household reduce energy consumptions.                       #
#			For that, the data analyst will perform modelling to check patterns of           #
#           energy usage by time of day and day of the year in a typical household     #
#			whose electrical system is monitored by multiple sub-meters.                     #
#                                                                                      #
#           Author: Ghazaleh Zamani                                                    #
#                                                                                      #
#           Date: August, 2018                                                         #
#                                                                                      #
#                                                                                      #
#                                                                                      #
########################################################################################


# Installing the libraries

install.packages(dplyr)
install.packages(c("tidyr", "devtools"))
devtools::install
library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)


# power<-read.csv("C:/Ubiqum/Week7/household_power_consumption/household_power_consumption.txt", sep = ';')
# View(power)
# power <-cbind(power,paste(power$Date,power$Time), stringsAsFactors=FALSE)
# colnames(power)[10] <-"DateTime"
# power <- power[,c(ncol(power), 1:(ncol(power)-1))]
# head(power)

Time_table <- read.csv("C:/Ubiqum/Week7/household_power_consumption/household_power_consumption.txt", sep = ';', header = TRUE, stringsAsFactors = FALSE)
str(Time_table)

Time_table$Global_active_power <- as.numeric(Time_table$Global_active_power)
summary(Time_table$Global_active_power)

Time_table$Global_reactive_power <- as.numeric(Time_table$Global_reactive_power)
# Time_table$Date <- as.numeric(Time_table$Date)
# Time_table$Time <- as.numeric(Time_table$Time)
Time_table$Voltage <- as.numeric(Time_table$Voltage)
Time_table$Global_intensity <- as.numeric(Time_table$Global_intensity)
Time_table$Sub_metering_1 <- as.numeric(Time_table$Sub_metering_1)
Time_table$Sub_metering_2 <- as.numeric(Time_table$Sub_metering_2)
Time_table$Sub_metering_3 <- as.numeric(Time_table$Sub_metering_3)

summary(Time_table)
str(Time_table)

# convert the KiloWatt power units to Watt
# Time_table$Global_active_power <- Time_table$Global_active_power*1000
# Time_table$Global_reactive_power <- Time_table$Global_reactive_power*1000

# converting Global Active Power to Global Active energy in a new column
Time_table <- dplyr::mutate(Time_table, GA_Energy = Global_active_power * 1000/60)



# correcting for the Date format in the Date column
Time_table$Date <- as.POSIXct(strptime(Time_table$Date,"%d/%m/%Y"))

# put date&time together
# Time_table$Date <- as.Date(Time_table$Date, format ="%d/%m/%Y" )
Time_table <- dplyr::mutate(Time_table, DateTime = paste(Date, Time))
str(Time_table)

# 
# Time_table$DateTime <- strptime(Time_table$DateTime, "%d/%m/%y %H:%M:%S", tz = "GMT")
Time_table$DateTime <- as.POSIXct(strptime(Time_table$DateTime,"%d/%m/%Y %H:%M:%S", tz="GMT"))
str(Time_table$DateTime)






# creating a new column for the weekdays , WORKED

Time_table <- dplyr::mutate(Time_table, year = format(Time_table$DateTime, "%Y"))
Time_table <- dplyr::mutate(Time_table, weekdays = format(Time_table$DateTime, "%A"))



# creating a new column for the unmeasured energy
Time_table <- dplyr::mutate(Time_table, Energy_Unm = Global_active_power * 1000/60 -Sub_metering_1 -Sub_metering_2 -Sub_metering_3 )


#comment on this "#mynepadatime$datime <- as.Date(mynepadatime$datime, format ="%d/%m/%Y" )
#run this only <- as.POSIXct(strptime(mynepadatime$datime,"%d/%m/%Y", tz="GMT"))
#comment on the as.date

# Defining the Time Zone
# Sys.timezone()




# groupYear <- dplyr::group_by(Time_table, year)
# groupDate <- dplyr::group_by(Time_table, Date)
# Na_groupDate <- dplyr::group_by(NA_Table, Date)
# Na_groupDate 
# 
# 
# group08Month <- dplyr::group_by(Time_table08, Month)
# group07Month <- dplyr::group_by(Time_table07, Month)
# hist(group08Month)
# group08Month

# Filtering the year 2006
Time_table06 <- groupYear[which (Time_table$year == 2006),]
Time_table06 <- dplyr::mutate(Time_table06, Month = lubridate::month(DateTime))
NA_Table06 <- Time_table06[!complete.cases(Time_table06),]

NA_Table06_group <- NA_Table06 %>% group_by(Month = factor(Month)) %>% summarise (NAs = n())


NA_Table06_group$Month  <- as.numeric(NA_Table06_group$Month)                                

ggplot(NA_Table06_group, aes(Month, NAs)) + geom_bar(stat="identity")


# Time_table <- Time_table %>% mutate(MonthYear = format(Time_table$DateTime, "%Y/%m"),
#                                   Month = format(DateTime, "%m"),
#                                   Day = format(DateTime, "%d"),
#                                   Hour = format(DateTime, "%H:%M:%S"))

Time_table <- Time_table %>% mutate(MonthYear = format(Time_table$DateTime, "%Y/%m"),
                                    Month = format(DateTime, "%m"),
                                    Day = format(DateTime, "%d"),
                                    Hour = format(DateTime, "%H"))

Group_hour <- Time_table %>% group_by(Hour) %>% summarise(Meanhour = mean(Global_active_power, na.rm = T))
GA_Energy_hour <- Time_table %>% group_by(Hour) %>% summarise(SumHour = sum(Global_active_power, na.rm = T))

# Group_MonthYear <- Time_table %>% group_by(MonthYear) %>%
#   summarise(meanactiveEnergy = mean(Global_active_power,na.rm = T),
#             meankitchen = mean(Sub_metering_1, na.rm = T),
#             meanlaundry = mean(Sub_metering_2, na.rm = T),
#             meanheating =mean(Sub_metering_3, na.rm = T),
#             meanMissingEnergy = mean(Energy_Unm, na.rm = T))


Group_MonthYear <- Time_table %>% group_by(MonthYear) %>% summarize(Total_ActiveEnergy = sum(GA_Energy,na.rm = T),
            Total_Kitchen = sum(Sub_metering_1, na.rm = T),
            Total_Laundry = sum(Sub_metering_2, na.rm = T),
            Total_HVAC = sum(Sub_metering_3, na.rm = T),
            Total_EnergyUnm = sum(Energy_Unm, na.rm = T))




#plotting the MOnthYear mean Initial Insights - Exploration#
ggplot() + geom_line(data = Group_MonthYear, aes(x = seq_len(nrow(Group_MonthYear)), y = Total_ActiveEnergy, colour = "ActiveEnergy"))+
  geom_line(data = Group_MonthYear, aes(x= (seq_len(nrow(Group_MonthYear))), y = Total_Kitchen, colour = "Kitchen"))+
  geom_line(data = Group_MonthYear, aes(x= (seq_len(nrow(Group_MonthYear))), y = Total_Laundry, colour = "Laundry"))+
  geom_line(data = Group_MonthYear, aes(x= (seq_len(nrow(Group_MonthYear))), y = Total_HVAC, colour = "Heating"))+
  geom_line(data = Group_MonthYear, aes(x= (seq_len(nrow(Group_MonthYear))), y = Total_EnergyUnm, colour = "Energy_Unm"))+
  scale_x_continuous(breaks = c(2, 8, 13, 21, 24, 26, 32, 33, 37, 45, 47))+
  xlab("Months")+ ylab("Total Active Energy")


# Filtering the year 2007
Time_table07 <- groupYear[which (Time_table$year == 2007),]
Time_table07 <- dplyr::mutate(Time_table07, Month = lubridate::month(DateTime))


NA_Table07 <- Time_table07[!complete.cases(Time_table07),]

NA_Table07_group <- NA_Table07 %>% group_by(Month = factor(Month)) %>% summarise (NAs = n())


NA_Table07_group$Month  <- as.numeric(NA_Table07_group$Month)                                
                                 
ggplot(NA_Table07_group, aes(Month, NAs)) + geom_bar(stat="identity")





NA_Table07$Month <- as.factor(NA_Table07$Month)

NA_Table07$Month <- as.numeric(NA_Table07$Month)


View(NA_Table07)


hist(NA_Table07$Month)


plot(Time_table07$Month, Time_table07$Energy_Unm)
plot(Time_table08$Month, Time_table08$Energy_Unm)



# Filtering the year 2008
Time_table08 <- groupYear[which (Time_table$year == 2008),]
Time_table08 <- dplyr::mutate(Time_table08, Month = lubridate::month(DateTime))
ggplot()

NA_Table08 <- Time_table08[!complete.cases(Time_table08),]

NA_Table08_group <- NA_Table08 %>% group_by(Month = factor(Month)) %>% summarise (NAs = n())


NA_Table08_group$Month  <- as.numeric(NA_Table08_group$Month)                                

ggplot(NA_Table08_group, aes(Month, NAs)) + geom_bar(stat="identity")

# Filtering the year 2009
Time_table09 <- groupYear[which (Time_table$year == 2009),]
Time_table09 <- dplyr::mutate(Time_table09, Month = lubridate::month(DateTime))
NA_Table09 <- Time_table09[!complete.cases(Time_table09),]

NA_Table09_group <- NA_Table09 %>% group_by(Month = factor(Month)) %>% summarise (NAs = n())


NA_Table09_group$Month  <- as.numeric(NA_Table09_group$Month)                                

ggplot(NA_Table09_group, aes(Month, NAs)) + geom_bar(stat="identity")

# Filtering the year 2010
Time_table10 <- groupYear[which (Time_table$year == 2010),]
Time_table10 <- dplyr::mutate(Time_table10, Month = lubridate::month(DateTime))
NA_Table10 <- Time_table10[!complete.cases(Time_table10),]

NA_Table10_group <- NA_Table10 %>% group_by(Month = factor(Month)) %>% summarise (NAs = n())


NA_Table10_group$Month  <- as.numeric(NA_Table10_group$Month)                                

ggplot(NA_Table10_group, aes(Month, NAs)) + geom_bar(stat="identity")








#Time_table %>% group_by(Species) %>% summarise(.)


# summarize_all(Na_groupDate, funs(mean))
# 
# 
# Na_groupMonth <- dplyr::group_by(month = Na_groupDate(DateTime, "month"))
# 
# 
# expenses %>% group_by(month=floor_date(date, "month")) %>%
#   summarize(amount=sum(amount))
# 
# Time_table$DateTime<-as.POSIXct(strptime(Time_table$DateTime, "%d/%m/%Y %H:%M:%S", tz="GMT"))
summary(Time_table)

str(Time_table)

# Removing NAs
Time_table <- na.omit(Time_table)
summary(Time_table)

#Weekdays comparison
groupWeekday <- dplyr::group_by(Time_table, weekdays)
View(groupWeekday)
Saturdays <- groupWeekday[which(Time_table$weekdays == "Saturday"),]
Sundays <- groupWeekday[which(Time_table$weekdays == "Sunday"),]
Mondays <- groupWeekday[which(Time_table$weekdays == "Monday"),]
Tuesdays <- groupWeekday[which(Time_table$weekdays == "Tuesday"),]
Wednesdays <- groupWeekday[which(Time_table$weekdays == "Wednesday"),]
head(Mondays)

#Group_Weekdays <- Time_table %>% group_by(MonthYear) %>% summarize(Total_ActiveEnergy = sum(GA_Energy,na.rm = T),
                                                                    # Total_Kitchen = sum(Sub_metering_1, na.rm = T),
                                                                    # Total_Laundry = sum(Sub_metering_2, na.rm = T),
                                                                    # Total_HVAC = sum(Sub_metering_3, na.rm = T),
                                                                    # Total_EnergyUnm = sum(Energy_Unm, na.rm = T))


#plotting the Weekdays Energy comparison
ggplot() + geom_line(data = Saturdays, aes(x = (seq_len(nrow(Saturdays))), y = Saturdays$GA_Energy, colour = "Saturdays"))+
  geom_line(data = Sundays, aes(x= (seq_len(nrow(Sundays))), y = Sundays$GA_Energy, colour = "Sundays"))+
  geom_line(data = Mondays, aes(x= (seq_len(nrow(Mondays))), y = Mondays$GA_Energy, colour = "Mondays"))+
  geom_line(data = Tuesdays, aes(x= (seq_len(nrow(Tuesdays))), y = Tuesdays$GA_Energy, colour = "Tuesdays"))+
  geom_line(data = Wednesdays, aes(x= (seq_len(nrow(Wednesdays))), y = Wednesdays$GA_Energy, colour = "Wednesdays"))+
  xlab("Weekdays")+ ylab("Global Active Energy")





