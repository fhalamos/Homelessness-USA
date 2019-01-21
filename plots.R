#install.packages(c("readr", "haven", "dplyr", "tidyr", "stringr", "ggplot2", "tidyverse"))

library(plyr)
library(tidyverse)
library(ggplot2)
library(readxl)
library(scales)

#PLOT 1 - NUMBER OF HOMELESS PER STATE IN 2018. Goal graph: identify states with most n homeless

#Load data
pit_by_state <- read_excel("2007-2018-PIT-Counts-by-State.xlsx", sheet=2)

#Fix columns names
names(pit_by_state)<-make.names(names(pit_by_state),unique = TRUE)

#Do not consider last row of totals
pit_by_state<- pit_by_state %>% head(-1)

#Plot states vs n_homeless
statesToLabel <- c("CA","NY","FL","TX","WA","MA")

homeless_per_state_2018_plot<-ggplot(data=pit_by_state, aes(x=State, y=Overall.Homeless..2018, fill=Overall.Homeless..2018))+
  geom_bar(stat = "identity")+
  geom_text(aes(label=State), 
            vjust = -0.5, #move text higher
            data = pit_by_state[pit_by_state$State %in% statesToLabel,])+ #show only some labels
  scale_y_continuous(name="Number of homeless", labels=comma, limits = c(0,150000))+#avoid scientific notation
  scale_fill_gradient(low = "blue", high = "red",aesthetics = "fill")+
  theme(axis.text.x = element_text(angle = 90, vjust=-0.001))+ #Rotate x axis text
  guides(fill=FALSE)+ #No legend, information is redundat
  labs(title = "Homeless in U.S. States on 2018",
       subtitle = "Distinction for states with highest homeless population",
       caption = "Source: U.S. Department of Housing and Urban Development (HUD)", 
       x = "Years")

#Visualize and save
homeless_per_state_2018_plot
ggsave("1. Homeless_per_state_2018.pdf", homeless_per_state_2018_plot)


#name="Number of homeless"
#PLOT 2 - Ratio of homeless/number of CoCs

homeless_to_coc<- pit_by_state %>% mutate(ratio_h_coc = Overall.Homeless..2018/Number.of.CoCs) %>% select(State,ratio_h_coc)

ratio_h_coc_plot<-ggplot(data=homeless_to_coc, aes(x=State, y=ratio_h_coc, color=ratio_h_coc))+
  geom_point()+
  geom_label(aes(label=State), vjust = -0.5, data = homeless_to_coc[homeless_to_coc$State %in% statesToLabel,])+
  scale_y_continuous(name="Ratio n homeless to n CoC", labels=comma)+#avoid scientific notation
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ #Rotate x axis text
  theme(legend.position="none")+
  labs(title = "Ratio of homeless/number of CoCs per state",
       subtitle = "States with highest homeless population also have high ratio homeless/number CoC",
       caption = "Source: U.S. Department of Housing and Urban Development (HUD)", 
       x = "Years")

#Visualize and save
ratio_h_coc_plot
ggsave("2. Ratio_homeless_coc_2018.pdf", ratio_h_coc_plot)



#######################################

#PLOT 3 - EVOLUTION OF TOTAL N OF HOMELESS IN TIME

#Load data
h_2018 <-read_excel("2007-2018-PIT-Counts-by-State.xlsx", sheet=2)
h_2017 <-read_excel("2007-2018-PIT-Counts-by-State.xlsx", sheet=3)
h_2016 <-read_excel("2007-2018-PIT-Counts-by-State.xlsx", sheet=4)
h_2015 <-read_excel("2007-2018-PIT-Counts-by-State.xlsx", sheet=5)
h_2014 <-read_excel("2007-2018-PIT-Counts-by-State.xlsx", sheet=6)
h_2013 <-read_excel("2007-2018-PIT-Counts-by-State.xlsx", sheet=7)
h_2012 <-read_excel("2007-2018-PIT-Counts-by-State.xlsx", sheet=8)
h_2011 <-read_excel("2007-2018-PIT-Counts-by-State.xlsx", sheet=9)
h_2010 <-read_excel("2007-2018-PIT-Counts-by-State.xlsx", sheet=10)
h_2009 <-read_excel("2007-2018-PIT-Counts-by-State.xlsx", sheet=11)
h_2008 <-read_excel("2007-2018-PIT-Counts-by-State.xlsx", sheet=12)
h_2007 <-read_excel("2007-2018-PIT-Counts-by-State.xlsx", sheet=13)

#Rename column of total number of homeless
names(h_2018)[names(h_2018) == "Overall Homeless, 2018"] <- "Total_homeless"
names(h_2017)[names(h_2017) == "Overall Homeless, 2017"] <- "Total_homeless"
names(h_2016)[names(h_2016) == "Overall Homeless, 2016"] <- "Total_homeless"
names(h_2015)[names(h_2015) == "Overall Homeless, 2015"] <- "Total_homeless"
names(h_2014)[names(h_2014) == "Overall Homeless, 2014"] <- "Total_homeless"
names(h_2013)[names(h_2013) == "Overall Homeless, 2013"] <- "Total_homeless"
names(h_2012)[names(h_2012) == "Overall Homeless, 2012"] <- "Total_homeless"
names(h_2011)[names(h_2011) == "Overall Homeless, 2011"] <- "Total_homeless"
names(h_2010)[names(h_2010) == "Overall Homeless, 2010"] <- "Total_homeless"
names(h_2009)[names(h_2009) == "Overall Homeless, 2009"] <- "Total_homeless"
names(h_2008)[names(h_2008) == "Overall Homeless, 2008"] <- "Total_homeless"
names(h_2007)[names(h_2007) == "Overall Homeless, 2007"] <- "Total_homeless"

#select only state and total homeless, add year and get only totals
h_2018<- h_2018 %>% select(Total_homeless) %>% mutate(Year = 2018)%>% tail(1)
h_2017<- h_2017 %>% select(Total_homeless) %>% mutate(Year = 2017)%>% tail(1)
h_2016<- h_2016 %>% select(Total_homeless) %>% mutate(Year = 2016)%>% tail(1)
h_2015<- h_2015 %>% select(Total_homeless) %>% mutate(Year = 2015)%>% tail(1)
h_2014<- h_2014 %>% select(Total_homeless) %>% mutate(Year = 2014)%>% tail(1)
h_2013<- h_2013 %>% select(Total_homeless) %>% mutate(Year = 2013)%>% tail(1)
h_2012<- h_2012 %>% select(Total_homeless) %>% mutate(Year = 2012)%>% tail(1)
h_2011<- h_2011 %>% select(Total_homeless) %>% mutate(Year = 2011)%>% tail(1)
h_2010<- h_2010 %>% select(Total_homeless) %>% mutate(Year = 2010)%>% tail(1)
h_2009<- h_2009 %>% select(Total_homeless) %>% mutate(Year = 2009)%>% tail(1)
h_2008<- h_2008 %>% select(Total_homeless) %>% mutate(Year = 2008)%>% tail(1)
h_2007<- h_2007 %>% select(Total_homeless) %>% mutate(Year = 2007)%>% tail(1)


#Merging all data of years in one data frame
all_years <- rbind.fill(h_2018,h_2017,h_2016,h_2015,h_2014,h_2013,h_2012,h_2011,h_2010,h_2009,h_2008,h_2007)

#Plotting total n_homeless for different years
homeless_time_plot<-ggplot(data=all_years, aes(x=Year, y=Total_homeless,group = 1))+
  geom_line(linetype = "solid",color="red")+
  geom_point(color = "red")+
  scale_y_discrete(name="Number of homeless")+
  scale_x_continuous(breaks = c(2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ #Rotate x axis text
  labs(title = "Homeless in USA last decade",
       subtitle = "Total number of homeless in USA during period from 2007 to 2018",
       caption = "Source: U.S. Department of Housing and Urban Development (HUD)", 
       x = "Years")

#Visualize and save
homeless_time_plot
ggsave("3. Homeless_time.pdf", homeless_time_plot)
