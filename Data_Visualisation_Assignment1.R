library(tidyverse )
library(plyr)
library(scales)
library(dplyr)
library(lubridate)
library(zoo)
library(ggplot2)
library(RColorBrewer)


world_population <- read_csv("WorldPopulation.csv")

world_population <- gather(world_population, key=Year, value = Population, `1960`:`2016`)
world_population <- world_population[,c("Year","Population")]
world_population$Year <- as.Date(as.character(world_population$Year), format="%Y")
world_population$Population <- world_population$Population / 10000000
colnames(world_population)[2] <- "Population (10 Millions)"

p<- ggplot(data=world_population, aes(x=Year, y=`Population (10 Millions)`, size=`Population (10 Millions)`))
p + geom_point(alpha=0.4, color="blue") + 
  scale_size_area(max_size = 6, breaks=c(400, 500, 700))+
  ylab("Population (in 10 millions)")+
  scale_x_date(date_breaks = "3 year", date_labels = "%Y")+
  ggtitle("Year Vs Population Growth Percentage") +
  stat_smooth(method=lm, se= FALSE, colour = "red", size = 0.8)+
  theme(axis.text.x = element_text(angle = 30, hjust=1, vjust = .5),
        plot.title = element_text(hjust = 0.5, face="bold"),
        panel.background = element_blank())




# Loading the nations data and fetility data
fert_data <- read_csv("Fetrtility.csv")
nat_data <- read_csv("nations.csv", col_types = cols(population=col_double()))
fert_world <- fert_data[fert_data$`Country or Area` == 'World', ]
fert_world$Year <- as.Date(as.character(fert_world$Year), format="%Y")



#Brazil, United States, Cambodia, Argentina, 
#India, Pakistan, Germany, Ireland, China, United Kingdom, United States
selc_data = fert_data[fert_data$`Country or Area` %in% c('Brazil', 'United States', 'Afghanistan', 'Argentina', 
              'India', 'Pakistan', 'Ireland', 'China', 'United Kingdom', 'World'), ]
selc_data$Year <- as.Date(as.character(selc_data$Year), format="%Y")
selc_data['Income'] <- nat_data[match(selc_data$`Country or Area`, nat_data$country), 11]

#fert_world <- fert_data[fert_data['Country or Area'] == 'World', ]
#fert_world$Year <- as.Date(as.character(fert_world$Year), format="%Y")
  

p<- ggplot(selc_data, aes(x = Year, y=Value, color=`Country or Area`), 
           show.legend=F, colour="grey40")

p + geom_line() + 
  geom_point(size = 1.8, aes(shape=Income), na.rm = T) + 
  scale_shape_manual(values = c(8, 11, 2, 12))+
  #geom_point(data=fert_world, size=1.4)+
  geom_line(data=fert_world, size=1.2)+
  scale_colour_brewer(palette = "Paired", name = "Country", 
                      labels=c('Afghanistan', 'Argentina', 'Brazil', 
                               'China', 'India', 'Ireland', 'Pakistan', 
                               'United Kingdom', 'United States', 'World')) +
  ylab("Fertility Value") + 
  ggtitle("Fertility Rate From 1960 to 2015") +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y")  +
  #labs(caption = "SWAROOP: 17230755")+
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = 0.25), 
        axis.title.x=element_blank(), 
        axis.text.x = element_text(angle = 50, hjust=1, vjust = .5),
        legend.key = element_rect(fill = NA, colour = NA, size = 0.25),
        plot.title = element_text(hjust = 0.5, face="bold"))+
        #plot.caption = element_text(vjust = 0.5, color="blue"))+
  geom_hline(yintercept = 2.1, size=0.8, linetype = 2, alpha=0.6)+
  annotate("text", x = as.Date("1965", format="%Y"), y = 1.8, 
           label = c("Replacement \n Rate: 2.1"), fontface=1, size=3)





per_fertility = 
  ddply(selc_data[selc_data$`Country or Area` != 'World', ], "Year", transform, percent_fertility = Value/sum(Value)* 100)



ggplot(per_fertility, aes(x = Year, y=percent_fertility, fill=Country.or.Area))+
  geom_bar( stat="identity")+
  scale_fill_brewer(palette = "Paired", name = "Country", 
                    labels=c('Afghanistan', 'Argentina', 'Brazil', 'China', 'India', 
                             'Ireland', 'Pakistan', 'United Kingdom', 'United States')) +
  ylab("percentage Fertility") + 
  labs(title='Yearly Proportion Fertility Rate',
       subtitle='Calculated only with rescpect to 9 nations') +
  scale_x_date(date_breaks = "2 year", labels = date_format("%Y"))  +
  theme(panel.grid.major = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = 0.25), 
        axis.title.x=element_blank(), axis.text.x = element_text(angle = 30, hjust=1, vjust = .5), 
        legend.key = element_rect(fill = NA, colour = NA, size = 0.25))+
  theme(plot.title = element_text(hjust = 0.5, face="bold"), 
        plot.subtitle = element_text(hjust = 0.5))



#################################################################################

Reg_data <- selc_data[selc_data$`Country or Area` != 'World', ]
colnames(Reg_data)[1] <- 'country'
Reg_data['Region'] <- nat_data[match(Reg_data$country, nat_data$country), 10]
Reg_data <- Reg_data %>% group_by(Year, Region) %>% dplyr::summarise(Value=mean(Value, na.rm=TRUE))
Reg_data$Year <- as.Date(as.character(Reg_data$Year), format="%Y")

d<- ggplot(Reg_data, aes(x = Year, y=Value, color= Region))

d + geom_point(size = 1)+
  geom_line(size=0.6)+
  scale_colour_brewer(palette = "Paired", name = "Country")+
  ylab("Fertility Value") + 
  ggtitle("Fertility Rate From 1960 to 2015 vs Regions") +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y")+
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.line = element_line(colour = "black", size = 0.25), 
        axis.title.x=element_blank(), 
        axis.text.x = element_text(angle = 50, hjust=1, vjust = .5), 
        legend.key = element_rect(fill = NA, colour = NA, size = 0.25),
        plot.title = element_text(hjust = 0.5, face="bold"))


ggplot(Reg_data, aes(x=Year, y=Value, fill=Region)) +
  
  geom_area(stat="identity", colour="black", size = .2)  +
  
  scale_fill_brewer(palette = "Set3", name = "Regions", 
                    labels=c("East Asia & Pacific", "Europe & Central Asia", 
                             "Latin America & Caribbean", "North America",
                             "South Asia")) +
  ylab("Fertility Rate") + 
  labs(title='Yearly Fertility Rate vs Regions') +
  scale_x_date(date_breaks = "2 year", labels = date_format("%Y"))  +
  theme(panel.grid.major = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = 0.25), 
        axis.title.x=element_blank(), axis.text.x = element_text(angle = 30, hjust=1, vjust = .5), 
        legend.key = element_rect(fill = NA, colour = NA, size = 0.25))+
  theme(plot.title = element_text(hjust = 0.5, face="bold"), 
        plot.subtitle = element_text(hjust = 0.5))


#########################################################################################################



