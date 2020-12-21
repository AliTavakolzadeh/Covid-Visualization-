#DATA VISUALIZATION FINAL PROJECT
#Group Members: Ali Razavi, Ali Tavakolzadeh, Saqib Hameed, Kaysi Castor, Collin Morgenroth


#Slide 3-5
confirmedraw <- read.csv("~/i485/Data Visualization MSA 8020/Week 6/Final Project/time_series_covid19_confirmed_global.csv")
deathsraw <- read.csv("~/i485/Data Visualization MSA 8020/Week 6/Final Project/time_series_covid19_deaths_global.csv")
recoveredraw <- read.csv("~/i485/Data Visualization MSA 8020/Week 6/Final Project/time_series_covid19_recovered_global.csv")


library(tidyr)
library(dplyr)
library(deSolve)
confirmed <- confirmedraw %>% gather(key="date", value="confirmed", -c(Country.Region, Province.State, Lat, Long)) %>% group_by(Country.Region, date) %>% summarize(confirmed=sum(confirmed))
deaths <- deathsraw %>% gather(key="date", value="deaths", -c(Country.Region, Province.State, Lat, Long)) %>% group_by(Country.Region, date) %>% summarize(deaths=sum(deaths))
recovered <- recoveredraw %>% gather(key="date", value="recovered", -c(Country.Region, Province.State, Lat, Long)) %>% group_by(Country.Region, date) %>% summarize(recovered=sum(recovered))
summary(confirmed)

# Final data
country <- full_join(confirmed, deaths) %>% full_join(recovered)

# Date variable
country$date <- country$date %>% sub("X", "", .) %>% as.Date("%m.%d.%y")
country <- country %>% group_by(Country.Region) %>% mutate(cumconfirmed=cumsum(confirmed), days = date - first(date) + 1)
# Aggregate at world level
world <- country %>% group_by(date) %>% summarize(confirmed=sum(confirmed), cumconfirmed=sum(cumconfirmed), deaths=sum(deaths), recovered=sum(recovered)) %>% mutate(days = date - first(date) + 1)
world2 <-country %>% group_by(date) %>% summarize(confirmed=mean(confirmed), cumconfirmed=mean(cumconfirmed), deaths=mean(deaths), recovered=mean(recovered)) %>% mutate(days = date - first(date) + 1)
# GRAPHS
library(ggplot2)
library(plotly)

# World confirmed, deaths and recovered
str(world)
world2 %>% 
  ggplot(aes(x=date, y=(cumconfirmed)/320000, colour="")) + geom_bar(stat="identity", width=1, fill="blue",color="Blue") +
  theme_classic() +
  labs(title = "Covid-19 Global Cases", x= "Date", y= " Confirmed Daily cases ") +
  theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.y=element_blank(),
                                                      axis.ticks.y=element_blank())


countryselection <- country %>% filter(Country.Region==c("United States", "Italy", "China", "France", "United Kingdom", "Germany"))
ggplot(countryselection, aes(x=date, y=countryselection$confirmed , colour=Country.Region)) + geom_line(size=1) +
  theme_classic() +
  labs(title = "Covid-19 Cumulative Confirmed Cases by Country", x= "Date", y= "Cumulative Daily Confirmed Cases (log scale)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(trans="log10")+theme(axis.title.y=element_text(),
                                          axis.text.y=element_blank(),
                                          axis.ticks.y=element_blank())
str(countryselection)
countryselection %>% gather("Type", "Cases", -c(date, days, Country.Region)) %>%
  ggplot(aes(x=date, y=Cases, colour=Country.Region)) + geom_line(size=1) +
  theme_classic() +
  labs(title = "Covid-19 Cases by Country", x= "Days", y= "Cumulative Daily cases (log scale)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(trans="log10") +
  facet_grid(rows=vars(Type))



## Map
countrytotal <- country %>% group_by(Country.Region) %>% summarize(cumconfirmed=sum(confirmed), deaths=mean(deaths), cumrecovered=sum(recovered))

library(tmap)
data(World)
class(World)

countrytotal$Country.Region[!countrytotal$Country.Region %in% World$name]
list <- which(!countrytotal$Country.Region %in% World$name)
countrytotal$country <- as.character(countrytotal$Country.Region)
countrytotal$country[list] <-
  c("Andorra", "Antigua and Barbuda", "Bahrain",
    "Barbados", "Bosnia and Herz.", "Myanmar",
    "Cape Verde", "Central African Rep.", "Congo",
    "Dem. Rep. Congo", "Czech Rep.", "Diamond Princess",
    "Dominica", "Dominican Rep.", "Eq. Guinea",
    "Swaziland", "Grenada", "Holy See",
    "Korea", "Lao PDR", "Liechtenstein",
    "Maldives", "Malta", "Mauritius",
    "Monaco", "MS Zaandam", "Macedonia",
    "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines",
    "San Marino", "Sao Tome and Principe", "Seychelles",
    "Singapore", "S. Sudan", "Taiwan",
    "United States", "Palestine", "W. Sahara")
countrytotal$Country.Region[!countrytotal$country %in% World$name]
World$country <- World$name
worldmap <- left_join(World, countrytotal, by="country")
worldmap$cumconfirmed[is.na(worldmap$cumconfirmed)] <- 0

# Map
ggplot(data = worldmap) + geom_sf(aes(fill=deaths), color="black") +
  ggtitle("World Map of Total Deaths",
          subtitle="Total Deaths,By OCT 2020") +scale_fill_gradient(low = "lightblue", high = "blue")+
  theme_bw()


ggplot(data = worldmap) + geom_sf(aes(fill=cumconfirmed), color="black") +
  ggtitle("World Map of Total Deaths",
          subtitle="Total Deaths,By OCT 2020") +scale_fill_gradient(low = "lightblue", high = "blue")+
  theme_bw()

#Slide 6-7

install.packages("ggplot2")
install.packages("dplyr")
install.packages("scales")
install.packages("stringr")
install.packages("tidyverse")
install.packages("gganimate")
install.packages("png")
install.packages("gifski")


library(readr)
library(ggplot2)
library(dplyr)
library(scales)
library(stringr)
library(tidyverse)
library(gganimate) 

cases_worldwide = read.csv("~/i485/Data Visualization MSA 8020/Week 6/Final Project/Raw Data.csv")
View(cases_worldwide)
# in the Raw Data CSV file Column Date showed as ï..date

cases_worldwide$format = substr(cases_worldwide$ï..date, 0, 1)
cases_worldwide$format = as.numeric(formatC(cases_worldwide$format,width = 2, flag = 0))
cases_worldwide$month = month.abb[cases_worldwide$format]


daily_confirmed_cases_worldwide = cases_worldwide %>% 
  filter(type == "confirmed") %>%
  group_by(ï..date) %>%
  summarise(total_cases = sum(cases))


daily_confirmed_cases_worldwide$format = substr(daily_confirmed_cases_worldwide$ï..date, 0, 1)
daily_confirmed_cases_worldwide$format = as.numeric(formatC(daily_confirmed_cases_worldwide$format,
                                                            width = 2, flag = 0))
daily_confirmed_cases_worldwide$month = month.abb[daily_confirmed_cases_worldwide$format]


monthly_confirmed_cases_worldwide = daily_confirmed_cases_worldwide %>% 
  group_by(month) %>%
  summarise(total_cases = sum(total_cases))


monthly_confirmed_cases_worldwide$month = factor(monthly_confirmed_cases_worldwide$month,levels =
                                                   c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct"))


ggplot(monthly_confirmed_cases_worldwide, aes(x = month, y = total_cases)) + 
  geom_col(aes(fill = total_cases)) +
  theme(
    legend.box      = "vertical",
    legend.key      = element_blank(),
    legend.title    = element_blank(),
    legend.position = "right") + 
  scale_fill_gradient(low = "green", high = "red", labels = comma) +
  scale_y_continuous(labels = comma) +
  geom_text(aes(label = round(total_cases / 1000000, digits = 2)), 
            position=position_dodge(width=0.9), vjust=-0.25) +
  labs(x = "Month", y = "Total Confirmed Cases", title  = "Confirmed Cases Worldwide by Month")





cases_worldwide$ï..date = as.Date(cases_worldwide$ï..date, "%m/%d/%y")


cases_worldwide_rank = cases_worldwide %>%
  select(ï..date, country, type, cases, month) %>%
  filter(type == "confirmed") %>%
  group_by(ï..date, country) %>%
  summarise(total_cases = sum(cases)) %>%
  arrange(ï..date, -total_cases) %>%
  mutate(rank = 1:n()) %>%
  filter(rank <= 10)


cases_worldwide_rank$cum_total_cases =  ave(cases_worldwide_rank$total_cases, cases_worldwide_rank$country, FUN = cumsum)

cases_worldwide_rank = cases_worldwide_rank %>%
  arrange(ï..date, -cum_total_cases) %>%
  mutate(rank = 1:n()) %>%
  filter(rank <= 10)



my_plot = cases_worldwide_rank %>% 
  ggplot() + aes(xmin = 0, xmax = cum_total_cases) +
  aes(ymin = rank - 0.45, ymax = rank + 0.45, y = rank) +
  facet_wrap(~ ï..date) + geom_rect(alpha = 0.7) + aes(fill = country, group = ï..date) +
  theme(legend.position = "none") +
  scale_fill_viridis_d(option = "magma",  direction = -1) +
  scale_x_continuous(labels = comma, limits = c(-2000000, 7100000)) +
  geom_text(col = "gray13", hjust = "right", aes(label = country), x = -5000) + 
  scale_y_reverse() + labs(fill = NULL, x = "Total Number of Cases", y = "")



anim = my_plot +  
  facet_null() +  
  geom_text(x = 6000000 , y = -10,  
            family = "Times",  
            aes(label = as.character(ï..date)),  
            size = 10, col = "grey18") +  
  aes(group = country) + transition_time(ï..date)


anim_save("~/i485/Data Visualization MSA 8020/Week 6/Final Project/coronavirus_ranking.gif", animate(anim, duration = 50, fps = 30))



# Slide 8-9
require("dplyr")
require("ggplot2")
require("readr")
require("tidyr")
require(plotly)
require(readxl)
library(scales)


US_StatesInfo<-read_csv("~/i485/Data Visualization MSA 8020/Week 6/Final Project/StatePopulationVs.CovidCases.csv")
US_StatesInfo

mydata<-US_StatesInfo %>%
  select(States,ConfirmedCases,Deaths,StatesPopulation)%>%
  mutate(Rate1= (Deaths / ConfirmedCases)*100)%>%
  mutate(Rate2= (Deaths / StatesPopulation)*100)%>%
  mutate(Rate3= (ConfirmedCases / StatesPopulation)*100)
mydata               

myplot1<- plot_ly(mydata, x = ~ States,
                  y = ~ StatesPopulation,
                  name = 'States Population', 
                  color = 'black',
                  type = 'bar',
                  mode = 'none', 
                  stackgroup = 'one')%>%
  add_trace(y = ~ ConfirmedCases, 
            name = "Confirmed Cases",
            color = 'yellow') %>%
  add_trace(y = ~ Deaths, 
            name = 'Death', 
            color = 'red')%>%
  layout(title = "Data of Population, Covid Cases and Death for U.S. States",
         legend = list(x = 0.4, y = 1),
         yaxis = list(title = "Population/Number of Cases"),
         xaxis = list(title = "U.S. States"))

myplot1

myplot3<- plot_ly(mydata, x = ~ States,
                  y = ~ Rate3,
                  name = 'Confirmed Cases to Population', 
                  color = 'black',
                  type = 'bar',
                  mode = 'none', 
                  stackgroup = 'one')%>%
  add_trace(y = ~ Rate2, 
            name = "Death to States Population",
            color = 'yellow') %>%
  add_trace(y = ~ Rate1, 
            name = 'Death to Confirmed Cases', 
            color = 'red')%>%
  layout(title = "Rates of Covid19 Cases for U.S. States",
         legend = list(x = 0.4, y = 1),
         yaxis = list(title = "Rate of Cases(%)"),
         xaxis = list(title = "U.S. States"))

myplot3

#Slide 10-11
library(plotly)
library(ggforce)
library(ggplot2)
library(datasets)
library(dplyr)
library(readr)
library(trelliscopejs)
library(scales)
library(tidyverse)

states_covid19 <- read_csv("~/i485/Data Visualization MSA 8020/Week 6/Final Project/us_states_covid19_daily.csv", col_names = TRUE)
View(states_covid19)
class(states_covid19$positive)

#changing format to date
states_covid19 = transform(states_covid19, date = as.Date(as.character(date), "%Y%m%d"))


#filtering by States for Graph
hithard = filter(states_covid19,state == "WA"| state =="NY"| state == "CA")
View(hithard)

#changing formating of columns
hithard$positive = as.numeric(hithard$positive)
hithard$totalTestResults = as.integer(hithard$totalTestResults)
hithard$death = as.numeric(hithard$death)
hithard$positive_vs_totaltests = as.numeric(hithard$positive_vs_totaltests)



#Graph 1 comparing positive cases to total cases for CA,NY,WA
library(crosstalk)
hh <- highlight_key(hithard)
widgets <- bscols(
  widths = c(12, 12),
  filter_slider("date", "Date", hh , ~date),
  filter_checkbox("State", "State", hh, ~state, inline = TRUE)
)
bscols(
  widths = c(12, 12), widgets, 
  plot_ly(hh, y = ~positive, x = ~totalTestResults, showlegend = TRUE)%>% 
    add_lines(color = ~state, colors = c("orange", "blue", "green"))%>%
    layout(
      title = 'Total Test vs. Positive Test Over Time',
      yaxis = list( title = 'Positive Test'),
      xaxis = list( title = 'Total Test')
    ))


#Graph 2
hithard %>%
  plot_ly()%>% 
  add_bars(y = ~death, x = ~date, color = ~state, color = "RdGy")%>%
  layout(
    title = 'Deaths Per State',
    yaxis = list( title = 'Deaths'),
    xaxis = list( title = 'Date'))

#Slide 12-13
library(tidyverse) 
library(ggplot2)
library(dplyr)
library(datasets)
install.packages("usmap")
require(usmap)
install.packages("openintro")
require(openintro)
require(maps)


covidus <- read_csv("~/i485/Data Visualization MSA 8020/Week 6/Final Project/us_states_covid19_daily.csv")
covidus = transform(covidus, date = as.Date(as.character(date), "%Y%m%d"))

abb=covidus$state
region=abbr2state(abb)

covidus2=cbind(covidus,region)
covidus2['fips'] <- fips(covidus2$state)
states=map_data("state")
####Aggregating the data ####

states2=states %>%
  group_by(region) %>%
  summarise(lat = max(lat),long=max(long))


covid_agg=covidus2 %>%
  group_by(region,state) %>%
  summarise(total_pos = as.integer(positive,na.rm=TRUE),total_neg=as.integer(negative,na.rm = TRUE),
            total_nt=as.integer(pending,na.rm = TRUE))
covid_agg$region=tolower(covid_agg$region)

map.df <- merge(covid_agg,states2, by="region", all.x = T)
####total positive cases in the US ####

plot_usmap(data = map.df, values = "total_pos",   labels=TRUE) + 
  scale_fill_continuous( low = "green", high = "red", 
                         name = "Positive Cases", label = scales::comma
  ) + 
  theme(legend.position = "right",plot.title=element_text(hjust=.5)) + 
  theme(panel.background = element_rect(colour = "black")) + 
  labs(title = "Positive Cases of Covid", caption = "Source: @SRK")


####total positive cases in Three State that opened early in US####
plot_usmap(data = map.df, values = "total_pos",   labels=TRUE,include = c("GA", "AL", "FL")) + 
  scale_fill_continuous( low = "green", high = "red", 
                         name = "Positive Cases", label = scales::comma
  ) + 
  theme(legend.position = "right",plot.title=element_text(hjust=.5)) + 
  theme(panel.background = element_rect(colour = "black")) + 
  labs(title = "Positive Cases of Covid in States that Re-opened Early", caption = "Source: @SRK")


### Subsetting the data for highly infected states ####
sub_us=subset(covidus2 , subset =(covidus2$state %in% c('GA','AL','FL')))
####Infected patients growth in 3 states
p <- ggplot(data = sub_us, aes(x = date, y = positive),color= state) + options(scipen=999)

p + geom_line(aes(group = state)) + facet_wrap(~ state) +
  labs(title = "Positive Case Rates in AL, GA, and FL")

### death trend in 3 states 
p <- ggplot(data = sub_us, aes(x = date, y = death),color= state)

p + geom_line(aes(group = state)) + facet_wrap(~ state) +
  labs(title = "Death Rates in AL, GA, and FL")
