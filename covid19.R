# https://78462f86-a-e2d7344e-s-sites.googlegroups.com/a/rdatamining.com/www/docs/Coronavirus-data-analysis-world.pdf?attachauth=ANoY7crOR5slz0B0eOmaP6cZS808VvyrPiudug-5FpnyPHUz4N5E8ExWazPuvBMOxhtRsUVTYv_uZtftB1bJ5hYUV1VB9Kgct9bOQEQGPVRcxzjEZhSwaMYgTF-m5-MSrx1HryLLmwjyUwgsaJEbhWWi-gUcxwjBLhlE3nzNAgx6XO9G4IiVYA40sU_1Lnl94xJxHdujj66042lrhda-Phdb41iEFKj3QWKqrg33Sofvzh_fyB7LI7w%3D&attredirects=0

library(magrittr)
library(lubridate)
library(tidyverse)
library(gridExtra)
library(kableExtra)

# 존스홉킨스 대학교 : CSSE COVID-19 Dataset
url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-"

## source data files
Confirmed = paste0(url, "Confirmed.csv")
Deaths    = paste0(url, "Deaths.csv")
Recovered = paste0(url, "Recovered.csv")

## load data into R
data.confirmed <- read.csv(Confirmed)
data.deaths    <- read.csv(Deaths)
data.recovered <- read.csv(Recovered)
dim(data.confirmed)

n.col <- ncol(data.confirmed)
## get dates from column names
dates <- names(data.confirmed)[5:n.col] %>% substr(2,8) %>% mdy()
range(dates)
max.date = max(dates)

## data cleaning and transformation
cleanData <- function(data) {
  ## remove some columns
  data %<>% select(-c(Province.State, Lat, Long)) %>% rename(country=Country.Region)
  ## convert from wide to long format
  data %<>% gather(key=date, value=count, -country)
  ## convert from character to date
  data %<>% mutate(date = date %>% substr(2,8) %>% mdy())
  ## aggregate by country
  data %<>% group_by(country, date) %>% summarise(count=sum(count)) %>% as.data.frame()
  return(data)
}
## clean the three datasets
data.confirmed %<>% cleanData() %>% rename(confirmed=count)
data.deaths %<>% cleanData() %>% rename(deaths=count)
data.recovered %<>% cleanData() %>% rename(recovered=count)

## merge above 3 datasets into one, by country and date
data <- data.confirmed %>% merge(data.deaths) %>% merge(data.recovered)
## first 10 records when it first broke out in China
data %>% filter(country=='Mainland China') %>% head(10)

## counts for the whole world
data.world <- data %>% group_by(date) %>%
  summarise(country='World',
            confirmed = sum(confirmed),
            deaths = sum(deaths),
            recovered = sum(recovered))
data %<>% rbind(data.world)
## remaining confirmed cases
data %<>% mutate(remaining.confirmed = confirmed - deaths - recovered)

## sort by country and date
data %<>% arrange(country, date)
## daily increases of deaths and cured cases
## set NA to the increases on day1
n <- nrow(data)
day1 <- min(data$date)
data %<>% mutate(confirmed.inc = ifelse(date == day1, NA, confirmed - lag(confirmed, n=1)),
                 deaths.inc = ifelse(date == day1, NA, deaths - lag(deaths, n=1)),
                 recovered.inc = ifelse(date == day1, NA, recovered - lag(recovered, n=1)))
## death rate based on total deaths and cured cases
data %<>% mutate(rate.upper = (100 * deaths / (deaths + recovered)) %>% round(1))
## lower bound: death rate based on total confirmed cases
data %<>% mutate(rate.lower = (100 * deaths / confirmed) %>% round(1))
## death rate based on the number of death/cured on every single day
data %<>% mutate(rate.daily = (100 * deaths.inc / (deaths.inc + recovered.inc)) %>% round(1))
## 시각화
## ranking by confirmed cases
data.latest <- data %>% filter(date == max(date)) %>%
  select(country, date, confirmed, deaths, recovered, remaining.confirmed) %>%
  mutate(ranking = dense_rank(desc(confirmed)))
## top 10 countries: 12 incl. 'World' and 'Others'
top.countries <- data.latest %>% filter(ranking <= 12) %>%
  arrange(ranking) %>% pull(country) %>% as.character()
## move 'Others' to the end
top.countries %<>% setdiff('Others') %>% c('Others')
top.countries

# a <- data %>% group_by(country) %>% tally()
## put all others in a single group of 'Others'
df <- data.latest %>% filter(!is.na(country) & country!='World') %>%
  mutate(country=ifelse(ranking <= 12, as.character(country), 'Others')) %>%
  mutate(country=country %>% factor(levels=c(top.countries)))
df %<>% group_by(country) %>% summarise(confirmed=sum(confirmed))
## precentage and label
df %<>% mutate(per = (100*confirmed/sum(confirmed)) %>% round(1)) %>%
  mutate(txt = paste0(country, ': ', confirmed, ' (', per, '%)'))
# pie(df$confirmed, labels=df$txt, cex=0.7)
df %>% ggplot(aes(fill=country)) +
  geom_bar(aes(x='', y=per), stat='identity') +
  coord_polar("y", start=0) +
  xlab('') + ylab('Percentage (%)') +
  labs(title=paste0('Top 10 Countries with Most Confirmed Cases (', max.date, ')')) +
  scale_fill_discrete(name='Country', labels=df$txt)

## convert from wide to long format, for purpose of drawing a area plot
data.long <- data %>% select(c(country, date, confirmed, remaining.confirmed, recovered, deaths)) %>%
  gather(key=type, value=count, -c(country, date))
## set factor levels to show them in a desirable order
data.long %<>% mutate(type = factor(type, c('confirmed', 'remaining.confirmed', 'recovered', 'deaths')))
## cases by type
df <- data.long %>% filter(country %in% top.countries) %<>%
  mutate(country=country %>% factor(levels=c(top.countries)))
df %>% filter(country != 'World') %>%
  ggplot(aes(x=date, y=count, fill=country)) +
  geom_area() + xlab('Date') + ylab('Count') +
  labs(title='Cases around the World') +
  theme(legend.title=element_blank()) +
  facet_wrap(~type, ncol=2, scales='free_y')

## excluding Mainland China
df %>% filter(!(country %in% c('World', 'Mainland China'))) %>%
  ggplot(aes(x=date, y=count, fill=country)) +
  geom_area() + xlab('Date') + ylab('Count') +
  labs(title='Cases around the World (excl. China)') +
  theme(legend.title=element_blank()) +
  facet_wrap(~type, ncol=2, scales='free_y')

## if Australia in not in top 10, add it in and remove 'Others'
if(!('Australia' %in% top.countries)) {
  top.countries %<>% setdiff('Others') %>% c('Australia')
  df <- data.long %>% filter(country %in% top.countries) %<>%
    mutate(country=country %>% factor(levels=c(top.countries)))
}
## cases by country
df %>% filter(type != 'confirmed') %>%
  ggplot(aes(x=date, y=count, fill=type)) +
  geom_area(alpha=0.5) + xlab('Date') + ylab('Count') +
  labs(title=paste0('COVID-19 Cases by Country (', max.date, ')')) +
  scale_fill_manual(values=c('red', 'green', 'black')) +
  theme(legend.title=element_blank(), legend.position='bottom') +
  facet_wrap(~country, ncol=3, scales='free_y')

name_top = c("세계", "중국", "이태리", "한국",
              "이란", "프랑스", "독일", "스페인", "미국", "일본", "스위스", "호주")

country_df = function(cnt){
# data %<>% filter(country=='Mainland China')
# data %<>% filter(country=='Australia')
# data %<>% filter(country=='South Korea')
# data %<>% filter(country=='World')
data1 = data %<>% filter(country==cnt)
n <- nrow(data1)
name1 <-  name_top[top.countries==cnt]
## current confirmed and its increase
plot1 <- ggplot(data1, aes(x=date, y=remaining.confirmed)) +
  geom_point() + geom_smooth() +
  xlab('월일') + ylab('인원수') + labs(title=paste(name1,' : 누적 확진자수'))
plot2 <- ggplot(data1, aes(x=date, y=confirmed.inc)) +
  geom_point() + geom_smooth() +
  xlab('월일') + ylab('인원수') + labs(title=paste(name1,' : 확진자수'))
# + ylim(0, 4500)
grid.arrange(plot1, plot2, ncol=2)
}
