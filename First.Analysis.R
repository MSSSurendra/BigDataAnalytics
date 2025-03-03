train_data1<-read.csv(file.choose())
dim(train_data1)
train_data<-train_data1
data.frame()
dim(train_data)
summary(train_data$SalePrice)
names(train_data)
colnames(train_data)[colSums(is.na(train_data))>nrow(train_data)/2]
train_data<-train_data[,colSums(is.na(train_data))<nrow(train_data)/2]
table(colSums(is.na(train_data)))
ind<-sample(2,nrow(train_data),replace=T,prob = c(0.8,0.2))
train_data<-train_data[ind==1,]
validation_data<-train_data[ind==2,]
dim(train_data)
summary(train_data$SalePrice)

set.seed(222)
library(randomForest)
attach(train_data)
model<-randomForest(SalePrice~GrLivArea+Neighborhood+OverallQual+TotalBsmtSF+X1stFlrSF+CentralAir+BsmtFinSF1+ExterQual+GarageArea+X2ndFlrSF, data = train_data, ntree=200 , importance=TRUE)
plot(model)
attributes(model)
model
varImpPlot(model,n.var = 10)
dim(validation_data)
validation<-predict(model,validation_data,na.action=na.roughfix)
write.csv(validation)
test_data=read.csv(file.choose())

install.packages("gapminder")
install.packages("prettydoc")
library(gapminder) # data from gapminder.org
library(dplyr) # used to perform data transformation and manipulation
library(ggplot2) # used for data visualization
library(prettydoc) # document themes for R Markdown
?gapminder
nrow(gapminder_unfiltered) # total number of observations
ncol(gapminder_unfiltered) # total number of variables
summary(gapminder_unfiltered)
#There appeared to be no missing values in the data set:
sum(is.na(gapminder_unfiltered))
#The following continents are in the data set:
unique(gapminder_unfiltered$continent)
#The total number of countries in the data set is shown below:
countries <- unique(gapminder_unfiltered$country)
length(countries)
#Exploratory Data Analysis
#1. Distribution of GDP per capital in 2007
# filter to obtain the data for 2007 
gapminder2007 <- filter(gapminder_unfiltered, year == 2007)
# plot the distribution of GDP per capital in 2007
ggplot(gapminder2007, aes(gdpPercap)) +
  geom_histogram(bins = 80, fill="darkgreen") + 
  ggtitle("2007 GDP Distribution across Countries") +
  xlab("GDP per Capita") + ylab("Count") +
  theme_classic()
xlab("Year") + theme_classic()

#2. Difference in GDP distribution for continents in 2007
# for the gapminder data for 2007, group by continent
# to show the average GDP per continent
GDPbyCont <- gapminder2007 %>% 
  group_by(continent) %>% 
  summarize(avgGDP = mean(gdpPercap))
# plot the avgGDP for each continent, to show the 
# GDP by continent in 2007
ggplot(GDPbyCont, 
       aes(x = continent, y = avgGDP, color = continent)) +
  geom_point(size=4) +
  ggtitle("2007 GDP by Continent") +
  xlab("Continent") + ylab("Avg. GDP per Capita") +
  theme_classic()

#3. Top 10 countries with largest GDP per capita in 2007
# arrange gapminder data for 2007 in order from largest
# to smallest GDP per capita, then select the top 10 countries
top_n(arrange(gapminder2007, -gdpPercap), 10)


#4. US GDP per Capita over Time
# filter to obtain only US data
USgapminder <- filter(gapminder_unfiltered, country == "United States")
# plot data of GDP per capita over time for the US
ggplot(USgapminder,aes(year,gdpPercap)) + 
  geom_smooth() + 
  ggtitle("US GDP per Capita over Time") + 
  ylab("GDP Per Capita") +
  xlab("Year") + theme_classic()


#5. Percent growth/decline in GDP per capita in 2007 for the US?
# Find US GDP per capita in 2007
GDP2007 <- select(filter(USgapminder, year == 2007),gdpPercap)
# Find US GDP per capita in 2005 (the last year with GDP
# data before 2007)
GDP2005 <- select(filter(USgapminder, year == 2005),gdpPercap)
# Calculate the percent change in GDP per capita
((GDP2007 - GDP2005)/GDP2005)*100


#6. Historical growth/decline in GDP per capita for the US
# gdp by continent
ggplot(gapminder_unfiltered, 
       aes(year,gdpPercap, colour = continent)) + 
  geom_smooth() + ggtitle("GDP over Time by Continent") +
  xlab("Year") + ylab("Avg. GDP per Capita") +
  theme_classic()


# population by continent
ggplot(gapminder_unfiltered, 
       aes(year,pop, colour = continent)) + 
  geom_smooth() + ggtitle("Population over Time by Continent") +
  xlab("Year") + ylab("Avg. Population") +
  theme_classic()


# life expectation by continent
ggplot(gapminder_unfiltered, 
       aes(year, lifeExp, colour = continent)) + 
  geom_smooth() + ggtitle("Life Expectation over Time by Continent") +
  xlab("Year") + ylab("Life Expectancy") +
  theme_classic()









