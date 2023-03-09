library(haven)
library(tidyverse)
library(tidycensus)
library(acs)
library(ggplot2)
library(sf)
library(sp)

data_2012 = read_dta('/home/sofi/DSI posdoc fellowship/codes/mortality_data/US mort 05-15/us_county2012.dta')
#names(data_2011)
#View(data_2011)

### select some variables 

D2012 = data_2012 %>% dplyr::select(countyoc,stateoc,monthdth,sex,weekday,year,mandeath,ucod)
rm(data_2012)

## select deaths due to (1) malignant neoplasm of bronchus and lung
## and (2) chronic obstructive pulmonary disease
wh1=which(str_detect(D2012$ucod,'C34')==1)
wh2=which(str_detect(D2012$ucod,'J44')==1)

D2012F = D2012[c(wh1,wh2),]
D2012F$stateoc=as.factor(D2012F$stateoc)


a=D2012F %>% group_by(stateoc) %>% summarise(nCounts=n())
View(a)

# WA=D2012F %>% filter(stateoc=='WA')
# TX=D2012F %>% filter(stateoc=='TX')

U.states=D2012F %>% filter(stateoc%in%c('NM', 'TX','OK','AZ','UT','CO','KS','NV'))


# to get county number FIPS codes
fips_codes %>% filter(state_name=='California')

U.states=U.states %>% 
  group_by(stateoc) %>%
  mutate(CountyF = case_when(
    all( stateoc=="TX") ~ paste('48',countyoc,sep=''),
    all( stateoc=="OK") ~ paste('40',countyoc,sep=''),
    all( stateoc=="NM") ~ paste('35',countyoc,sep=''),
    all( stateoc=="AZ") ~ paste('04',countyoc,sep=''),
    all( stateoc=="UT") ~ paste('49',countyoc,sep=''),
    all( stateoc=="CO") ~ paste('08',countyoc,sep=''),
    all( stateoc=="KS") ~ paste('20',countyoc,sep=''),
    all( stateoc=="NV") ~ paste('32',countyoc,sep='')
  ))


states.counts=U.states %>% group_by(CountyF) %>% summarise(nCounts=n())

# to get county number FIPS codes
# View(fips_codes %>% filter(state_name=='Washington'))
# dim(fips_codes %>% filter(state_name=='Washington'))
# WA=WA %>% mutate(CountyF=paste('53',countyoc,sep=''))
# WA.counts=WA %>% group_by(CountyF) %>% summarise(nCounts=n())
# 
# 
# #View(fips_codes %>% filter(state_name=='Texas'))
# # dim(fips_codes %>% filter(state_name=='Washington'))
# TX=TX %>% mutate(CountyF=paste('48',countyoc,sep=''))
# TX.counts=TX %>% group_by(CountyF) %>% summarise(nCounts=n())


### Try to convert it into spatial data
census_api_key("f587b3ec8f8141986f57549151c63064f1ae1bb7",install=TRUE,overwrite = TRUE)
readRenviron("~/.Renviron")
Sys.getenv("CENSUS_API_KEY")

year = 2012
### US state ###
states_p <- get_acs(geography = "county", year=year,survey='acs5',
                    state = c("TX",'OK','NM','AZ','UT','CO','KS','NV'),geometry = TRUE,
                    variables=c(population = "B01001_001"))

U.states$CountyF=as.factor(U.states$CountyF)
states_p$CountyF=as.factor(states_p$GEOID)

pt2=states_p$geometry %>% sf::st_cast("POLYGON",group_or_split=FALSE)
county_geometry=as(states_p$geometry %>% st_cast("POLYGON",group_or_split=FALSE), 'Spatial')


all.states.tpm=sp::merge(states.counts,states_p,by="CountyF",all=TRUE)
all.states.tpm=subset(all.states.tpm, select = -c(moe,variable,NAME,geometry))
all.states=SpatialPolygonsDataFrame(Sr=county_geometry, data=all.states.tpm,match.ID=FALSE)
#all.states

# # mortality counts
spplot(all.states,zcol="nCounts",axes=TRUE)
# # population size
spplot(all.states,zcol="estimate",axes=TRUE)

# # Population by tract
population_by_tracts <- get_acs(geography = "tract", year=year,survey='acs5',
                                state = c("TX",'OK','NM','AZ','UT','CO','KS','NV'),geometry = TRUE,
                                variables=c(population = "B01001_001"))

#population_by_tracts$estimate=population_by_tracts$estimate#/100
#population_by_tracts$estimate[which(!is.na(population_by_tracts$estimate))]=runif(length(which(!is.na(population_by_tracts$estimate))),10,10000)

pt1=population_by_tracts$geometry %>% st_cast("POLYGON",group_or_split=FALSE)

tract_geometry=as(pt1, 'Spatial')
all.population=SpatialPolygonsDataFrame(Sr=tract_geometry, 
                                        data=data.frame('estimate'=population_by_tracts$estimate),FALSE)

spplot(all.population,zcol="estimate",axes=TRUE)
plot(tract_geometry)
lines(county_geometry,col='red')



### Washington state ###
# WA_p <- get_acs(geography = "county", year=year,survey='acs5',
#                  state = "WA",geometry = TRUE,
#                  variables=c(population = "B01001_001"))
# 
# WA.counts$CountyF=as.factor(WA.counts$CountyF)
# WA_p$CountyF=as.factor(WA_p$GEOID)
# 
# county_geometry=as(WA_p$geometry %>% st_cast("POLYGON",group_or_split=FALSE), 'Spatial')
# 
# 
# all.wash.tpm=sp::merge(WA.counts,WA_p,by="CountyF",all=TRUE)
# all.wash.tpm=subset(all.wash.tpm, select = -c(moe,variable,NAME,geometry))
# all.wash=SpatialPolygonsDataFrame(Sr=county_geometry, data=all.wash.tpm,match.ID=FALSE)
# all.wash
# 
# # mortality counts
# spplot(all.wash,zcol="nCounts",axes=TRUE)
# # population size
# spplot(all.wash,zcol="estimate",axes=TRUE)
# 
# # Population by tract
# population_by_tracts <- get_acs(geography = "tract", year=year,survey='acs5',
#                                 state = "WA",geometry = TRUE,
#                                 variables = c(population = "B01001_001"))
# 
# pt1=population_by_tracts$geometry %>% sf::st_cast("POLYGON",group_or_split=FALSE)
# tract_geometry=as(pt1, 'Spatial')
# 
# ### Texas state ###
# TX_p <- get_acs(geography = "county", year=year,survey='acs5',
#                 state = "TX",geometry = TRUE,
#                 variables=c(population = "B01001_001"))
# 
# TX.counts$CountyF=as.factor(TX.counts$CountyF)
# TX_p$CountyF=as.factor(TX_p$GEOID)
# 
# county_geometry=as(TX_p$geometry %>% st_cast("POLYGON",group_or_split=FALSE), 'Spatial')
# 
# all.tx.tpm=sp::merge(TX.counts,TX_p,by="CountyF",all=TRUE)
# all.tx.tpm=subset(all.tx.tpm, select = -c(moe,variable,NAME,geometry))
# all.tx=SpatialPolygonsDataFrame(Sr=county_geometry, data=all.tx.tpm,match.ID=FALSE)
# all.tx
# 
# # mortality counts
# spplot(all.tx,zcol="nCounts",axes=TRUE)
# # population size
# spplot(all.tx,zcol="estimate",axes=TRUE)
# 
# # Population by tract
# 
# population_by_tracts <- get_acs(geography = "tract", year=year,survey='acs5',
#                                 state = "TX",geometry = TRUE,
#                                 variables = c(population = "B01001_001"))
# 
# pt1=population_by_tracts$geometry %>% sf::st_cast("POLYGON",group_or_split=FALSE)
# tract_geometry=as(pt1, 'Spatial')
# 


# Simulate covarite 
# cov1=SpatialPolygonsDataFrame(Sr=tract_geometry, data=data.frame(rep(0,dim(coordinates(tract_geometry))[1])),FALSE)
# #spplot(cov1,axes=TRUE)
# 
# #population_by_tracts$cov1=x1 
# population_by_tracts$cov1=rep(0,dim(coordinates(tract_geometry))[1])  

