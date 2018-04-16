library(xml2)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)
library(leaflet)
library(zipcode)
### NSF 2017 data
setwd('/Users/zhang/Downloads/NSFgrants')
folders = list.files()
xmlfile = list.files('/Users/zhang/Downloads/NSFgrants/2017')


parsexml = function(file){
  x=read_xml(file)
  c=xml_child(x)
  l1=as_list(c)
  b=t(unlist(l1))
  as.data.frame(b)
}

setwd('/Users/zhang/Downloads/NSFgrants/2017')
xmldf=parsexml(xmlfile[1])
for (i in xmlfile[-1]){
  xmldf=bind_rows(xmldf,parsexml(i))
}

# allna <- apply(xmldf, 1, function(x) all(is.na(x)))
# summary(allna)

count(xmldf$Institution.CityName)
count(xmldf$Institution.CountryName)
count(xmldf$Institution.StateName)
count(xmldf$AwardInstrument.Value)


xmldf = xmldf %>% rename(Org.Directorate = Organization.Directorate.LongName,
                 Org.Division = Organization.Division.LongName,
                 Award.Type=AwardInstrument.Value) %>% 
  mutate(Org.Directorate=gsub('Dir.*For ','',Org.Directorate),
         Org.Directorate=gsub('Off.*f ','',Org.Directorate),
         Org.Directorate=replace(Org.Directorate,Org.Directorate=='The Director','Office Of The Director'),
         Institution.CityName = tolower(Institution.CityName),
         AwardAmount=as.numeric(AwardAmount),
         Duration.Year = as.duration(mdy(xmldf$AwardExpirationDate)-mdy(xmldf$AwardEffectiveDate))/dyears(1),
         Zip = substr(gsub('^0000','',Institution.ZipCode),0,5))
xmldf = xmldf %>% mutate(Institution.CityName = gsub('[,.\']','',Institution.CityName),
  Institution.CityName = gsub('^mt ','mount ',gsub('^st ','saint ',gsub('-',' ', Institution.CityName)))) 

count(xmldf$Org.Division)
count(xmldf$Org.Directorate)
count(xmldf$Institution.CityName)

save(xmldf, file = '/Users/zhang/Downloads/NSFgrants/xmldf07edit.rda')
load('/Users/zhang/Downloads/NSFgrants/xmldf07edit.rda')

### NIH 2017 data
nihdf=readxl::read_xls('/Users/zhang/Downloads/Worldwide2017.xls')
nihdf = nihdf %>% mutate(Country = ifelse(`State or Country Name` %in% toupper(state.name), 'USA', `State or Country Name`))
nihdf$Country[nihdf$`State or Country Name`==  'DIST OF COL'] = 'USA'
nihdf$Country[nihdf$`State or Country Name`==  'PUERTO RICO'] = 'USA'

nihdf$City[nihdf$Country=='USA'] = tolower(nihdf$City[nihdf$Country=='USA'])
nihdf = nihdf %>% mutate(City = gsub('[,.\']','', City),
                         City = gsub('^st ','saint ',gsub('-',' ', City))) 
nihdf$City[nihdf$City=='ofallon'&nihdf$Country=='USA'] ='o fallon'
nihdf$City[nihdf$City=="newton centre"&nihdf$Country=='USA'] ="newton center"

nihdf = nihdf %>% mutate(Zip = gsub('[a-zA-Z, ]','', `Medical School Location`),
                         Zip = substr(Zip,0,5))
## get grant ID and YEAR
row = grep('^[0-9][A-Z][0-9][0-9][A-Z][A-Z]',nihdf$`Project Number`)
nihdf$id_year = NA
nihdf$id_year[row] = substr(nihdf$`Project Number`[row],5,15) 
nihdf=nihdf %>% separate(id_year,into = c('grant.ID', 'Year'))
nihdf$Year = as.numeric(nihdf$Year)  

save(nihdf, file='/Users/zhang/Downloads/nihdf2017.rda')
load('/Users/zhang/Downloads/nihdf2017.rda')

table(nihdf$Country)


#------plot count by directorate--------------------------


# g1=ggplot(xmldf, aes(x = reorder(Org.Directorate,table(Org.Directorate)[Org.Directorate]))) + 
#  geom_bar(aes(fill=Award.Type)) +scale_fill_brewer(palette = "Set3") +labs(x='Directorate') +
#  theme_light() + theme(axis.text.x = element_text(angle = -40))
# ggplotly(g1)


#---nsf
sum = xmldf %>% select(Org.Directorate,Award.Type,AwardAmount) %>% 
  group_by(Org.Directorate,Award.Type) %>% summarise(Count=n(),AmountSum=sum(AwardAmount)) %>% ungroup() %>%
  gather(key = stat,value = value,3:4)
reorder=levels(reorder(xmldf$Org.Directorate,-table(xmldf$Org.Directorate)[xmldf$Org.Directorate]))
sum$Org.Directorate = factor(sum$Org.Directorate,levels = reorder)
g2=sum %>%
  ggplot(aes(x = Org.Directorate, y= value,fill=Award.Type)) + 
  geom_bar(stat="identity") + scale_fill_brewer(palette = "Set3") + labs(x='Directorate') +
  facet_grid(stat~., scales = "free") + theme_light() +
  theme(axis.text.x = element_text(angle = -40),
        strip.text.y = element_text(colour = "black", face = "bold")) +
   ggtitle("NSF Awards Summary by Diretorate in 2017")
ggplotly(g2)


#sum = as.data.frame(table(xmldf$Org.Directorate,xmldf$Award.Type))
#sum %>% spread(Var2,Freq) %>%
#  plot_ly(y = ~Var1, x = ~`Continuing grant`, type = 'bar', orientation = 'h', name = 'Continuing grant') %>% 
#  add_trace(x = ~`Contract Interagency Agreement`, name = 'Contract Interagency Agreement') %>%
#  layout(xaxis = list(title = 'Count'),yaxis = list(title = ''), barmode = 'stack',
#         margin = list(l = 200, r = 10, t = 10, b = 40))

##---nih
sum2 = nihdf %>% select(`NIH Dept Combining Name`,`Funding Mechanism`,Funding) %>% 
  group_by(`NIH Dept Combining Name`,`Funding Mechanism`) %>% summarise(Count=n(),AmountSum=sum(Funding)) %>% ungroup() %>%
  gather(key = stat,value = value,3:4)
reorder2=levels(reorder(nihdf$`NIH Dept Combining Name`,-table(nihdf$`NIH Dept Combining Name`)[nihdf$`NIH Dept Combining Name`]))
sum2$`NIH Dept Combining Name` = factor(sum2$`NIH Dept Combining Name`,levels = reorder2)
g3=sum2 %>%
  ggplot(aes(x = `NIH Dept Combining Name`, y= value,fill=`Funding Mechanism`)) + 
  geom_bar(stat="identity") + scale_fill_brewer(palette = "Set3") + labs(x='Department') +
  facet_grid(stat~., scales = "free") + theme_light() +
  labs(fill = 'Funding Mechanism\nRPG:Research Project Grants\nSBIR: Small Business Innovation Research\nSTTR: Small Business Technology Transfer')+
  ggtitle("NIH Awards Summary by Department in 2017") +
  theme(axis.text.x = element_text(angle = -40),
        strip.text.y = element_text(colour = "black", face = "bold"),
        legend.position = 'right')
ggplotly(g3)

ggp_build <- plotly_build(g3)
ggp_build$layout$height = 800
ggp_build$layout$width = 400
ggp_build

#------------plot amount by year ------------
ggplotly(xmldf %>% filter(Award.Type %in% c('Continuing grant','Standard Grant')) %>% 
           ggplot(aes(x=Duration.Year, y = AwardAmount, color = Award.Type) + 
           geom_point(alpha=0.8))+ ggtitle('NSF Award Amount by Year Duration in 2017'))
ggplotly(nihdf %>% filter(!is.na(Year)) %>% 
           ggplot(aes(x=Year, y = Funding, color = `Funding Mechanism`)) + 
           geom_point(alpha=0.5)+ ggtitle('NIH Award Amount by Year Duration in 2017')+
           ylim(0,3.1e7))


##----------10 largest amount---------
top_n(xmldf, 10, AwardAmount) %>% select(Institution.Name, AwardAmount, Award.Type, Duration.Year, AwardID, Org.Directorate) %>%
  arrange(desc(AwardAmount))



#------------plot award amount by country/city/institute------------------
data(zipcode)
### add lat and long data--------------

# check na zip in USA
summary(is.na(xmldf$Zip[xmldf$Institution.CountryName=='United States']))
summary(is.na(nihdf$Zip[nihdf$Country=='USA']))

# city name not in zipcode data:
names(table(nihdf$City[nihdf$Country=='USA' & is.na(nihdf$Zip)]))[!names(table(nihdf$City[nihdf$Country=='USA' & is.na(nihdf$Zip)])) %in% tolower(zipcode$city)]
nrow(nihdf%>% filter(City %in% x)) # 677 rows no zip and not found in zipdata
# names(table(xmldf$Institution.CityName[xmldf$Institution.CountryName=='United States']))[!names(table(xmldf$Institution.CityName[xmldf$Institution.CountryName=='United States'])) %in% tolower(zipcode$city)]

state = data.frame(state.name,state.abb)
state = rbind(state,data.frame(state.name=c('District of Columbia','Puerto Rico'),state.abb=c('DC','PR')))
state = state %>% mutate(stateupper=toupper(state.name))
state$stateupper[state$state.abb=='DC'] = "DIST OF COL"
# number of institution per zipcode
# xmlloc %>% group_by(Zip) %>% summarise(n = length(unique(Institution.Name))) %>% arrange(desc(n))

### NSF
sumCity = xmldf %>% group_by(Zip) %>% summarise(FundingSum = sum(AwardAmount)) %>% 
  left_join(zipcode, by = c('Zip'='zip')) %>% 
  group_by(city,state) %>% 
  summarise(FundingSum = sum(FundingSum),
            latitude = mean(latitude),longitude = mean(longitude))
sumState=sumCity %>% group_by(state) %>% mutate(StateSum = sum(FundingSum)) %>% slice(1) %>%
  select(state,StateSum) %>% left_join(state, by = c('state'='state.abb'))

### mapstates + sumState
doc <- readLines("http://leafletjs.com/examples/choropleth/us-states.js")
# remove the javascript assignment at the front 
doc2 <- gsub("var statesData = ", "", doc)
# write out as a temp file and read
write(doc2, file = "tempgeo.json")
states <- geojsonio::geojson_read("tempgeo.json", what = "sp")
states@data=states@data %>% left_join(sumState, by = c('name'='state.name')) %>% mutate(name = as.factor(name))

pops=sprintf(
  "<strong>%s</strong><br/>Funding Sum: %g $",
  sumCity$city, sumCity$FundingSum
)%>% lapply(htmltools::HTML)

qpal <- colorBin("YlOrRd", states$StateSum/1e6, bins = c(0,5e6,1e7,2e7,8e7,2e8,4e8)/1e6)

sumCity %>% leaflet() %>% addTiles()%>%
  addPolygons(fillColor = ~qpal(StateSum/1e6), stroke = FALSE, data = states,
              label = as.character(paste0(states$name,', State Sum:',states$StateSum))) %>%
  addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
             radius = ~sqrt(FundingSum) * 20, label = ~city, popup = pops) %>%
  addLegend(pal = qpal, values = ~states$StateSum, opacity = 0.7, 
            title = "NSF Funding (2017)", position = "bottomright",
            labFormat = labelFormat(prefix = "$", suffix = ' Million' ))%>% 
  setView( lng = -100, lat = 40, zoom = 4)


### NIH
## join with zipdata
nihloc = nihdf %>% left_join(state, by = c('State or Country Name'='stateupper'))
nihState = nihloc %>% filter(!is.na(state.abb)) %>% select(state.name,Funding)%>% 
  group_by(state.name) %>% summarise(FundingSum = sum(Funding))

cityloc = zipcode %>% mutate(citylower = tolower(city)) %>% 
  group_by(citylower,state) %>% summarise(lat = mean(latitude),long=mean(longitude))
nihCity = nihloc %>% filter ( !is.na(state.abb)) %>% group_by(City,state.abb,Country) %>% summarise(citySum = sum(Funding)) %>% 
  left_join(cityloc, by = c('City'='citylower', 'state.abb'='state'))

states@data=states@data %>% left_join(nihState, by = c('name'='state.name')) %>% mutate(name = as.factor(name))

pops2=sprintf(
  "<strong>%s</strong><br/>Funding Sum: %g $",
  nihCity$City, nihCity$citySum
)%>% lapply(htmltools::HTML)

pal <- colorBin("YlGnBu", states$FundingSum/1e6, bins = c(0,2e7,1e8,2e8,4e8,8e8,2e9,4e9)/1e6)

nihCity %>% leaflet() %>% addTiles()%>%
  addPolygons(fillColor = ~pal(FundingSum/1e6), stroke = FALSE, data = states,
              label = as.character(paste0(states$name,', State Sum:',states$FundingSum))) %>%
  addCircles(lng = ~long, lat = ~lat, weight = 1,color = 'red',
             radius = ~sqrt(citySum) * 10, label = ~City, popup = pops2) %>%
  addLegend(pal = pal, values = ~states$FundingSum, opacity = 0.7, 
            title = "NIH Funding (2017)", position = "bottomright",
            labFormat = labelFormat(prefix = "$", suffix = ' Million'))%>% 
  setView( lng = -100, lat = 40, zoom = 4)




## sum amount by Institution-----------
xmldf %>% select(Institution.Name,AwardAmount) %>% 
  group_by(Institution.Name) %>% summarise(Count=n(),AmountSum=sum(AwardAmount)) %>% 
  arrange(desc(AmountSum)) %>% mutate(percent = AmountSum/sum(AmountSum)*100) %>%
  plot_ly(labels = ~ifelse(AmountSum> 3e7,Institution.Name, 'Others'), values = ~AmountSum)%>%
  add_pie(hole = 0.6)%>% layout(title = 'NSF Funding by Institution in 2017')


nihdf %>% select(`Organization Name`,Funding) %>%
  group_by(`Organization Name`) %>% summarise(Count=n(),AmountSum=sum(Funding)) %>% 
  arrange(desc(AmountSum)) %>% mutate(percent = AmountSum/sum(AmountSum)*100) %>%
plot_ly(labels = ~ifelse(AmountSum> 25e7,`Organization Name`, 'Others'), values = ~AmountSum)%>%
  add_pie(hole = 0.6) %>% layout(title = 'NIH Funding by Institution in 2017')

## country

#plot_ly(nihdf, labels = ~ifelse(Country=='USA','USA','Others'), values = ~Funding, type = 'pie',textinfo = 'label+percent',textposition = 'inside' )%>%
#  layout(title = 'NIH Funding by Country in 2017')

#plot_ly(xmldf, labels = ~Institution.CountryName=='United States', values = ~AwardAmount, type = 'pie',textinfo = 'label+percent')%>%
#  layout(title = 'NSF Funding by Country in 2017')

nihdf %>% group_by(Country) %>% summarise(n=n(),Funding = sum(Funding))%>%
  arrange(desc(Funding)) %>% mutate(percent = Funding/sum(Funding)*100)
xmldf %>% group_by(Institution.CountryName) %>% summarise(n=n(),Funding = sum(AwardAmount))%>%
  arrange(desc(Funding)) %>% mutate(percent = Funding/sum(Funding)*100)
