library(easyPubMed)
library(RISmed)
library(tidytext)
library(wordcloud2)
query = paste0(nihdf$grant.ID[!is.na(nihdf$grant.ID)],'[Grant Number] AND NIH[Grant Number] AND ("2016/09/01"[PDAT] : "2018/2/31"[PDAT])')
#res = EUtilsSummary('ES015022[Grant Number] AND NIH[Grant Number] AND ("2016/09/01"[PDAT] : "2018/2/31"[PDAT])')
#fetch <- EUtilsGet(res, type = "efetch", db = "pubmed")
#length(fetch@PMID)
leng = sum(!is.na(nihdf$grant.ID))


npaper2 =plyr::ldply(query[15001:17000],function(x)QueryCount(EUtilsSummary(x)))
npapers = rbind(npapers,npaper2)


save(npapers, file = 'npapers.rda')
setwd('/Users/zhang/Downloads')
load('npapers.rda')  # 15000 grants checked


nihdf$npaper = NA
nihdf$npaper[!is.na(nihdf$grant.ID)][1:15000] = t(npapers)

# plot npaper by funding
ggplotly(nihdf  %>% filter(!is.na(nihdf$npaper)) %>%
  ggplot(aes(x=log(Funding),y=npaper,color = `Funding Mechanism`)) + 
  geom_point() )       #+ theme(legend.position = 'none')

## 8,852 projects have non 0 paper from 2016-9 up to now
cloud = nihdf  %>% filter(!is.na(nihdf$npaper),nihdf$npaper!=0) %>%
  unnest_tokens(word, `Project Title`) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) 
cloud$wordlen = plyr::ldply(cloud$word,nchar)

cl2=cloud %>% filter(wordlen>2,n>4) %>% mutate(freq = n) 
wordcloud2(cl2 ,shape = 'circle')




