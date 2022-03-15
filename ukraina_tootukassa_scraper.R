library(tidyverse)
library(rvest)
library(gsheet)

myurl <- "https://www.onlineexpo.com/ee/tookohad-ukrainlaste-jaoks-eestis/tooandjad/"

tööandjad <- myurl %>% 
  read_html() %>% 
  html_nodes(".ce1") %>% 
  html_text() %>% 
  as.data.frame() %>% 
  rename(firma_nimi=1) 

myurl <- "https://www.onlineexpo.com/ee/tookohad-ukrainlaste-jaoks-eestis/toopakkumised/"

lingid_kokku = data.frame()

for (x in c(1:50))
  

{
  
  myurl  <- paste0("https://www.onlineexpo.com/ee/tookohad-ukrainlaste-jaoks-eestis/toopakkumised/Leht", 
                   x)
  
  lingid <- myurl %>% 
  read_html() %>% 
  html_nodes("*") %>% 
  html_attr("href") %>% 
  as.data.frame() %>% 
  rename(link=1) %>% 
  filter(str_detect(link, "tooandjad\\/"), 
         link != "/ee/tookohad-ukrainlaste-jaoks-eestis/tooandjad/") %>% 
    mutate(lehelink = myurl)
  
  if (nrow(lingid)==0)
  {next}
  
  lingid_kokku = bind_rows(lingid_kokku, lingid)
  print(x)
}

## võtamaha tööandjate lingid

lingid_kokku <- lingid_kokku %>% 
  mutate(kontroll = str_extract(link, "tooandjad/.*/$") %>% 
           str_extract("/.*/.*/$")) %>% 
  filter(!is.na(kontroll))

lingid_kokku <- lingid_kokku %>% 
  mutate(link = paste0("https://www.onlineexpo.com", 
                       link)
  )

pakkumised_kokku <- data.frame() 
  
for (myurl in lingid_kokku$link)

{
  
  kuulutus <- myurl %>% 
    read_html() %>% 
    html_nodes("#main > div.w100.content.wrapper_around_menu > div.summary-headline > h1") %>% 
    html_text() %>% 
    as.data.frame() %>% 
    rename(amet = 1) %>% 
  mutate(ettevotte_link = ettevotte_link %>% 
             str_remove("\\/.*\\/$")) %>% 
    mutate(ettevotte_link = paste0("https://www.onlineexpo.com/ee/tookohad-ukrainlaste-jaoks-eestis/tooandjad/",ettevotte_link))%>% 
  mutate(kuulutuse_link= myurl)
  
pakkumised_kokku <- bind_rows(pakkumised_kokku, kuulutus)

print(n_distinct(pakkumised_kokku$link))
                              
                  
}

failinimi =  paste0("pakkumised_", Sys.Date() %>% 
  as.character(), 
  ".csv")
write_csv(pakkumised_kokku, failinimi)
  
  
