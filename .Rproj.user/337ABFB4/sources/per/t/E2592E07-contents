library(rvest)
library(stringr)
library(ggplot2)
url<-"https://pl.wikipedia.org/wiki/Katastrofy_i_incydenty_cywilnych_samolot%C3%B3w_pasa%C5%BCerskich?fbclid=IwAR2tXXC4Z4bFOv6Ml1uVeepVWg2S7EEd53hQ2D3WLABtRH8WFLYf5MIppX8"
x<-url
parse_result<-function(x){
  read_html(x)%>%
    html_nodes("table")%>%
    html_table()->tabele
}

em<-parse_result(url)

N <- 89 # lata 1930 - 2018
J <- 9
new_em <- em[6:94] #wywalam spis tresci i lata z dziurami

for (n in 1:N) { #"czyszcze danych, ujednolicam format"
  #new_em[[n]][["Data"]] <-substr(new_em[[n]][["Data"]],1,10)
  #if(new_em[[n]][["Data"]]<1) #jesli jest format z 0000
  #new_em[[n]][["Data"]] <-gsub("0000", n+1929, new_em[[n]][["Data"]])
  new_em[[n]][["Data"]][new_em[[n]][["Data"]]<1] <-str_sub(new_em[[n]][["Data"]][new_em[[n]][["Data"]]<1],start=11)
  dzien <-substr(new_em[[n]][["Data"]],1,2)
  new_em[[n]][["Data"]]<-replace(new_em[[n]][["Data"]],grep("stycznia", new_em[[n]][["Data"]]),paste(toString(n+1929),"01",sep="-"))
  new_em[[n]][["Data"]] <-replace(new_em[[n]][["Data"]],grep("lutego", new_em[[n]][["Data"]]),paste(toString(n+1929),"02",sep="-"))
  new_em[[n]][["Data"]] <-replace(new_em[[n]][["Data"]],grep("marca", new_em[[n]][["Data"]]),paste(toString(n+1929),"03",sep="-"))
  new_em[[n]][["Data"]] <-replace(new_em[[n]][["Data"]],grep("kwietnia", new_em[[n]][["Data"]]),paste(toString(n+1929),"04",sep="-"))
  new_em[[n]][["Data"]] <-replace(new_em[[n]][["Data"]],grep("maja", new_em[[n]][["Data"]]),paste(toString(n+1929),"05",sep="-"))
  new_em[[n]][["Data"]] <-replace(new_em[[n]][["Data"]],grep("czerwca", new_em[[n]][["Data"]]),paste(toString(n+1929),"06",sep="-"))
  new_em[[n]][["Data"]] <-replace(new_em[[n]][["Data"]],grep("lipca", new_em[[n]][["Data"]]),paste(toString(n+1929),"07",sep="-"))
  new_em[[n]][["Data"]] <-replace(new_em[[n]][["Data"]],grep("sierpnia", new_em[[n]][["Data"]]),paste(toString(n+1929),"08",sep="-"))
  new_em[[n]][["Data"]] <-replace(new_em[[n]][["Data"]],grep("wrze", new_em[[n]][["Data"]]),paste(toString(n+1929),"09",sep="-"))
  new_em[[n]][["Data"]] <-replace(new_em[[n]][["Data"]],grep("listopada", new_em[[n]][["Data"]]),paste(toString(n+1929),"11",sep="-"))
  new_em[[n]][["Data"]] <-replace(new_em[[n]][["Data"]],grep("grudnia", new_em[[n]][["Data"]]),paste(toString(n+1929),"12",sep="-"))
  new_em[[n]][["Data"]] <-replace(new_em[[n]][["Data"]],grep("pa", new_em[[n]][["Data"]]),paste(toString(n+1929),"10",sep="-"))
  dzien <-gsub(" ","",dzien)
  dzien[strtoi(dzien)<10] <- paste0("0",dzien[strtoi(dzien)<10])
  new_em[[n]][["Data"]]<-paste(new_em[[n]][["Data"]],dzien,sep="-")
  # pewnie da się to zrobić krócej ale już spedziłem na tym chyba z 5h przez to że format daty z wikipedii raz był "0000-02-1111 lutego" a raz "14 lipca" itp
  # a do tego wszystkiego grep nie obsługuje 'ś' i 'ź' więc był problem z październikiem i wrześniem ale chyba jest ok narazie XD
  for(j in 1:J)
    {
      new_em[[n]][[j]][new_em[[n]][[j]]=='?'] <- NA
    }
    #zamieniam '?' na <NA>
  }
new_em[[89]][["Data"]]

superdane <- new_em
lapply(superdane, function(x) write.table( data.frame(x), 'superdane.csv'  , append= T, sep=',' ))
View(superdane) # lista 89 elementów (jeden element == lista z danego roku)

suma_zgonow<-0
suma_ocalen<-0
zgony_w_latach<-c()
ocalenia_w_latach<-c()
for (n in 1:N){
  zgony_w_latach<-append(zgony_w_latach,sum(strtoi(superdane[[n]][["Ofiary"]]),na.rm = T))
  ocalenia_w_latach<-append(ocalenia_w_latach,sum(strtoi(superdane[[n]][["Ocaleni"]]),na.rm = T))
  # 
  # suma_zgonow<-suma_zgonow+sum(strtoi(superdane[[n]][["Ofiary"]][!is.na(superdane[[n]][["Ocaleni"]])]),na.rm = T)
  # suma_ocalen<-suma_ocalen+sum(strtoi(superdane[[n]][["Ocaleni"]][!is.na(superdane[[n]][["Ofiary"]])]),na.rm = T)
  # #print(superdane[[n]][["Ofiary"]][!is.na(superdane[[n]][["Ofiary"]]) & is.na(superdane[[n]][["Ocaleni"]])])
}
#print(suma_ocalen/(suma_zgonow+suma_ocalen)) #47% szansy na ocalanie?
procent_ocalalalych_w_latach <-round(ocalenia_w_latach/(ocalenia_w_latach+zgony_w_latach),2)
#w 4 przypadkach są ofiary a ocaleni są NA

lata<-c(1930:2018)
aaa<-data.frame(lata,procent_ocalalalych_w_latach)
ggplot(aaa, aes(x=lata, y=procent_ocalalalych_w_latach))+geom_bar(stat="identity", width=1,color="blue",fill="lightblue")+theme_minimal()
