library(rvest)
library(stringr)
url<-"https://pl.wikipedia.org/wiki/Katastrofy_i_incydenty_cywilnych_samolot%C3%B3w_pasa%C5%BCerskich?fbclid=IwAR2tXXC4Z4bFOv6Ml1uVeepVWg2S7EEd53hQ2D3WLABtRH8WFLYf5MIppX8"
x<-url
parse_result<-function(x){
  read_html(x)%>%
    html_nodes("table")%>%
    html_table()->tabele
}

em<-parse_result(url)

N <- 89 # lata 1930 - 2018
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
}
new_em[[89]][["Data"]]

superdane <- new_em
lapply(superdane, function(x) write.table( data.frame(x), 'superdane.csv'  , append= T, sep=',' ))
View(superdane) # lista 89 elementów (jeden element == lista z danego roku)
