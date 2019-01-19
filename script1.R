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
M <- 12
miesiace <- matrix(c("stycznia", "01", "lutego", "02", "marca", "03", "kwietnia", "04", "maja", "05", "czerwca", "06", "lipca", "07", "sierpnia", "08", "wrze", "09", "pa", "10", "listopada", "11", "grudnia", "12"), 2, 12)
new_em <- em[6:94] #wywalam spis tresci i lata z dziurami

for (n in 1:N) { #"czyszcze danych, ujednolicam format"
  #new_em[[n]][["Data"]] <-substr(new_em[[n]][["Data"]],1,10)
  #if(new_em[[n]][["Data"]]<1) #jesli jest format z 0000
  #new_em[[n]][["Data"]] <-gsub("0000", n+1929, new_em[[n]][["Data"]])
  new_em[[n]][["Data"]][new_em[[n]][["Data"]]<1] <-str_sub(new_em[[n]][["Data"]][new_em[[n]][["Data"]]<1],start=11)
  dzien <-substr(new_em[[n]][["Data"]],1,2)
  for(m in 1:M){
    new_em[[n]][["Data"]]<-replace(new_em[[n]][["Data"]],grep(miesiace[1,m], new_em[[n]][["Data"]]),paste(toString(n+1929),miesiace[2,m],sep="-"))
  }
  #new_em[[n]][["Data"]]<-replace(new_em[[n]][["Data"]],grep("stycznia", new_em[[n]][["Data"]]),paste(toString(n+1929),"01",sep="-"))
  #new_em[[n]][["Data"]] <-replace(new_em[[n]][["Data"]],grep("lutego", new_em[[n]][["Data"]]),paste(toString(n+1929),"02",sep="-"))
  #new_em[[n]][["Data"]] <-replace(new_em[[n]][["Data"]],grep("marca", new_em[[n]][["Data"]]),paste(toString(n+1929),"03",sep="-"))
  #new_em[[n]][["Data"]] <-replace(new_em[[n]][["Data"]],grep("kwietnia", new_em[[n]][["Data"]]),paste(toString(n+1929),"04",sep="-"))
  #new_em[[n]][["Data"]] <-replace(new_em[[n]][["Data"]],grep("maja", new_em[[n]][["Data"]]),paste(toString(n+1929),"05",sep="-"))
  #new_em[[n]][["Data"]] <-replace(new_em[[n]][["Data"]],grep("czerwca", new_em[[n]][["Data"]]),paste(toString(n+1929),"06",sep="-"))
  #new_em[[n]][["Data"]] <-replace(new_em[[n]][["Data"]],grep("lipca", new_em[[n]][["Data"]]),paste(toString(n+1929),"07",sep="-"))
  #new_em[[n]][["Data"]] <-replace(new_em[[n]][["Data"]],grep("sierpnia", new_em[[n]][["Data"]]),paste(toString(n+1929),"08",sep="-"))
  #new_em[[n]][["Data"]] <-replace(new_em[[n]][["Data"]],grep("wrze", new_em[[n]][["Data"]]),paste(toString(n+1929),"09",sep="-"))
  #new_em[[n]][["Data"]] <-replace(new_em[[n]][["Data"]],grep("listopada", new_em[[n]][["Data"]]),paste(toString(n+1929),"11",sep="-"))
  #new_em[[n]][["Data"]] <-replace(new_em[[n]][["Data"]],grep("grudnia", new_em[[n]][["Data"]]),paste(toString(n+1929),"12",sep="-"))
  #new_em[[n]][["Data"]] <-replace(new_em[[n]][["Data"]],grep("pa", new_em[[n]][["Data"]]),paste(toString(n+1929),"10",sep="-"))
  dzien <-gsub(" ","",dzien)
  dzien[strtoi(dzien)<10] <- paste0("0",dzien[strtoi(dzien)<10])
  new_em[[n]][["Data"]]<-paste(new_em[[n]][["Data"]],dzien,sep="-")
  # wykombinowałem sposób na skrócenie tego dziadostwa. U mnie działa, nie wiem jak u ciebie.
  # jeśli napotkasz problemy, to usuń moje dodatki i zostaw to, co ty wpisałeś
  for(j in 1:J)
    {
      new_em[[n]][[j]][new_em[[n]][[j]]=='?'] <- NA
    }
    #zamieniam '?' na <NA>
  new_em[[n]][["Zobacz więcej"]] <- NULL #pozbywam się kolumny "Zobacz więcej"
  }

superdane <- new_em
lapply(superdane, function(x) write.table( data.frame(x), 'superdane.csv'  , append= T, sep=',' ))
#View(superdane) # lista 89 elementów (jeden element == lista z danego roku)

zgony_w_latach<-c()
ocalenia_w_latach<-c()
wszystkie_linie_w_latach<-c()
wszystkie_kraje_w_latach<-c()
ilosc_katastrof_w_latach<-c()

for (n in 1:N){
  zgony_w_latach<-append(zgony_w_latach,sum(strtoi(superdane[[n]][["Ofiary"]]),na.rm = T))
  ocalenia_w_latach<-append(ocalenia_w_latach,sum(strtoi(superdane[[n]][["Ocaleni"]]),na.rm = T))
  wszystkie_linie_w_latach<-append(wszystkie_linie_w_latach,superdane[[n]][["Linie lotnicze"]])
  wszystkie_kraje_w_latach<-append(wszystkie_kraje_w_latach,superdane[[n]][["Miejsce zdarzenia"]])
  ilosc_katastrof_w_latach<-append(ilosc_katastrof_w_latach,length(superdane[[n]][[1]][!is.na(superdane[[n]][["Ofiary"]])]))
}
#ocalenia
procent_ocalalalych_w_latach <-round(ocalenia_w_latach/(ocalenia_w_latach+zgony_w_latach),2)

#procent ocalalych wykres
lata<-c(1930:2018)
aaa<-data.frame(lata,procent_ocalalalych_w_latach)
ggplot(aaa, aes(x=lata, y=procent_ocalalalych_w_latach))+geom_bar(stat="identity", width=1,color="blue",fill="lightblue")+theme_minimal()

suma_ocalenia<-sum(ocalenia_w_latach)
suma_zgonow<-sum(zgony_w_latach)

round(suma_ocalenia/(suma_ocalenia+suma_zgonow),2) #statystycznie 47% szansy na ocalanie 

#najniebezpieczniejsze linie
tabela_linii <-sort(table(wszystkie_linie_w_latach),decreasing = T)
#View(tabela_linii)
tabela_linii[1:10]
#wynika to zapewne ze skalą przedśiębiorstw lotniczych, nie mogłem znaleźć danych
#o ilościach lotów dla lini lotniczych w tamtych latach
plot(tabela_linii[1:10],type='h',lwd=10)

#najniebezpieczniejsze kraje
tabela_krajow <-sort(table(wszystkie_kraje_w_latach),decreasing = T)
tabela_krajow[1:10] # tak samo jak wyżej
plot(tabela_krajow[1:10],type='h',lwd=10)

#ilośc zabitych / ilośc katastrof na przestrzeni lat
wskaznik_zgony_per_katastrofa<-zgony_w_latach/ilosc_katastrof_w_latach
#plot(wskaznik_zgony_per_katastrofa, type='h',xlab="lata")
bbb <- data.frame(lata,wskaznik_zgony_per_katastrofa)
ggplot(bbb, aes(x=lata, y=wskaznik_zgony_per_katastrofa))+geom_bar(stat="identity", width=1,color="blue",fill="lightblue")+theme_minimal()

suma_katastrof <- sum(ilosc_katastrof_w_latach)

srednia_zgonow_per_katastrofa<-round(suma_zgonow/suma_katastrof) 
print(srednia_zgonow_per_katastrofa) #statystycznie w każdej katastrofie zginęło średnio ok.45 osób

#ilość ocalałych / ilość katastrof na przestrzeni lat
wskaznik_ocaleni_per_katastrofa<-ocalenia_w_latach/ilosc_katastrof_w_latach
ccc<-data.frame(lata,wskaznik_ocaleni_per_katastrofa)
ggplot(ccc, aes(x=lata, y=wskaznik_ocaleni_per_katastrofa))+geom_bar(stat="identity", width=1,color="blue",fill="lightblue")+theme_minimal()

srednia_ocalalych_per_katastrofa<-round(suma_ocalenia/suma_katastrof) 
print(srednia_ocalalych_per_katastrofa)#statystycznie po każdej katastrofie ocalało średnio ok. 40 osób

#stosunek liczby zgonów do liczby ocalałych
stosunek_zgonow_do_ocalalych<-zgony_w_latach/ocalenia_w_latach
ddd<-data.frame(lata,stosunek_zgonow_do_ocalalych)
ggplot(ddd, aes(x=lata, y=stosunek_zgonow_do_ocalalych))+geom_bar(stat="identity", width=1,color="blue",fill="lightblue")+theme_minimal()

round(suma_zgonow/suma_ocalenia, 2) #statystycznie na jedną osobę ocalałą po katastrofie przypada jedna/dwie, która/e zginęła/y (stosunek 1.13)

#odchylenia standardowe zgonów w poszczególnych latach
odchylenie_zgonow_w_latach<-c()

for(n in 1:N){
  odchylenie_zgonow_w_latach<-append(odchylenie_zgonow_w_latach,sqrt(sum((strtoi(superdane[[n]][["Ofiary"]])-wskaznik_zgony_per_katastrofa[n])^2,na.rm = T)/ilosc_katastrof_w_latach[n]))
}
eee<-data.frame(lata,odchylenie_zgonow_w_latach)
ggplot(eee, aes(x=lata, y=odchylenie_zgonow_w_latach))+geom_bar(stat="identity", width=1,color="blue",fill="lightblue")+theme_minimal()

round(sqrt(sum((zgony_w_latach-srednia_zgonow_per_katastrofa)^2)/suma_katastrof)) #w niektórych katastrofach liczba zgonów odchylała się od średniej o ok. 193 osoby

#odchylenia standardowe ocalałych w poszczególnych latach
odchylenie_ocalalych_w_latach<-c()

for(n in 1:N){
  odchylenie_ocalalych_w_latach<-append(odchylenie_ocalalych_w_latach,sqrt(sum((strtoi(superdane[[n]][["Ocaleni"]])-wskaznik_ocaleni_per_katastrofa[n])^2,na.rm = T)/ilosc_katastrof_w_latach[n]))
}
fff<-data.frame(lata,odchylenie_ocalalych_w_latach)
ggplot(fff, aes(x=lata, y=odchylenie_ocalalych_w_latach))+geom_bar(stat="identity", width=1,color="blue",fill="lightblue")+theme_minimal()

round(sqrt(sum((ocalenia_w_latach-srednia_ocalalych_per_katastrofa)^2)/suma_katastrof)) #w niektórych katastrofach liczba ocalałych odchylała się od średniej o ok. 205 osób

#średnia liczba zgonów ogólnie i odchylenie standardowe
round(mean(zgony_w_latach)) #średnio w każdym roku zginęło ok. 550 osób
round(sd(zgony_w_latach)) #w niektórych latach liczba zgonów odchylała się od średniej o ok. 439 osób

#średnia liczba ocalałych ogólnie i odchylenie standardowe
round(mean(ocalenia_w_latach)) #średnio w każdym roku ocalało ok. 488 osób
round(sd(ocalenia_w_latach)) #w niektórych latach liczba ocalałych odchylała się od średniej o ok. 559 osób
