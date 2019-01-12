#print("dupa")

library(rvest)
url<-"https://pl.wikipedia.org/wiki/Katastrofy_i_incydenty_cywilnych_samolot%C3%B3w_pasa%C5%BCerskich?fbclid=IwAR2tXXC4Z4bFOv6Ml1uVeepVWg2S7EEd53hQ2D3WLABtRH8WFLYf5MIppX8"
x<-url
parse_result<-function(x){
  read_html(x)%>%
    html_nodes("table")%>%
    html_table()->tabele
}

em<-parse_result(url)
#lapply(em, function(x) write.table( data.frame(x), 'test1.csv'  , append= T, sep=',' ))

new_em <- em[6:94]
#new_em[1][[1]]["Data"]
# testowa <- new_em
# new_em <-testowa
N <- 89

for (n in 1:N) {
  #new_em[n][[1]]["Data"]<-"1"
  new_em[[n]][["Data"]] <-gsub("0000", n+1929, new_em[[n]][["Data"]])
  substr(new_em[[n]][["Data"]],1,10)
}
# new_em[[1]][["Data"]]
superdane <- new_em

#substring(new_em[[1]][["Data"]], 1:6, 1:6)

# 
# MyData <- read.csv(file="D:\\studia\\PwR\\projekt_PwR\\pwr_projekt\\test.csv", header=F, sep=",")
# MyData <-MyData[c(16:1205),]
# xd<-MyData[MyData$V1 !="Data",]
# labels <-c("Data", "Miejsce zdarzenia", "Linie lotnicze", "Numer lotu", "Samolot", "Nr rej.","Ofiary","Ocaleni")
# xd2<-xd[c(2:9)]
# colnames(xd2) <- labels
# View(xd2)
