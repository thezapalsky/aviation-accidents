#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
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
  new_em[[n]][["Data"]][new_em[[n]][["Data"]]<1] <-str_sub(new_em[[n]][["Data"]][new_em[[n]][["Data"]]<1],start=11)
  dzien <-substr(new_em[[n]][["Data"]],1,2)
  for(m in 1:M){
    new_em[[n]][["Data"]]<-replace(new_em[[n]][["Data"]],grep(miesiace[1,m], new_em[[n]][["Data"]]),paste(toString(n+1929),miesiace[2,m],sep="-"))
  }
  #dzien <-gsub(" ","",dzien)
  #dzien[strtoi(dzien)<10] <- paste0("0",dzien[strtoi(dzien)<10])
  new_em[[n]][["Data"]]<-paste(new_em[[n]][["Data"]],dzien,sep="-")
  for(j in 1:J)
  {
    new_em[[n]][[j]][new_em[[n]][[j]]=='?'] <- NA
  }
  #zamieniam '?' na <NA>
  new_em[[n]][["Zobacz więcej"]] <- NULL #pozbywam się kolumny "Zobacz więcej"
}

superdane <- new_em
lapply(superdane, function(x) write.table( data.frame(x), 'superdane.csv'  , append= T, sep=',' ))

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

suma_ocalenia<-sum(ocalenia_w_latach)
suma_zgonow<-sum(zgony_w_latach)
suma_katastrof <- sum(ilosc_katastrof_w_latach)
procent_ocalalalych_w_latach <-round(ocalenia_w_latach/(ocalenia_w_latach+zgony_w_latach),2)
procent_ocalalych <- round(suma_ocalenia/(suma_ocalenia+suma_zgonow),2)
wskaznik_zgony_per_katastrofa<-zgony_w_latach/ilosc_katastrof_w_latach
srednia_zgonow_per_katastrofa<-round(suma_zgonow/suma_katastrof) 
srednia_zgonow <- round(mean(zgony_w_latach))
wskaznik_ocaleni_per_katastrofa<-ocalenia_w_latach/ilosc_katastrof_w_latach
srednia_ocalalych_per_katastrofa<-round(suma_ocalenia/suma_katastrof)
srednia_ocalalych <- round(mean(ocalenia_w_latach))
stosunek_zgonow_do_ocalalych<-zgony_w_latach/ocalenia_w_latach
stosunek <- round(suma_zgonow/suma_ocalenia, 2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$Wykres <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    lata<-c(1930:2018)
    wskaznik <- input$Wskazniki
    # draw the histogram with the specified number of bins
    
    if(wskaznik == "Ilosc katastrof"){
      xxx<-data.frame(lata,ilosc_katastrof_w_latach)
      
      output$IK <- renderText({paste("Wraz ze wzrostem popularnosci samolotow jako srodku tranposrtu, zwiekszala sie ilosc wypadkow")})
      
      ggplot(xxx, aes(x=lata, y=ilosc_katastrof_w_latach))+geom_bar(stat="identity", width=1,color="blue",fill="lightblue")+theme_minimal()
    }
    else if(wskaznik == "Procent ocalalych"){
      aaa<-data.frame(lata,procent_ocalalalych_w_latach)
      
      output$PO <- renderText({paste("Procent ocalalych ogolnie: ", procent_ocalalych)})
      
      ggplot(aaa, aes(x=lata, y=procent_ocalalalych_w_latach))+geom_bar(stat="identity", width=1,color="blue",fill="lightblue")+theme_minimal()
    }
    else if(wskaznik == "Najniebezpieczniejsze linie"){
      tabela_linii <-sort(table(wszystkie_linie_w_latach),decreasing = T)
      
      plot(tabela_linii[1:10],type='h',lwd=10)
    }
    else if(wskaznik == "Najniebezpieczniejsze kraje"){
      tabela_krajow <-sort(table(wszystkie_kraje_w_latach),decreasing = T)
      
      plot(tabela_krajow[1:10],type='h',lwd=10)
    }
    else if(wskaznik == "Srednia liczba zgonow"){
      bbb <- data.frame(lata,wskaznik_zgony_per_katastrofa)
      
      output$SZ1 <- renderText({paste("Srednia liczba zgonow ogolnie (per katastrofa): ", srednia_zgonow_per_katastrofa)})
      output$SZ2 <- renderText({paste("Srednia liczba zgonow ogolnie (per rok): ", srednia_zgonow)})
      
      ggplot(bbb, aes(x=lata, y=wskaznik_zgony_per_katastrofa))+geom_bar(stat="identity", width=1,color="blue",fill="lightblue")+theme_minimal()
    }
    else if(wskaznik == "Srednia liczba ocalalych"){
      ccc<-data.frame(lata,wskaznik_ocaleni_per_katastrofa)
      
      output$SO1 <- renderText({paste("Srednia liczba ocalalych ogolnie (per katastrofa): ", srednia_ocalalych_per_katastrofa)})
      output$SO2 <- renderText({paste("Srednia liczba ocalalych ogolnie (per rok): ", srednia_ocalalych)})
      
      ggplot(ccc, aes(x=lata, y=wskaznik_ocaleni_per_katastrofa))+geom_bar(stat="identity", width=1,color="blue",fill="lightblue")+theme_minimal()
    }
    else if(wskaznik == "Zgony/ocaleni"){
      stosunek_zgonow_do_ocalalych[stosunek_zgonow_do_ocalalych==Inf | stosunek_zgonow_do_ocalalych=="NaN"]<-0
      ddd<-data.frame(lata,stosunek_zgonow_do_ocalalych)
      
      output$ZO <- renderText({paste("Zgony/ocaleni ogolnie: ", stosunek)})
      
      ggplot(ddd, aes(x=lata, y=stosunek_zgonow_do_ocalalych))+geom_bar(stat="identity", width=1,color="blue",fill="lightblue")+theme_minimal()
    }
    else if(wskaznik == "Odchylenie standardowe zgonow"){
      odchylenie_zgonow_w_latach<-c()
      
      for(n in 1:N){
        odchylenie_zgonow_w_latach<-append(odchylenie_zgonow_w_latach,sqrt(sum((strtoi(superdane[[n]][["Ofiary"]])-wskaznik_zgony_per_katastrofa[n])^2,na.rm = T)/ilosc_katastrof_w_latach[n]))
      }
      eee<-data.frame(lata,odchylenie_zgonow_w_latach)
      odchylenie_zgonow_per_katastrofa <- round(sqrt(sum((zgony_w_latach-srednia_zgonow_per_katastrofa)^2)/suma_katastrof))
      odchylenie_zgonow <- round(sd(zgony_w_latach))
      
      output$OZ1 <- renderText({paste("Odchylenie standardowe zgonow ogolnie (per katastrofa): ", odchylenie_zgonow_per_katastrofa)})
      output$OZ2 <- renderText({paste("Odchylenie standardowe zgonow ogolnie (per rok): ", odchylenie_zgonow)})
      
      ggplot(eee, aes(x=lata, y=odchylenie_zgonow_w_latach))+geom_bar(stat="identity", width=1,color="blue",fill="lightblue")+theme_minimal()
    }
    else{
      odchylenie_ocalalych_w_latach<-c()
      
      for(n in 1:N){
        odchylenie_ocalalych_w_latach<-append(odchylenie_ocalalych_w_latach,sqrt(sum((strtoi(superdane[[n]][["Ocaleni"]])-wskaznik_ocaleni_per_katastrofa[n])^2,na.rm = T)/ilosc_katastrof_w_latach[n]))
      }
      fff<-data.frame(lata,odchylenie_ocalalych_w_latach)
      odchylenie_ocalalych_per_katastrofa <- round(sqrt(sum((ocalenia_w_latach-srednia_ocalalych_per_katastrofa)^2)/suma_katastrof))
      odchylenie_ocalalych <- round(sd(ocalenia_w_latach))
      
      output$OO1 <- renderText({paste("Odchylenie standardowe ocalalych ogolnie (per katastrofa): ", odchylenie_ocalalych_per_katastrofa)})
      output$OO2 <- renderText({paste("Odchylenie standardowe ocalalych ogolnie (per rok): ", odchylenie_ocalalych)})
      
      ggplot(fff, aes(x=lata, y=odchylenie_ocalalych_w_latach))+geom_bar(stat="identity", width=1,color="blue",fill="lightblue")+theme_minimal()
    }
    
  })
  
})
