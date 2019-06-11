############################################################
################### Pasiruošimas darbui ####################
############################################################
  setwd("C:/Users/Tautvydas/Desktop/Duomenu_analizes_duomenys")

      if(!require(quantmod)) install.packages("quantmod"); require("quantmod")
      if(!require(xts)) install.packages("xts"); require("xts")
      if(!require(rvest)) install.packages("rvest"); require("rvest")
      if(!require(tidyverse)) install.packages("tidyverse"); require("tidyverse")
      if(!require(stringr)) install.packages("stringr"); require("stringr")
      if(!require(forcasts)) install.packages("forcasts"); require("forcasts")
      if(!require(plotly)) install.packages("plotly"); require("plotly")
      if(!require(dplyr)) install.packages("dplyr"); require("dplyr")
      if(!require(PerformanceAnalytics)) install.packages("PerformanceAnalytics"); require("PerformanceAnalytics")
      if(!require(readr)) install.packages("readr"); require("readr")
      if(!require(stringr)) install.packages("stringr"); require("stringr")

###########################################################
################ Duomenų atsisiuntimas ####################
###########################################################
    
start <- as.Date("2010-01-01")
end <- as.Date("2019-06-02")

getSymbols(c("APG1L.VS",
             "VLP1L.VS",
             "OLF1R.RG",
             "SAB1L.VS",
             "TEL1L.VS"),src = "yahoo", from = start, to = end)

getSymbols("^OMXBBGI", src ="yahoo", from= start, to=end)  

    stocks <- as.xts(data.frame(APG1L.VS = APG1L.VS[, "APG1L.VS.Close"],
                                VLP1L.VS = VLP1L.VS[, "VLP1L.VS.Close"],
                                OLF1R.RG = OLF1R.RG[, "OLF1R.RG.Close"],
                                SAB1L.VS = SAB1L.VS[, "SAB1L.VS.Close"],
                                TEL1L.VS = TEL1L.VS[, "TEL1L.VS.Close"]))
    
###########################################################
# Akcijos kainos dauginamos iš turimų akcijų kiekio #######
#### ir gaunamas hipotetinis VU SIF portfelis. ############
###########################################################
# Taip pat atsiranda logikos klaida, nes nuo 2010-11-22 ###
# keitėsi valiuta, kuria yra prekiaujama Baltijos VP bir- #
# žoje, o naujaja oficialia valiuta tapo euras.############
# Portfelio rezultatai ir palyginimas indeksas turi būti ##
# taikomas arba lito, arba euro laikotarpiui. #############
###########################################################
    
visas_data_frame<-fortify(stocks)%>%mutate(PORTFELIO_VERTE=APG1L.VS.Close*367+VLP1L.VS.Close*250+OLF1R.RG.Close*80+SAB1L.VS.Close*2016+TEL1L.VS.Close*890)
stocks$Portfelis<-stocks$APG1L.VS.Close*367+stocks$VLP1L.VS.Close*250+stocks$OLF1R.RG.Close*80+stocks$SAB1L.VS.Close*2016+stocks$TEL1L.VS.Close*890

###########################################################
############## Akcijų kursų grafikai ######################
###########################################################
# Du skirtingi variantai: #################################
# Pirmas variantas nubrėžia akcijos kurso pokyčio grafiką #
# ir pateikia konkrečios dienos Volume. Basic stuff #######
# Antras variantas nubrėžia kurso pokyčio grafiką ir ######
# pateikia galimybe pasirinkti norimus techninės analizės #
# indikatorius.############################################
###########################################################
    
candleChart(APG1L.VS, up.col = "black", dn.col = "red", theme = "white", subset = "2017-01-01/")
    chartSeries(APG1L.VS,TA='addBBands();addVo();addMACD();addSMA()',subset='2017')

candleChart(VLP1L.VS, up.col = "black", dn.col = "red", theme = "white", subset = "2018-01-01/")
    chartSeries(VLP1L.VS,name= "Vilkyškių Pieninės akcijos kainos grafikas",TA='addVo();addEMA(n = 20, on = 1, with.col = Cl, overlay = TRUE, col = "orange");addEMA(n = 100, on = 1, with.col = Cl, overlay = TRUE, col = "white")',subset='2019-01/')

candleChart(OLF1R.RG, up.col = "black", dn.col = "red", theme = "white", subset = "2018-01-01/")
    chartSeries(OLF1R.RG,TA='addBBands();addVo();addMACD()',subset='2018')

candleChart(SAB1L.VS, up.col = "black", dn.col = "red", theme = "white", subset = "2018-01-01/")
    chartSeries(SAB1L.VS,TA='addBBands();addVo();addMACD()',subset='2018')

candleChart(TEL1L.VS, up.col = "black", dn.col = "red", theme = "white", subset = "2018-01-01/")
    chartSeries(TEL1L.VS,TA='addBBands();addVo();addMACD()',subset='2018')

###########################################################
############# Portfelio grafikai (Version 4.1) ############
###########################################################
# Naudojimosi instrukcija: ################################
# 1) Užkrauti po du kartus ################################
###########################################################

    bandymas<-fortify(visas_data_frame)
    bandymas_OMX<-fortify(OMXBBGI)
    
    bandymas<-bandymas%>%filter(Index>="2015-01-01",
                                         Index<="2019-06-01")%>%mutate(PORTFELIO_VIENETO_VERTE=PORTFELIO_VERTE/(as.numeric(bandymas[1,7])/100))
    
    bandymas_OMX<-bandymas_OMX%>%filter(Index>="2015-01-01",Index<="2019-06-01")%>%mutate(INDEKSO_VIENETO_VERTE=OMXBBGI.Adjusted/(as.numeric(bandymas_OMX[1,7])/100))
    
    bandymas$Index<-as.Date(bandymas$Index)
    bandymas$Index<-as.numeric(bandymas$Index)
    bandymas_OMX$Index<-as.Date(bandymas_OMX$Index)
    bandymas_OMX$Index<-as.numeric(bandymas_OMX$Index)

    Visas<-merge(bandymas,bandymas_OMX, by.x=c("Index"))
    Visas$Index<-as.Date(Visas$Index)
  
###########################################################
######### Portfelio grafiko braižymas (Version 5.1) #######
###########################################################

    df2<-fortify(Visas)%>%select(-c(2:7,9:14))
    df2<-df2[complete.cases(df2), ]
    
   plot1<-ggplot(df2,aes(x=df2$Index, colour=Legenda))+
      geom_line(aes(y=df2$PORTFELIO_VIENETO_VERTE,color="VU SIF fondas"),lwd=1.1)+
      geom_line(aes(y=df2$INDEKSO_VIENETO_VERTE,color="OMX Baltic Benchmark"),lwd=1.3)+
      labs(title = "OMX Baltic Benchmark ir VUSIF hipoteninio investicinio portfelio palyginimas",
           subtitle = "Šaltinis: Yahoo Finance. 2015 m. - 100 proc.",
           x="Laikotarpis",
           y="Procentai [%]")+
      scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Procentai [%]"))+
      scale_color_manual(values = c("VU SIF fondas" = "cyan", "OMX Baltic Benchmark" = "orange"))+
      theme(text = element_text(family = 'Gill Sans', color = "#444444",size = 25)
            ,panel.background = element_rect(fill = '#444B5A')
            ,panel.grid.minor = element_line(color = '#4d5566')
            ,panel.grid.major = element_line(color = '#586174')
            ,plot.title = element_text(size = 25)
            ,axis.title = element_text(size = 30, color = '#555555')
            ,axis.title.y = element_text(vjust = 0.5, angle = 90,size = 25)
            ,legend.position="bottom"
            ,legend.box = "horizontal"
            ,legend.title=element_blank()
            ,legend.text=element_text(size=25)
      )
  
    png(filename = "PORTFELIS.png",width = 1200, height = 800, units = "px", pointsize = 12,
        bg = "white")
    print(plot1)
    dev.off()
        
###########################################################
##### Portfelio grafikai xts formatu (Version 4.1) ########
###########################################################
    
    chartSeries(stocks$Portfelis,TA='addBBands();addMACD()',subset='2015-01-01/', name = "Portfelio vertės pokytis")
    chartSeries(OMXBBGI$OMXBBGI.Adjusted,TA='addBBands();addMACD()', name = "OMX Baltic Benchmark", subset = '2015-01-01/')

#-------------Fundamentali analize----------------
  Imoniu_palyginimas<-Visas
    
  #Imoniu_palyginimas<-fortify(visas_data_frame)%>%filter(Index>="2015-01-01")
  
  Imoniu_palyginimas<-Imoniu_palyginimas%>%mutate(Aprangos_vieneto_verte=APG1L.VS.Close/(as.numeric(Imoniu_palyginimas[1,2])/100))
  
  Imoniu_palyginimas<-Imoniu_palyginimas%>%mutate(Vilkyskiu_vieneto_verte=VLP1L.VS.Close/(as.numeric(Imoniu_palyginimas[1,3])/100))
      
  Imoniu_palyginimas<-Imoniu_palyginimas%>%mutate(Olainfarm_vieneto_verte=OLF1R.RG.Close/(as.numeric(Imoniu_palyginimas[1,4])/100))
  
  Imoniu_palyginimas<-Imoniu_palyginimas%>%mutate(Siauliu_banko_vieneto_verte=SAB1L.VS.Close/(as.numeric(Imoniu_palyginimas[1,5])/100))
  
  Imoniu_palyginimas<-Imoniu_palyginimas%>%mutate(Telia_vieneto_verte=TEL1L.VS.Close/(as.numeric(Imoniu_palyginimas[1,6])/100))
  
  Imoniu_palyginimas<-Imoniu_palyginimas%>%mutate(INDEKSO_VIENETO_VERTE=OMXBBGI.Adjusted/(as.numeric(Imoniu_palyginimas[1,14])/100))
  
  Imoniu_palyginimas<-Imoniu_palyginimas%>%select(-c(2:14))
  
############################################################
############## Palyginamieji grafikai ######################
############################################################
  
  plot2<-ggplot(Imoniu_palyginimas,aes(x=Imoniu_palyginimas$Index,colour=Legenda))+
    geom_line(aes(y=Imoniu_palyginimas$INDEKSO_VIENETO_VERTE,color="OMX Baltic Benchmark"),lwd=1.3)+
    geom_line(aes(y=Imoniu_palyginimas$Aprangos_vieneto_verte,color="Aprangos vieneto vertė"),lwd=1)+
    labs(title = "OMX Baltic Benchmark vieneto ir Aprangos akcijos kainos vieneto palyginimas",
         subtitle = "Šaltinis: Yahoo Finance. 2015 m. - 100 proc.",
         x="Laikotarpis",
         y="Procentai [%]")+
    scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Procentai [%]"))+
    scale_color_manual(values = c("Aprangos vieneto vertė" = "cyan", "OMX Baltic Benchmark" = "orange"))+
    theme(text = element_text(family = 'Gill Sans', color = "#444444",size = 25)
          ,panel.background = element_rect(fill = '#444B5A')
          ,panel.grid.minor = element_line(color = '#4d5566')
          ,panel.grid.major = element_line(color = '#586174')
          ,plot.title = element_text(size = 25)
          ,axis.title = element_text(size = 30, color = '#555555')
          ,axis.title.y = element_text(vjust = 0.5, angle = 90,size = 25)
          ,axis.title.x = element_text(hjust = 0.5,angle = 0)
          ,legend.position="bottom"
          ,legend.box = "horizontal"
          ,legend.title=element_blank()
          ,legend.text=element_text(size=25)
    )
  png(filename = "APG.png",width = 1200, height = 800, units = "px", pointsize = 12,
      bg = "white")
  print(plot2)
  dev.off()

###########################################################  
  
  plot3<-ggplot(Imoniu_palyginimas,aes(x=Imoniu_palyginimas$Index,colour=Legenda))+
    geom_line(aes(y=Imoniu_palyginimas$INDEKSO_VIENETO_VERTE,color="OMX Baltic Benchmark"),lwd=1.3)+
    geom_line(aes(y=Imoniu_palyginimas$Vilkyskiu_vieneto_verte,color="Vilkyskiu pienines vieneto vertė"),lwd=1)+
    labs(title = "OMX Baltic Benchmark vieneto ir Vilkyškių pieninės akcijos kainos vieneto palyginimas",
         subtitle = "Šaltinis: Yahoo Finance. 2015 m. - 100 proc.",
         x="Laikotarpis",
         y="Procentai")+
    scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Procentai [%]"))+
    scale_color_manual(values = c("Vilkyskiu pienines vieneto vertė" = "cyan", "OMX Baltic Benchmark" = "orange"))+
    theme(text = element_text(family = 'Gill Sans', color = "#444444",size = 25)
          ,panel.background = element_rect(fill = '#444B5A')
          ,panel.grid.minor = element_line(color = '#4d5566')
          ,panel.grid.major = element_line(color = '#586174')
          ,plot.title = element_text(size = 25)
          ,axis.title = element_text(size = 30, color = '#555555')
          ,axis.title.y = element_text(vjust = 0.5, angle = 90,size = 25)
          ,axis.title.x = element_text(hjust = 0.5)
          ,legend.position="bottom"
          ,legend.box = "horizontal"
          ,legend.title=element_blank()
          ,legend.text=element_text(size=25)
                              
    )
  png(filename = "VLP.png",width = 1200, height = 800, units = "px", pointsize = 12,
      bg = "white")
  print(plot3)
  dev.off()
###########################################################  
  
  plot4<-ggplot(Imoniu_palyginimas,aes(x=Imoniu_palyginimas$Index,colour=Legenda))+
    geom_line(aes(y=Imoniu_palyginimas$INDEKSO_VIENETO_VERTE,color="OMX Baltic Benchmark"),lwd=1.3)+
    geom_line(aes(y=Imoniu_palyginimas$Olainfarm_vieneto_verte,color="Olainfarm vieneto vertė"),lwd=0.9)+
    labs(title = "OMX Baltic Benchmark vieneto ir Olainfarm akcijos kainos vieneto palyginimas",
         subtitle = "Šaltinis: Yahoo Finance. 2015 m. - 100 proc.",
         x="Laikotarpis",
         y="Procentai")+
    scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Procentai [%]"))+
    scale_color_manual(values = c( "OMX Baltic Benchmark" = "orange","Olainfarm vieneto vertė" = "cyan"))+
    theme(text = element_text(family = 'Gill Sans', color = "#444444",size = 25)
          ,panel.background = element_rect(fill = '#444B5A')
          ,panel.grid.minor = element_line(color = '#4d5566')
          ,panel.grid.major = element_line(color = '#586174')
          ,plot.title = element_text(size = 25)
          ,axis.title = element_text(size = 30, color = '#555555')
          ,axis.title.y = element_text(vjust = 0.5, angle = 90,size = 25)
          ,axis.title.x = element_text(hjust = 0.5)
          ,legend.position="bottom"
          ,legend.box = "horizontal"
          ,legend.title=element_blank()
          ,legend.text=element_text(size=25)
      
    )
  png(filename = "OLF.png",width = 1200, height = 800, units = "px", pointsize = 12,
      bg = "white")
  print(plot4)
  dev.off()
###########################################################
  
  plot5<-ggplot(Imoniu_palyginimas,aes(x=Imoniu_palyginimas$Index,colour=Legenda))+
    geom_line(aes(y=Imoniu_palyginimas$INDEKSO_VIENETO_VERTE,color="OMX Baltic Benchmark"),lwd=1.3)+
    geom_line(aes(y=Imoniu_palyginimas$Siauliu_banko_vieneto_verte,color="Šiaulių banko vieneto vertė"),lwd=1)+
    labs(title = "OMX Baltic Benchmark vieneto ir Šiaulių banko akcijos kainos vieneto palyginimas",
         subtitle = "Šaltinis: Yahoo Finance. 2015 m. - 100 proc.",
         x="Laikotarpis",
         y="Procentai")+
    scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Procentai [%]"))+
    scale_color_manual(values = c("Šiaulių banko vieneto vertė" = "cyan", "OMX Baltic Benchmark" = "orange"))+
    theme(text = element_text(family = 'Gill Sans', color = "#444444",size = 25)
          ,panel.background = element_rect(fill = '#444B5A')
          ,panel.grid.minor = element_line(color = '#4d5566')
          ,panel.grid.major = element_line(color = '#586174')
          ,plot.title = element_text(size = 25)
          ,axis.title = element_text(size = 30, color = '#555555')
          ,axis.title.y = element_text(vjust = 0.5, angle = 90,size = 25)
          ,axis.title.x = element_text(hjust = 0.5)
          ,legend.position="bottom"
          ,legend.box = "horizontal"
          ,legend.title=element_blank()
          ,legend.text=element_text(size=25)
    )
  png(filename = "SAB.png",width = 1200, height = 800, units = "px", pointsize = 12,
      bg = "white")
  print(plot5)
  dev.off()
##########################################################
  
  plot6<-ggplot(Imoniu_palyginimas,aes(x=Imoniu_palyginimas$Index,colour=Legenda))+
    geom_line(aes(y=Imoniu_palyginimas$INDEKSO_VIENETO_VERTE,color="OMX Baltic Benchmark"),lwd=1.3)+
    geom_line(aes(y=Imoniu_palyginimas$Telia_vieneto_verte,color="Telia vieneto vertė"),lwd=1)+
    labs(title = "OMX Baltic Benchmark vieneto ir Telia akcijos kainos vieneto palyginimas",
         subtitle = "Šaltinis: Yahoo Finance. 2015 m. - 100 proc.",
         x="Laikotarpis",
         y="Procentai")+
    scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Procentai [%]"))+
    scale_color_manual(values = c("Telia vieneto vertė" = "cyan", "OMX Baltic Benchmark" = "orange"))+
    theme(text = element_text(family = 'Gill Sans', color = "#444444",size = 25)
          ,panel.background = element_rect(fill = '#444B5A')
          ,panel.grid.minor = element_line(color = '#4d5566')
          ,panel.grid.major = element_line(color = '#586174')
          ,plot.title = element_text(size = 25)
          ,axis.title = element_text(size = 30, color = '#555555')
          ,axis.title.y = element_text(vjust = 0.5, angle = 90,size = 25)
          ,axis.title.x = element_text(hjust = 0.5)
          ,legend.position="bottom"
          ,legend.box = "horizontal"
          ,legend.title=element_blank()
          ,legend.text=element_text(size=25)
    )
  png(filename = "TEL.png",width = 1200, height = 800, units = "px", pointsize = 12,
      bg = "white")
  print(plot6)
  dev.off()
###########################################################

