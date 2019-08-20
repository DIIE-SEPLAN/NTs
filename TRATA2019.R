require(tidyverse)
require(ggplot2)
require(wesanderson)

####PARTE 0 - Base das capitais do nordeste #####
#Uma das dificuldades encontradas na base do TRATA 2019 foi o fato dela ter sido disponibilizada
#apenas em formato .pdf, sendo necessário a conversão manualmente ou por meio de programas. Por
#conta disso não há um método mais prático para a automatização do processo.

t2019 <- read.csv2("T2019.csv") #.pdf convertido para .csv

capitais <- t2019 %>% filter(
  Município %in% 
    c('Maceió','Salvador','Fortaleza','São Luís',
      'João Pessoa', 'Recife', 'Teresina', 'Natal', 'Aracaju'
    )
)
colnames(capitais)
capcap <- capitais[,c(
  1:5,8,12,16,34,35
)]
colnames(capcap) <- c(
  "Município", "UF", "Ranking 2019", "Ranking 2018",
  "Variação", "Atendimento Total de Água 2017", "Atendimento total de Esgoto 2017",
  "Esgoto Tratado por Água Consumida 2017", "Nota Total 2017", "Tarifa Média 2017"
)
ccc <- read.csv2("tant.csv")
ccc <- ccc[1:9,2:7]
colnames(ccc) <- c(
  "Atendimento total de Água 2015", "Atendimento total de Água 2016", 
  "Atendimento total de Esgoto 2015", "Atendimento total de Esgoto 2016",
  "Esgoto tratado por Água consumida 2015", 'Esgoto tratado por Água consumida 2016'
)
capitais <- cbind(capcap, ccc)
rm(cap,capcap, ccc, t2019)

capitais <- capitais[,c(
  1,2,4,3,5,9,11,12,6,13,14,7,15,16,8,10
)]
#ATENDIMENTO TOTAL DE ÁGUA DE TODAS AS CAPITAIS DO NORDESTE
atendagua <- capitais[,1:9]
colnames(cap) <- c(
  "Município", "UF", "Ranking 2018", "Ranking 2019",
  "Variação","Nota Total", "2015", "2016", "2017"
)
atendagua <- atendagua %>% gather(
  `2015`, `2016`, `2017`, key = "Ano", value = "Atendimento total de água"
)

##############Parte 1 - São Luís###############
##Filtrando tudo que é de são luís
slz <- capitais %>% filter(UF == "MA")
#atendimento de água
slzatagua <- slz[,1:9]
colnames(slzatagua) <- c(
  "Município", "UF","Ranking 2018","Ranking 2019",
  "Variação","Nota Total 17", "2015", "2016", "2017"
  )
slzatagua <- slzatagua %>% gather(
  `2015`, `2016`, `2017`, key = "Ano", value = "Atendimento total de água"
)
#atendimento de esgoto
slzatesgoto <- slz[,c(10,11,12)]
colnames(slzatesgoto) <- c(
  "2015", "2016", "2017"
  )
slzatesgoto <- slzatesgoto %>% gather(
  `2015`, `2016`, `2017`, key = "Ano", value = "Atendimento total de esgoto"
)
`Atendimento Total de Esgoto` <- slzatesgoto[,2]
#esgoto tratado por água consumida
slzatesgotoagua <- slz[,c(13:15)]
colnames(slzatesgotoagua) <- c(
  "2015", "2016", "2017"
  )
slzatesgotoagua <- slzatesgotoagua %>% gather(
  `2015`, `2016`, `2017`, key = "Ano", value = "Esgoto Tratado por Água Consumida"
)
`Esgoto Tratado por Água Consumida` <- slzatesgotoagua[,2]
SLZ <- cbind(`slzatagua`,`Atendimento Total de Esgoto`,`Esgoto Tratado por Água Consumida`)
rm(slz,slzatagua,slzatesgoto,slzatesgotoagua,`Atendimento Total de Esgoto`, `Esgoto Tratado por Água Consumida`)
SLZ <- SLZ[,c(7:10)]
##SLZ continua - fazer a evolução dos indices de 2012 até 2017,
##criando um vetor para cada indicador de acordo com os valores na nota
atagua <- c(88.02, 90.15, 80.62)
atesgoto <- c(47.09,48.8,45.55)
esgotoporagua <- c(4.03,8.48,8.07)
Ano <- c(2012,2013,2014)
TT <- cbind(Ano,atagua,atesgoto,esgotoporagua)
colnames(TT) <- colnames(SLZ)
SLZ <- rbind(TT,SLZ)
SLZ <- SLZ %>%gather(
  `Atendimento total de água`,
  `Atendimento Total de Esgoto`,
  `Esgoto Tratado por Água Consumida`,
  key = "Indicador", value = "Valor")

####Parte 1.1 - Gráfico 1 : Evolução dos indicadores de São Luís de 2012 até 2017 ####
ggplot(SLZ,aes(x=Ano, y=Valor, color = Indicador, group = Indicador, ymin=0, ymax=100)) +
  geom_line(size = 0.7) + geom_point(size=2) + 
  scale_color_manual(values= wes_palette("IsleofDogs1")) + xlab(NULL) + ylab(NULL) +
  theme(legend.position = "bottom", legend.title = element_blank()) + 
  geom_text(aes(label=Valor),fontface ="bold", size=3, color = "black", vjust = 2)


####PARTE 2####

#Nessa parte estão alguns gráficos relacionando todas as capitais anualmente. Por facilidade na
#visualização da variação anual, substituí pelos gráficos apresentados na parte 3.

####Parte 2.1 - Gráfico 2 : Atendimento Total de Água das Capitais Nordestinas####
#2016
ggplot(capitais,aes(x = Município, y =`Atendimento total de Água 2016`)) + 
  geom_bar(stat = "identity", fill = c("#a1e58d","#f2d99e", "#9f96fb",
                                       "#e2c6ee", "#add7f0","#f26925","#11948b","#c7518e", "#f9d448")) +
  geom_text(aes(label=`Atendimento total de Água 2016`),
            fontface ="bold", size=3, color = "black", vjust = 2) +
  aes(stringr::str_wrap(Município, 10), `Atendimento total de Água 2016`) +
  xlab(NULL) + ylab("Atendimento Total de Água em 2016 (%)") +
  theme_minimal()
#2017

ggplot(capitais,aes(x = Município, y =`Atendimento Total de Água 2017`)) + 
  geom_bar(stat = "identity", fill = c("#a1e58d","#f2d99e", "#9f96fb",
                                       "#e2c6ee", "#add7f0","#f26925","#11948b","#c7518e", "#f9d448")) +
  geom_text(aes(label=`Atendimento Total de Água 2017`),
            fontface ="bold", size=3, color = "black", vjust = 2) +
  aes(stringr::str_wrap(Município, 10), `Atendimento Total de Água 2017`) +
  xlab(NULL) + ylab("Atendimento Total de Água em 2017 (%)") +
  theme_minimal()

####Parte 2.2 - Gráfico 3 : Atendimento total de esgoto das Capitais Nordestinas#### 
#2016
ggplot(capitais,aes(x = Município, y =`Atendimento total de Esgoto 2016`)) + 
  geom_bar(stat = "identity", fill = c("#a1e58d","#f2d99e", "#9f96fb",
                                       "#e2c6ee", "#add7f0","#f26925","#11948b","#c7518e", "#f9d448")) +
  geom_text(aes(label=`Atendimento total de Esgoto 2016`),
            fontface ="bold", size=3, color = "black", vjust = 2) +
  aes(stringr::str_wrap(Município, 10), `Atendimento total de Esgoto 2016`) +
  xlab(NULL) + ylab("Atendimento Total de Esgoto em 2016 (%)") +
  theme_minimal()
#2017
ggplot(capitais,aes(x = Município, y =`Atendimento total de Esgoto 2017`)) + 
  geom_bar(stat = "identity", fill = c("#a1e58d","#f2d99e", "#9f96fb",
                                       "#e2c6ee", "#add7f0","#f26925","#11948b","#c7518e", "#f9d448")) +
  geom_text(aes(label=`Atendimento total de Esgoto 2017`),
            fontface ="bold", size=3, color = "black", vjust = 2) +
  aes(stringr::str_wrap(Município, 10), `Atendimento total de Esgoto 2017`) +
  xlab(NULL) + ylab("Atendimento Total de Esgoto em 2017 (%)") +
  theme_minimal()
####Parte 2.3 - Gráfico 4 : Esgoto Tratado por Água Consumida das Capitais Nordestinas####
#2016
ggplot(capitais,aes(x = Município, y =`Esgoto tratado por Água consumida 2016`)) + 
  geom_bar(stat = "identity", fill = c("#a1e58d","#f2d99e", "#9f96fb",
                                       "#e2c6ee", "#add7f0","#f26925","#11948b","#c7518e", "#f9d448")) +
  geom_text(aes(label=`Esgoto tratado por Água consumida 2016`),
            fontface ="bold", size=3, color = "black", vjust = 2) +
  aes(stringr::str_wrap(Município, 10), `Esgoto tratado por Água consumida 2016`) +
  xlab(NULL) + ylab("Esgoto Tratado por Água Consumida em 2016 (%)") +
  theme_minimal()
#2017
ggplot(capitais,aes(x = Município, y =`Esgoto Tratado por Água Consumida 2017`)) + 
  geom_bar(stat = "identity", fill = c("#a1e58d","#f2d99e", "#9f96fb",
                                       "#e2c6ee", "#add7f0","#f26925","#11948b","#c7518e", "#f9d448")) +
  geom_text(aes(label=`Esgoto Tratado por Água Consumida 2017`),
            fontface ="bold", size=3, color = "black", vjust = 2) +
  aes(stringr::str_wrap(Município, 10), `Esgoto Tratado por Água Consumida 2017`) +
  xlab(NULL) + ylab("Esgoto Tratado por Água Consumida em 2017 (%)") +
  theme_minimal()

####PARTE 3####
#Aqui estão os gráficos utilizados na comparação entre as capitais nordestinas. 

####Parte 3.1 = Tratamento dos dados####
agua <- capitais[,7:9]
agua <- agua %>% gather(1:3, key = ano, value = Valor)
agua <- agua %>% mutate(Ano = substr(ano, nchar(ano)-4+1, nchar(ano)))
agua$ano = NULL
agua[,3] <- "Atendimento Total de Água"
esgoto <- capitais[,10:12]
esgoto <- esgoto %>% gather(1:3, key ="ano", value = "Valor")
esgoto <- esgoto %>% mutate(Ano = substr(ano, nchar(ano)-4+1, nchar(ano)))
esgoto$ano = NULL
esgoto[,3] <- "Atendimento Total de Esgoto"
esgotoporagua <- capitais[,13:15]
esgotoporagua<- esgotoporagua %>% gather(1:3, key ="ano", value = "Valor")
esgotoporagua <- esgotoporagua %>% mutate(Ano = substr(ano, nchar(ano)-4+1, nchar(ano)))
esgotoporagua$ano = NULL
esgotoporagua[,3] <- "Esgoto Tratado por Água Consumida"

tst <- rbind(agua,esgoto,esgotoporagua)

tt <- as.character(rep(capitais$Município, 9))
View(capitais)
tst$Município <- tt

#Atendimento Total de Água, Esgoto e Esgoto Tratado Água Consumida
totalagua <- tst %>% filter(V3 == "Atendimento Total de Água")
totalesgoto <- tst %>% filter(V3 == "Atendimento Total de Esgoto")
totalesgotoporagua <- tst %>% filter (V3 == "Esgoto Tratado por Água Consumida")


####Parte 3.2 - Gráficos####

#Atendimento Total de Água - Gráfico
ggplot(totalagua,aes(x = Município, y = Valor, group = Ano, fill = Ano)) + 
  geom_bar(width = 0.76, stat = "identity", color = "white", position = "dodge") +
  aes(stringr::str_wrap(Município, 10), `Valor`) + coord_flip() +
  geom_text(aes(label=Valor),fontface ="bold", size=4, color = "#2F4F4F", position=position_dodge(width=0.8), vjust = 0.1, hjust =-0.2)+
  theme_grey() + xlab(NULL) + ylab("Atendimento Total de Água (%)") +
  scale_fill_manual(name = "Ano", values=c("#f2d99e","#9f96fb","#11948b"), 
                    labels=c("2015","2016","2017"))


#Atendimento Total de Esgoto - Gráfico
ggplot(totalesgoto,aes(x = Município, y = Valor, group = Ano, fill = Ano)) + 
  geom_bar(width = 0.76, stat = "identity", color = "white", position = "dodge") +
  aes(stringr::str_wrap(Município, 10), `Valor`) + coord_flip() +
  geom_text(aes(label=Valor),fontface ="bold", size=4, color = "#2F4F4F", position=position_dodge(width=0.8), vjust = 0.1, hjust =-0.2)+
  theme_grey() + xlab(NULL) + ylab("Atendimento Total de Esgoto (%)") +
  scale_fill_manual(name = "Ano", values=c("#f2d99e","#9f96fb","#11948b"), 
                    labels=c("2015","2016","2017"))

#Esgoto Tratado por Água Consumida - Gráfico
ggplot(totalesgotoporagua,aes(x = Município, y = Valor, group = Ano, fill = Ano)) + 
  geom_bar(width = 0.76, stat = "identity", color = "white", position = "dodge") +
  aes(stringr::str_wrap(Município, 10), `Valor`) + coord_flip() +
  geom_text(aes(label=Valor),fontface ="bold", size=4, color = "#2F4F4F", position=position_dodge(width=0.8), vjust = 0.1, hjust =-0.2)+
  theme_grey() + xlab(NULL) + ylab("Esgoto Tratado por Água Consumida (%)") +
  scale_fill_manual(name = "Ano", values=c("#f2d99e","#9f96fb","#11948b"), 
                    labels=c("2015","2016","2017"))