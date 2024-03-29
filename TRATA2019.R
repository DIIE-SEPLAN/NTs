require(tidyverse)
require(ggplot2)
require(wesanderson)

####PARTE 0 - Base das capitais do nordeste #####
#Uma das dificuldades encontradas na base do TRATA 2019 foi o fato dela ter sido disponibilizada
#apenas em formato .pdf, sendo necess�rio a convers�o manualmente ou por meio de programas. Por
#conta disso n�o h� um m�todo mais pr�tico para a automatiza��o do processo.

t2019 <- read.csv2("T2019.csv") #.pdf convertido para .csv

capitais <- t2019 %>% filter(
  Munic�pio %in% 
    c('Macei�','Salvador','Fortaleza','S�o Lu�s',
      'Jo�o Pessoa', 'Recife', 'Teresina', 'Natal', 'Aracaju'
    )
)
colnames(capitais)
capcap <- capitais[,c(
  1:5,8,12,16,34,35
)]
colnames(capcap) <- c(
  "Munic�pio", "UF", "Ranking 2019", "Ranking 2018",
  "Varia��o", "Atendimento Total de �gua 2017", "Atendimento total de Esgoto 2017",
  "Esgoto Tratado por �gua Consumida 2017", "Nota Total 2017", "Tarifa M�dia 2017"
)
ccc <- read.csv2("tant.csv")
ccc <- ccc[1:9,2:7]
colnames(ccc) <- c(
  "Atendimento total de �gua 2015", "Atendimento total de �gua 2016", 
  "Atendimento total de Esgoto 2015", "Atendimento total de Esgoto 2016",
  "Esgoto tratado por �gua consumida 2015", 'Esgoto tratado por �gua consumida 2016'
)
capitais <- cbind(capcap, ccc)
rm(cap,capcap, ccc, t2019)

capitais <- capitais[,c(
  1,2,4,3,5,9,11,12,6,13,14,7,15,16,8,10
)]
#ATENDIMENTO TOTAL DE �GUA DE TODAS AS CAPITAIS DO NORDESTE
atendagua <- capitais[,1:9]
colnames(cap) <- c(
  "Munic�pio", "UF", "Ranking 2018", "Ranking 2019",
  "Varia��o","Nota Total", "2015", "2016", "2017"
)
atendagua <- atendagua %>% gather(
  `2015`, `2016`, `2017`, key = "Ano", value = "Atendimento total de �gua"
)

##############Parte 1 - S�o Lu�s###############
##Filtrando tudo que � de s�o lu�s
slz <- capitais %>% filter(UF == "MA")
#atendimento de �gua
slzatagua <- slz[,1:9]
colnames(slzatagua) <- c(
  "Munic�pio", "UF","Ranking 2018","Ranking 2019",
  "Varia��o","Nota Total 17", "2015", "2016", "2017"
  )
slzatagua <- slzatagua %>% gather(
  `2015`, `2016`, `2017`, key = "Ano", value = "Atendimento total de �gua"
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
#esgoto tratado por �gua consumida
slzatesgotoagua <- slz[,c(13:15)]
colnames(slzatesgotoagua) <- c(
  "2015", "2016", "2017"
  )
slzatesgotoagua <- slzatesgotoagua %>% gather(
  `2015`, `2016`, `2017`, key = "Ano", value = "Esgoto Tratado por �gua Consumida"
)
`Esgoto Tratado por �gua Consumida` <- slzatesgotoagua[,2]
SLZ <- cbind(`slzatagua`,`Atendimento Total de Esgoto`,`Esgoto Tratado por �gua Consumida`)
rm(slz,slzatagua,slzatesgoto,slzatesgotoagua,`Atendimento Total de Esgoto`, `Esgoto Tratado por �gua Consumida`)
SLZ <- SLZ[,c(7:10)]
##SLZ continua - fazer a evolu��o dos indices de 2012 at� 2017,
##criando um vetor para cada indicador de acordo com os valores na nota
atagua <- c(88.02, 90.15, 80.62)
atesgoto <- c(47.09,48.8,45.55)
esgotoporagua <- c(4.03,8.48,8.07)
Ano <- c(2012,2013,2014)
TT <- cbind(Ano,atagua,atesgoto,esgotoporagua)
colnames(TT) <- colnames(SLZ)
SLZ <- rbind(TT,SLZ)
SLZ <- SLZ %>%gather(
  `Atendimento total de �gua`,
  `Atendimento Total de Esgoto`,
  `Esgoto Tratado por �gua Consumida`,
  key = "Indicador", value = "Valor")

####Parte 1.1 - Gr�fico 1 : Evolu��o dos indicadores de S�o Lu�s de 2012 at� 2017 ####
ggplot(SLZ,aes(x=Ano, y=Valor, color = Indicador, group = Indicador, ymin=0, ymax=100)) +
  geom_line(size = 0.7) + geom_point(size=2) + 
  scale_color_manual(values= wes_palette("IsleofDogs1")) + xlab(NULL) + ylab(NULL) +
  theme(legend.position = "bottom", legend.title = element_blank()) + 
  geom_text(aes(label=Valor),fontface ="bold", size=3, color = "black", vjust = 2)


####PARTE 2####

#Nessa parte est�o alguns gr�ficos relacionando todas as capitais anualmente. Por facilidade na
#visualiza��o da varia��o anual, substitu� pelos gr�ficos apresentados na parte 3.

####Parte 2.1 - Gr�fico 2 : Atendimento Total de �gua das Capitais Nordestinas####
#2016
ggplot(capitais,aes(x = Munic�pio, y =`Atendimento total de �gua 2016`)) + 
  geom_bar(stat = "identity", fill = c("#a1e58d","#f2d99e", "#9f96fb",
                                       "#e2c6ee", "#add7f0","#f26925","#11948b","#c7518e", "#f9d448")) +
  geom_text(aes(label=`Atendimento total de �gua 2016`),
            fontface ="bold", size=3, color = "black", vjust = 2) +
  aes(stringr::str_wrap(Munic�pio, 10), `Atendimento total de �gua 2016`) +
  xlab(NULL) + ylab("Atendimento Total de �gua em 2016 (%)") +
  theme_minimal()
#2017

ggplot(capitais,aes(x = Munic�pio, y =`Atendimento Total de �gua 2017`)) + 
  geom_bar(stat = "identity", fill = c("#a1e58d","#f2d99e", "#9f96fb",
                                       "#e2c6ee", "#add7f0","#f26925","#11948b","#c7518e", "#f9d448")) +
  geom_text(aes(label=`Atendimento Total de �gua 2017`),
            fontface ="bold", size=3, color = "black", vjust = 2) +
  aes(stringr::str_wrap(Munic�pio, 10), `Atendimento Total de �gua 2017`) +
  xlab(NULL) + ylab("Atendimento Total de �gua em 2017 (%)") +
  theme_minimal()

####Parte 2.2 - Gr�fico 3 : Atendimento total de esgoto das Capitais Nordestinas#### 
#2016
ggplot(capitais,aes(x = Munic�pio, y =`Atendimento total de Esgoto 2016`)) + 
  geom_bar(stat = "identity", fill = c("#a1e58d","#f2d99e", "#9f96fb",
                                       "#e2c6ee", "#add7f0","#f26925","#11948b","#c7518e", "#f9d448")) +
  geom_text(aes(label=`Atendimento total de Esgoto 2016`),
            fontface ="bold", size=3, color = "black", vjust = 2) +
  aes(stringr::str_wrap(Munic�pio, 10), `Atendimento total de Esgoto 2016`) +
  xlab(NULL) + ylab("Atendimento Total de Esgoto em 2016 (%)") +
  theme_minimal()
#2017
ggplot(capitais,aes(x = Munic�pio, y =`Atendimento total de Esgoto 2017`)) + 
  geom_bar(stat = "identity", fill = c("#a1e58d","#f2d99e", "#9f96fb",
                                       "#e2c6ee", "#add7f0","#f26925","#11948b","#c7518e", "#f9d448")) +
  geom_text(aes(label=`Atendimento total de Esgoto 2017`),
            fontface ="bold", size=3, color = "black", vjust = 2) +
  aes(stringr::str_wrap(Munic�pio, 10), `Atendimento total de Esgoto 2017`) +
  xlab(NULL) + ylab("Atendimento Total de Esgoto em 2017 (%)") +
  theme_minimal()
####Parte 2.3 - Gr�fico 4 : Esgoto Tratado por �gua Consumida das Capitais Nordestinas####
#2016
ggplot(capitais,aes(x = Munic�pio, y =`Esgoto tratado por �gua consumida 2016`)) + 
  geom_bar(stat = "identity", fill = c("#a1e58d","#f2d99e", "#9f96fb",
                                       "#e2c6ee", "#add7f0","#f26925","#11948b","#c7518e", "#f9d448")) +
  geom_text(aes(label=`Esgoto tratado por �gua consumida 2016`),
            fontface ="bold", size=3, color = "black", vjust = 2) +
  aes(stringr::str_wrap(Munic�pio, 10), `Esgoto tratado por �gua consumida 2016`) +
  xlab(NULL) + ylab("Esgoto Tratado por �gua Consumida em 2016 (%)") +
  theme_minimal()
#2017
ggplot(capitais,aes(x = Munic�pio, y =`Esgoto Tratado por �gua Consumida 2017`)) + 
  geom_bar(stat = "identity", fill = c("#a1e58d","#f2d99e", "#9f96fb",
                                       "#e2c6ee", "#add7f0","#f26925","#11948b","#c7518e", "#f9d448")) +
  geom_text(aes(label=`Esgoto Tratado por �gua Consumida 2017`),
            fontface ="bold", size=3, color = "black", vjust = 2) +
  aes(stringr::str_wrap(Munic�pio, 10), `Esgoto Tratado por �gua Consumida 2017`) +
  xlab(NULL) + ylab("Esgoto Tratado por �gua Consumida em 2017 (%)") +
  theme_minimal()

####PARTE 3####
#Aqui est�o os gr�ficos utilizados na compara��o entre as capitais nordestinas. 

####Parte 3.1 = Tratamento dos dados####
agua <- capitais[,7:9]
agua <- agua %>% gather(1:3, key = ano, value = Valor)
agua <- agua %>% mutate(Ano = substr(ano, nchar(ano)-4+1, nchar(ano)))
agua$ano = NULL
agua[,3] <- "Atendimento Total de �gua"
esgoto <- capitais[,10:12]
esgoto <- esgoto %>% gather(1:3, key ="ano", value = "Valor")
esgoto <- esgoto %>% mutate(Ano = substr(ano, nchar(ano)-4+1, nchar(ano)))
esgoto$ano = NULL
esgoto[,3] <- "Atendimento Total de Esgoto"
esgotoporagua <- capitais[,13:15]
esgotoporagua<- esgotoporagua %>% gather(1:3, key ="ano", value = "Valor")
esgotoporagua <- esgotoporagua %>% mutate(Ano = substr(ano, nchar(ano)-4+1, nchar(ano)))
esgotoporagua$ano = NULL
esgotoporagua[,3] <- "Esgoto Tratado por �gua Consumida"

tst <- rbind(agua,esgoto,esgotoporagua)

tt <- as.character(rep(capitais$Munic�pio, 9))
View(capitais)
tst$Munic�pio <- tt

#Atendimento Total de �gua, Esgoto e Esgoto Tratado �gua Consumida
totalagua <- tst %>% filter(V3 == "Atendimento Total de �gua")
totalesgoto <- tst %>% filter(V3 == "Atendimento Total de Esgoto")
totalesgotoporagua <- tst %>% filter (V3 == "Esgoto Tratado por �gua Consumida")


####Parte 3.2 - Gr�ficos####

#Atendimento Total de �gua - Gr�fico
ggplot(totalagua,aes(x = Munic�pio, y = Valor, group = Ano, fill = Ano)) + 
  geom_bar(width = 0.76, stat = "identity", color = "white", position = "dodge") +
  aes(stringr::str_wrap(Munic�pio, 10), `Valor`) + coord_flip() +
  geom_text(aes(label=Valor),fontface ="bold", size=4, color = "#2F4F4F", position=position_dodge(width=0.8), vjust = 0.1, hjust =-0.2)+
  theme_grey() + xlab(NULL) + ylab("Atendimento Total de �gua (%)") +
  scale_fill_manual(name = "Ano", values=c("#f2d99e","#9f96fb","#11948b"), 
                    labels=c("2015","2016","2017"))


#Atendimento Total de Esgoto - Gr�fico
ggplot(totalesgoto,aes(x = Munic�pio, y = Valor, group = Ano, fill = Ano)) + 
  geom_bar(width = 0.76, stat = "identity", color = "white", position = "dodge") +
  aes(stringr::str_wrap(Munic�pio, 10), `Valor`) + coord_flip() +
  geom_text(aes(label=Valor),fontface ="bold", size=4, color = "#2F4F4F", position=position_dodge(width=0.8), vjust = 0.1, hjust =-0.2)+
  theme_grey() + xlab(NULL) + ylab("Atendimento Total de Esgoto (%)") +
  scale_fill_manual(name = "Ano", values=c("#f2d99e","#9f96fb","#11948b"), 
                    labels=c("2015","2016","2017"))

#Esgoto Tratado por �gua Consumida - Gr�fico
ggplot(totalesgotoporagua,aes(x = Munic�pio, y = Valor, group = Ano, fill = Ano)) + 
  geom_bar(width = 0.76, stat = "identity", color = "white", position = "dodge") +
  aes(stringr::str_wrap(Munic�pio, 10), `Valor`) + coord_flip() +
  geom_text(aes(label=Valor),fontface ="bold", size=4, color = "#2F4F4F", position=position_dodge(width=0.8), vjust = 0.1, hjust =-0.2)+
  theme_grey() + xlab(NULL) + ylab("Esgoto Tratado por �gua Consumida (%)") +
  scale_fill_manual(name = "Ano", values=c("#f2d99e","#9f96fb","#11948b"), 
                    labels=c("2015","2016","2017"))