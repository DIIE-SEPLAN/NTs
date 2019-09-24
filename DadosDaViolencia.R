require(tidyverse)
require(ggplot2)
require(plotly)
require(lubridate)
require(shadowtext)

options(OutDec= ",") #Assim os valores numéricos são separados por vírgula
#CVLI na Grande São Luís - Dados da SSP-MA, disponíveis no site da SSP-MA,
#https://www.ssp.ma.gov.br/estatisticas/estatisticas-da-grande-sao-luis/
#Sessão Grande São Luís

cvli <- read.csv2("CVLI da Grande Ilha.csv", encoding = "UTF-6", header = T)
cvli[,6] <- round(as.numeric(cvli[,5]/163 * 100))
colnames(cvli) <- c("Cidade", "Homicídio Doloso", "Roubo Seguido de Morte", "Lesão Corporal Seguida de morte", "CVLI", "Percentual")

colors <- c('rgb(0,177,89)','rgb(209,17,65)',  'rgb(0,174,219)','rgb(243,119,53)')
plot_ly(cvli, labels = ~Cidade, values = ~CVLI, type = 'pie',textposition = 'outside',textinfo = 'label+value', marker = list(colors = colors)) %>%
  layout(title = NULL, 
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

#Quantitativo de Mortes Violentas na Grande São Luís
#Dados do CAOP-CRIM, disponíveis em 
#https://mpma.mp.br/arquivos/COCOM/arquivos/centros_de_apoio/caop_crim/CVLI/2019/MVI_2019_06_-_RELAT%C3%93RIO.pdf

Ano <- as.numeric(2010:2018)
`Quantidade de Mortes` <- as.numeric(c(535, 655, 687, 984, 1227, 1016, 858, 630, 408))
homilha <- tibble(Ano, `Quantidade de Mortes`)



ggplot(`homilha`, aes(x = Ano, y = `Quantidade de Mortes`,label = Ano)) + 
  geom_bar(stat = "identity", fill = c("#EF7447", "#2EC4B6", "#5F506B", "#DF7373", "#208AAE", 
                                       "#58BC82", "#22AED1", "#F4A259", "#4D5057"))  + ylab("Quantidade de Mortes Violentas") + xlab(NULL) +
    theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "#6f7275"), legend.position = "none", panel.background = element_blank(), 
        axis.ticks = element_line(color = "#6f7275"),axis.title.y = element_text(color = "#282929")) + 
  geom_shadowtext(aes(label = `Quantidade de Mortes`),fontface ="bold", size=5, color = "white", position=position_dodge(width=0.9), vjust = 1.5, hjust =0.5)

#CVLI distribuído por faixa etária
#Dados do CAOP-CRIM, disponíveis em
#https://mpma.mp.br/arquivos/COCOM/arquivos/centros_de_apoio/caop_crim/INFOGR%C3%81FICOS/INFOGR%C3%81FICO_-_CVLI__-_01%C2%BA_SEMESTRE_2017.pdf
#em "Faixa Etária".
#Quanto aos dados abertos do CAOP-CRIM, estão todos disponíveis em Dados e Estatística
#em https://mpma.mp.br/index.php/dados-estat-controle-controle-atividade-policial

etaria <- as.factor(c("Abaixo de 11", "12 a 17", "18 a 29", "30 a 59", "Acima de 60"))
porcentual <- as.numeric(c(0.28, 12.04, 45.38, 40.90, 1.40))
fxet <- tibble(etaria,porcentual)
colnames(fxet) <- c("Faixa Etária", "Porcentagem")                       


colors = c("#ffe066", "#50514f", "#247ba0", "#70c1b3", "#f25f5c")

m = list(
  l = 20,
  r = 100,
  b = 50,
  t = 200,
  pad = 50
) #Margens do plot 
fxet <- ranking %>%  filter
#Gráfico Faixa Etára
plot_ly(fxet, labels = ~`Faixa Etária`, values = ~Porcentagem, type = 'pie',
        textposition = 'outside',textinfo = 'label+percent', 
        marker = list(colors = colors), hole = 0.6, sort = F) %>% 
  layout(title = NULL, 
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE), margins = m)

#Dados da SSP-MA, disponíveis no site da SSP-MA,
#https://www.ssp.ma.gov.br/estatisticas/estatisticas-da-grande-sao-luis/
anoacumulado <- as.character(2014:2018)
acumul <- as.numeric(c(910,800,693,540,336))
totalvitimas <- tibble(anoacumulado, acumul)
colnames(totalvitimas) = c("Ano", "Mortes")
ggplot(totalvitimas, aes(x=Ano, y= Mortes, fill = Ano)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("#00b159","#d11141", "#00aedb", 	"#f37735", "#ffc425")) +
  geom_shadowtext(aes(label = Mortes), size=5, fontface = "bold",
            color = "white", position=position_dodge(width=0.9), 
            vjust = 1.3, hjust =0.5) + ylab("Quantidade de Homicídios") + xlab(NULL) +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "#6f7275"), legend.position = "none", panel.background = element_blank(), 
      axis.ticks = element_line(color = "#6f7275"),axis.title.y = element_text(color = "#282929"))


#Ranking das Capitais
#Dados obtidos das estimativas gerais das capitais, disponíveis no atlas da violência - municípios
#Embora a tabela disponível pelo IPEA esteja em excel, uma versão csv, convertida pelo DIIE
#Está disponível.
ranking <- read.csv2("Taxa Estimada Capitais.csv", header = T)
ranking$hilite <- ifelse(ranking$Capital == "São Luís", "orange", "blue")
ggplot(ranking, aes(x = reorder(Capital, X2017), y = X2017)) + 
  geom_bar(stat = "identity", aes(fill = hilite)) + 
  xlab(NULL) + ylab("Taxa Estimada de Homicídios") +
  scale_fill_manual (values = c("#006766", "#f37835")) +
  geom_shadowtext(aes(label = X2017), size=3, family = "Arial", 
            color = "white", position=position_dodge(width=0.9), 
            vjust = 0.35, hjust =1.2, angle = 90) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "#6f7275"), legend.position = "none", panel.background = element_blank(), 
        axis.ticks = element_line(color = "#6f7275"),axis.title.y = element_text(color = "#282929", size = 11, family = "Times New Roman"),  axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3, 
                                                                                                                                 family = "Times New Roman"))

        