## Análise de dados de exportação 

?read.csv
export_products <- read.csv(file = 
                              '~/Documents/DatabasesGHUB/EXP_COMPLETA_MUN.csv',
                            sep = ';')
head(export_products)

# Tipos de dados
str(export_products)

# Colunas nulas
colSums(is.na(export_products))


install.packages("dplyr")
library(dplyr)

# exportações baianas
ba_exports <- filter(export_products, SG_UF_MUN == "BA")
head(ba_exports)

# media de valor das exportações
summary(ba_exports$VL_FOB)

# total por ano
total_exportacao_dolar_ano_ba <- ba_exports %>% group_by(CO_ANO) %>% summarise(Total_FOB = sum(VL_FOB))
total_exportacao_dolar_ano_ba
str(total_exportacao_dolar_ano_ba)

# Formata total de dólares para número em bilhões de dólares
total_exportacao_dolar_ano_ba <- transform(total_exportacao_dolar_ano_ba, 
                                Total_FOB = format(round(Total_FOB / 1e9, 1), TRIM = TRUE))
total_exportacao_dolar_ano_ba

?plot
# grafico de pontos
plot(x = total_exportacao_dolar_ano_ba$CO_ANO,
     y = total_exportacao_dolar_ano_ba$Total_FOB,
     main = "Histórico de exportações baianas anuais",
     xlab = "Período em anos",
     ylab = "Valor em bilhões de dólares")
?lines
# grafico de linhas
lines(x = total_exportacao_dolar_ano_ba$CO_ANO,
      y = total_exportacao_dolar_ano_ba$Total_FOB,
      col = "red")

# grafico com ggplot
library(ggplot2)
?ggplot

## GRAFICO COM 2021
ggplot(data=total_exportacao_dolar_ano_ba, 
       aes(x=CO_ANO, y=Total_FOB, group=1))+
geom_line(color="red")+
geom_point()

## removendo 2021 (dados incompletos)
total_exportacao_dolar_ano_ba <- filter(total_exportacao_dolar_ano_ba, CO_ANO != 2021)
ggplot(data=total_exportacao_dolar_ano_ba, 
       aes(x=CO_ANO, y=Total_FOB, group=1))+
  geom_line(color="red")+
  geom_point()

## dados SP
total_exportacao_dolar_ano_sp = filter(export_products, SG_UF_MUN == "SP") %>%
  group_by(CO_ANO) %>% 
  summarise(Total_FOB = sum(VL_FOB)) %>%
  transform(total_exportacao_dolar_ano_sp, 
            Total_FOB = format(round(Total_FOB / 1e9, 1), TRIM = TRUE))

###

lista_estados_nordestinos = list("BA", "CE", "PE", "SE", "MA", "PB",
                                 "AL", "PI", "RN")
dados_nordeste_brutos = filter(export_products, SG_UF_MUN %in% lista_estados_nordestinos)
head(dados_nordeste_brutos)

dados_nordeste = dados_nordeste_brutos %>%
                group_by(SG_UF_MUN, CO_ANO) %>%
                summarise(TOTAL_FOB = sum(VL_FOB), groups=SG_UF_MUN)
head(dados_nordeste)

# remove duplicatas
dados_nordeste <- unique(dados_nordeste)
head(dados_nordeste)

# remove coluna indesejada
dados_nordeste <- subset(dados_nordeste, select = -groups)

# transforma valores para bi dolares
dados_nordeste <- transform(dados_nordeste, 
          TOTAL_FOB = format(round(TOTAL_FOB / 1e9, 1), TRIM = TRUE))
head(dados_nordeste)

# remove 2021
dados_nordeste <- filter(dados_nordeste, CO_ANO != 2021)

# grafico comparativo das exportações entre os estados nordestinos

ggplot(dados_nordeste, aes(x=CO_ANO, y = TOTAL_FOB, group=SG_UF_MUN)) +
  geom_line(aes(color=SG_UF_MUN))+
  geom_point(aes(color=SG_UF_MUN)) +
  labs(title = "Exportações anuais dos estados nordestinos em bilhões de dólares", 
       x = "Período", y = "Valor em bilhões de dólares", color = "Estados")
