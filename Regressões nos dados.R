#Aqui iremos rodar as regressões lineares do problema:

rm(list = ls())
gc()

library(dplyr)
library(rdrobust)
library(ggplot2)
library(readxl)
library(readr)
library(stringr)
library(rdd)
library(lubridate)
library(rddtools)
library(stargazer)

BaseTJSP <- read_xlsx("C:/Users/José Luiz/Documents/BMAC/TCC/Base de dados decisão TJSP.xlsx")
BasePrimeira <- read_xlsx("C:/Users/José Luiz/Documents/BMAC/TCC/BaseDados 1ª Instância Bruta.xlsx")

#Criando um subset para testar o modelo:

novo_df <- BaseTJSP %>%
  filter(`Situação do Provimento` %in% c('Provimento', 'Não-Provimento', 
                                         'Provimento em Parte'))

novo_df <- novo_df %>%
  mutate(`Situação do Provimento` = ifelse(`Situação do Provimento` == "Provimento em Parte", "Provimento", `Situação do Provimento`))

novo_df <- novo_df %>%
  mutate(`Principal Parte Ativa` = case_when(
    str_detect(`Principal Parte Ativa`, "Ministério") ~ "Ente 1",
    str_detect(`Principal Parte Ativa`, "Prefeitura") | str_detect(`Principal Parte Ativa`, "Município") | str_detect(`Principal Parte Ativa`, "Estado") ~ "Ente 2",
    TRUE ~ "Particular"
  ))
novo_df <- novo_df %>%
  mutate(`Principal Parte Passiva` = case_when(
    str_detect(`Principal Parte Passiva`, "Ministério") ~ "Ente 1",
    str_detect(`Principal Parte Passiva`, "Prefeitura") | str_detect(`Principal Parte Passiva`, "Município") | str_detect(`Principal Parte Passiva`, "Estado") ~ "Ente 2",
    TRUE ~ "Particular"
  ))


novo_df <- novo_df %>%
 filter(`Assunto Principal` %in% c('10011-Improbidade Administrativa'))



novo_df$`Data da Movimentação` <- as.Date(novo_df$`Data da Movimentação`)

novo_df <- novo_df %>%
  filter(novo_df$`Data da Movimentação` >= '2016-01-01')


novo_df$Ano <- format(novo_df$`Data da Movimentação`, "%Y")
novo_df$Mes <- format(novo_df$`Data da Movimentação`, "%m")

#nesta segunda parte rodaremos inicialmente algumas regressões

novo_df$`Data da Movimentação` <- as.Date(novo_df$`Data da Movimentação`)
novo_df$`Data da Movimentação` <- floor_date(novo_df$`Data da Movimentação`, "day")
data_corte <- as.Date("2021-10-14")
novo_df$tempo <- as.numeric(novo_df$`Data da Movimentação` - data_corte)
novo_df$Entrada <- as.Date(novo_df$Entrada)
novo_df$Entrada <- floor_date(novo_df$Entrada, "day")
novo_df$temporecurso <- as.numeric(novo_df$Entrada- data_corte)

# Particionamento e cálculo das médias
#a base e a grande dúvida é saber o tamanho dos bins, para ter estatísticas relevantes
#
#
#

bin_size <- 45
#segundo critério n^1/5
novo_df$bin <- floor(novo_df$tempo / bin_size)

#
#
#
#

dados_particulares <- subset(novo_df, novo_df$`Principal Parte Ativa` == 'Particular') %>%
  group_by(bin, `Situação do Provimento`) %>%
  summarise(Contagem = n())

#dados_particulares <- subset(dados_particulares, bin >-44)

## deve ajustar a bin, e verificar as primeiras observações pois podem estar
## atrapalhando os ajustes



total_por_período <- dados_particulares %>%
  group_by(bin) %>%
  summarise(Total = sum(Contagem))



dados_particulares <- merge(dados_particulares, total_por_período, by = c("bin"))

dados_particulares$Percentual <- (dados_particulares$Contagem / dados_particulares$Total) * 100

dados_particulares$`Situação do Provimento` <- factor(dados_particulares$`Situação do Provimento`, levels = c("Não-Provimento", "Provimento"))

ggplot(dados_particulares, aes(x = interaction(bin, sep = "-"), y = Percentual, color = `Situação do Provimento`,)) +
  geom_line() +
  geom_point() +
  labs(x = "Mês-Ano", y = "Percentual") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

dados_agrupados_part <- subset(dados_particulares, dados_particulares$`Situação do Provimento` == 'Provimento')
dados_agrupados_part$pos_lei <- ifelse(dados_agrupados_part$bin > 0, 1, 0)

#Rodando regressão linear:

modelo_part <- lm(dados_agrupados_part$Percentual ~ dados_agrupados_part$pos_lei, data = dados_agrupados_part)
stargazer(modelo_part, type = 'text')


#Rodando as regressões descontínuas:




rdd_object <- rdd_data(y = dados_agrupados_part$Percentual, x = dados_agrupados_part$bin, cutpoint = 0)
resultado <- rdd_reg_lm(rdd_object)

summary(resultado)
plot(rdd_object)

rd_dat_fakefuzzy <- rdd_data(x=dados_agrupados_part$bin, y = dados_agrupados_part$Percentual, 
                             z=ifelse(dados_agrupados_part$bin > 0, 1, 0), 
                             cutpoint=0)
summary(rd_dat_fakefuzzy)
plot(rd_dat_fakefuzzy)
resultadofuzzy <- rdd_reg_lm(rd_dat_fakefuzzy)
summary(resultadofuzzy)

rdplot(y = dados_particulares$Percentual, x = dados_particulares$bin, p=1,
       subset = dados_particulares$`Situação do Provimento` == 'Provimento',
       x.label = paste("Períodos de grupo de", bin_size, " dias"),
       y.label = "Percentual de Provimentos nas ações",
       title = "Diferença em percentual de provimentos em recursos de - Particulares")

summary(rdrobust(y = dados_particulares$Percentual, x = dados_particulares$bin, 
         subset = dados_particulares$`Situação do Provimento` == 'Provimento',
         masspoints = 'off', c=0))



### MINISTÉRIO PÚBLICO

# Particionamento e cálculo das médias
#a base e a grande dúvida é saber o tamanho dos bins, para ter estatísticas relevantes
bin_size <- 60
novo_df$bin <- floor(novo_df$tempo / bin_size)


dados_MP <- subset(novo_df, novo_df$`Principal Parte Ativa` == 'Ente 1') %>%
  group_by(bin, `Situação do Provimento`) %>%
  summarise(Contagem = n())



## deve ajustar a bin, e verificar as primeiras observações pois podem estar
## atrapalhando os ajustes

#dados_MP <- subset(dados_MP, bin >-280)

total_por_período <- dados_MP %>%
  group_by(bin) %>%
  summarise(Total = sum(Contagem))

dados_MP <- merge(dados_MP, total_por_período, by = c("bin"))

dados_MP$Percentual <- (dados_MP$Contagem / dados_MP$Total) * 100

dados_MP$`Situação do Provimento` <- factor(dados_MP$`Situação do Provimento`, levels = c("Não-Provimento", "Provimento"))

ggplot(dados_MP, aes(x = interaction(bin, sep = "-"), y = Percentual, color = `Situação do Provimento`,)) +
  geom_line() +
  geom_point() +
  labs(x = "Mês-Ano", y = "Percentual") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

dados_agrupados_MP <- subset(dados_MP, dados_MP$`Situação do Provimento` == 'Provimento')
dados_agrupados_MP$pos_lei <- ifelse(dados_agrupados_MP$bin > 0, 1, 0)

#Rodando regressão linear:

modelo_MP <- lm(dados_agrupados_MP$Percentual ~ dados_agrupados_MP$pos_lei, data = dados_agrupados_MP)
stargazer(modelo_MP, type = 'text')


#Rodando as regressões descontínuas:


rdd_object <- rdd_data(y = dados_agrupados_MP$Percentual, x = dados_agrupados_MP$bin, cutpoint = 0)
resultado <- rdd_reg_lm(rdd_object)

summary(resultado)
plot(rdd_object)

rd_dat_fakefuzzy <- rdd_data(x=dados_agrupados_MP$bin, y = dados_agrupados_MP$Percentual, 
                             z=ifelse(dados_agrupados_MP$bin > 0, 1, 0), 
                             cutpoint=0)
summary(rd_dat_fakefuzzy)
plot(rd_dat_fakefuzzy)
resultadofuzzy <- rdd_reg_lm(rd_dat_fakefuzzy)
summary(resultadofuzzy)

rdplot(y = dados_MP$Percentual, x = dados_MP$bin, p=1,
       subset = dados_MP$`Situação do Provimento` == 'Provimento',
       x.label = paste("Períodos de grupo de", bin_size, " dias"),
       y.label = "Percentual de Provimentos nas ações",
       title = "Diferença em percentual de provimentos em recursos de MP")

summary(rdrobust(y = dados_MP$Percentual, x = dados_MP$bin, 
         subset = dados_MP$`Situação do Provimento` == 'Provimento',
         masspoints = 'off'))





## 2. Analisando a principal parte que recorreu: (Ajustar base de dados junto ao TJSP)


bin_size <- 7
novo_df$bin <- floor(novo_df$temporecurso / bin_size)

dados_recorrentes <- novo_df %>%
  group_by(bin, `Principal Parte Ativa`) %>%
  summarise(Contagem = n())

total_por_bin <- dados_recorrentes %>%
  group_by(bin) %>%
  summarise(Total = sum(Contagem))

dados_recorrentes <- subset(dados_recorrentes, bin >-380)

### Particular recorrendo:

dados_rec_particulares <- subset(dados_recorrentes, dados_recorrentes$`Principal Parte Ativa` == 'Particular')

dados_rec_particulares <- merge(dados_rec_particulares, total_por_bin, by = c("bin"))

dados_rec_particulares$Percentual <- (dados_rec_particulares$Contagem / dados_rec_particulares$Total) * 100

ggplot(dados_rec_particulares, aes(x = interaction(bin, sep = "-"), y = Percentual,)) +
  geom_line() +
  geom_point() +
  labs(x = "Mês-Ano", y = "Percentual") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 


dados_rec_particulares$pos_lei <- ifelse(dados_rec_particulares$bin > 0, 1, 0)

#Rodando regressão linear:

modelo_part <- lm(dados_rec_particulares$Percentual ~ dados_rec_particulares$pos_lei, data = dados_rec_particulares)
stargazer(modelo_part, type = 'text')


#Rodando as regressões descontínuas:


rdd_object <- rdd_data(y = dados_rec_particulares$Percentual, x = dados_rec_particulares$bin, cutpoint = 0)
resultado <- rdd_reg_lm(rdd_object)

summary(resultado)
plot(rdd_object)

rd_dat_fakefuzzy <- rdd_data(x=dados_rec_particulares$bin, y = dados_rec_particulares$Percentual, 
                             z=ifelse(dados_rec_particulares$bin > 0, 1, 0), 
                             cutpoint=0)
summary(rd_dat_fakefuzzy)
plot(rd_dat_fakefuzzy)
resultadofuzzy <- rdd_reg_lm(rd_dat_fakefuzzy)
summary(resultadofuzzy)

rdplot(y = dados_rec_particulares$Percentual, x = dados_rec_particulares$bin, p=1,
       x.label = paste("Períodos de grupo de", bin_size, " dias"),
       y.label = "Percentual de Recursos dos Particulares frente ao total de recursos",
       title = "Diferença em percentual de recorribilidade após a alteração legal")

summary(rdrobust(y = dados_rec_particulares$Percentual, x = dados_rec_particulares$bin, 
         masspoints = 'off'))


### MP recorrendo:

dados_rec_MP <- subset(dados_recorrentes, dados_recorrentes$`Principal Parte Ativa` == 'Ente 1')

dados_rec_MP <- merge(dados_rec_MP, total_por_bin, by = c("bin"))

dados_rec_MP$Percentual <- (dados_rec_MP$Contagem / dados_rec_MP$Total) * 100


ggplot(dados_rec_MP, aes(x = interaction(bin, sep = "-"), y = Percentual,)) +
  geom_line() +
  geom_point() +
  labs(x = "Mês-Ano", y = "Percentual") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 


dados_rec_MP$pos_lei <- ifelse(dados_rec_MP$bin > 0, 1, 0)

#Rodando regressão linear:

modelo_MP_rec <- lm(dados_rec_MP$Percentual ~ dados_rec_MP$pos_lei, data = dados_rec_MP)
stargazer(modelo_MP_rec, type = 'text')


#Rodando as regressões descontínuas:


rdd_object <- rdd_data(y = dados_rec_MP$Percentual, x = dados_rec_MP$bin, cutpoint = 0)
resultado <- rdd_reg_lm(rdd_object)

summary(resultado)
plot(rdd_object)

rd_dat_fakefuzzy <- rdd_data(x=dados_rec_MP$bin, y = dados_rec_MP$Percentual, 
                             z=ifelse(dados_rec_MP$bin > 0, 1, 0), 
                             cutpoint=0)
summary(rd_dat_fakefuzzy)
plot(rd_dat_fakefuzzy)
resultadofuzzy <- rdd_reg_lm(rd_dat_fakefuzzy)
summary(resultadofuzzy)

rdplot(y = dados_rec_MP$Percentual, x = dados_rec_MP$bin, p=1,
       x.label = paste("Períodos de grupo de", bin_size, " dias"),
       y.label = "Percentual de Recursos dos Particulares frente ao total de recursos",
       title = "Diferença em percentual de recorribilidade após a alteração legal")

summary(rdrobust(y = dados_rec_MP$Percentual, x = dados_rec_MP$bin, 
         masspoints = 'off'))



##3.Analisando a base de dados da primeira instância:

#Agora iniciaremos analisando a segunda base de dados na qual temos os processos
#em primeira instância e iremos verificar quantas novas ações foram propostas após a 
#alteração na LIA'''

primeira_df <- BasePrimeira %>%
  mutate(`Principal Parte Ativa` = case_when(
    str_detect(`Principal Parte Ativa`, "Ministério") ~ "Ente 1",
    str_detect(`Principal Parte Ativa`, "Prefeitura") | 
      str_detect(`Principal Parte Ativa`, "Município") | 
      str_detect(`Principal Parte Ativa`, "Estado") | 
      str_detect(`Principal Parte Ativa`, "MUNICÍPIO") |
      str_detect(`Principal Parte Ativa`, "PREFEITURA") ~ "Ente 2",
    TRUE ~ "Particular"
  ))


#primeira_df <- primeira_df %>%
  #filter(`Assunto` %in% c('Improbidade Administrativa'))

primeira_df$Entrada <- dmy(primeira_df$Entrada)
primeira_df <- primeira_df %>%
  filter(primeira_df$Entrada >= '2017-01-01')


primeira_df$Ano <- format(primeira_df$Entrada, "%Y")
primeira_df$Mes <- format(primeira_df$Entrada, "%m")


#nesta segunda parte rodaremos inicialmente algumas regressões

primeira_df$Entrada <- as.Date(primeira_df$Entrada)
primeira_df$Entrada <- floor_date(primeira_df$Entrada, "day")
data_corte <- as.Date("2021-10-14")
primeira_df$tempo <- as.numeric(primeira_df$Entrada - data_corte)

#Total por ano, parte indiscriminada - Percentual aparentemente mostra que o MP avocou as
#ações para sua competência, dada a mudança de quem poderá entrar com ações agora.

dados_primeira1 <- primeira_df %>%
  group_by(Ano, `Principal Parte Ativa`) %>%
  summarise(Contagem = n())

total_por_ano <- dados_primeira1 %>%
  group_by(Ano) %>%
  summarise(Total = sum(Contagem))

dados_primeira1 <- merge(dados_primeira1, total_por_ano, by = c("Ano"))

dados_primeira1$Percentual <- (dados_primeira1$Contagem / dados_primeira1$Total) * 100


ggplot(dados_primeira1, aes(x = interaction(Ano, sep = "-"), y = Percentual, color = `Principal Parte Ativa`)) +
  geom_line() +
  geom_point() +
  labs(x = "Mês-Ano", y = "Percentual") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



bin_size <- 7
primeira_df$bin <- floor(primeira_df$tempo / bin_size)

dados_primeira2 <- primeira_df %>%
  group_by(bin, `Principal Parte Ativa`) %>%
  summarise(Contagem = n())

total_por_bin <- dados_primeira2 %>%
  group_by(bin) %>%
  summarise(Total = sum(Contagem))

dados_MP_prim <- subset(dados_primeira2, dados_primeira2$`Principal Parte Ativa` == 'Ente 1')

dados_MP_prim <- merge(dados_MP_prim, total_por_bin, by = c("bin"))

dados_MP_prim$Percentual <- (dados_MP_prim$Contagem / dados_MP_prim$Total) * 100


ggplot(dados_MP_prim, aes(x = interaction(bin, sep = "-"), y = Percentual,)) +
  geom_line() +
  geom_point() +
  labs(x = "Mês-Ano", y = "Percentual") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 


dados_MP_prim$pos_lei <- ifelse(dados_MP_prim$bin > 0, 1, 0)

#Rodando regressão linear:

modelo_MP <- lm(dados_MP_prim$Percentual ~ dados_MP_prim$pos_lei, data = dados_MP_prim)
stargazer(modelo_MP, type = 'text')


#Rodando as regressões descontínuas:


rdd_object <- rdd_data(y = dados_MP_prim$Percentual, x = dados_MP_prim$bin, cutpoint = 0)
resultado <- rdd_reg_lm(rdd_object)

summary(resultado)
plot(rdd_object)

rd_dat_fakefuzzy <- rdd_data(x=dados_MP_prim$bin, y = dados_MP_prim$Percentual, 
                             z=ifelse(dados_MP_prim$bin > 0, 1, 0), 
                             cutpoint=0)
summary(rd_dat_fakefuzzy)
plot(rd_dat_fakefuzzy)
resultadofuzzy <- rdd_reg_lm(rd_dat_fakefuzzy)
summary(resultadofuzzy)

rdplot(y = dados_MP_prim$Percentual, x = dados_MP_prim$bin, p=2,
       x.label = paste("Períodos de grupo de", bin_size, " dias"),
       y.label = "Percentual de Ações do Ministério Público",
       title = "Diferença em percentual de ações do MP após a alteração legal")

summary(rdrobust(y = dados_MP_prim$Percentual, x = dados_MP_prim$bin, 
         masspoints = 'off'))

