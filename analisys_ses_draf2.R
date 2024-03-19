pacman::p_load(tidyverse,lubridate,vroom,readxl,
               gifski,DescTools,janitor,geobr,ggspatial,
               gtsummary,ggthemes,ggrepel,grid,sf,sp, 
               gganimate)

library(INLA)

#importação e tratamento
df = read.csv2("20231218.csv") #banco 2021-2023
df = clean_names(df)

#filtragem de dados
df <- df %>% 
  filter(criterio == "TESTE RÁPIDO"| criterio == "RT-PCR")
df = df %>% filter(!is.na(data_sintomas))
df <- df %>% 
  filter(idade < 18 & data_sintomas <= dmy(c("31/12/2022")))


#importação de banco de dados e tratamento - 2020
df1 = read.csv2("20231218_Ano2020.csv")
df1 = clean_names(df1)

#filtragem de dados
df1 <- df1 %>% 
  filter(criterio == "TESTE RÁPIDO"| criterio == "RT-PCR")
df1 <- df1 %>% 
  filter(idade < 18 & data_sintomas <= dmy(c("31/12/2020")))


#união de df e df1
df = rbind(df,df1)

#exclusão para liberar memória
rm(df1)

df$data_sintomas <- as.Date(df$data_sintomas, format = "%d/%m/%Y")
df$data_inclusao = dmy(df$data_inclusao)
df$data_evolucao = dmy(df$data_evolucao)

#criação de variáveis
df <- df %>%
  mutate(
    periodo = case_when(
      between(data_inclusao, as.Date("01/01/2020", format = "%d/%m/%Y"), as.Date("31/05/2020", format = "%d/%m/%Y")) ~ "Primeiro trimestre",
      between(data_inclusao, as.Date("01/06/2020", format = "%d/%m/%Y"), as.Date("31/08/2020", format = "%d/%m/%Y")) ~ "Segundo trimestre",
      between(data_inclusao, as.Date("01/09/2020", format = "%d/%m/%Y"), as.Date("30/11/2020", format = "%d/%m/%Y")) ~ "Terceiro trimestre",
      between(data_inclusao, as.Date("01/12/2020", format = "%d/%m/%Y"), as.Date("28/02/2021", format = "%d/%m/%Y")) ~ "Quarto trimestre",
      between(data_inclusao, as.Date("01/03/2021", format = "%d/%m/%Y"), as.Date("31/05/2021", format = "%d/%m/%Y")) ~ "Quinto trimestre",
      between(data_inclusao, as.Date("01/06/2021", format = "%d/%m/%Y"), as.Date("31/08/2021", format = "%d/%m/%Y")) ~ "Sexto trimestre",
      between(data_inclusao, as.Date("01/09/2021", format = "%d/%m/%Y"), as.Date("30/11/2021", format = "%d/%m/%Y")) ~ "Sétimo trimestre",
      between(data_inclusao, as.Date("01/12/2021", format = "%d/%m/%Y"), as.Date("28/02/2022", format = "%d/%m/%Y")) ~ "Oitavo trimestre",
      between(data_inclusao, as.Date("01/03/2022", format = "%d/%m/%Y"), as.Date("31/05/2022", format = "%d/%m/%Y")) ~ "Novo trimestre",
      between(data_inclusao, as.Date("01/06/2022", format = "%d/%m/%Y"), as.Date("31/08/2022", format = "%d/%m/%Y")) ~ "Décimo trimestre",
      between(data_inclusao, as.Date("01/09/2022", format = "%d/%m/%Y"), as.Date("30/11/2022", format = "%d/%m/%Y")) ~ "Décimo primeiro trimestre",
      between(data_inclusao, as.Date("01/12/2022", format = "%d/%m/%Y"), as.Date("31/12/2023", format = "%d/%m/%Y")) ~ "Décimo segundo trimestre"
    ))

df$periodo = factor(df$periodo, levels = c("Primeiro trimestre","Segundo trimestre","Terceiro trimestre",
                                           "Quarto trimestre","Quinto trimestre","Sexto trimestre",
                                           "Sétimo trimestre","Oitavo trimestre","Novo trimestre",
                                           "Décimo trimestre","Décimo primeiro trimestre","Décimo segundo trimestre"))

df <- df %>%
  mutate(dia_mes = floor_date(data_sintomas, unit = "month"))

df = df %>% filter(!(dia_mes>"2022-12-30"))

df <- df %>%
  mutate(
    periodo2 = case_when(
      between(dia_mes, as.Date("01/01/2020", format = "%d/%m/%Y"), as.Date("30/06/2020", format = "%d/%m/%Y")) ~ "Primeiro semestre",
      between(dia_mes, as.Date("01/07/2020", format = "%d/%m/%Y"), as.Date("31/12/2020", format = "%d/%m/%Y")) ~ "Segundo semestre",
      between(dia_mes, as.Date("01/01/2021", format = "%d/%m/%Y"), as.Date("30/06/2021", format = "%d/%m/%Y")) ~ "Terceiro semestre",
      between(dia_mes, as.Date("01/07/2021", format = "%d/%m/%Y"), as.Date("28/12/2021", format = "%d/%m/%Y")) ~ "Quarto semestre",
      between(dia_mes, as.Date("01/01/2022", format = "%d/%m/%Y"), as.Date("30/06/2022", format = "%d/%m/%Y")) ~ "Quinto semestre",
      between(dia_mes, as.Date("01/07/2022", format = "%d/%m/%Y"), as.Date("31/12/2022", format = "%d/%m/%Y")) ~ "Sexto semestre"))



#importação de banco populacional por município
df1 = read_xlsx("pop.xlsx")

df1 = clean_names(df1)

df <- left_join(df, df1, by = c("cod_ibge"))

df$municipio.x = NULL
df = rename(df, "municipio" = "municipio.y")

#importações geobr e organização

##regiões de saúde
hr = read.csv("regiao_covid.csv", sep = ";")
hr$code_health_region = as.character(hr$code_health_region)
hr1 <- read_health_region( year=2013, macro = FALSE)
hr1 = hr1 %>% filter(abbrev_state == "RS")
hr <- left_join(hr, hr1, by = c("code_health_region"))
hr$abbrev_state.x = NULL
hr$name_health_region.x = NULL
hr = rename(hr, "name_health_region" = "name_health_region.y")
hr = rename(hr, "abbrev_state" = "abbrev_state.y")
hr <- hr %>%
  mutate(hr = ifelse(cod_ibge == 431490, "Capital", "Interior")) #capital e interior

rm(hr1)

df = left_join(df, hr, by = c("cod_ibge"))

hr = left_join(df1, hr, by = c("municipio"))


df$ano = year(df$data_inclusao)

df$faixaetaria = gsub("15 a 19", "15 a 18", df$faixaetaria)

df = df %>% filter(!(ano==2023))


demo = df %>% filter()%>% tbl_summary(include = c(sexo,faixaetaria,raca_cor,hospitalizado, uti,srag,name_health_region,evolucao, periodo, ano),
                          label = list(sexo~"Sexo",
                                       faixaetaria~"Faixa etária",
                                       raca_cor~"Raça/Cor",
                                       hospitalizado~"Hospitalizado",
                                       uti~"UTI",
                                       srag~"SRAG",
                                       name_health_region~"Nome da região de saúde",
                                       evolucao~'Evolução',
                                       periodo~"Trimestre"),
                          by=ano) %>% add_p() %>% bold_labels() %>% as_flex_table() %>% 
  flextable::save_as_docx(demo, path="demo.docx")


demo1 = df %>% filter(hospitalizado=="SIM")  %>%  tbl_summary(include = c(sexo,faixaetaria,raca_cor, uti,srag,
                                                                          evolucao, periodo, ano),
                          label = list(sexo~"Sexo",
                                       faixaetaria~"Faixa etária",
                                       raca_cor~"Raça/Cor",
                                       uti~"UTI",
                                       srag~"SRAG",
                                       evolucao~'Evolução',
                                       periodo~"Trimestre"),
                          by=ano) %>% add_p() %>% bold_labels() %>% as_flex_table() %>% 
  flextable::save_as_docx(demo, path="demo1.docx")





table(df$name_health_region,df$hospitalizado)

table(df$name_health_region,df$evolucao)







rm(df1)


df1 <- hr %>%
  group_by(name_health_region) %>%
  summarize(pop = sum(pop))


df2 <- df %>%
  group_by(name_health_region, dia_mes) %>%
  summarize(covid_cases = n())

df1 <- left_join(df1, df2, by = c("name_health_region"))
rm(df2)

df1$inc = df1$covid_cases/df1$pop*100000

df1 = df1[1:1001,]

# incluindo o momento de cada variante no grafico

onda_breaksteste <-  c (ymd("2020-10-04", "2021-01-26", 
                            "2021-06-21",
                            "2021-12-10", "2022-05-01" , "2022-09-07" ))

# Maximos por onda e por regional
onda_breaksteste


df1 <- df1 %>% 
  mutate(
    onda = case_when(dia_mes <= dia_mes[1] ~ "B.1.1.33",
                     dia_mes > dia_mes[1] & dia_mes <= onda_breaksteste[2] ~ "Zeta",
                     dia_mes > dia_mes[2] & dia_mes <= onda_breaksteste[3] ~ "Gama",
                     dia_mes > dia_mes[3] & dia_mes <= onda_breaksteste[4] ~ "Delta",
                     dia_mes > dia_mes[4] & dia_mes <= onda_breaksteste[5] ~ "Omicron BA.1",
                     dia_mes > dia_mes[5] & dia_mes <= onda_breaksteste[6] ~ "Omicron BA.2",
                     dia_mes > dia_mes[6] ~ "Omicron BA.5",
                     # Sanity check
                     TRUE ~ "BANANA"),
    onda = factor(onda, levels = c("B.1.1.33", "Zeta", "Gama", 
                                   "Delta", "Omicron BA.1", "Omicron BA.2", "Omicron BA.5" ), ordered = T)
  ) 

df1 = df1 %>% arrange(name_health_region)

df2 = df1[1:500,]
df3 = df1[501:1001,]

  quadro_graficos <- df2 %>%
    ggplot(aes(x = dia_mes, y = inc, group = name_health_region, color = name_health_region)) +
    geom_line() +
    geom_vline(xintercept = as.numeric(onda_breaksteste), linetype = "dashed", color = "black") + # Adiciona linhas verticais pontilhadas
    facet_wrap(~name_health_region, scales = "free_y", ncol = 4) +
    labs(title = "Incidência de casos de COVID-19 por região de saúde",
         x = "Período",
         y = "Incidência por 100,000 habitantes") +
    theme_minimal() +
    ylim(0,6000)+
    theme(legend.position = "none",
          strip.background = element_rect(fill = "darkgray"),  # Define o fundo do cabeçalho como cinza escuro
          strip.text = element_text(color = "black"),
          axis.line = element_line(color = "black"),
          panel.background = element_rect(fill = 'white', colour = 'black'))+
    annotate(geom = "text", x = as.Date("2020-08-01"),
             y = 5000, label = "Zeta",
             size=2)+
    annotate(geom = "text", x = as.Date("2020-12-01"),
             y = 4600, label = "Gama",
             size=2)+
    annotate(geom = "text", x = as.Date("2021-04-01"),
             y = 4200, label = "Delta",
             size=2)+
    annotate(geom = "text", x = as.Date("2021-10-01"),
             y = 3800, label = "Omicron 
           BA.1",size=2)+
    annotate(geom = "text", x = as.Date("2022-02-01"),
             y = 4400, label = "Omicron 
          BA.2",size=2)+
    annotate(geom = "text", x = as.Date("2022-07-01"),
             y = 3000, label = "Omicron 
         BA.5",size=2)

  quadro_graficos2 <- df3 %>%
    ggplot(aes(x = dia_mes, y = inc, group = name_health_region, color = name_health_region)) +
    geom_line() +
    geom_vline(xintercept = as.numeric(onda_breaksteste), linetype = "dashed", color = "black") + # Adiciona linhas verticais pontilhadas
    facet_wrap(~name_health_region, scales = "free_y", ncol = 4) +
    labs(title = "Incidência de casos de COVID-19 por região de saúde",
         x = "Período",
         y = "Incidência por 100,000 habitantes") +
    theme_minimal() +
    ylim(0,6000)+
    theme(legend.position = "none",
          strip.background = element_rect(fill = "darkgray"),  # Define o fundo do cabeçalho como cinza escuro
          strip.text = element_text(color = "black"),
          axis.line = element_line(color = "black"),
          panel.background = element_rect(fill = 'white', colour = 'black'))+
    annotate(geom = "text", x = as.Date("2020-08-01"),
             y = 5000, label = "Zeta",size=3)+
    annotate(geom = "text", x = as.Date("2020-12-01"),
             y = 4600, label = "Gama",size=3)+
    annotate(geom = "text", x = as.Date("2021-04-01"),
             y = 4200, label = "Delta",size=3)+
    annotate(geom = "text", x = as.Date("2021-10-01"),
             y = 3800, label = "Omicron 
           BA.1",size=3)+
    annotate(geom = "text", x = as.Date("2022-02-01"),
             y = 4400, label = "Omicron 
          BA.2",size=3)+
    annotate(geom = "text", x = as.Date("2022-07-01"),
             y = 3000, label = "Omicron 
         BA.5",size=3)
  

  
  
  
  df1 <- hr %>%
    group_by(name_health_region) %>%
    summarize(pop = sum(pop))
  
  
  df2 <- df %>% filter(srag=="SIM") %>% 
    group_by(name_health_region, dia_mes) %>%
    summarize(srag_cases = n())
  
  df1 <- left_join(df1, df2, by = c("name_health_region"))
  rm(df2)
  
  df1$inc = df1$srag_cases/df1$pop*100000
  
  df1 = df1[1:546,]
  
  # incluindo o momento de cada variante no grafico
  
  onda_breaksteste <-  c (ymd("2020-10-04", "2021-01-26", 
                              "2021-06-21",
                              "2021-12-10", "2022-05-01" , "2022-09-07" ))
  
  # Maximos por onda e por regional
  onda_breaksteste
  
  
  df1 <- df1 %>% 
    mutate(
      onda = case_when(dia_mes <= dia_mes[1] ~ "B.1.1.33",
                       dia_mes > dia_mes[1] & dia_mes <= onda_breaksteste[2] ~ "Zeta",
                       dia_mes > dia_mes[2] & dia_mes <= onda_breaksteste[3] ~ "Gama",
                       dia_mes > dia_mes[3] & dia_mes <= onda_breaksteste[4] ~ "Delta",
                       dia_mes > dia_mes[4] & dia_mes <= onda_breaksteste[5] ~ "Omicron BA.1",
                       dia_mes > dia_mes[5] & dia_mes <= onda_breaksteste[6] ~ "Omicron BA.2",
                       dia_mes > dia_mes[6] ~ "Omicron BA.5",
                       # Sanity check
                       TRUE ~ "BANANA"),
      onda = factor(onda, levels = c("B.1.1.33", "Zeta", "Gama", 
                                     "Delta", "Omicron BA.1", "Omicron BA.2", "Omicron BA.5" ), ordered = T)
    ) 
  
  df1 = df1 %>% arrange(name_health_region)
  
  df2 = df1[1:284,]
  df3 = df1[285:546,]
  
  quadro_graficos <- df2 %>%
    ggplot(aes(x = dia_mes, y = inc, group = name_health_region, color = name_health_region)) +
    geom_line() +
    geom_vline(xintercept = as.numeric(onda_breaksteste), linetype = "dashed", color = "black") + # Adiciona linhas verticais pontilhadas
    facet_wrap(~name_health_region, scales = "free_y", ncol = 4) +
    labs(title = "Incidência de casos de SRAG por COVID-19 por região de saúde",
         x = "Período",
         y = "Incidência por 100,000 habitantes") +
    theme_minimal() +
    ylim(0,100)+
    theme(legend.position = "none",
          strip.background = element_rect(fill = "darkgray"),  # Define o fundo do cabeçalho como cinza escuro
          strip.text = element_text(color = "black"),
          axis.line = element_line(color = "black"),
          panel.background = element_rect(fill = 'white', colour = 'black'))+
    annotate(geom = "text", x = as.Date("2020-08-01"),
             y = 80, label = "Zeta",size=3)+
    annotate(geom = "text", x = as.Date("2020-12-01"),
             y = 80, label = "Gama",size=3)+
    annotate(geom = "text", x = as.Date("2021-04-01"),
             y = 80, label = "Delta",size=3)+
    annotate(geom = "text", x = as.Date("2021-10-01"),
             y = 80, label = "Omicron 
           BA.1",size=3)+
    annotate(geom = "text", x = as.Date("2022-02-01"),
             y = 80, label = "Omicron 
          BA.2",size=3)+
    annotate(geom = "text", x = as.Date("2022-07-01"),
             y = 80, label = "Omicron 
         BA.5",size=3)
  
  quadro_graficos2 <- df3 %>%
    ggplot(aes(x = dia_mes, y = inc, group = name_health_region, color = name_health_region)) +
    geom_line() +
    geom_vline(xintercept = as.numeric(onda_breaksteste), linetype = "dashed", color = "black") + # Adiciona linhas verticais pontilhadas
    facet_wrap(~name_health_region, scales = "free_y", ncol = 4) +
    labs(title = "Incidência de casos de SRAG por COVID-19 por região de saúde",
         x = "Período",
         y = "Incidência por 100,000 habitantes") +
    theme_minimal() +
    ylim(0,100)+
    theme(legend.position = "none",
          strip.background = element_rect(fill = "darkgray"),  # Define o fundo do cabeçalho como cinza escuro
          strip.text = element_text(color = "black"),
          axis.line = element_line(color = "black"),
          panel.background = element_rect(fill = 'white', colour = 'black'))+
    annotate(geom = "text", x = as.Date("2020-08-01"),
             y = 80, label = "Zeta",size=3)+
    annotate(geom = "text", x = as.Date("2020-12-01"),
             y = 80, label = "Gama",size=3)+
    annotate(geom = "text", x = as.Date("2021-04-01"),
             y = 80, label = "Delta",size=3)+
    annotate(geom = "text", x = as.Date("2021-10-01"),
             y = 80, label = "Omicron 
           BA.1",size=3)+
    annotate(geom = "text", x = as.Date("2022-02-01"),
             y = 80, label = "Omicron 
          BA.2",size=3)+
    annotate(geom = "text", x = as.Date("2022-07-01"),
             y = 80, label = "Omicron 
         BA.5",size=3)
  
  
  
  
  df1 <- hr %>%
    group_by(name_health_region) %>%
    summarize(pop = sum(pop))
  
  
  df2 <- df %>% filter(srag=="SIM") %>% 
    group_by(name_health_region, dia_mes) %>%
    summarize(srag_cases = n())
  
  df1 <- left_join(df1, df2, by = c("name_health_region"))
  rm(df2)
  
  df1$inc = df1$srag_cases/df1$pop*100000
  
  df1 = df1[1:546,]
  
  # incluindo o momento de cada variante no grafico
  
  onda_breaksteste <-  c (ymd("2020-10-04", "2021-01-26", 
                              "2021-06-21",
                              "2021-12-10", "2022-05-01" , "2022-09-07" ))
  
  # Maximos por onda e por regional
  onda_breaksteste
  
  
  df1 <- df1 %>% 
    mutate(
      onda = case_when(dia_mes <= dia_mes[1] ~ "B.1.1.33",
                       dia_mes > dia_mes[1] & dia_mes <= onda_breaksteste[2] ~ "Zeta",
                       dia_mes > dia_mes[2] & dia_mes <= onda_breaksteste[3] ~ "Gama",
                       dia_mes > dia_mes[3] & dia_mes <= onda_breaksteste[4] ~ "Delta",
                       dia_mes > dia_mes[4] & dia_mes <= onda_breaksteste[5] ~ "Omicron BA.1",
                       dia_mes > dia_mes[5] & dia_mes <= onda_breaksteste[6] ~ "Omicron BA.2",
                       dia_mes > dia_mes[6] ~ "Omicron BA.5",
                       # Sanity check
                       TRUE ~ "BANANA"),
      onda = factor(onda, levels = c("B.1.1.33", "Zeta", "Gama", 
                                     "Delta", "Omicron BA.1", "Omicron BA.2", "Omicron BA.5" ), ordered = T)
    ) 
  
  df1 = df1 %>% arrange(name_health_region)
  
  df2 = df1[1:284,]
  df3 = df1[285:546,]
  
  quadro_graficos <- df2 %>%
    ggplot(aes(x = dia_mes, y = inc, group = name_health_region, color = name_health_region)) +
    geom_line() +
    geom_vline(xintercept = as.numeric(onda_breaksteste), linetype = "dashed", color = "black") + # Adiciona linhas verticais pontilhadas
    facet_wrap(~name_health_region, scales = "free_y", ncol = 4) +
    labs(title = "Incidência de casos de SRAG por COVID-19 por região de saúde",
         x = "Período",
         y = "Incidência por 100,000 habitantes") +
    theme_minimal() +
    ylim(0,100)+
    theme(legend.position = "none",
          strip.background = element_rect(fill = "darkgray"),  # Define o fundo do cabeçalho como cinza escuro
          strip.text = element_text(color = "black"),
          axis.line = element_line(color = "black"),
          panel.background = element_rect(fill = 'white', colour = 'black'))+
    annotate(geom = "text", x = as.Date("2020-08-01"),
             y = 80, label = "Zeta",size=3)+
    annotate(geom = "text", x = as.Date("2020-12-01"),
             y = 80, label = "Gama",size=3)+
    annotate(geom = "text", x = as.Date("2021-04-01"),
             y = 80, label = "Delta",size=3)+
    annotate(geom = "text", x = as.Date("2021-10-01"),
             y = 80, label = "Omicron 
           BA.1",size=3)+
    annotate(geom = "text", x = as.Date("2022-02-01"),
             y = 80, label = "Omicron 
          BA.2",size=3)+
    annotate(geom = "text", x = as.Date("2022-07-01"),
             y = 80, label = "Omicron 
         BA.5",size=3)
  
  quadro_graficos2 <- df3 %>%
    ggplot(aes(x = dia_mes, y = inc, group = name_health_region, color = name_health_region)) +
    geom_line() +
    geom_vline(xintercept = as.numeric(onda_breaksteste), linetype = "dashed", color = "black") + # Adiciona linhas verticais pontilhadas
    facet_wrap(~name_health_region, scales = "free_y", ncol = 4) +
    labs(title = "Incidência de casos de SRAG por COVID-19 por região de saúde",
         x = "Período",
         y = "Incidência por 100,000 habitantes") +
    theme_minimal() +
    ylim(0,100)+
    theme(legend.position = "none",
          strip.background = element_rect(fill = "darkgray"),  # Define o fundo do cabeçalho como cinza escuro
          strip.text = element_text(color = "black"),
          axis.line = element_line(color = "black"),
          panel.background = element_rect(fill = 'white', colour = 'black'))+
    annotate(geom = "text", x = as.Date("2020-08-01"),
             y = 80, label = "Zeta",size=3)+
    annotate(geom = "text", x = as.Date("2020-12-01"),
             y = 80, label = "Gama",size=3)+
    annotate(geom = "text", x = as.Date("2021-04-01"),
             y = 80, label = "Delta",size=3)+
    annotate(geom = "text", x = as.Date("2021-10-01"),
             y = 80, label = "Omicron 
           BA.1",size=3)+
    annotate(geom = "text", x = as.Date("2022-02-01"),
             y = 80, label = "Omicron 
          BA.2",size=3)+
    annotate(geom = "text", x = as.Date("2022-07-01"),
             y = 80, label = "Omicron 
         BA.5",size=3) 
  
  
  
  
  
  
  
  
  resumo <- df %>%
  group_by(dia_mes, name_health_region) %>%
  summarize(total_pacientes = n())

df <- left_join(df, resumo, by = c("name_health_region","dia_mes"))
df <- left_join(df, df1, by = c("name_health_region","dia_mes"))
df$inc_mun = NULL
df$total_pacientes.x = NULL

df$pop.y = NULL

df$inc_regiao_saude = df$covid_cases/df$pop.x *100000 #regiao de saúde


media_inc_geral <- df %>%
  group_by(name_health_region) %>%
  summarize(media_inc = mean(inc_regiao_saude))

media_inc_geral = media_inc_geral[-1,]

media_inc_geral = left_join(media_inc_geral, hr, by = c("name_health_region"))

attach(df)

df_test = df %>% filter(df$periodo.x=="Primeiro trimestre") %>% group_by(name_health_region,geom) %>% 
  summarise(mean = mean(inc_regiao_saude)) 

writexl::write_xlsx(df_test, "casos_covid.xlsx")

#ESSE EXEMPLO FICOU MLEHOR
p = ggplot(df_test, aes(geometry=geom, fill=name_health_region)) +
  geom_sf(aes(fill = (mean))) +
  scale_fill_distiller(type = "seq",
                       palette = "Reds",
                       direction = 1,
                       limits=c(0,40000)) +
  theme_minimal() +
  labs(title = "Casos de COVID-19 por região de saúde, 2020 - 2022",
         fill = "Taxa por 100.000", caption = "Secretaria Estadual da Saúde do Rio Grande do Sul") +
  annotation_north_arrow(location = 'tl',
                         style = north_arrow_fancy_orienteering(text_size = 7,
                                                                fill = c("white", "grey2")))

p

#DF LEITOS

leitos = read.csv("leitos.csv", sep = ";")
leitos = clean_names(leitos)

leitos = rename(leitos, "name_health_region"="regiao_saude")

leitos = leitos %>% group_by(name_health_region) %>%
  summarize(leitos = max(numero_leitos_uti_pediatrico_sus_privado))


#DF SRAG

df_srag = df %>% filter(srag=="SIM")

covid <- df %>%
  group_by(name_health_region) %>%
  summarize(casos = n())

srag <- df_srag %>%
  group_by(name_health_region) %>%
  summarize(srag = n())

obito_srag <- df_srag %>% filter(evolucao=="OBITO") %>% 
  group_by(name_health_region) %>%
  summarize(obito = n())


df_srag = left_join(srag, obito_srag, by = c("name_health_region"))
df_srag = left_join(df_srag, leitos, by = c("name_health_region"))
df_srag = left_join(df_srag, covid, by = c("name_health_region"))

hr1 <- hr %>%
  select(name_health_region, geom, pop) %>% group_by(name_health_region, geom) %>% 
  summarise(pop=sum(pop)) %>% 
  distinct()

df_srag = left_join(df_srag, hr1, by = c("name_health_region"))

df_srag[is.na(df_srag)] = 0
df_srag$let = (df_srag$obito/df_srag$srag)*100
df_srag$srag_inc = (df_srag$srag/df_srag$casos)*100000
df_srag$leitos_pop = (df_srag$leitos/df_srag$pop)*100000

writexl::write_xlsx(df_srag,"srag.xlsx")

library(patchwork)

p1 = ggplot(df_srag, aes(geometry=geom, fill=name_health_region)) +
  geom_sf(aes(fill = (let))) +
  scale_fill_distiller(type = "seq",
                       palette = "Reds",
                       direction = 1,
                       limits=c(0,20)) +
  theme_minimal() +
  labs(title = "Letalidade - SRAG por COVID-19 por região de saúde, 2020 - 2022",
       fill = "Letalidade", caption = "") +
  annotation_north_arrow(location = 'tl',
                         style = north_arrow_fancy_orienteering(text_size = 7,
                                                                fill = c("white", "grey2")))

p1

p2 = ggplot(df_srag, aes(geometry=geom, fill=name_health_region)) +
  geom_sf(aes(fill = (srag_inc))) +
  scale_fill_distiller(type = "seq",
                       palette = "Reds",
                       direction = 1,
                       limits=c(0,2000)) +
  theme_minimal() +
  labs(title = "Taxa - SRAG por COVID-19 por região de saúde, 2020 - 2022",
       fill = "Taxa por 100.000 hab", caption = "") +
  annotation_north_arrow(location = 'tl',
                         style = north_arrow_fancy_orienteering(text_size = 7,
                                                                fill = c("white", "grey2")))

p2

p3 = ggplot(df_srag, aes(geometry=geom, fill=name_health_region)) +
  geom_sf(aes(fill = (leitos_pop))) +
  scale_fill_distiller(type = "seq",
                       palette = "blue",
                       direction = 1,
                       limits=c(0,100)) +
  theme_minimal() +
  labs(title = "Leitos pediátricos por habitante, 2020 - 2022",
       fill = "Leitos por 100.000 hab", caption = "Secretaria Estadual da Saúde do Rio Grande do Sul") +
  annotation_north_arrow(location = 'tl',
                         style = north_arrow_fancy_orienteering(text_size = 7,
                                                                fill = c("white", "grey2")))

p3

model=glm(let~leitos_pop*srag_inc,data=df_srag)
summary(model)

pg = p1+p2+p3 + plot_layout(guides = "collect")

pg


###########################
#######
#######
#######


media_inc_1 <- df %>% 
  group_by(name_health_region) %>%
  summarize(media_inc = mean(ifelse(periodo2 == "Primeiro semestre", ifelse(is.na(inc), 0, inc), 0)))

p1 = ggplot(media_inc_1) +
  geom_sf(aes(fill =(media_inc))) +
  scale_fill_distiller(type = "seq",
                       palette = "Reds",
                       direction = 1,
                       limits=c(0,1500)) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme_minimal() +
  labs(title = "16/03/2020 - 31/06/2020",
       fill=NULL) +
  coord_sf(datum = NA) 

p1

media_inc_2 <- df_test %>% 
  group_by(name_muni) %>%
  summarize(media_inc = mean(ifelse(periodo2 == "Segundo semestre", ifelse(is.na(inc), 0, inc), 0)))

p2 = ggplot(media_inc_2) +
  geom_sf(aes(fill =(media_inc))) +
  scale_fill_distiller(type = "seq",
                       palette = "Reds",
                       direction = 1,
                       limits=c(0,1500)) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme_minimal() +
  labs(title = "01/07/2020 - 31/12/2020",
       fill=NULL) +
  coord_sf(datum = NA) 

p2

media_inc_3 <- df_test %>% 
  group_by(name_muni) %>%
  summarize(media_inc = mean(ifelse(periodo2 == "Terceiro semestre", ifelse(is.na(inc), 0, inc), 0)))


p3 = ggplot(media_inc_3) +
  geom_sf(aes(fill =(media_inc))) +
  scale_fill_distiller(type = "seq",
                       palette = "Reds",
                       direction = 1,
                       limits=c(0,1500)) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme_minimal() +
  labs(title = "01/01/2021 - 31/06/2021",
       fill=NULL) +
  coord_sf(datum = NA) 

p3

media_inc_4 <- df_test %>% 
  group_by(name_muni) %>%
  summarize(media_inc = mean(ifelse(periodo2 == "Quarto semestre", ifelse(is.na(inc), 0, inc), 0)))

p4 = ggplot(media_inc_4) +
  geom_sf(aes(fill =(media_inc))) +
  scale_fill_distiller(type = "seq",
                       palette = "Reds",
                       direction = 1,
                       limits=c(0,1500)) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme_minimal() +
  labs(title = "01/07/2021 - 31/12/2021",
       fill=NULL) +
  coord_sf(datum = NA) 

p4

media_inc_5 <- df_test %>% 
  group_by(name_muni) %>%
  summarize(media_inc = mean(ifelse(periodo2 == "Quinto semestre", ifelse(is.na(inc), 0, inc), 0)))

p5 = ggplot(media_inc_5) +
  geom_sf(aes(fill =(media_inc))) +
  scale_fill_distiller(type = "seq",
                       palette = "Reds",
                       direction = 1,
                       limits=c(0,1500)) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme_minimal() +
  labs(title = "01/01/2022 - 31/06/2022",
       fill=NULL) +
  coord_sf(datum = NA) 

p5

media_inc_6 <- df_test %>% 
  group_by(name_muni) %>%
  summarize(media_inc = mean(ifelse(periodo2 == "Sexto semestre", ifelse(is.na(inc), 0, inc), 0)))


p6 = ggplot(media_inc_3) +
  geom_sf(aes(fill =(media_inc))) +
  scale_fill_distiller(type = "seq",
                       palette = "Reds",
                       direction = 1,
                       limits=c(0,1500)) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme_minimal() +
  labs(title = "01/07/2022 - 31/12/2022",
       fill=NULL) +
  coord_sf(datum = NA)


#       fill = NULL, caption = "Secretaria Estadual da Saúde do Rio Grande do Sul") +
 # annotation_scale(bar_cols = c("gray88", "white")) +
#  coord_sf(datum = NA) +
 # annotation_north_arrow(location = 'tl',
   #           style = north_arrow_fancy_orienteering(text_size = 7,
  #                                                              fill = c("white", "gray88")))

p6

library(patchwork)
pg = p1+p2+p3+p4+p5+p6 + plot_layout(guides = "collect")

pg


df <- df %>%
  mutate(ano = lubridate::year(dia_mes))

tbl_summary(df, include = c(name_health_region,sexo,
                            faixaetaria,evolucao,
                            hospitalizado,uti,srag,ano),
            by = ano)










df_test1 = data.frame(df$cod_ibge, df$inc, df$regiao_covid, df$periodo.x, df$hospitalizado, df$srag)

df_test1 <- df_test1 %>% filter(df.periodo=="Primeiro trimestre")
  group_by(df.cod_ibge, df.regiao_covid) %>%
  summarize(inc = mean(df.inc))

df_test1 = df_test1 %>% rename("cod_ibge"="df.cod_ibge")

df_test1 <- rs_01 %>% left_join(df_test1, by ="cod_ibge")




















summary_data <- summary(factor(df$criterio))

freq_porcentagem <- data.frame(
  criterio = names(summary_data),
  frequencia = summary_data,
  percentagem = prop.table(summary_data) * 100
)

ggplot(freq_porcentagem, aes(x = fct_infreq(criterio), y = frequencia, label = paste(round(percentagem, 1), "%"))) +
  geom_bar(stat = "identity", width = 0.4, fill = "skyblue") +
  geom_text(size = 6, vjust = -0.5) +
  labs(
    x = "Critério",
    y = "Frequência (%)",
    caption = "SES-RS"
  ) +
  theme_minimal()+
  ylim(0,300000)+
  theme(axis.title = element_text(face = "bold", size =12))

#transformando regiao em fator

df$regiao_covid = factor(df$regiao_covid)


# incluindo o ano na tabela

df$ano <- year(df$data_sintomas)

# tabela One

library(tableone)
tabelaOne <- CreateTableOne(vars=c("faixaetaria", "sexo", "raca_cor", 
                                   "uti","evolucao", 
                                   "regiao_de_saude","macrorregiao"), 
                            strata = "hospitalizado", argsApprox = list(correct = F), 
                            data=df)
print(tabelaOne, showAllLevels = T)

summary_table <- print(tabelaOne, noSpaces = TRUE)

write.csv(summary_table,file="tableone")

# criando as tabelas de letalidade

ver = df %>% 
group_by(uti,
   regiao_covid,
   epiweek = data_sintomas,
   faixaetaria,
   sexo,
) %>%
  summarise(
    hosp = n(),
    obitos = sum(evolucao=="OBITO", na.rm = T), letalidade = sum(obitos/hosp*100, na.rm = T)) %>% 
  View()


# Lendo regionais de saude + mapa já salvos
df=df[-305556,]

# Lendo regionais de saude + mapa -----------------------------------------
hr <- unique(df$name_health_region)
str(hr)

RS.hr = hr %>% filter(abbrev_state == "RS")

hr.cod <- as.character(RS.hr$code_health_region)
hr.name <- sort(as.character(RS.hr$name_health_region))

# Remove plot axis
no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())


nomes <- unique(df$name_health_region)
letras <- LETTERS[1:length(nomes)]
nomes2 <- paste(nomes)


df_hcr = unique(df$name_health_region,df$pop.x)

# Plot all regions in RJ state 
apresentacao = ggplot(df_test, aes(geometry=geom)) + 
  geom_sf(aes(fill = name_health_region), show.legend = T) +
  theme_minimal(base_size = 14) +
  labs(title = "Regiões de Saúde do Estado do Rio Grande do Sul",
    fill = "") +
  no_axis


# incluindo as populaçoes de cada regiao de saude
name_health_region = unique(df1$name_health_region)
pop = unique(df1$pop)

poprs.hr <- tibble(name_health_region = name_health_region, pop = pop)

#  transformando a variavel de primeiros sintomas para data 

df2 <- df %>%
  #  mutate(DATE = as.Date(`Data dos primeiros sintomas`)) %>%
  # mutate(DATE1 = as.Date(DATE, unit = "week")) 
  mutate(
    date0 = floor_date(as.Date(data_sintomas, format = "%y/%m/%d", unit ="week" )),
    date = ymd(data_sintomas),
    date_sem = date - as.numeric(format.Date(date,"%w"))
  )


# retirando a regiao de saude categorizada como incompleto
df2 <- df2 %>%filter(name_health_region != "Incompleto")

### suavizando os graficos

# incluindo o momento de cada variante no grafico

onda_breaksteste <-  c (ymd("2020-10-04", "2021-01-26", 
                            "2021-06-21",
                            "2021-12-10", "2022-05-01" , "2022-09-07" ))

# Maximos por onda e por regional
onda_breaksteste


df2 <- df2 %>% 
  mutate(
    onda = case_when(data_sintomas <= onda_breaksteste[1] ~ "B.1.1.33",
                     data_sintomas > onda_breaksteste[1] & data_sintomas <= onda_breaksteste[2] ~ "Zeta",
                     data_sintomas > onda_breaksteste[2] & data_sintomas <= onda_breaksteste[3] ~ "Gama",
                     data_sintomas > onda_breaksteste[3] & data_sintomas <= onda_breaksteste[4] ~ "Delta",
                     data_sintomas > onda_breaksteste[4] & data_sintomas <= onda_breaksteste[5] ~ "Omicron BA.1",
                     data_sintomas > onda_breaksteste[5] & data_sintomas <= onda_breaksteste[6] ~ "Omicron BA.2",
                     data_sintomas > onda_breaksteste[6] ~ "Omicron BA.5",
                     # Sanity check
                     TRUE ~ "BANANA"),
    onda = factor(onda, levels = c("B.1.1.33", "Zeta", "Gama", 
                                   "Delta", "Omicron BA.1", "Omicron BA.2", "Omicron BA.5" ), ordered = T)
  ) 


Freq(df2$onda)    

df_summ <- df2 %>% 
  group_by(data_sintomas, name_health_region, onda, code_health_region) %>%
  count() %>% 
  mutate(covid = n)



df_summ %>% 
  #filter(name_health_region == "Metropolitana I") %>% 
  group_by(onda) %>% 
  summarise(
    #  maxRMI = dt_evento_w[which.max(covid)/],
    a = data_sintomas[which.max(covid)])


a = df_summ %>% 
  group_by(onda) %>% 
  mutate(
    maxRMI = data_sintomas[which.max(covid)],
  ) %>% ungroup() %>% 
  group_by(onda, name_health_region) %>% 
  summarise(
    maxRMI = maxRMI[1],
    maxHR = data_sintomas[which.max(covid)]
  ) 




df_summ = left_join(df_summ, a, by ="name_health_region")

### modelando

pacman::p_load(INLA)



inla_df <- tibble(data_sintomas = sort(unique(df_summ$data_sintomas))) %>% filter(data_sintomas >= "2020-04-01", data_sintomas < "2022-12-01") %>% 
  rowid_to_column(var = "time")


df_summ <- df_summ %>% 
  left_join(
    poprs.hr,
    by = "name_health_region")

RS.inla.teste <- df_summ %>% 
  filter(data_sintomas >= "2020-03-05", data_sintomas < "2022-12-31") %>% 
  left_join(inla_df) %>%
  left_join(tibble( name_health_region = name_health_region) %>% 
              rowid_to_column(var = "Region")
  ) %>% drop_na()

RS.inla.teste$code_health_region = as.numeric(RS.inla.teste$code_health_region)
RS.inla.teste$Region = as.numeric(RS.inla.teste$Region)

formula.teste = covid ~ 1 + code_health_region + f(time, model = "rw2", 
                                           group = Region, 
                                           control.group = list(model = "iid")
)


output.teste <- inla(formula = formula.teste, family = "poisson",
                     data = RS.inla.teste)





tbl.picos <- RS.inla.teste %>% 
  #filter(name_health_region == "10 Capital/Vale Gravataí") %>% 
  group_by(name_health_region, onda.x) %>% 
  summarise(
    #  maxRMI = dt_evento_w[which.max(covid)/],
    dt_pico = data_sintomas[which.max(covid)],
    #DT_pico_2 = paste(epiweek(DT_pico),epiyear(DT_pico), sep = "/")
  ) 


##### Tempo a partir do pico da RMI - hospitalizacoes
tbl.picos <- tbl.picos %>% 
  group_by(onda.x) %>% 
  mutate(
    dt_pico_rmi = dt_pico[name_health_region == "10 Capital/Vale Gravataí"]
  ) %>% ungroup() %>%
  mutate(
    tempo = dt_pico - dt_pico_rmi
  ) 

# GRAFICOOOOOOOOOOOOOS

nomes <- unique(hr$name_health_region)
nomes = nomes[1:30]

no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())

hr1 = read_health_region(macro = FALSE, year=2013) %>% filter(abbrev_state=="RS")
hr1 = hr1[1:30,]

RS.hr = left_join(RS.inla.teste,hr1, by="name_health_region")
nomes = unique(RS.hr$name_health_region)
levels= unique(RS.hr$Region)

data = RS.inla.teste %>% 
    left_join(
      tbl.picos %>% 
        mutate(
          tempo = as.numeric(tempo)/7,
          tempo.d = cut(tempo,
                        breaks = c(-1,0,1,2,3,20),
                        labels = c("-1",
                                   "0",
                                   "1",
                                   "2",
                                   "3 ou mais"),
                        right = F),
          labels = factor(name_health_region, 
                          levels = nomes, 
                          labels = nomes)
        )
    )

data = left_join(data,hr1, by="name_health_region")

us_tbl <- as_tibble(data)

ggplot(data,aes(geometry = geom, fill=name_health_region)) +
  geom_sf(mapping = aes(fill=tempo.d), 
          size=.25, 
          show.legend = T) + 
  # geom_sf(data = RMI.hachurado, size = 1) +
  scale_fill_brewer(palette = "Blues", drop=F, na.value = "grey50") +
  labs(title="Regiões de saúde no RS" , 
       #subtitle = "Variante predominante: Original B.1.33", 
       size=8, 
       fill = "Semanas após \n pico na Região \n Capital/Vale Gravataí" ) + 
  theme_minimal(base_size = 10) +
  no_axis + 
  theme(legend.position = c(.0000000001,.5) , plot.title = element_text(hjust = 0.5)) + 
  facet_wrap(~onda.x, nrow = 3)




###########################


df_summ_srag <- df2 %>% filter(srag=="SIM") %>% 
  group_by(data_sintomas, name_health_region, onda) %>%
  count() %>% 
  mutate(covid = n)



df_summ_srag %>% 
  #filter(name_health_region == "Metropolitana I") %>% 
  group_by(onda) %>% 
  summarise(
    #  maxRMI = dt_evento_w[which.max(covid)/],
    a = data_sintomas[which.max(covid)])


a_srag = df_summ_srag %>% 
  group_by(onda) %>% 
  mutate(
    maxRMI = data_sintomas[which.max(covid)],
  ) %>% ungroup() %>% 
  group_by(onda, name_health_region) %>% 
  summarise(
    maxRMI = maxRMI[1],
    maxHR = data_sintomas[which.max(covid)]
  ) 




df_summ_srag = left_join(df_summ_srag, a_srag, by ="name_health_region")

### modelando

pacman::p_load(INLA)



inla_df_srag <- tibble(data_sintomas = sort(unique(df_summ_srag$data_sintomas))) %>% 
  filter(data_sintomas >= "2020-04-01", data_sintomas < "2022-12-01") %>% 
  rowid_to_column(var = "time")


df_summ_srag <- df_summ_srag %>% 
  left_join(
    poprs.hr,
    by = "name_health_region")

RS.inla.teste_srag <- df_summ_srag %>% 
  filter(data_sintomas >= "2020-03-05", data_sintomas < "2022-12-31") %>% 
  left_join(inla_df_srag) %>%
  left_join(tibble( name_health_region = name_health_region) %>% 
              rowid_to_column(var = "Region")
  ) %>% drop_na()

RS.inla.teste_srag$code_health_region = as.numeric(RS.inla.teste_srag$code_health_region)
RS.inla.teste_srag$Region = as.numeric(RS.inla.teste_srag$Region)

formula.teste = covid ~ 1 + code_health_region + f(time, model = "rw2", 
                                                   group = Region, 
                                                   control.group = list(model = "iid")
)


output.teste <- inla(formula = formula.teste, family = "poisson",
                     data = RS.inla.teste)





tbl.picos_srag <- RS.inla.teste_srag %>% 
  #filter(name_health_region == "10 Capital/Vale Gravataí") %>% 
  group_by(name_health_region, onda.x) %>% 
  summarise(
    #  maxRMI = dt_evento_w[which.max(covid)/],
    dt_pico = data_sintomas[which.max(covid)],
    #DT_pico_2 = paste(epiweek(DT_pico),epiyear(DT_pico), sep = "/")
  ) 


##### Tempo a partir do pico da RMI - hospitalizacoes
tbl.picos_srag <- tbl.picos_srag %>% 
  group_by(onda.x) %>% 
  mutate(
    dt_pico_rmi = dt_pico[name_health_region == "10 Capital/Vale Gravataí"]
  ) %>% ungroup() %>%
  mutate(
    tempo = dt_pico - dt_pico_rmi
  ) 

# GRAFICOOOOOOOOOOOOOS

no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())

hr1_srag = read_health_region(macro = FALSE, year=2013) %>% filter(abbrev_state=="RS")
hr1_srag = hr1_srag[1:30,]

RS.hr_srag = left_join(RS.inla.teste_srag,hr1_srag, by="name_health_region")
nomes = unique(RS.hr_srag$name_health_region)
levels= unique(RS.hr_srag$Region)

data_srag = RS.hr_srag %>% 
  left_join(
    tbl.picos %>% 
      mutate(
        tempo = as.numeric(tempo)/7,
        tempo.d = cut(tempo,
                      breaks = c(-1,0,1,2,3,20),
                      labels = c("-1",
                                 "0",
                                 "1",
                                 "2",
                                 "3 ou mais"),
                      right = F),
        labels = factor(name_health_region, 
                        levels = nomes, 
                        labels = nomes)
      )
  )

data_srag = left_join(data_srag,hr1_srag, by="name_health_region")

ggplot(data_srag,aes(geometry = geom.y, fill=name_health_region)) +
  geom_sf(mapping = aes(fill=tempo.d), 
          size=.25, 
          show.legend = T) + 
  # geom_sf(data = RMI.hachurado, size = 1) +
  scale_fill_brewer(palette = "Blues", drop=F, na.value = "grey50") +
  labs(title="Regiões de saúde no RS" , 
       #subtitle = "Variante predominante: Original B.1.33", 
       size=8, 
       fill = "Semanas após \n pico na Região \n Capital/Vale Gravataí" ) + 
  theme_minimal(base_size = 10) +
  no_axis + 
  theme(legend.position = c(.0000000001,.5) , plot.title = element_text(hjust = 0.5)) + 
  facet_wrap(~onda.x, nrow = 3)

