
# Packages ----------------------------------------------------------------


library(tidyverse)
library(lubridate)
library(janitor)
library(gt)
library(gtExtras)


# Data read online SES- DF  -----------------------------------------------

dados_brutos <- read.csv("https://covid19.ssp.df.gov.br/resources/dados/dados-abertos.csv",
                         encoding = 'UTF-8',
                         sep = ';',
                         stringsAsFactors = F,
                         na.strings = c('', "")) %>% 
  clean_names()


# Pre-processing  ---------------------------------------------------------

#adjust date time

dados <- dados_brutos %>% 
  mutate(data_cadastro = as.Date(data_cadastro, '%d/%m/%Y'),
         data_do_obito = as.Date(data_do_obito, '%d/%m/%Y'),
         data_primeirosintomas = as.Date(data_primeirosintomas, '%d/%m/%Y')) %>% 
  mutate_if(is.character, .funs = factor) %>%
  select(data_cadastro, data_primeirosintomas, data_do_obito,-x_u_feff_data, everything())


# Order age levels

dados <- dados %>%
  mutate(faixa_etaria = factor(
    faixa_etaria,
    levels = c('<= 19 anos',
               '20 a 29 anos',
               '30 a 39 anos',
               '40 a 49 anos',
               '50 a 59 anos',
               '>= 60 anos')))

# Recode disease variables 
dados <- dados %>% 
  mutate(pneumopatia = str_replace(pneumopatia, 'Sim', 'pneumopatia'),
         nefropatia = str_replace(nefropatia, 'Sim', 'nefropatia'),
         doenca_hematologica = str_replace(doenca_hematologica, 'Sim', 'doenca_hematologica'),
         disturbios_metabolicos = str_replace(disturbios_metabolicos, 'Sim', 'disturbios_metabolicos'),
         imunopressao = str_replace(imunopressao, 'Sim', 'imunopressao'),
         obesidade = str_replace(obesidade, 'Sim', 'obesidade'),
         outros = str_replace(outros, 'Sim', 'outros'),
         cardiovasculopatia = str_replace(cardiovasculopatia, 'Sim', 'cardiovasculopatia'))


# Explore data  --------------------------------------------------------


# Prevalence of mortality COVID-2019
dados %>% 
  tabyl(obito, sexo) %>% 
  adorn_percentages(denominator = 'col') %>% 
  adorn_pct_formatting() %>% 
  gt() %>% 
  tab_header(title = 'Percentual de Mortes por Covid-19', 
             subtitle = 'Proporção de mortes por sexo') %>%
  cols_label(obito = md('**Óbitos**')) %>% 
  cols_label(Feminino = md('**Feminino**')) %>%
  cols_label(Masculino = md('**Masculino**')) %>% 
  tab_options(table.align = 'center')


# Mortality each age levels
dados %>%
  tabyl(faixa_etaria, obito) %>% 
  adorn_percentages() %>% 
  adorn_pct_formatting()


#Graphic of mortality prevalence

dados %>% group_by(data_cadastro, obito) %>% 
  summarise(total = n()) %>% 
  mutate(mes = month(data_cadastro, label = T, abbr = T),
         ano = year(data_cadastro)) %>%
  na.omit() %>% 
  ggplot(aes(data_cadastro, total))+
  geom_col(color = 'grey', alpha = 0.5)+
  geom_smooth(method = 'loess')+
  scale_x_date(date_breaks = '1 month', date_labels = '%m-%y')+
  theme_classic()

# Grafico de letalidade em relação ao numero de casos

dados %>% select(-x_u_feff_data) %>%  
  group_by(data_cadastro) %>%
  filter(obito == 'Sim') %>% 
  summarise(across(where(is.factor), ~sum(!is.na(.x)))) %>% 
  pivot_longer(cols = c('pneumopatia', 'nefropatia','doenca_hematologica', 'disturbios_metabolicos',
                        'imunopressao',"obesidade", 'outros', 'cardiovasculopatia'),
               names_to = 'doencas',
               values_to = 'contagem') %>% 
  summarise(data_cadastro, 
            doencas)



dados %>% glimpse()
dados %>% colnames()
