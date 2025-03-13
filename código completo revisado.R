# Carregar pacotes necessários
library(readxl)
library(dplyr)
library(readr)
library(writexl)

# Ler os dados das planilhas
domicilio22 <- read_xlsx("domicilio 2022.xlsx", skip = 1)
domicilio9110 <- read_xlsx("domicilio1991e2010.xlsx", skip = 1)

# Ajustar os nomes das colunas para os dados de 1991 a 2010
colnames(domicilio9110) <- c("UF_Municipio", "Total_1991", "Urbana_1991", "Rural_1991",
                             "Total_2000", "Urbana_2000", "Rural_2000", 
                             "Total_2010", "Urbana_2010", "Rural_2010")

# Ajustar os nomes das colunas para os dados de 2022
colnames(domicilio22) <- c("UF_Municipio", "Total_2022", "Urbana_2022", "Rural_2022")

# Função para converter colunas numéricas removendo caracteres inválidos
convert_numeric <- function(x) {
  as.numeric(gsub("[^0-9.]", "", as.character(x))) 
}

# Aplicar conversão para as colunas numéricas
domicilio9110 <- domicilio9110 %>%
  mutate(across(where(is.character), as.character)) %>%
  mutate(across(-UF_Municipio, convert_numeric))

domicilio22 <- domicilio22 %>%
  mutate(across(where(is.character), as.character)) %>%
  mutate(across(-UF_Municipio, convert_numeric))

# Unir as bases de dados pelos municípios
dados <- left_join(domicilio9110, domicilio22, by = "UF_Municipio")

# Função para calcular a razão de crescimento
calcular_razao <- function(pop_final, pop_inicial, anos) {
  ifelse(pop_final > 0 & pop_inicial > 0, log(pop_final / pop_inicial) / anos, NA)
}

# Cálculo das razões de crescimento
dados <- dados %>%
  mutate(
    # Razão de crescimento de 1991 a 2000
    Razao_Cresc_Urbano_9100 = calcular_razao(Urbana_2000, Urbana_1991, 9),
    Razao_Cresc_Rural_9100 = calcular_razao(Rural_2000, Rural_1991, 9),
    Diferenca_9100 = Razao_Cresc_Urbano_9100 - Razao_Cresc_Rural_9100,
    
    # Razão de crescimento de 2000 a 2010
    Razao_Cresc_Urbano_0010 = calcular_razao(Urbana_2010, Urbana_2000, 10),
    Razao_Cresc_Rural_0010 = calcular_razao(Rural_2010, Rural_2000, 10),
    Diferenca_0010 = Razao_Cresc_Urbano_0010 - Razao_Cresc_Rural_0010,
    
    # Razão de crescimento de 2010 a 2022
    Razao_Cresc_Urbano_1022 = calcular_razao(Urbana_2022, Urbana_2010, 12),
    Razao_Cresc_Rural_1022 = calcular_razao(Rural_2022, Rural_2010, 12),
    Diferenca_1022 = Razao_Cresc_Urbano_1022 - Razao_Cresc_Rural_1022
  )

# Definir valores ausentes para zero na diferença de crescimento
dados <- dados %>%
  mutate(Diferenca_1022 = ifelse(is.na(Diferenca_1022), 0, Diferenca_1022))

# Função para estimar a população urbana
calcular_pop_urbana <- function(ano, diferenca, pop_rural, pop_total, pop_urbana) {
  ifelse(!is.na(diferenca) & pop_total > 0 & pop_urbana > 0, 
         pop_urbana * exp(diferenca * (ano - as.numeric(substr(names(pop_urbana), 9, 12)))), 
         NA)
}

# Primeiro, calcular a população urbana estimada para cada período
dados_estimados <- dados %>%
  mutate(
    # População urbana de 1992 a 1999 (com base em 1991-2000)
    across(
      as.character(1992:1999), 
      ~ round(calcular_pop_urbana(.x, Diferenca_9100, Rural_1991, Total_1991, Urbana_1991)), 
      .names = "Pop_Estimada_Urbana_{.col}"
    ),
    
    # População urbana de 2000 a 2009 (com base em 2000-2010)
    across(
      as.character(2000:2009), 
      ~ round(calcular_pop_urbana(.x, Diferenca_9100, Rural_2000, Total_2000, Urbana_2000)), 
      .names = "Pop_Estimada_Urbana_{.col}"
    ),
    
    # População urbana de 2010 a 2021 (com base em 2010-2022)
    across(as.character(2010:2021), 
           ~ round(ifelse(is.na(Rural_2010), .x, calcular_pop_urbana(.x, Diferenca_0010, Rural_2010, Total_2010, Urbana_2010))), 
           .names = "Pop_Estimada_Urbana_{.col}"),
    
    # População urbana de 2022 a 2024 (com base em 2022)
    across(as.character(2022:2024), 
           ~ round(ifelse(is.na(Rural_2022), .x, calcular_pop_urbana(.x, Diferenca_1022, Rural_2022, Total_2022, Urbana_2022))), 
           .names = "Pop_Estimada_Urbana_{.col}")
  )

# Agora calcular a população rural SEPARADAMENTE
dados_estimados <- dados_estimados %>%
  mutate(
    across(as.character(1992:2024), 
           ~ .x - dados_estimados[[paste0("Pop_Estimada_Urbana_", cur_column())]], 
           .names = "Pop_Estimada_Rural_{.col}")
  )

# Salvar os dados organizados
write_xlsx(dados_estimados, "populacao_estimada_urbana_rural.xlsx")
