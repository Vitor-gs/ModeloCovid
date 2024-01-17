##################################################################################
#                  INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS              #
##################################################################################
#Pacotes utilizados
pacotes <- c("plotly","tidyverse","knitr","kableExtra","fastDummies","rgl","car",
             "reshape2","jtools","stargazer","lmtest","caret","pROC","ROCR","nnet",
             "magick","cowplot","globals","haven", "data.table")

options(rgl.debug = TRUE)

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# Carrega dados
df21 <- fread("INFLUD21-01-05-2023.csv", select = c(
    "DT_NOTIFIC", "CS_SEXO", "DT_NASC", "CS_GESTANT", "CS_RACA","SG_UF", 
    "NOSOCOMIAL", "PUERPERA", "CARDIOPATI", "HEMATOLOGI", "SIND_DOWN", 
    "HEPATICA", "ASMA", "DIABETES", "NEUROLOGIC", "PNEUMOPATI", "IMUNODEPRE", 
    "RENAL", "OBESIDADE", "VACINA_COV", "DOSE_1_COV", "DOSE_2_COV", "DOSE_REF", 
    "DOSE_2REF",  "LOTE_1_COV", "LOTE_2_COV", "LOTE_REF", "LOTE_REF2", 
    "SUPORT_VEN", "CLASSI_FIN", "EVOLUCAO"
))
df22 <- fread("INFLUD22-03-04-2023.csv", select = c(
  "DT_NOTIFIC", "CS_SEXO", "DT_NASC", "CS_GESTANT", "CS_RACA","SG_UF", 
  "NOSOCOMIAL", "PUERPERA", "CARDIOPATI", "HEMATOLOGI", "SIND_DOWN", 
  "HEPATICA", "ASMA", "DIABETES", "NEUROLOGIC", "PNEUMOPATI", "IMUNODEPRE", 
  "RENAL", "OBESIDADE", "VACINA_COV", "DOSE_1_COV", "DOSE_2_COV", "DOSE_REF", 
  "DOSE_2REF",  "LOTE_1_COV", "LOTE_2_COV", "LOTE_REF", "LOTE_REF2", 
  "SUPORT_VEN", "CLASSI_FIN", "EVOLUCAO"
))
df23 <- fread("INFLUD23-16-10-2023.csv", select = c(
  "DT_NOTIFIC", "CS_SEXO", "DT_NASC", "CS_GESTANT", "CS_RACA","SG_UF", 
  "NOSOCOMIAL", "PUERPERA", "CARDIOPATI", "HEMATOLOGI", "SIND_DOWN", 
  "HEPATICA", "ASMA", "DIABETES", "NEUROLOGIC", "PNEUMOPATI", "IMUNODEPRE", 
  "RENAL", "OBESIDADE", "VACINA_COV", "DOSE_1_COV", "DOSE_2_COV", "DOSE_REF", 
  "DOSE_2REF",  "LOTE_1_COV", "LOTE_2_COV", "LOTE_REF", "LOTE_REF2", 
  "SUPORT_VEN", "CLASSI_FIN", "EVOLUCAO"
))
df <- rbind(df21, df22, df23, fill=TRUE)


# Data Wrangling

## Remover duplicatas, filtrar por casos de covid e padronizar nulos
df_sem_dupl <- df %>% distinct()
df_covid_all <- df_sem_dupl %>% filter(CLASSI_FIN == 5)
df_covid_all[df_covid_all == ""] <- NA

## Apenas casos a partir de novembro de 2021 (70% da população vacinada)
df_covid_all$DT_NOTIFIC <- as.Date(df_covid_all$DT_NOTIFIC, format = "%d/%m/%Y")
df_covid <- df_covid_all %>% filter(DT_NOTIFIC >= "2021-11-01")

## Formatação das colunas
df_covid$DT_NASC <- as.Date(df_covid$DT_NASC, format = "%d/%m/%Y")
df_covid$DOSE_1_COV <- as.Date(df_covid$DOSE_1_COV, format = "%d/%m/%Y")
df_covid$DOSE_2_COV <- as.Date(df_covid$DOSE_2_COV, format = "%d/%m/%Y")
df_covid$DOSE_REF <- as.Date(df_covid$DOSE_REF, format = "%d/%m/%Y")
df_covid$DOSE_2REF <- as.Date(df_covid$DOSE_2REF, format = "%d/%m/%Y")
df_covid <- df_covid %>% mutate(
  CLASSI_FIN = NULL,
  IS_GESTANT = case_when(CS_GESTANT %in% c(1, 2, 3, 4) ~ 1, .default = 0),
  CS_GESTANT = NULL,
  IS_NOSOCOMIAL = case_when(NOSOCOMIAL == 1 ~ 1, .default = 0),
  NOSOCOMIAL = NULL,
  IS_PUERPERA = case_when(PUERPERA == 1 ~ 1, .default = 0),
  PUERPERA = NULL,
  IS_CARDIOPATI = case_when(CARDIOPATI == 1 ~ 1, .default = 0),
  CARDIOPATI = NULL,
  IS_HEMATOLOGIL = case_when(HEMATOLOGI == 1 ~ 1, .default = 0),
  HEMATOLOGI = NULL,
  IS_SIND_DOWN = case_when(SIND_DOWN == 1 ~ 1, .default = 0),
  SIND_DOWN = NULL,
  IS_HEPATICA = case_when(HEPATICA == 1 ~ 1, .default = 0),
  HEPATICA = NULL,
  IS_ASMA = case_when(ASMA == 1 ~ 1, .default = 0),
  ASMA = NULL,
  IS_DIABETES = case_when(DIABETES == 1 ~ 1, .default = 0),
  DIABETES = NULL,
  IS_NEUROLOGIC = case_when(NEUROLOGIC == 1 ~ 1, .default = 0),
  NEUROLOGIC = NULL,
  IS_PNEUMOPATI = case_when(PNEUMOPATI == 1 ~ 1, .default = 0),
  PNEUMOPATI = NULL,
  IS_IMUNODEPRE = case_when(IMUNODEPRE == 1 ~ 1, .default = 0),
  IMUNODEPRE = NULL,
  IS_RENAL = case_when(RENAL == 1 ~ 1, .default = 0),
  RENAL = NULL,
  IS_OBESIDADE = case_when(OBESIDADE == 1 ~ 1, .default = 0),
  OBESIDADE = NULL,
  SEXO = CS_SEXO,
  CS_SEXO = NULL,
  RACA = CS_RACA,
  CS_RACA = NULL,
  UF = SG_UF,
  SG_UF = NULL
)
df_covid$VACINA_COV <- as.factor(df_covid$VACINA_COV)
df_covid$SUPORT_VEN <- as.factor(df_covid$SUPORT_VEN)
df_covid$EVOLUCAO <- as.factor(df_covid$EVOLUCAO)
df_covid$SEXO <- as.factor(df_covid$SEXO)
df_covid$RACA <- as.factor(df_covid$RACA)
df_covid$UF <- as.factor(df_covid$UF)

## Remover dados inconsistentes
df_covid_limpo <- df_covid %>% 
  filter(EVOLUCAO %in% c(1, 2, 3)) %>%
  filter(SUPORT_VEN %in% c(1, 2, 3)) %>% 
  filter(VACINA_COV %in% c(1, 2)) %>% 
  filter(SEXO %in% c("F", "M")) %>% 
  filter(RACA %in% c(1, 2, 3, 4, 5)) %>% 
  filter(!is.na(DT_NASC)) %>% 
  filter(!is.na(UF)) %>%
  filter(!(SEXO == "M" & (IS_GESTANT == 1 | IS_PUERPERA == 1))) %>%
  filter((IS_GESTANT + IS_PUERPERA) < 2) %>%
  filter((
    IS_NOSOCOMIAL + IS_CARDIOPATI + IS_HEMATOLOGIL + IS_SIND_DOWN + 
      IS_HEPATICA + IS_ASMA + IS_DIABETES + IS_NEUROLOGIC + IS_PNEUMOPATI + 
      IS_IMUNODEPRE + IS_RENAL + IS_OBESIDADE) < 9) %>%
  filter(!(VACINA_COV == 1 & 
             is.na(DOSE_1_COV) & is.na(LOTE_1_COV) & 
             is.na(DOSE_2_COV) & is.na(LOTE_2_COV) & 
             is.na(DOSE_REF) & is.na(LOTE_REF) & 
             is.na(DOSE_2REF) & is.na(LOTE_REF2))) %>%
  filter(!(VACINA_COV == 2 & (
    !is.na(DOSE_1_COV) | !is.na(LOTE_1_COV) | 
      !is.na(DOSE_2_COV) | !is.na(LOTE_2_COV) | 
      !is.na(DOSE_REF) | !is.na(LOTE_REF) | 
      !is.na(DOSE_2REF) | !is.na(LOTE_REF2))))

df_covid_limpo$SEXO <- droplevels(df_covid_limpo$SEXO)
df_covid_limpo$RACA <- droplevels(df_covid_limpo$RACA)
df_covid_limpo$SUPORT_VEN <- droplevels(df_covid_limpo$SUPORT_VEN)
df_covid_limpo$EVOLUCAO <- droplevels(df_covid_limpo$EVOLUCAO)
df_covid_limpo$IS_GESTANT <- as.factor(df_covid_limpo$IS_GESTANT)
df_covid_limpo$IS_NOSOCOMIAL <- as.factor(df_covid_limpo$IS_NOSOCOMIAL)
df_covid_limpo$IS_PUERPERA <- as.factor(df_covid_limpo$IS_PUERPERA)
df_covid_limpo$IS_CARDIOPATI <- as.factor(df_covid_limpo$IS_CARDIOPATI)
df_covid_limpo$IS_HEMATOLOGIL <- as.factor(df_covid_limpo$IS_HEMATOLOGIL)
df_covid_limpo$IS_SIND_DOWN <- as.factor(df_covid_limpo$IS_SIND_DOWN)
df_covid_limpo$IS_HEPATICA <- as.factor(df_covid_limpo$IS_HEPATICA)
df_covid_limpo$IS_ASMA <- as.factor(df_covid_limpo$IS_ASMA)
df_covid_limpo$IS_DIABETES <- as.factor(df_covid_limpo$IS_DIABETES)
df_covid_limpo$IS_NEUROLOGIC <- as.factor(df_covid_limpo$IS_NEUROLOGIC)
df_covid_limpo$IS_PNEUMOPATI <- as.factor(df_covid_limpo$IS_PNEUMOPATI)
df_covid_limpo$IS_IMUNODEPRE <- as.factor(df_covid_limpo$IS_IMUNODEPRE)
df_covid_limpo$IS_RENAL <- as.factor(df_covid_limpo$IS_RENAL)
df_covid_limpo$IS_OBESIDADE <- as.factor(df_covid_limpo$IS_OBESIDADE)

## Explicitar raças
df_covid_limpo <- df_covid_limpo %>% mutate(
  RACA = case_when(
    RACA == 1 ~ "Branca",
    RACA == 2 ~ "Preta",
    RACA == 3 ~ "Amarela",
    RACA == 4 ~ "Parda",
    RACA == 5 ~ "Indígena"
    )
)
df_covid_limpo$RACA <- as.factor(df_covid_limpo$RACA)

## Coluna Número de doses recebidas da vacina
df_analise <- df_covid_limpo %>% mutate(
  VACINA_1 = case_when(
    !is.na(DOSE_1_COV) | !is.na(LOTE_1_COV) | 
      !is.na(DOSE_2_COV) | !is.na(LOTE_2_COV) | 
      !is.na(DOSE_REF) | !is.na(LOTE_REF) | 
      !is.na(DOSE_2REF) | !is.na(LOTE_REF2) ~ 1, 
    .default = 0),
  VACINA_2 = case_when(
    !is.na(DOSE_2_COV) | !is.na(LOTE_2_COV) |
      !is.na(DOSE_REF) | !is.na(LOTE_REF) | 
      !is.na(DOSE_2REF) | !is.na(LOTE_REF2) ~ 1, 
    .default = 0),
  VACINA_REF = case_when(
    !is.na(DOSE_REF) | !is.na(LOTE_REF) | 
      !is.na(DOSE_2REF) | !is.na(LOTE_REF2) ~ 1, 
    .default = 0),
  VACINA_2REF = case_when(
    !is.na(DOSE_2REF) | !is.na(LOTE_REF2) ~ 1, 
    .default = 0),
  VACINA_COV = NULL, 
  DOSE_1_COV = NULL, 
  DOSE_2_COV = NULL,
  DOSE_REF = NULL,
  DOSE_2REF = NULL,
  LOTE_1_COV = NULL,
  LOTE_2_COV = NULL,
  LOTE_REF = NULL,
  LOTE_REF2 = NULL,
  VACINA_DOSES = VACINA_1 + VACINA_2 + VACINA_REF + VACINA_2REF,
  VACINA_1 = NULL, 
  VACINA_2 = NULL, 
  VACINA_REF = NULL,
  VACINA_2REF = NULL,
)
df_analise$VACINA_DOSES <- as.factor(df_analise$VACINA_DOSES)

## Coluna Idade - [0, 15) / [15, 30) / [30, 45) / [45, 60) / [60, 75) / [75, +∞)
df_analise <- df_analise %>% mutate(
  IDADE_Y = difftime(DT_NOTIFIC, DT_NASC, units = "days")/365,
  IDADE = case_when(
    IDADE_Y < 15 ~ "[0, 15)",
    IDADE_Y >= 15 & IDADE_Y < 30 ~ "[15, 30)",
    IDADE_Y >= 30 & IDADE_Y < 45 ~ "[30, 45)",
    IDADE_Y >= 45 & IDADE_Y < 60 ~ "[45, 60)",
    IDADE_Y >= 60 & IDADE_Y < 75 ~ "[60, 75)",
    IDADE_Y >= 75 ~ "[75, +∞)"
    ),
  DT_NASC = NULL,
  DT_NOTIFIC = NULL
)
df_analise$IDADE <- as.factor(df_analise$IDADE)
df_analise$IDADE_Y <- as.double(df_analise$IDADE_Y)
df_analise <- df_analise %>% 
  filter(IDADE_Y < 110) %>% 
  mutate(IDADE_Y = NULL)

## Colunas variáveis dependentes
df_analise <- df_analise %>% mutate(
  VENT_INV = ifelse(SUPORT_VEN == 1, 1, 0),
  VENT_NAO_INV = ifelse(SUPORT_VEN == 2, 1, 0),
  SUPORT_VEN = NULL,
  OBITO = ifelse(EVOLUCAO == 2, 1, 0),
  EVOLUCAO = NULL
)
df_analise$VENT_INV <- as.factor(df_analise$VENT_INV)
df_analise$VENT_NAO_INV <- as.factor(df_analise$VENT_NAO_INV)
df_analise$OBITO <- as.factor(df_analise$OBITO)
df_analise <- df_analise %>% relocate(VENT_INV, .after = last_col())
df_analise <- df_analise %>% relocate(VENT_NAO_INV, .after = last_col())
df_analise <- df_analise %>% relocate(OBITO, .after = last_col())

# Salva dados tratados
fwrite(df_analise, "covid_data.csv")
save(df_analise, file = "covid_data.Rdata")

# Visualização da base de dados
summary(df_analise)

df_reduced <- df_analise %>%
  mutate(
    UF = case_when(
      UF == "SP" ~ "SP",
      UF == "MG" ~ "MG",
      UF == "RS" ~ "RS",
      UF == "PR" ~ "PR",
      UF == "SC" ~ "SC",
      UF == "RJ" ~ "RJ",
      UF == "GO" ~ "GO",
      .default = "Outros"
    ),
    IDADE = case_when(
      IDADE == "[0, 15)" ~ "0-15",
      IDADE == "[15, 30)" ~ "15-30",
      IDADE == "[30, 45)" ~ "30-45",
      IDADE == "[45, 60)" ~ "45-60",
      IDADE == "[60, 75)" ~ "60-75",
      .default = "75+"
    ),
    RACA = case_when(
      RACA == "Branca" ~ "Branca",
      RACA == "Parda" ~ "Parda",
      RACA == "Preta" ~ "Preta",
      .default = "I+A"
    ),
  )

df_graph <- df_reduced %>% 
  pivot_longer(cols = everything()) %>% 
  group_by(name, value) %>% 
  summarise(n = n()) %>% 
  mutate(rank = dense_rank(desc(n)))

order_name <- c(
  "OBITO", "VENT_INV", "VENT_NAO_INV", "IDADE", "VACINA_DOSES", "UF", "RACA", 
  "SEXO", "IS_OBESIDADE", "IS_RENAL", "IS_IMUNODEPRE", "IS_PNEUMOPATI", 
  "IS_NEUROLOGIC", "IS_DIABETES", "IS_ASMA", "IS_HEPATICA", "IS_SIND_DOWN",
  "IS_HEMATOLOGIL", "IS_CARDIOPATI", "IS_PUERPERA", "IS_NOSOCOMIAL", "IS_GESTANT")

df_graph %>% 
  ggplot(aes(x = n, y = factor(name, levels = order_name), fill = fct_rev(as.factor(rank)))) +
  geom_col(position = "stack", width = 0.85) +
  scale_x_continuous(limits = c(-1500, 203000), expand = c(0, 0)) + 
  scale_fill_manual(values = c("#FFFFFF", "#F7FBFF", "#DEEBF7", "#C6DBEF", "#9ECAE1", "#6BAED6", "#4292C6", "#2171B5")) +
  labs(x = "Quantidade de pacientes", y = NULL, title = NULL) +
  theme(legend.position = "none") +
  geom_text(aes(label = value), position = position_stack(vjust = 0.5)) + 
  geom_hline(yintercept=3.5, linetype="dashed", color = "red", size=0.85)
