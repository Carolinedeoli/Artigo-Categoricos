#Artigo de Desempenho e escolaridade mae

#Variáveis para base de dados: 
# Notas
# Escolaridade mae
#renda
#st_conclusao

#===============================================================================
#                 BIBLIOTECAS
#===============================================================================

library(readr)
library(dplyr)
library(tidyr)
library(nortest)
library(scales)
library(ggplot2)
library(knitr)
library(kableExtra)
library(patchwork)
library(patchwork)

#===============================================================================
#                               FILTRAGEM 
#===============================================================================

# enem_2023 <- read_csv("DADOS/ENEM_2023.csv")
# 
# dados_concluintes <- enem_2023 %>%
#   # Primeiro, filtramos as linhas (quem conclui após 2023)
#   filter(TP_ST_CONCLUSAO == 'Estou cursando e concluirei o Ensino Médio em 2023' &
#          IN_TREINEIRO == 'Não') %>%
#   # Depois, selecionamos as colunas desejadas
#   select(desempenho,
#          TP_SEXO,
#          TP_FAIXA_ETARIA,
#          TP_DEPENDENCIA_ADM_ESC,
#          TP_ESCOLA,
#          NU_NOTA_CN,
#          NU_NOTA_CH,
#          NU_NOTA_LC,
#          NU_NOTA_MT,
#          NU_NOTA_REDACAO,
#          escolaridade_mae,
#          renda_familiar)

#write.csv(dados_concluintes, "CONCLUINTES_2023.csv", row.names = FALSE)
dados_concluintes <- read_csv("Artigo escolaridade desempenho/CONCLUINTES_2023.csv")
tabela_cruzada <- read_csv("Artigo escolaridade desempenho/tabela_cruzada.csv")
dim(dados_concluintes)
# # 1. Calculamos os pontos de corte (vetor com Mínimo, Q1, Mediana, Q3 e Máximo)
# cortes <- quantile(dados_concluintes$desempenho, 
#                    probs = c(0, 0.25, 0.5, 0.75, 1), 
#                    na.rm = TRUE)
# 
# # 2. Criamos a variável categorizando os alunos nesses intervalos
# dados_concluintes <- dados_concluintes %>%
#   mutate(setor_desempenho = cut(desempenho, 
#                                 breaks = cortes, 
#                                 include.lowest = TRUE,
#                                 right = FALSE,
#                                 labels = c("Baixo (até Q1)", 
#                                            "Médio-Baixo (Q1 a Mediana)", 
#                                            "Médio-Alto (Mediana a Q3)", 
#                                            "Alto (acima de Q3)")))
# 

##################################################################
#Tabela de Notas Resumo
##################################################################

tabela_concluintes_notas <- dados_concluintes %>%
  summarise(across(
    # Adicionamos 'desempenho' junto com as colunas que começam com NU_NOTA_
    c(desempenho, starts_with("NU_NOTA_")), 
    list(
      N = ~sum(!is.na(.x)), # Conta quantos valores não nulos existem
      Média = ~mean(.x, na.rm = TRUE),
      DP = ~sd(.x, na.rm = TRUE),
      Mín = ~min(.x, na.rm = TRUE),
      Q1 = ~quantile(.x, 0.25, na.rm = TRUE), # Primeiro Quartil
      Mediana = ~median(.x, na.rm = TRUE),
      Q3 = ~quantile(.x, 0.75, na.rm = TRUE), # Terceiro Quartil
      Máx = ~max(.x, na.rm = TRUE)
    ),
    .names = "{.col}###{.fn}"
  )) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Materia", "Estatistica"),
    names_sep = "###"
  ) %>%
  pivot_wider(
    names_from = Estatistica,
    values_from = value
  ) %>%
  mutate(
    # Removemos o prefixo apenas se ele existir
    Materia = gsub("NU_NOTA_", "", Materia),
    Materia = recode(Materia,
                     "desempenho" = "Média Geral (Desempenho)",
                     "CN" = "Ciências da Natureza",
                     "CH" = "Ciências Humanas",
                     "LC" = "Linguagens e Códigos",
                     "MT" = "Matemática",
                     "REDACAO" = "Nota Total da Redação")
  )

# 2. Gerando a saída visual com kable
tabela_concluintes_notas %>%
  knitr::kable(
    digits = 2,
    format.args = list(big.mark = ".", decimal.mark = ","),
    align = "lrrrrrrrr",
    caption = "Estatísticas Descritivas Detalhadas - Concluintes ENEM 2023",
    col.names = c("Área de Conhecimento", "N", "Média", "DP", "Mínimo", "Q1", "Mediana", "Q3", "Máximo")
  ) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE
  )

##################################################################
#            ESTATÍSTICAS ESCOLARIDADE POR DESEMPENHO 
##################################################################

tabela_resumo_profissional <- function(dados, var_cat, var_num, titulo = "") {
  
  library(dplyr)
  library(knitr)
  library(kableExtra)
  
  resumo <- dados %>%
    group_by(categoria = .data[[var_cat]]) %>%   # 🔥 nome limpo
    summarise(
      N = n(),
      Média = mean(.data[[var_num]], na.rm = TRUE),
      DP = sd(.data[[var_num]], na.rm = TRUE),
      Mín = min(.data[[var_num]], na.rm = TRUE),
      Q1 = quantile(.data[[var_num]], 0.25, na.rm = TRUE),
      Mediana = median(.data[[var_num]], na.rm = TRUE),
      Q3 = quantile(.data[[var_num]], 0.75, na.rm = TRUE),
      Máx = max(.data[[var_num]], na.rm = TRUE),
      .groups = "drop"
    )
  
  resumo %>%
    kable(
      digits = 2, 
      format.args = list(big.mark = ".", decimal.mark = ","),
      caption = titulo,
      booktabs = TRUE,
      align = "lrrrrrrrr"   # 🔥 alinhamento correto
    ) %>%
    kable_styling(
      latex_options = c("striped", "hold_position", "scale_down"),
      full_width = FALSE,
      font_size = 8
    ) %>%
    column_spec(1, width = "5cm")   # 🔥 controla texto
}

# Exemplo por Sexo
tabela_resumo_profissional(dados_concluintes, "escolaridade_mae", "desempenho", "Resumo do Desempenho por Escolaridade da Mãe")


##################################################################
#  Descritivas Concluintes
##################################################################



#=================================================================
#  Gráfico de Barras do Perfil
#=================================================================
table(dados_concluintes$TP_ESCOLA)
table(dados_concluintes$TP_DEPENDENCIA_ADM_ESC)
table(dados_concluintes$TP_FAIXA_ETARIA)


# Criando as novas categorias
dados_concluintes <- dados_concluintes %>%
  mutate(faixa_etaria_agrupada = case_when(
    TP_FAIXA_ETARIA %in% c("Menor de 17 anos") ~ "Menor de 17",
    TP_FAIXA_ETARIA == "17 anos" ~ "17 anos",
    TP_FAIXA_ETARIA == "18 anos" ~ "18 anos",
    TP_FAIXA_ETARIA == "19 anos" ~ "19 anos",
    TP_FAIXA_ETARIA == "20 anos" ~ "20 anos",
    TRUE ~ "Acima de 20 anos" # Agrupa todo o restante
  )) %>%
  # Definindo a ordem exata para o gráfico
  mutate(faixa_etaria_agrupada = factor(faixa_etaria_agrupada, 
                                        levels = c("Acima de 20 anos", "20 anos", "19 anos", 
                                                   "18 anos", "17 anos", "Menor de 17")))
grafico_barra_ordenado <- function(data, var, titulo){
  
  df_counts <- data %>%
    filter(!is.na(!!sym(var))) %>% 
    count(!!sym(var)) %>%
    mutate(pct = n / sum(n) * 100)
  
  max_n <- max(df_counts$n)
  
  ggplot(df_counts, aes(x = !!sym(var), y = n)) +
    geom_col(fill = "#4C72B0") +
    # Removido o fontface="bold" daqui:
    geom_text(aes(label = sprintf("%.1f%%", pct)), 
              hjust = -0.1, size = 3.8) + 
    coord_flip() +
    scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ","),
                       limits = c(0, max_n * 1.3)) + 
    labs(title = titulo, x = "", y = "Frequência") +
    theme_minimal(base_size = 12) +
    # Garantindo que o título não fique em negrito:
    theme(plot.title = element_text(hjust = 0.5, face = "plain"))
}

# Chamando o gráfico
g1 <- grafico_barra_ordenado(dados_concluintes, "faixa_etaria_agrupada", "Faixa Etária (Agrupada)")
g2 <- grafico_barra_ordenado(dados_concluintes, "TP_DEPENDENCIA_ADM_ESC", "Dependência Administrativa")

# Combinando-os (lado a lado ou um sobre o outro)
g1 + g2
#=================================================================
#  Gráfico de Pizza do Perfil
#=================================================================


{
  grafico_pizza <- function(data, var, titulo, legenda){
    
    df <- data %>%
      filter(!is.na(!!sym(var))) %>%
      count(!!sym(var)) %>%
      mutate(pct = n / sum(n),
             label = sprintf("%.1f%%", pct * 100))
    
    # Títulos de legenda personalizados
    legenda_titulo <- case_when(
      var == "TP_SEXO"   ~ "Sexo",
      var == "TP_ESCOLA" ~ "Tipo de Escola",
      TRUE ~ var
    )
    
    cores_personalizadas <- c("#F2B6C6", "#4C72B0")
    
    ggplot(df, aes(x = "", y = pct, fill = !!sym(var))) +
      geom_col(width = 1, color = "white") +
      geom_text(aes(label = label),
                position = position_stack(vjust = 0.5),
                size = 4) +
      scale_fill_manual(values = cores_personalizadas) +
      coord_polar(theta = "y") +
      labs(title = titulo, fill = legenda) +
      theme_minimal(base_size = 12) +
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank())
  }
  
  
  grafico_pizza(dados_concluintes, "TP_SEXO", "Distribuição por Sexo", "Sexo")
  grafico_pizza(dados_concluintes, "TP_ESCOLA", "Distribuição por Tipo de Escola", "Tipo de Escola")
}


#=================================================================
#  Histograma Notas
#=================================================================


{
  grafico_histograma <- function(data, var, titulo){
    ggplot(data, aes(x = !!sym(var))) +
      geom_histogram(bins = 30, fill = "#4C72B0", color = "white") +
      labs(title = titulo, x = "Nota", y = "Frequência") +
      theme_minimal(base_size = 12)
  }}
  
 g1 <-  grafico_histograma(dados_concluintes, "NU_NOTA_CN", "Distribuição da Nota de Ciências da Natureza")
 g3 <-  grafico_histograma(dados_concluintes, "NU_NOTA_CH", "Distribuição da Nota de Ciências Humanas")
 g4 <-  grafico_histograma(dados_concluintes, "NU_NOTA_LC", "Distribuição da Nota de Linguagens")
 g2 <-  grafico_histograma(dados_concluintes, "NU_NOTA_MT", "Distribuição da Nota de Matemática")
  grafico_histograma(dados_concluintes, "desempenho", "Distribuição da Média Geral (Desempenho)")
  grafico_histograma(dados_concluintes, "NU_NOTA_REDACAO", "Distribuição Notas da Redação")

g1+g2
g3+g4
#=================================================================
#  REDAÇÃO
#=================================================================

{
  grafico_comp_redacao <- function(data, var, titulo){
    
    df <- data %>%
      count(!!sym(var)) %>%
      mutate(pct = n / sum(n) * 100)
    
    ggplot(df, aes(x = as.numeric(as.character(!!sym(var))), y = pct)) +
      geom_col(fill = "#4C72B0") +
      geom_text(aes(label = sprintf("%.1f%%", pct)),
                vjust = -0.3, size = 3.5) +
      # Configurando o pulo de 100 em 100 no eixo X
      scale_x_continuous(breaks = seq(0, 1000, by = 100)) +
      labs(title = titulo, 
           x = "Nota", 
           y = "Porcentagem (%)") +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(hjust = 0.5, face = "plain"))
  }
  
  
  grafico_comp_redacao(dados_concluintes, "NU_NOTA_REDACAO", "Distribuição Notas Redação")
}

{
  grafico_comp_redacao <- function(data, var, titulo){
    
    df <- data %>%
      count(!!sym(var)) %>%
      mutate(pct = n / sum(n) * 100)
    
    ggplot(data, aes(x = NU_NOTA_REDACAO)) +
      # Criamos o histograma com bordas brancas para separar as barras
      geom_histogram(aes(y = after_stat(count / sum(count) * 100)), 
                     binwidth = 40, # Agrupa as notas de 40 em 40
                     fill = "#4C72B0", 
                     color = "white") +
      # Adicionamos uma linha de densidade para suavizar o desenho
      geom_density(aes(y = after_stat(density) * 40 * 100), 
                   color = "#C44E52", size = 1) +
      scale_x_continuous(breaks = seq(0, 1000, by = 100)) +
      labs(title = "Distribuição das Notas de Redação - ENEM 2023",
           x = "Nota Final",
           y = "Frequência Relativa (%)") +
      theme_minimal() +
      theme(panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5))
  }
  
  
  grafico_comp_redacao(dados_concluintes, "NU_NOTA_REDACAO", "Distribuição Notas Redação")
}

##############################################3
#    DENSIDADE
################################################


plot_densidade <- function(dados, var_cat, var_num){
  
  ggplot(dados, aes(x = .data[[var_num]], color = .data[[var_cat]], fill = .data[[var_cat]])) +
    # Multiplicamos a densidade por 50 para simular uma "janela" de 50 pontos de nota
    geom_density(aes(y = after_stat(density)), linewidth = 1, alpha = 0.05) + 
    
    # Transforma o eixo Y em porcentagem real (ex: 0.1 vira 10%)
    scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
    
    scale_color_discrete(na.translate = FALSE) +
    scale_fill_discrete(na.translate = FALSE) +
    
    labs(
      title = "Distribuição do Desempenho por Escolaridade da Mãe",
      x = "Nota (Desempenho Geral)",
      y = "Proporção de Alunos (%)",
      color = "Escolaridade",
      fill = "Escolaridade"
    ) +
    
    theme_minimal() +
    theme(
      plot.title = element_text(face = "plain", hjust = 0.5),
      plot.subtitle = element_text(size = 9, color = "gray30", hjust = 0.5),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
}

plot_densidade(dados_concluintes, "TP_SEXO", "desempenho")
plot_densidade(dados_concluintes, "escolaridade_mae", "desempenho")

dados_concluintes <- dados_concluintes %>%
  mutate(escolaridade_mae = recode(escolaridade_mae,
                                   "Não completou EF (até 5º ano)" = "Não completou EF (até 5º ano)",
                                   "EF incompleto (até 9º ano)"   = "EF incompleto (até 9º ano)",
                                   "EF completo/Médio incompleto" = "Ensino Fundamental Completo",
                                   "Médio completo/Superior incompleto" = "Ensino Médio Completo",
                                   "Superior completo/Pós incompleta"   = "Ensino Superior Completo",
                                   "Pós-graduação" = "Pós-graduação"
  )) %>%
  # Reordenando para que o gráfico siga a hierarquia correta
  mutate(escolaridade_mae = factor(escolaridade_mae, levels = c(
    "Nunca estudou", "Não completou EF (até 5º ano)", "EF incompleto (até 9º ano)", 
    "Ensino Fundamental Completo", "Ensino Médio Completo", 
    "Ensino Superior Completo", "Pós-graduação"
  )))


###############################################################
##      Desempenho por quartil
###############################################################

table(dados_concluintes$setor_desempenho)

dados_concluintes %>%
  group_by(setor_desempenho) %>%
  summarise(Minimo = sprintf("%.2f", min(desempenho, na.rm = TRUE)),
            Maximo = sprintf("%.2f", max(desempenho, na.rm = TRUE)),
            N = n())


tabela_cruzada <- dados_concluintes %>%
  # Remove as linhas onde a escolaridade da mãe não foi informada
  filter(!is.na(escolaridade_mae)) %>% 
  count(escolaridade_mae, setor_desempenho) %>%
  group_by(escolaridade_mae) %>%
  mutate(porcentagem = n / sum(n) * 100)

# Visualizando com ggplot
ggplot(tabela_cruzada, aes(x = escolaridade_mae, y = porcentagem, fill = setor_desempenho)) +
  geom_col() +
  coord_flip() +
  labs(title = "Distribuição de Quartis por Escolaridade Materna",
       x = "Escolaridade da Mãe",
       y = "Porcentagem do Grupo (%)",
       fill = "Faixa de Desempenho") +
  theme_minimal()

#write.csv(tabela_cruzada, "tabela_cruzada.csv", row.names = FALSE)

sum(tabela_cruzada$n)


################################################################################
###       CORRELAÇÃO
################################################################################

cor(dados_concluintes$desempenho, dados_concluintes$escolaridade_mae)
ggplot(dados_concluintes, aes(x = escolaridade_mae, y = TP_ESCOLA, fill = desempenho)) +
  geom_tile(color = "white") +
  # Transição de cores quentes/frias para representar a magnitude da nota
  scale_fill_distiller(palette = "YlOrRd", direction = 1, name = "Nota Média") +
  geom_text(aes(label = round(desempenho, 1)), color = "black", fontface = "bold") +
  theme_minimal() +
  labs(
    title = "Gráfico de Calor: Desempenho Médio por Escolaridade da Mãe e Escola",
    x = "Escolaridade da Mãe",
    y = "Tipo de Escola"
  ) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
