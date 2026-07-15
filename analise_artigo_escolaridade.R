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

enem_2023 <- read_csv("Artigo-Categoricos/ENEM_2023.csv")

dados_concluintes <- enem_2023 %>%
  # Primeiro, filtramos as linhas
  filter(
    TP_ST_CONCLUSAO == 'Estou cursando e concluirei o Ensino Médio nesse ano' &
      IN_TREINEIRO == 'Não' &
      escolaridade_mae != "Não Informado" &
      !is.na(escolaridade_mae)
  ) %>%
  # Depois, selecionamos as colunas desejadas explicitando o pacote dplyr
  dplyr::select(
    desempenho,
    TP_SEXO,
    TP_FAIXA_ETARIA,
    dep_adm,
    tipo_escola,
    NU_NOTA_CN,
    NU_NOTA_CH,
    NU_NOTA_LC,
    NU_NOTA_MT,
    NU_NOTA_COMP1,
    NU_NOTA_COMP2,
    NU_NOTA_COMP3,
    NU_NOTA_COMP4,
    NU_NOTA_COMP5,
    NU_NOTA_REDACAO,
    escolaridade_mae,
    renda_familiar
  )


write.csv(dados_concluintes, "CONCLUINTES_2023.csv", row.names = FALSE)


dados_concluintes <- read_csv("Artigo-Categoricos/CONCLUINTES_2023.csv")

tabela_cruzada <- read_csv("tabela_cruzada.csv")
dim(dados_concluintes)

# 1. Calculamos os pontos de corte (vetor com Mínimo, Q1, Mediana, Q3 e Máximo)
cortes <- quantile(dados_concluintes$desempenho,
                   probs = c(0, 0.25, 0.5, 0.75, 1),
                   na.rm = TRUE)

# 2. Criamos a variável categorizando os alunos nesses intervalos
dados_concluintes <- dados_concluintes %>%
  mutate(setor_desempenho = cut(desempenho,
                                breaks = cortes,
                                include.lowest = TRUE,
                                right = FALSE,
                                labels = c("Baixo (até Q1)",
                                           "Médio-Baixo (Q1 a Mediana)",
                                           "Médio-Alto (Mediana a Q3)",
                                           "Alto (acima de Q3)")))

###############################################################################
##  TABELA CRUZADA 
###############################################################################
tabela_cruzada <- dados_concluintes %>%
  # Remove as linhas onde a escolaridade da mãe não foi informada
  filter(!is.na(escolaridade_mae)) %>%
  count(escolaridade_mae, setor_desempenho) %>%
  group_by(escolaridade_mae) %>%
  mutate(porcentagem = n / sum(n) * 100)

sum(tabela_cruzada$n)
write.csv(tabela_cruzada, "tabela_cruzada.csv", row.names = FALSE)


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

tabela_concluintes_notas %>%
  knitr::kable(
    format = "latex",        # 1. Força a saída para ser código LaTeX
    booktabs = TRUE,         # 2. Deixa a tabela com estilo acadêmico profissional
    digits = 2,
    format.args = list(big.mark = ".", decimal.mark = ","),
    align = "lrrrrrrrr",
    caption = "Estatísticas Descritivas Detalhadas - Concluintes ENEM 2023",
    col.names = c("Área de Conhecimento", "N", "Média", "DP", "Mínimo", "Q1", "Mediana", "Q3", "Máximo")
  ) %>%
  kableExtra::kable_styling(
    latex_options = c("striped", "hold_position"), # 3. Estilo específico para LaTeX
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
    group_by(Categoria = .data[[var_cat]]) %>%   # Deixei "Categoria" com 'C' maiúsculo para ficar bonito na tabela
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
      format = "latex", # 🔥 FORÇA A SAÍDA EM CÓDIGO LATEX AQUI
      digits = 2, 
      format.args = list(big.mark = ".", decimal.mark = ","),
      caption = titulo,
      booktabs = TRUE,
      align = "lrrrrrrrr"   
    ) %>%
    kable_styling(
      latex_options = c("striped", "hold_position", "scale_down"),
      full_width = FALSE,
      font_size = 8
    ) %>%
    column_spec(1, width = "5cm")   
}

# Ao rodar isso, o R vai imprimir o bloco \begin{table} ... \end{table} no console!
tabela_resumo_profissional(
  dados_concluintes, 
  "escolaridade_mae", 
  "desempenho", 
  "Resumo do Desempenho por Escolaridade da Mãe"
)

##################################################################
#  Descritivas Concluintes
##################################################################



#=================================================================
#  Gráfico de Barras do Perfil
#=================================================================
table(dados_concluintes$tipo_escola)
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
g2 <- grafico_barra_ordenado(dados_concluintes, "dep_adm", "Dependência Administrativa")

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
      var == "tipo_escola" ~ "Tipo de Escola",
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
  grafico_pizza(dados_concluintes, "tipo_escola", "Distribuição por Tipo de Escola", "Tipo de Escola")
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
g4+g3
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


# Visualizando com ggplot
ggplot(tabela_cruzada, aes(x = escolaridade_mae, y = porcentagem, fill = setor_desempenho)) +
  geom_col() +
  coord_flip() +
  labs(title = "Distribuição de Quartis por Escolaridade Materna",
       x = "Escolaridade da Mãe",
       y = "Porcentagem do Grupo (%)",
       fill = "Faixa de Desempenho") +
  theme_minimal()

#==================================================================================
# 1. Criando um dataframe de exemplo (substitua pelo seu 'df' dos Microdados)
df_notas <- dados_concluintes %>% select(NU_NOTA_CN, NU_NOTA_CH, NU_NOTA_LC, NU_NOTA_MT, NU_NOTA_REDACAO)

df_notas <- data.frame(
  "Ciências da Natureza" = dados_concluintes$NU_NOTA_CN,
  "Ciências Humanas"     = dados_concluintes$NU_NOTA_CH,
  "Linguagens e Códigos" = dados_concluintes$NU_NOTA_LC,
  "Matemática"           = dados_concluintes$NU_NOTA_MT
  #"Redação"              = dados_concluintes$NU_NOTA_REDACAO
)

# 2. Transformando os dados para o formato longo (long format)
df_long <- df_notas %>%
  pivot_longer(cols = everything(), names_to = "Prova", values_to = "Nota")


#-------------
library(ggplot2)
library(dplyr)
library(scales)

plot_densidade_long <- function(dados, var_cat = "Prova", var_num = "Nota", titulo) {
  
  # Filtra para remover a Redação caso ela ainda esteja no dataset
  dados_filtrados <- dados %>% 
    filter(.data[[var_cat]] != "Redação")
  
  ggplot(dados_filtrados, aes(x = .data[[var_num]], color = .data[[var_cat]], fill = .data[[var_cat]])) +
    
    # Preenchimento cinza sutil ao fundo de todas as curvas para manter a estética de base unificada
    geom_density(aes(fill = "Fundo"), color = NA, fill = "#EAEAEA", alpha = 0.5) +
    
    # As linhas de densidade com preenchimento quase transparente (alpha = 0.05)
    geom_density(aes(y = after_stat(density)), linewidth = 1, alpha = 0.05, show.legend = TRUE) + 
    
    # Limita e ajusta o eixo X para a escala clássica do ENEM (0 a 1000)
    scale_x_continuous(
      limits = c(0, 1000), 
      breaks = seq(0, 1000, 200)
    ) +
    
    # Transforma o eixo Y em porcentagem real (ex: 0.1 vira 10%)
    scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
    
    # Paleta discreta de cores (scale_color_manual para as linhas)
    # E removemos categorias nulas (na.translate = FALSE)
    scale_color_discrete(na.translate = FALSE) +
    scale_fill_discrete(na.translate = FALSE) +
    
    labs(
      title = titulo,
      x = "Nota (Desempenho Geral)",
      y = "Proporção de Alunos (%)",
      color = "Provas",
      fill = "Provas"
    ) +
    
    theme_minimal() +
    theme(
      # Título centralizado e limpo
      plot.title = element_text(face = "bold", hjust = 0.5, size = 14, margin = margin(b = 15)),
      plot.subtitle = element_text(size = 9, color = "gray30", hjust = 0.5),
      
      # Gridlines sutis
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#EFEFEF"),
      
      # Legenda elegante na parte inferior
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      legend.key = element_blank()
    ) +
    # Força a legenda a mostrar contornos coloridos com preenchimento limpo
    guides(
      fill = "none",
      color = guide_legend(override.aes = list(fill = "white", size = 1.5))
    )
}

# Supondo que sua tabela longa se chama 'df_long'
# e as colunas sejam exatamente 'Prova' e 'Nota':

plot_densidade_long(
  dados = df_long, 
  var_cat = "Prova", 
  var_num = "Nota", 
  titulo = "Distribuição de Desempenho no ENEM"
)
#-------------
#===============================================================================

#============================================================================

plot_competencias_bar <- function(dados, var_cat = "Competencia", var_num = "Nota", titulo) {
  
  # 1. Filtra para garantir que usaremos apenas as competências da redação
  # (Ajuste os nomes dos filtros caso na sua base estejam diferentes, ex: 'COMP1', 'COMP2')
  dados_filtrados <- dados %>% 
    filter(grepl("COMP|Competência|Competencia", .data[[var_cat]], ignore.case = TRUE)) %>% 
    # Garante que a nota seja tratada como fator para gerar as barras discretas (0, 40, 80, 120, 160, 200)
    mutate(!!sym(var_num) := as.factor(.data[[var_num]]))
  
  ggplot(dados_filtrados, aes(x = .data[[var_num]], fill = .data[[var_cat]])) +
    
    # Gráfico de barras com posição lado a lado ('dodge') e opacidade sutil
    geom_bar(aes(y = after_stat(prop), group = .data[[var_cat]]), 
             position = position_dodge(width = 0.8), 
             width = 0.7, 
             alpha = 0.85,
             color = NA) +
    
    # Transforma o eixo Y em porcentagem real
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    
    # Paleta de cores menos saturadas (tons suaves semelhantes à imagem de referência)
    scale_fill_manual(
      values = c(
        "#E08A8A", # Vermelho pastel sutil
        "#B2B2D8", # Roxo/Azul acinzentado pastel
        "#66C2A5", # Verde menta pastel
        "#4EAED6", # Azul piscina suave
        "#E293E2"  # Rosa/Magenta queimado
      ),
      na.translate = FALSE
    ) +
    
    labs(
      title = titulo,
      x = "Nota da Competência",
      y = "Proporção de Alunos (%)",
      fill = "Competências"
    ) +
    
    theme_minimal() +
    theme(
      # Título centralizado e limpo
      plot.title = element_text(face = "bold", hjust = 0.5, size = 14, margin = margin(b = 15)),
      
      # Gridlines sutis apenas na horizontal para facilitar a leitura das proporções
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#EFEFEF"),
      
      # Legenda elegante na parte inferior com contornos limpos
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      legend.key = element_blank()
    ) +
    # Força a legenda a exibir as caixas de cor perfeitamente combinando com as barras
    guides(
      fill = guide_legend(override.aes = list(alpha = 0.9))
    )
}
#---
# 1. Preparando a base longa específica para as competências
df_comp_long <- dados_concluintes %>%
  select(NU_NOTA_COMP1, NU_NOTA_COMP2, NU_NOTA_COMP3, NU_NOTA_COMP4, NU_NOTA_COMP5) %>%
  # Renomeando para ficar estético na legenda do gráfico
  rename(
    "Competência 1"  = NU_NOTA_COMP1,
    "Competência 2"     = NU_NOTA_COMP2,
    "Competência 3"  = NU_NOTA_COMP3,
    "Competência 4"   = NU_NOTA_COMP4,
    "Competência 5" = NU_NOTA_COMP5
  ) %>%
  pivot_longer(cols = everything(), names_to = "Competência", values_to = "Nota") %>%
  # Filtra possíveis NAs de faltosos na redação
  filter(!is.na(Nota))

# 2. Gerando o gráfico
plot_competencias_bar(
  dados = df_comp_long,
  var_cat = "Competência",
  var_num = "Nota",
  titulo = "Desempenho dos Alunos por Competência da Redação"
)

#===============================================================================

plot_competencias_facet <- function(dados, var_cat = "Competencia", var_num = "Nota", titulo) {
  
  # 1. Filtra para garantir as competências e as transforma em fator
  dados_filtrados <- dados %>% 
    filter(grepl("COMP|Competência|Competencia", .data[[var_cat]], ignore.case = TRUE)) %>% 
    mutate(!!sym(var_num) := as.factor(.data[[var_num]]))
  
  ggplot(dados_filtrados, aes(x = .data[[var_num]], fill = .data[[var_cat]])) +
    
    # Desenha as barras individuais para cada subplot
    geom_bar(aes(y = after_stat(prop), group = .data[[var_cat]]), 
             width = 0.9, 
             alpha = 0.85, 
             color = NA) +
    
    # Divide o gráfico em linhas separadas (uma para cada competência)
    # ncol = 1 coloca um embaixo do outro. Se preferir em colunas, use nrow = 1 ou retire para grid automático
    facet_wrap(vars(.data[[var_cat]]), ncol = 1, strip.position = "right") +
    
    # Transforma o eixo Y em porcentagem real
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    
    # Paleta de cores menos saturadas/pastel
    scale_fill_manual(
      values = c(
        "#E08A8A", # Vermelho sutil
        "#B2B2D8", # Roxo/Azul acinzentado
        "#66C2A5", # Verde menta
        "#4EAED6", # Azul suave
        "#E293E2"  # Rosa queimado
      ),
      na.translate = FALSE
    ) +
    
    labs(
      title = titulo,
      x = "Nota da Competência",
      y = "Proporção de Alunos (%)"
    ) +
    
    theme_minimal() +
    theme(
      # Título centralizado
      plot.title = element_text(face = "bold", hjust = 0.5, size = 14, margin = margin(b = 15)),
      
      # Remove linhas de grade verticais e deixa as horizontais bem sutis
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#EFEFEF"),
      
      # Estilo dos títulos de cada faceta (os "labels" de cada linha)
      strip.text = element_text(face = "bold", size = 10, color = "#444444"),
      strip.background = element_rect(fill = "#F5F5F5", color = NA), # Fundo cinza sutil nas abas
      
      # Oculta a legenda já que o título da faceta já indica qual é a competência
      legend.position = "none", 
      
      # Adiciona um pequeno espaçamento entre cada gráfico linha
      panel.spacing = unit(1.5, "lines")
    )
}

# Gerando os gráficos empilhados
plot_competencias_facet(
  dados = df_comp_long, # Seu dataframe no formato longo
  var_cat = "Competência",
  var_num = "Nota",
  titulo = "Distribuição de Desempenho por Competência da Redação"
)

#===============================================================================


table(dados_filtrado_2023$NU_NOTA_COMP1)
table(dados_filtrado_2023$NU_NOTA_COMP2)
table(dados_filtrado_2023$NU_NOTA_COMP3)
table(dados_filtrado_2023$NU_NOTA_COMP4)
table(dados_filtrado_2023$NU_NOTA_COMP5)

#=====================================================================================


