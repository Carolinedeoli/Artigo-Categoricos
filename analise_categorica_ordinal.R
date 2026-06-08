###############################################################################

### APLICAÇÃO DE TESTES PARA DADOS CATEGÓRICOS ORDINAIS

###############################################################################

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
library(vcdExtra)
library(DescTools)
library(vcd)
#===============================================================================
#                 DADOS
#===============================================================================

dados_concluintes <- read_csv("Artigo escolaridade desempenho/CONCLUINTES_2023.csv")
tabela_cruzada <- read_csv("Artigo escolaridade desempenho/tabela_cruzada.csv")

# 1. Garantir a ordem lógica/ordinal das categorias
tabela_cruzada$escolaridade_mae <- factor(
  tabela_cruzada$escolaridade_mae, 
  levels = c("Nunca estudou","Não completou EF (até 5º ano)", "EF incompleto (até 9º ano)", "EF completo", "Médio completo", "Superior completo","Pós-graduação"), 
  ordered = TRUE
)

tabela_cruzada$setor_desempenho <- factor(
  tabela_cruzada$setor_desempenho, 
  levels = c("Baixo (até Q1)", "Médio-Baixo (Q1 a Mediana)", "Médio-Alto (Mediana a Q3)", "Alto (acima de Q3)"), 
  ordered = TRUE
)

# 2. Criar a tabela (o seu código)
tab <- xtabs(
  n ~ escolaridade_mae + setor_desempenho,
  data = tabela_cruzada
)

# 3. Visualizar para conferir
print(tab)

#===============================================================================
#  TESTE DE INDEPENDÊNCIA QUI-QUADRADO DE PEARSON (GERAL/NOMINAL)
#===============================================================================
# H0: A escolaridade da mãe e o setor de desempenho são independentes.
# H1: A escolaridade da mãe e o setor de desempenho são dependentes.

teste_independencia <- chisq.test(tab)
print(teste_independencia)

# CONCLUSÃO: p-valor < 2.2e-16 (p < 0.05). Rejeita-se H0.
# As variáveis são estatisticamente dependentes; o perfil de desempenho varia 
# de acordo com a escolaridade da mãe.


#===============================================================================
#                 Mantel - Haenszel 
#===============================================================================
# H0: NÃO existe uma tendência linear entre a escolaridade da mãe e o desempenho.
# H1: EXISTE uma tendência linear entre a escolaridade da mãe e o desempenho.

library(vcdExtra)
lbl_teste <- CMHtest(tab)
print(lbl_teste) # Focar na linha 'cor' (Nonzero correlation)

# Extração do p-valor exato da tendência linear
p_valor_linear <- lbl_teste$table["cor", "Prob"]
cat("P-valor exato da tendência linear:", format(p_valor_linear, scientific = TRUE), "\n")

# CONCLUSÃO: Chisq = 109914, Df = 1, p-valor < 0.0001 (indicado como 0e+00). 
# Rejeita-se H0. Há uma tendência linear altamente significativa. À medida que 
# o nível de instrução da mãe se eleva, o desempenho do aluno no ENEM tende 
# a aumentar sistematicamente.


#===============================================================================
#                 MEDIDA DE ASSOCIAÇÃO ORDINAL: GAMA DE GOODMAN E KRUSKAL
#===============================================================================
# H0: A associação ordinal entre as variáveis na população é igual a zero (Gama = 0).
# H1: A associação ordinal na população é diferente de zero (Gama != 0).

gama_res <- GKgamma(tab)
print(gama_res)

GoodmanKruskalGamma(tab)
assocstats(tab)

# CONCLUSÃO: Gama = 0.376 (IC95%: 0.374 a 0.378). Rejeita-se H0 (IC não inclui o 0).
# Indica uma associação positiva moderada. Estudantes com mães de maior 
# escolaridade têm probabilidade substancialmente maior de atingir quartis 
# mais altos de desempenho.

#===============================================================================
#                 Tau-b de Kendall
#===============================================================================

# H0: O coeficiente de correlação de postos Tau-b na população é igual a zero (Tau-b = 0).
# H1: O coeficiente de correlação de postos Tau-b é diferente de zero (Tau-b != 0).

tau_b_res <- KendallTauB(tab, conf.level = 0.95)
print(tau_b_res)

# CONCLUSÃO: Tau-b = 0.287 (IC95%: 0.286 a 0.289). Rejeita-se H0 (IC não inclui o 0).
# Confirma a associação positiva. O valor é numericamente menor que o Gama porque
# o Tau-b ajusta e penaliza a métrica pela grande quantidade de empates (ties) 
# nas categorias, sendo uma estimativa mais conservadora e robusta para este banco.


# ==============================================================================
# SÍNTESE GERAL DOS ACHADOS:
# Todos os testes convergem. A hipótese de que o background educacional materno
# não interfere no resultado do aluno é rejeitada. Há um efeito direto, positivo
# e de magnitude moderada da escolaridade da mãe sobre o desempenho no ENEM.
# ==============================================================================


################################################################################

##          GRÁFICOS

################################################################################

#===============================================================================
#  GRÁFICO DE BARRAS
#===============================================================================

# Convertendo sua tabela xtabs de volta para data.frame para o ggplot
df_plot <- as.data.frame(tab)

ggplot(df_plot, aes(x = escolaridade_mae, y = Freq, fill = setor_desempenho)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Blues", direction = 1) + # Tons de azul para o desempenho
  theme_minimal() +
  labs(
    title = "Tendência de Desempenho no ENEM por Escolaridade Materna",
    x = "Escolaridade da Mãe",
    y = "Percentual (%)",
    fill = "Setor de Desempenho"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotaciona o texto para não encavalar



#===============================================================================
#  GRÁFICO DE MOSAICO
#===============================================================================

# Gráfico de mosaico simples e elegante
mosaicplot(tab, 
           main = "Gráfico de Mosaico: Escolaridade vs Desempenho",
           shade = TRUE, # Colore de acordo com a força da associação (Azul/Vermelho)
           las = 2,  
           
           color = TRUE,
           xlab = "Escolaridade da Mãe",
           ylab = "Desempenho"
           )






# O gráfico de mosaico confirma visualmente a tendência linear identificada no teste CMH. 
# Observa-se uma clara transição diagonal de resíduos: nos níveis de menor escolaridade materna 
# (Nunca estudou e EF incompleto), há um excesso significativo de estudantes no quartil de desempenho Baixo 
# (resíduos azuis > 4). À medida que a escolaridade da mãe avança para o nível Superior e Pós-graduação,
# a concentração de estudantes desloca-se de forma acentuada para os quartis Médio-Alto e Alto,
# onde os resíduos positivos indicam uma frequência observada drasticamente maior do que a esperada 
# sob a hipótese de independência.

# Interpretação:Filhos de mães que "Nunca estudaram" ou têm "EF incompleto" estão massivamente concentrados
# no desempenho Baixo (muito acima do esperado) e quase não aparecem no desempenho 
# Alto (muito abaixo do esperado).

# Interpretação: Filhos de mães com Superior Completo e Pós-Graduação têm uma presença esmagadora 
# nos níveis mais altos de nota (muito acima do esperado) e raramente ficam no quartil Baixo
# (muito abaixo do esperado).
#===============================================================================
#  GRÁFICO DE TENDÊNCIA
#===============================================================================

# 1. Converter sua tabela xtabs em um data.frame formatado
df_linhas <- as.data.frame(tab)

# 2. Atribuir valores numéricos (scores/ranks) para o desempenho
# Garantindo que a conversão siga a ordem correta das categorias
df_linhas <- df_linhas %>%
  mutate(
    score_desempenho = as.numeric(setor_desempenho) # Baixo=1, Médio-Baixo=2, etc.
  )

# 3. Calcular a média ponderada do desempenho para cada escolaridade da mãe
df_tendencia <- df_linhas %>%
  group_by(escolaridade_mae) %>%
  summarise(
    desempenho_medio = sum(score_desempenho * Freq) / sum(Freq)
  )

# 4. Plotar o Gráfico de Linha de Tendência
ggplot(df_tendencia, aes(x = escolaridade_mae, y = desempenho_medio, group = 1)) +
  geom_line(color = "#2b8cbe", size = 1.2) + # Linha da tendência
  geom_point(color = "#084081", size = 4) +   # Pontos em cada categoria
  scale_y_continuous(
    limits = c(1, 4), 
    breaks = 1:4, 
    labels = c("Baixo (1)", "Médio-Baixo (2)", "Médio-Alto (3)", "Alto (4)")
  ) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Linha de Tendência: Desempenho Médio por Escolaridade Materna",
    subtitle = "Demonstração visual do Teste de Associação Linear (CMH)",
    x = "Escolaridade da Mãe",
    y = "Score Médio de Desempenho"
  ) +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1, face = "bold"),
    panel.grid.minor = element_blank()
  )

########################################################
library(dplyr)
library(ggplot2)
library(scales)

tab_prop <- prop.table(tab, margin = 1) |>  # proporção por escolaridade
  as.data.frame()

names(tab_prop) <- c(
  "Escolaridade",
  "Desempenho",
  "Proporcao"
)

ggplot(tab_prop,
       aes(x = Desempenho,
           y = Escolaridade,
           fill = Proporcao)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = percent(Proporcao, accuracy = 0.1)),
            size = 3.5) +
  scale_fill_gradient(
    low = "#F7FBFF",
    high = "#2171B5",
    labels = percent
  ) +
  labs(
    x = "Desempenho",
    y = "Escolaridade da mãe",
    fill = "Proporção"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )
