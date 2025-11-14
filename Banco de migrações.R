library(readxl)
library(writexl)
library(tidyverse)
library(patchwork)
library(ggraph)
library(htmlwidgets) # Para edições nos sankey plots
library(htmltools)   # Para edições nos sankey plots
library(officer)     # Para limpeza de texto na junção das variáveis
library(stringi)     # Para limpeza de texto na junção das variáveis
library(lmtest)     # Para teste de razão de verossimilhança
library(plotly)     # Para os sankey plots

#Puxando e transformando os dados
  banco_de_migrações <- read_xlsx("C:/Users/felip/Downloads/BD_Sainz&Codato_ClassePolíticadoBrasil_INCT_ReDem_2025.xlsx")
  
  banco_de_migrações_cap <- banco_de_migrações_cap |>
    mutate(
      idLegislatura = case_when(
        Ano_Eleição == 1998 ~ "51",
        Ano_Eleição == 2002 ~ "52",
        Ano_Eleição == 2006 ~ "53",
        Ano_Eleição == 2010 ~ "54",
        Ano_Eleição == 2014 ~ "55",
        Ano_Eleição == 2018 ~ "56",
        Ano_Eleição == 2022 ~ "57"
      ),
      Ideologia = case_when(
        Sigla_Partido == "PT DO B" ~ "Direita",
        Sigla_Partido == "PTdoB" ~ "Direita",
        Sigla_Partido == "AVANTE" ~ "Direita",
        Sigla_Partido == "PPS" ~ "Centro",
        Sigla_Partido == "CIDADANIA" ~ "Centro",
        Sigla_Partido == "PSDC" ~ "Direita",
        Sigla_Partido == "DC" ~ "Direita",
        Sigla_Partido == "PFL" ~ "Direita",
        Sigla_Partido == "DEM" ~ "Direita",
        Sigla_Partido == "PMDB" ~ "Centro",
        Sigla_Partido == "MDB" ~ "Centro",
        Sigla_Partido == "NOVO" ~ "Direita",
        Sigla_Partido == "PAN" ~ "Direita",
        Sigla_Partido == "PEN" ~ "Direita",
        Sigla_Partido == "PATRIOTA" ~ "Direita",
        Sigla_Partido == "PC DO B" ~ "Esquerda",
        Sigla_Partido == "PCdoB" ~ "Esquerda",
        Sigla_Partido == "PDT" ~ "Esquerda",
        Sigla_Partido == "PHS" ~ "Direita",
        Sigla_Partido == "PR" ~ "Direita",
        Sigla_Partido == "PL" ~ "Direita",
        Sigla_Partido == "PMN" ~ "Direita",
        Sigla_Partido == "PTN" ~ "Direita",
        Sigla_Partido == "PODE" ~ "Direita",
        Sigla_Partido == "PPB" ~ "Direita",
        Sigla_Partido == "PP" ~ "Direita",
        Sigla_Partido == "PP**" ~ "Direita",
        Sigla_Partido == "PL*" ~ "Direita",
        Sigla_Partido == "PPL" ~ "Direita",
        Sigla_Partido == "PRB" ~ "Direita",
        Sigla_Partido == "PMR" ~ "Direita",
        Sigla_Partido == "REPUBLICANOS" ~ "Direita",
        Sigla_Partido == "PRONA" ~ "Direita",
        Sigla_Partido == "PROS" ~ "Direita",
        Sigla_Partido == "PRP" ~ "Direita",
        Sigla_Partido == "PRTB" ~ "Direita",
        Sigla_Partido == "PSB" ~ "Centro",
        Sigla_Partido == "PSC" ~ "Direita",
        Sigla_Partido == "PSD" ~ "Direita",
        Sigla_Partido == "PSDB" ~ "Centro",
        Sigla_Partido == "PSL" ~ "Direita",
        Sigla_Partido == "PSOL" ~ "Esquerda",
        Sigla_Partido == "PST" ~ "Direita",
        Sigla_Partido == "PT" ~ "Esquerda",
        Sigla_Partido == "PTB" ~ "Direita",
        Sigla_Partido == "PTC" ~ "Direita",
        Sigla_Partido == "PTN" ~ "Direita",
        Sigla_Partido == "PV" ~ "Centro",
        Sigla_Partido == "REDE" ~ "Centro",
        Sigla_Partido == "SD" ~ "Direita",
        Sigla_Partido == "SDD" ~ "Direita",
        Sigla_Partido == "SOLIDARIEDADE" ~ "Direita",
        Sigla_Partido == "PRD" ~ "Direita",
        Sigla_Partido == "PMB" ~ "Direita",
        Sigla_Partido == "PATRI" ~ "Direita",
        Sigla_Partido == "UNIÃO" ~ "Direita"
      ),
      Tamanho = case_when(
        # === LEGISLATURA 57 (2022-2026) === 
        # Grandes (50+)
        Sigla_Partido %in% c("PL", "PT", "UNIÃO") & idLegislatura == 57 ~ "Grande",
        
        # Médios (21-49)
        Sigla_Partido %in% c("REPUBLICANOS", "PSD", "MDB", "PP") & idLegislatura == 57 ~ "Médio",
        
        # Pequenos (5-20)
        Sigla_Partido %in% c("CIDADANIA", "PRD", "AGIR", "PSC", "PSOL", "AVANTE", "PODE", "PSB", "PDT", "PSDB") & idLegislatura == 57 ~ "Pequeno",
        
        # Nanicos (até 4)
        Sigla_Partido %in% c("PTB", "PC DO B", "PV", "REDE", "PMB", "UP", "DC", "PATRIOTA", "PROS", "SOLIDARIEDADE", "NOVO") & idLegislatura == 57 ~ "Nanico",
        
        # === LEGISLATURA 56 (2019-2023) ===
        # Grandes (50+)
        Sigla_Partido %in% c("PT", "PSL", "PL") & idLegislatura == 56 ~ "Grande",
        
        # Médios (21-49)
        Sigla_Partido %in% c("PP", "PSD", "MDB", "PR", "PSB", "PRB", "DEM", "PSDB", "PDT", "UNIÃO") & idLegislatura == 56 ~ "Médio",
        
        # Pequenos (5-20)
        Sigla_Partido %in% c("SOLIDARIEDADE", "PODE", "PSOL", "PTB", "PCdoB", "NOVO", "PPS", "PROS", "PSC", "AVANTE", "PHS", "PC DO B") & idLegislatura == 56 ~ "Pequeno",
        
        # Nanicos (até 4)
        Sigla_Partido %in% c("PATRI", "PRP", "PV", "PMN", "PTC", "DC", "PPL", "REDE") & idLegislatura == 56 ~ "Nanico",
        
        # === LEGISLATURA 55 (2015-2019) ===
        # Grandes (50+)
        Sigla_Partido %in% c("PT", "PMDB", "PSDB", "MDB") & idLegislatura == 55 ~ "Grande",
        
        # Médios (21-49)
        Sigla_Partido %in% c("PP**", "PSD", "PSB", "PR", "PTB", "PRB", "DEM", "PDT", "UNIÃO") & idLegislatura == 55 ~ "Médio",
        
        # Pequenos (5-20)
        Sigla_Partido %in% c("SD", "PSC", "PROS", "PPS", "PCdoB", "PV", "PSOL", "PHS", "SOLIDARIEDADE", "PMB", "REDE", "PC DO B") & idLegislatura == 55 ~ "Pequeno",
        
        # Nanicos (até 4)
        Sigla_Partido %in% c("PTN", "PRP", "PMN", "PEN", "PSDC", "PTC", "PTdoB", "PSL", "PRTB", "AVANTE", "PODE", "PATRI", "PPL", "PATRIOTA", "PT DO B") & idLegislatura == 55 ~ "Nanico",
        
        # === LEGISLATURA 54 (2011-2015) ===
        # Grandes (50+)
        Sigla_Partido %in% c("PT", "PMDB", "PSDB", "PSD") & idLegislatura == 54 ~ "Grande",
        
        # Médios (21-49)
        Sigla_Partido %in% c("PP**", "DEM", "PR", "PSB", "PDT", "PTB") & idLegislatura == 54 ~ "Médio",
        
        # Pequenos (5-20)
        Sigla_Partido %in% c("PSC", "PCdoB", "PV", "PPS", "PRB", "SD", "SDD", "PROS", "PC DO B") & idLegislatura == 54 ~ "Pequeno",
        
        # Nanicos (até 4)
        Sigla_Partido %in% c("PMN", "PTdoB", "PSOL", "PHS", "PRTB", "PRP", "PTC", "PSL", "PEN", "PSDC", "PT DO B") & idLegislatura == 54 ~ "Nanico",
        
        # === LEGISLATURA 53 (2007-2011) ===
        # Grandes (50+)
        Sigla_Partido %in% c("PMDB", "PT", "PSDB", "PFL", "DEM") & idLegislatura == 53 ~ "Grande",
        
        # Médios (21-49)
        Sigla_Partido %in% c("PP**", "PR", "PSB", "PDT", "PTB") & idLegislatura == 53 ~ "Médio",
        
        # Pequenos (5-20)
        Sigla_Partido %in% c("PPS", "PV", "PCdoB", "PSC", "PC DO B") & idLegislatura == 53 ~ "Pequeno",
        
        # Nanicos (até 4)
        Sigla_Partido %in% c("PAN", "PSOL", "PMN", "PTC", "PHS", "PTdoB", "PRB", "PRTB", "PRONA", "PT DO B") & idLegislatura == 53 ~ "Nanico",
        
        # === LEGISLATURA 52 (2003-2007) ===
        # Grandes (50+)
        Sigla_Partido %in% c("PT", "PFL", "PMDB", "PSDB") & idLegislatura == 52 ~ "Grande",
        
        # Médios (21-49)
        Sigla_Partido %in% c("PPB", "PTB", "PL*", "PSB", "PPS", "PP**", "PR") & idLegislatura == 52 ~ "Médio",
        
        # Pequenos (5-20)
        Sigla_Partido %in% c("PDT", "PCdoB", "PRONA", "PV", "PC DO B") & idLegislatura == 52 ~ "Pequeno",
        
        # Nanicos (até 4)
        Sigla_Partido %in% c("PMN", "PSC", "PSL", "PHS", "PSOL", "PST", "PTC", "PMR", "PRB", "PRP", "PSD", "PSDC") & idLegislatura == 52 ~ "Nanico",
        
        # === LEGISLATURA 51 (1999-2003) ===
        # Grandes (50+)
        Sigla_Partido %in% c("PFL", "PSDB", "PMDB", "PPB", "PT") & idLegislatura == 51 ~ "Grande",
        
        # Médios (21-49)
        Sigla_Partido %in% c("PTB", "PDT") & idLegislatura == 51 ~ "Médio",
        
        # Pequenos (5-20)
        Sigla_Partido %in% c("PSB", "PL", "PCdoB", "PC DO B") & idLegislatura == 51 ~ "Pequeno",
        
        # Nanicos (até 4)
        Sigla_Partido %in% c("PPS", "PSD", "PMN", "PSC", "PRONA", "PSL", "PST", "PV") & idLegislatura == 51 ~ "Nanico",
        
        # === CASOS ESPECIAIS DE MUDANÇA DE CATEGORIA ===
        
        # PP: Médio até leg. 54, depois volta para Médio na 56
        Sigla_Partido == "PP" & idLegislatura %in% c(51, 52, 53, 54, 56) ~ "Médio",
        Sigla_Partido == "PP" & idLegislatura == 55 ~ "Médio", # 38 deputados
        
        # PSB: cresce de Pequeno para Médio
        Sigla_Partido == "PSB" & idLegislatura %in% c(51, 52) ~ "Pequeno", # 17 e 28
        Sigla_Partido == "PSB" & idLegislatura >= 53 ~ "Médio", # 28, 34, 34, 32
        
        # PDT: Médio nas primeiras, depois Médio mantido
        Sigla_Partido == "PDT" & idLegislatura %in% c(51, 52) ~ "Médio", # 25, 17
        Sigla_Partido == "PDT" & idLegislatura >= 53 ~ "Médio", # 23, 26, 20, 28
        
        # PTB: varia de Médio para Médio/Pequeno
        Sigla_Partido == "PTB" & idLegislatura %in% c(51, 52) ~ "Médio", # 31, 41
        Sigla_Partido == "PTB" & idLegislatura == 53 ~ "Médio", # 21
        Sigla_Partido == "PTB" & idLegislatura == 54 ~ "Médio", # 22
        Sigla_Partido == "PTB" & idLegislatura == 55 ~ "Médio", # 25
        Sigla_Partido == "PTB" & idLegislatura == 56 ~ "Pequeno", # 10
        
        # PL: Médio até fusão em 2006, depois vira PR
        Sigla_Partido == "PL" & idLegislatura %in% c(51, 52) ~ "Médio", # 12, 33
        Sigla_Partido == "PL" & idLegislatura == 53 ~ "Médio", # virou PR em 2006
        
        # PSDB: Grande nas primeiras, depois oscila
        Sigla_Partido == "PSDB" & idLegislatura %in% c(51, 52, 53) ~ "Grande", # 99, 63, 64
        Sigla_Partido == "PSDB" & idLegislatura >= 54 ~ "Grande", # 53, 54, mantém como Grande
        Sigla_Partido == "PSDB" & idLegislatura == 56 ~ "Médio", # 29 deputados
        
        # DEM/PFL: Grande até 2007, depois Médio
        Sigla_Partido == "PFL" & idLegislatura %in% c(51, 52, 53) ~ "Grande", # 105, 75, 62
        Sigla_Partido == "DEM" & idLegislatura >= 54 ~ "Médio", # 43, 21, 29
        
        # Mudanças de nome que devem manter classificação
        Sigla_Partido == "MDB" & idLegislatura == 56 ~ "Médio", # era PMDB
        Sigla_Partido == "REPUBLICANOS" & idLegislatura == 56 ~ "Médio", # era PRB
        Sigla_Partido == "CIDADANIA" & idLegislatura == 56 ~ "Pequeno", # era PPS
        Sigla_Partido == "PATRIOTA" & idLegislatura == 56 ~ "Nanico", # era PEN/PATRI
        Sigla_Partido == "PROGRESSISTAS" & idLegislatura == 56 ~ "Médio", # era PP
        Sigla_Partido == "AVANTE" & idLegislatura == 56 ~ "Pequeno", # era PTdoB
        Sigla_Partido == "MOBILIZA" & idLegislatura >= 57 ~ "Nanico", # era PMN
        
        # Padrão para partidos não listados ou legislaturas futuras
        TRUE ~ "Indefinido"
      )
    )
  banco_final_cap <- banco_final_cap |>
    arrange(idLegislatura, Nome_Deputado) |>  # Ordenar por legislatura para ordem cronológica
    group_by(Nome_Deputado) |>
    mutate(
      # Se não é a primeira vez que esse nome aparece = Reeleito
      Reeleito = ifelse(row_number() > 1, "Sim", "Não")
    ) |>
    ungroup()

#Sankey plots
{
  # 1. Filtrar e preparar os dados
  sankey <- banco_final_cap |>
    filter(Reeleito == "Sim") |>
    arrange(Nome_Deputado, Ano_Eleição)
    
    print(gerar_sankey_ideologia_plotly(sankey))
    
    # Função para Sankey de Tamanho com Plotly - Versão Melhorada
    gerar_sankey_tamanho_plotly <- function(df) {
      
      # Criar pares de tamanho consecutivos por deputado
      df_pairs <- df |>
        group_by(Nome_Deputado) |>
        mutate(origem = lag(Tamanho),
               destino = Tamanho,
               ano_origem = lag(Ano_Eleição),
               ano_destino = Ano_Eleição) |>
        filter(!is.na(origem), ano_destino - ano_origem == 4) |>
        ungroup() |>
        mutate(source = paste0(origem, " em ", ano_origem),
               target = paste0(destino, " em ", ano_destino)) |>
        count(source, target, name = "value")
      
      # Criar lista de nós únicos
      nodes <- data.frame(name = unique(c(df_pairs$source, df_pairs$target)))
      
      # Extrair tamanho e ano para ordenação
      nodes$tamanho <- gsub(" em .*", "", nodes$name)
      nodes$ano <- as.numeric(gsub(".* em ", "", nodes$name))
      
      # Definir posição X baseada no ano (0 a 1) - mais próximos
      anos_unicos <- sort(unique(nodes$ano))
      # Reduzir distância entre anos pela metade
      nodes$x <- 0.1 + (match(nodes$ano, anos_unicos) - 1) * 0.8 / (length(anos_unicos) - 1) * 0.5
      
      # Definir posição Y baseada no tamanho dentro de cada ano
      nodes <- nodes |>
        group_by(ano) |>
        mutate(y = case_when(
          tamanho == "Grande" ~ 0.8,
          tamanho == "Médio" ~ 0.6,
          tamanho == "Pequeno" ~ 0.4,
          tamanho == "Nanico" ~ 0.2,
          TRUE ~ 0.5
        )) |>
        ungroup() |>
        arrange(ano, factor(tamanho, levels = c("Grande", "Médio", "Pequeno", "Nanico")))
      
      # Definir cores
      cores_tamanho <- c(
        "Grande" = "#e2030a",
        "Médio" = "#0a6b00", 
        "Pequeno" = "#0073e6",
        "Nanico" = "#ff6d0f"
      )
      
      nodes$color <- cores_tamanho[nodes$tamanho]
      
      # Mapear índices
      df_pairs$source_id <- match(df_pairs$source, nodes$name) - 1
      df_pairs$target_id <- match(df_pairs$target, nodes$name) - 1
      
      # Colorir conexões baseado no nó de origem
      df_pairs$link_color <- paste0(substr(nodes$color[df_pairs$source_id + 1], 1, 7), "80")  # Add transparency
      
      # Criar sankey com plotly
      fig <- plot_ly(
        type = "sankey",
        orientation = "h",
        node = list(
          #label = nodes$name,
          color = nodes$color,
          x = nodes$x,
          y = nodes$y,
          pad = 5,  # Reduzido para nós mais próximos
          thickness = 40,
          line = list(color = "black", width = 0.5)
        ),
        link = list(
          source = df_pairs$source_id,
          target = df_pairs$target_id,
          value = df_pairs$value,
          color = df_pairs$link_color
        )
      )
      
      # Layout com legenda customizada
      fig <- fig |>
        layout(
          title = "Migração Por Tamanho de Partido (1998–2022)",
          font = list(family = "Garamond", size = 25),
          margin = list(l = 50, r = 150, t = 80, b = 50),
          annotations = list(
            # Título da legenda
            list(x = 1.05, y = 0.95, text = "<b>Legenda:</b>", 
                 showarrow = FALSE, xref = "paper", yref = "paper",
                 font = list(family = "Garamond", size = 25)),
            
            # Itens da legenda
            list(x = 1.05, y = 0.85, text = "■ Grande (50+ deputados)", 
                 showarrow = FALSE, xref = "paper", yref = "paper",
                 font = list(family = "Garamond", size = 18, color = "#e2030a")),
            
            list(x = 1.05, y = 0.78, text = "■ Médio (21-49 deputados)", 
                 showarrow = FALSE, xref = "paper", yref = "paper",
                 font = list(family = "Garamond", size = 18, color = "#0a6b00")),
            
            list(x = 1.05, y = 0.71, text = "■ Pequeno (5-20 deputados)", 
                 showarrow = FALSE, xref = "paper", yref = "paper",
                 font = list(family = "Garamond", size = 18, color = "#0073e6")),
            
            list(x = 1.05, y = 0.64, text = "■ Nanico (até 4 deputados)", 
                 showarrow = FALSE, xref = "paper", yref = "paper",
                 font = list(family = "Garamond", size = 18, color = "#ff6d0f"))
          )
        )
      
      return(fig)
    }
    
    print(gerar_sankey_tamanho_plotly(sankey))

banco_migrações <- readxl::read_xlsx("C:/Users/controladoria/Downloads/banco_final_migrações.xlsx")

banco_migrações$nome <- toupper(banco_migrações$nome)
banco_migrações$nomeEleitoral <- toupper(banco_migrações$nomeEleitoral)

situações_migrantes <- data.frame(table(banco_migrações$descricaoStatus))
write_xlsx(list("Migrações" = banco_migrações, "Status categorias" = situações_migrantes), "banco bruto de migrações.xlsx")

#Tratamento do banco final do livro
{
  #Recategorização dos dados
  {
    # Primeiro, ordenar por Nome_Deputado e idLegislatura para garantir ordem cronológica
    banco_de_migrações_cap <- banco_de_migrações_cap %>%
      arrange(Nome_Deputado, idLegislatura)
    
    banco_de_migrações_cap <- banco_de_migrações
    banco_de_migrações_cap$Nome_Deputado <- stri_trans_general(banco_de_migrações_cap$Nome_Deputado, "LATIN-ASCII")
    banco_de_migrações_cap$Nome_Deputado <- str_to_title(banco_de_migrações_cap$Nome_Deputado)
    banco_de_migrações_cap <- banco_de_migrações_cap |> 
      arrange(Nome_Deputado, idLegislatura)
    
    # Aplicar a lógica de migração
    banco_de_migrações_cap <- banco_de_migrações_cap %>%
      arrange(Nome_Deputado, Ano_Eleição) %>%  # USAR ANO_ELEIÇÃO, não idLegislatura
      group_by(Nome_Deputado) %>%
      mutate(
        primeira_aparicao = row_number() == 1,
        
        mudanca_legitima = case_when(
          primeira_aparicao ~ FALSE,
          lag(Sigla_Partido) == Sigla_Partido ~ FALSE,
          
          # === MUDANÇAS DE NOME - usar período temporal correto ===
          # PSN -> PHS (mudança em 2000, aparece na eleição 2002)
          (lag(Sigla_Partido) == "PSN" & Sigla_Partido == "PHS" & 
             Ano_Eleição == 2002) ~ FALSE,
          
          # PRN -> PTC (mudança em 2001, aparece na eleição 2002)  
          (lag(Sigla_Partido) == "PRN" & Sigla_Partido == "PTC" & 
             Ano_Eleição == 2002) ~ FALSE,
          
          # PPB -> PP (mudança em 2003, aparece na eleição 2006)
          (lag(Sigla_Partido) == "PPB" & Sigla_Partido == "PP" & 
             Ano_Eleição == 2006) ~ FALSE,
          
          # PFL -> DEM (mudança em 2007, aparece na eleição 2010)
          (lag(Sigla_Partido) == "PFL" & Sigla_Partido == "DEM" & 
             Ano_Eleição == 2010) ~ FALSE,
          
          # PMR -> PRB (mudança em 2009, aparece na eleição 2010)
          (lag(Sigla_Partido) == "PMR" & Sigla_Partido == "PRB" & 
             Ano_Eleição == 2010) ~ FALSE,
          
          # PMDB -> MDB (mudança em 2018, aparece na eleição 2018)
          (lag(Sigla_Partido) == "PMDB" & Sigla_Partido == "MDB" & 
             Ano_Eleição == 2018) ~ FALSE,
          
          # PT DO B -> AVANTE (mudança em 2017, aparece na eleição 2018)
          (lag(Sigla_Partido) == "PT DO B" & Sigla_Partido == "AVANTE" & 
             Ano_Eleição == 2018) ~ FALSE,
          
          # PTN -> PODE (mudança em 2017, aparece na eleição 2018)
          (lag(Sigla_Partido) == "PTN" & Sigla_Partido == "PODE" & 
             Ano_Eleição == 2018) ~ FALSE,
          
          # PRB -> REPUBLICANOS (mudança em 2019, aparece na eleição 2022)
          (lag(Sigla_Partido) == "PRB" & Sigla_Partido == "REPUBLICANOS" & 
             Ano_Eleição == 2022) ~ FALSE,
          
          # PPS -> CIDADANIA (mudança em 2019, aparece na eleição 2022)
          (lag(Sigla_Partido) == "PPS" & Sigla_Partido == "CIDADANIA" & 
             Ano_Eleição == 2022) ~ FALSE,
          
          # PR -> PL (mudança em 2019, aparece na eleição 2022)
          (lag(Sigla_Partido) == "PR" & Sigla_Partido == "PL" & 
             Ano_Eleição == 2022) ~ FALSE,
          
          # SD -> SOLIDARIEDADE (mudança em 2018, aparece na eleição 2018)  
          (lag(Sigla_Partido) == "SD" & Sigla_Partido == "SOLIDARIEDADE" & 
             Ano_Eleição >= 2018) ~ FALSE,
          
          # === FUSÕES ===
          # PL/PRONA -> PR (fusão em 2006, aparece na eleição 2006)
          (lag(Sigla_Partido) %in% c("PL", "PRONA") & Sigla_Partido == "PR" & 
             Ano_Eleição >= 2006) ~ FALSE,
          
          # DEM/PSL -> UNIÃO (fusão em 2022, aparece na eleição 2022)
          (lag(Sigla_Partido) %in% c("DEM", "PSL") & Sigla_Partido == "UNIÃO" & 
             Ano_Eleição >= 2022) ~ FALSE,
          
          # Outras mudanças são migrações reais
          TRUE ~ TRUE
        ),
        
        # Classificar o deputado
        Migrantes = case_when(
          primeira_aparicao ~ "Primeiro mandato",
          mudanca_legitima ~ "Migrante", 
          !mudanca_legitima ~ "Não migrante",
          TRUE ~ "Erro" # Para debug
        )
      ) %>%
      # Remover colunas auxiliares
      select(-primeira_aparicao, -mudanca_legitima) %>%
      ungroup()
    
    banco_para_verificação <- banco_de_migrações_cap |>
      select(ID_BD, Ano_Eleição, idLegislatura, Nome_Deputado, Sigla_Partido, Ideologia, Tamanho, Migrantes, Reeleitos)
    
    write_xlsx(banco_de_migrações_cap, "banco final cap.xlsx")
  }
  
  banco_final_cap <- read_excel("C:/Users/controladoria/Documents/banco final cap.xlsx")
  
  #Análises
  {
    banco_final_cap <- banco_final_cap %>%
      mutate(
        Migrou = case_when(
          Migrantes == "Migrante" ~ "Migrou",
          TRUE ~ "Não migrou"
        ),
        # Converter para fator binário (0/1 para regressão logística)
        Migrou_bin = as.numeric(Migrou == "Migrou"),
        
        # Definir fatores com níveis de referência especificados
        Ideologia_fct = factor(Ideologia, levels = c("Esquerda", "Centro", "Direita")),
        Tamanho_fct = factor(Tamanho, levels = c("Nanico", "Pequeno", "Médio", "Grande")),
        idLegislatura_fct = factor(idLegislatura, levels = c("51", "52", "53", "54", "55", "56", "57"))
      )
    
    # Verificar distribuição da variável dependente
    table(banco_de_migrações_cap$Migrou)
    prop.table(table(banco_de_migrações_cap$Migrou))
    
    # 2. VARIANCE INFLATION FACTOR (VIF)
    # Primeiro, criar modelo linear para calcular VIF
    modelo_vif <- lm(Migrou_bin ~ Ideologia_fct + Tamanho_fct + idLegislatura_fct, 
                     data = banco_de_migrações_cap)
    
    # Calcular VIF
    vif_values <- vif(modelo_vif)
    print("=== VARIANCE INFLATION FACTOR (VIF) ===")
    print(vif_values)
    print(paste("VIF médio:", round(mean(vif_values), 3)))
    
    # Interpretação VIF:
    # VIF < 5: Multicolinearidade aceitável
    # VIF 5-10: Multicolinearidade moderada  
    # VIF > 10: Multicolinearidade problemática
    
    # 3. REGRESSÃO LOGÍSTICA BINÁRIA
    modelo_logit <- glm(Migrou_bin ~ Ideologia_fct + Tamanho_fct + idLegislatura_fct,
                        family = binomial(link = "logit"),
                        data = banco_de_migrações_cap)
    
    print("=== RESUMO DA REGRESSÃO LOGÍSTICA ===")
    summary(modelo_logit)
    
    # 4. R² AJUSTADO DE NAGELKERKE
    nagelkerke_r2 <- PseudoR2(modelo_logit, which = "Nagelkerke")
    print("=== R² DE NAGELKERKE ===")
    print(paste("R² de Nagelkerke:", round(nagelkerke_r2, 4)))
    
    # 5. TESTE OMNIBUS DO RÁCIO DE VEROSSIMILHANÇAS PARA idLegislatura
    # Modelo sem idLegislatura (modelo reduzido)
    modelo_sem_legislatura <- glm(Migrou_bin ~ Ideologia_fct + Tamanho_fct,
                                  family = binomial(link = "logit"),
                                  data = banco_de_migrações_cap)
    
    # Teste de razão de verossimilhança
    teste_lr <- lrtest(modelo_sem_legislatura, modelo_logit)
    print("=== TESTE OMNIBUS - RAZÃO DE VEROSSIMILHANÇAS PARA idLegislatura ===")
    print(teste_lr)
    
    # 6. ODDS RATIOS E INTERVALOS DE CONFIANÇA
    odds_ratios <- exp(coef(modelo_logit))
    ic_odds <- exp(confint(modelo_logit))
    
    print("=== ODDS RATIOS E INTERVALOS DE CONFIANÇA ===")
    resultado_or <- data.frame(
      Variável = names(odds_ratios),
      OR = round(odds_ratios, 3),
      IC_2.5 = round(ic_odds[,1], 3),
      IC_97.5 = round(ic_odds[,2], 3)
    )
    print(resultado_or)
    
    # 7. ANÁLISES ADICIONAIS
    
    # Teste de Hosmer-Lemeshow (bondade do ajuste)
    library(ResourceSelection)
    hoslem_test <- hoslem.test(banco_de_migrações_cap$Migrou_bin, 
                               fitted(modelo_logit))
    print("=== TESTE DE HOSMER-LEMESHOW ===")
    print(hoslem_test)
    
    # Matriz de classificação
    probabilidades <- predict(modelo_logit, type = "response")
    predicoes <- ifelse(probabilidades > 0.5, 1, 0)
    matriz_confusao <- table(Observado = banco_de_migrações_cap$Migrou_bin, 
                             Predito = predicoes)
    print("=== MATRIZ DE CONFUSÃO ===")
    print(matriz_confusao)
    
    # Acurácia
    acuracia <- sum(diag(matriz_confusao)) / sum(matriz_confusao)
    print(paste("Acurácia do modelo:", round(acuracia, 4)))
    
    # 8. GRÁFICO DE DIAGNÓSTICO
    # Resíduos vs Valores Ajustados
    par(mfrow = c(2, 2))
    plot(modelo_logit)
    
    # 9. RESUMO FINAL DOS RESULTADOS
    cat("\n=== RESUMO FINAL ===\n")
    cat("1. VIF médio:", round(mean(vif_values), 3), "\n")
    cat("2. R² de Nagelkerke:", round(nagelkerke_r2, 4), "\n")
    cat("3. Teste LR para Legislatura - p-valor:", round(teste_lr$`Pr(>Chisq)`[2], 4), "\n")
    cat("4. Acurácia do modelo:", round(acuracia, 4), "\n")
    cat("5. Hosmer-Lemeshow p-valor:", round(hoslem_test$p.value, 4), "\n")
  }
}

#Banco para o artigo
{
  
  #Recategorização dos dados, assim como inclusão das colunas "ideologia" e "tamanho do partido"
  {
    migrações_filtrado <- migrações_filtrado |> 
      mutate(situaçãoReclassificada = case_when(
        str_detect(descricaoStatus, "Alteração de nome parlamentar") ~ "Mudou de nome",
        str_detect(descricaoStatus, "Alteração de partido") ~ "Migrou",
        str_detect(descricaoStatus, "Diverso - ") ~ "Outros",
        str_detect(descricaoStatus, "Entrada - ") ~ "Assumiu mandato",
        str_detect(descricaoStatus, "Nome no início da legislatura / Partido no início da legislatura") ~ "Dados no início da legislatura",
        str_detect(descricaoStatus, "Situação e condição ao fim da legislatura") ~ "Dados no fim da legislatura",
        str_detect(descricaoStatus, "Primeira posse na legislatura") ~ "Dados no início da legislatura",
        str_detect(descricaoStatus, "Saída - Afastamento sem prazo determinado") & str_detect(descricaoStatus, "Ministro") ~ "Assume ministério",
        str_detect(descricaoStatus, "Saída - Afastamento sem prazo determinado") & str_detect(descricaoStatus, "Ministra") &  !str_detect(descricaoStatus, "Aposentadoria")  & !str_detect(descricaoStatus, "Falecimento") & !str_detect(descricaoStatus, "Perda de Mandato") ~ "Assume ministério",
        str_detect(descricaoStatus, "Saída - Afastamento com prazo determinado") & str_detect(descricaoStatus, "Secretaria") &  !str_detect(descricaoStatus, "Aposentadoria")  & !str_detect(descricaoStatus, "Falecimento") & !str_detect(descricaoStatus, "Perda de Mandato") ~ "Assume secretaria",
        str_detect(descricaoStatus, "Saída - Afastamento com prazo determinado") & str_detect(descricaoStatus, "Secretária") &  !str_detect(descricaoStatus, "Aposentadoria")  & !str_detect(descricaoStatus, "Falecimento") & !str_detect(descricaoStatus, "Perda de Mandato") ~ "Assume secretaria",
        str_detect(descricaoStatus, "Saída - Afastamento com prazo determinado") & str_detect(descricaoStatus, "Secretário") &  !str_detect(descricaoStatus, "Aposentadoria")  & !str_detect(descricaoStatus, "Falecimento") & !str_detect(descricaoStatus, "Perda de Mandato") ~ "Assume secretaria",
        str_detect(descricaoStatus, "Saída - Afastamento sem prazo determinado") & str_detect(descricaoStatus, "Secretaria") &  !str_detect(descricaoStatus, "Aposentadoria")  & !str_detect(descricaoStatus, "Falecimento") & !str_detect(descricaoStatus, "Perda de Mandato") ~ "Assume secretaria",
        str_detect(descricaoStatus, "Saída - Afastamento sem prazo determinado") & str_detect(descricaoStatus, "Secretária") &  !str_detect(descricaoStatus, "Aposentadoria")  & !str_detect(descricaoStatus, "Falecimento") & !str_detect(descricaoStatus, "Perda de Mandato") ~ "Assume secretaria",
        str_detect(descricaoStatus, "Saída - Afastamento sem prazo determinado") & str_detect(descricaoStatus, "Secretário") &  !str_detect(descricaoStatus, "Aposentadoria")  & !str_detect(descricaoStatus, "Falecimento") & !str_detect(descricaoStatus, "Perda de Mandato") ~ "Assume secretaria",
        str_detect(descricaoStatus, "Saída - Afastamento definitivo") & str_detect(descricaoStatus, "Secretaria") &  !str_detect(descricaoStatus, "Aposentadoria")  & !str_detect(descricaoStatus, "Falecimento") & !str_detect(descricaoStatus, "Perda de Mandato") ~ "Assume secretaria",
        str_detect(descricaoStatus, "Saída - Afastamento definitivo") & str_detect(descricaoStatus, "Secretária") &  !str_detect(descricaoStatus, "Aposentadoria")  & !str_detect(descricaoStatus, "Falecimento") & !str_detect(descricaoStatus, "Perda de Mandato") ~ "Assume secretaria",
        str_detect(descricaoStatus, "Saída - Afastamento definitivo") & str_detect(descricaoStatus, "Secretário") &  !str_detect(descricaoStatus, "Aposentadoria")  & !str_detect(descricaoStatus, "Falecimento") & !str_detect(descricaoStatus, "Perda de Mandato") ~ "Assume secretaria",
        str_detect(descricaoStatus, "Saída - Afastamento com prazo determinado") & str_detect(descricaoStatus, " Afastamento Conjunto Consecutivo") ~ "Licença",
        str_detect(descricaoStatus, "Saída - Afastamento com prazo determinado") & str_detect(descricaoStatus, "Licença ") ~ "Licença",
        str_detect(descricaoStatus, "Saída - Afastamento com prazo determinado") & str_detect(descricaoStatus, "Suspensão ") ~ "Licença",
        str_detect(descricaoStatus, "Saída - Afastamento definitivo") &  str_detect(descricaoStatus, " Prefeito") &  !str_detect(descricaoStatus, "Aposentadoria")  & !str_detect(descricaoStatus, "Falecimento") & !str_detect(descricaoStatus, "Perda de Mandato") ~ "Assume prefeitura",
        str_detect(descricaoStatus, "Saída - Afastamento definitivo") &  str_detect(descricaoStatus, " Prefeita") &  !str_detect(descricaoStatus, "Aposentadoria")  & !str_detect(descricaoStatus, "Falecimento") & !str_detect(descricaoStatus, "Perda de Mandato") ~ "Assume prefeitura",
        str_detect(descricaoStatus, "Saída - Afastamento definitivo") &  str_detect(descricaoStatus, "Vice-Prefeito") &  !str_detect(descricaoStatus, "Aposentadoria")  & !str_detect(descricaoStatus, "Falecimento") & !str_detect(descricaoStatus, "Perda de Mandato") ~ "Assume vice-prefeitura",
        str_detect(descricaoStatus, "Saída - Afastamento definitivo") &  str_detect(descricaoStatus, "Vice-Prefeita") &  !str_detect(descricaoStatus, "Aposentadoria")  & !str_detect(descricaoStatus, "Falecimento") & !str_detect(descricaoStatus, "Perda de Mandato") ~ "Assume vice-prefeitura",
        str_detect(descricaoStatus, "Saída - Afastamento definitivo") &  str_detect(descricaoStatus, "Vice-Governador") &  !str_detect(descricaoStatus, "Aposentadoria")  & !str_detect(descricaoStatus, "Falecimento") & !str_detect(descricaoStatus, "Perda de Mandato") ~ "Assume vice-governador",
        str_detect(descricaoStatus, "Saída - Afastamento definitivo") &  str_detect(descricaoStatus, "Vice-Governandor") &  !str_detect(descricaoStatus, "Aposentadoria")  & !str_detect(descricaoStatus, "Falecimento") & !str_detect(descricaoStatus, "Perda de Mandato") ~ "Assume vice-governador",
        str_detect(descricaoStatus, "Saída - Afastamento definitivo") &  str_detect(descricaoStatus, "Vice-Governadora") &  !str_detect(descricaoStatus, "Aposentadoria")  & !str_detect(descricaoStatus, "Falecimento") & !str_detect(descricaoStatus, "Perda de Mandato") ~ "Assume vice-governador",
        str_detect(descricaoStatus, "Saída - Afastamento definitivo") &  str_detect(descricaoStatus, " Governador") &  !str_detect(descricaoStatus, "Aposentadoria")  & !str_detect(descricaoStatus, "Falecimento") & !str_detect(descricaoStatus, "Perda de Mandato") ~ "Assume governo do estado",
        str_detect(descricaoStatus, "Saída - Afastamento definitivo") &  str_detect(descricaoStatus, " Governadora") &  !str_detect(descricaoStatus, "Aposentadoria")  & !str_detect(descricaoStatus, "Falecimento") & !str_detect(descricaoStatus, "Perda de Mandato") ~ "Assume governo do estado",
        str_detect(descricaoStatus, "Saída - Afastamento definitivo") &  str_detect(descricaoStatus, "Deputado") &  !str_detect(descricaoStatus, "Aposentadoria")  & !str_detect(descricaoStatus, "Falecimento") & !str_detect(descricaoStatus, "Perda de Mandato") & !str_detect(descricaoStatus, "Comunicação de renúncia") ~ "Assume cargo legislativo",
        str_detect(descricaoStatus, "Saída - Afastamento definitivo") &  str_detect(descricaoStatus, "Deputada") &  !str_detect(descricaoStatus, "Aposentadoria")  & !str_detect(descricaoStatus, "Falecimento") & !str_detect(descricaoStatus, "Perda de Mandato") & !str_detect(descricaoStatus, "Comunicação de renúncia") ~ "Assume cargo legislativo",
        str_detect(descricaoStatus, "Saída - Afastamento definitivo") &  str_detect(descricaoStatus, "Senador") &  !str_detect(descricaoStatus, "Aposentadoria")  & !str_detect(descricaoStatus, "Falecimento") & !str_detect(descricaoStatus, "Perda de Mandato") ~ "Assume cargo legislativo",
        str_detect(descricaoStatus, "Saída - Afastamento definitivo") &  str_detect(descricaoStatus, "Senadora") &  !str_detect(descricaoStatus, "Aposentadoria")  & !str_detect(descricaoStatus, "Falecimento") & !str_detect(descricaoStatus, "Perda de Mandato") ~ "Assume cargo legislativo",
        str_detect(descricaoStatus, "Saída - Afastamento definitivo") &  str_detect(descricaoStatus, "Conselheiro") &  !str_detect(descricaoStatus, "Aposentadoria")  & !str_detect(descricaoStatus, "Falecimento") & !str_detect(descricaoStatus, "Perda de Mandato") ~ "Assume cargo político executivo",
        str_detect(descricaoStatus, "Saída - Afastamento definitivo") &  str_detect(descricaoStatus, "TCU") &  !str_detect(descricaoStatus, "Aposentadoria")  & !str_detect(descricaoStatus, "Falecimento") & !str_detect(descricaoStatus, "Perda de Mandato") ~ "Assume cargo político executivo",
        str_detect(descricaoStatus, "Saída - Afastamento definitivo - Renúncia - Ministra") &  !str_detect(descricaoStatus, "Aposentadoria")  & !str_detect(descricaoStatus, "Falecimento") & !str_detect(descricaoStatus, "Perda de Mandato") ~ "Assume cargo político executivo",
        str_detect(descricaoStatus, "Saída - Afastamento definitivo - Renúncia - Ministro") &  !str_detect(descricaoStatus, "Aposentadoria")  & !str_detect(descricaoStatus, "Falecimento") & !str_detect(descricaoStatus, "Perda de Mandato") ~ "Assume cargo político executivo",
        str_detect(descricaoStatus, "Saída - Afastamento com prazo determinado") & str_detect(descricaoStatus, "Presidência") &  !str_detect(descricaoStatus, "Aposentadoria")  & !str_detect(descricaoStatus, "Falecimento") & !str_detect(descricaoStatus, "Perda de Mandato") ~ "Assume presidência de órgão",
        str_detect(descricaoStatus, "Saída - Afastamento sem prazo determinado") & str_detect(descricaoStatus, "Presidência") &  !str_detect(descricaoStatus, "Aposentadoria")  & !str_detect(descricaoStatus, "Falecimento") & !str_detect(descricaoStatus, "Perda de Mandato") ~ "Assume presidência de órgão",
        str_detect(descricaoStatus, "Saída - Afastamento definitivo") & str_detect(descricaoStatus, "Presidência") &  !str_detect(descricaoStatus, "Aposentadoria")  & !str_detect(descricaoStatus, "Falecimento") & !str_detect(descricaoStatus, "Perda de Mandato") ~ "Assume presidência de órgão",
        str_detect(descricaoStatus, "Saída - Afastamento sem prazo determinado") &  str_detect(descricaoStatus, "sem Convocação de Suplente") ~ "Saída definitiva",
        str_detect(descricaoStatus, "Saída - Afastamento sem prazo determinado") &  str_detect(descricaoStatus, "Suplente") &  !str_detect(descricaoStatus, "Aposentadoria")  & !str_detect(descricaoStatus, "Falecimento") & !str_detect(descricaoStatus, "Perda de Mandato") ~ "Saída de suplente",
        str_detect(descricaoStatus, "Saída - Afastamento definitivo - Aposentadoria") ~ "Saída definitiva",
        str_detect(descricaoStatus, "Saída - Afastamento definitivo - Perda de Mandato") ~ "Saída definitiva",
        TRUE ~ "Saída definitiva"
      ),
      Ideologia = case_when(
        siglaPartido == "PT DO B" ~ "Direita",
        siglaPartido == "PTdoB" ~ "Direita",
        siglaPartido == "AVANTE" ~ "Direita",
        siglaPartido == "PPS" ~ "Centro",
        siglaPartido == "CIDADANIA" ~ "Centro",
        siglaPartido == "PSDC" ~ "Direita",
        siglaPartido == "DC" ~ "Direita",
        siglaPartido == "PFL" ~ "Direita",
        siglaPartido == "DEM" ~ "Direita",
        siglaPartido == "PMDB" ~ "Centro",
        siglaPartido == "MDB" ~ "Centro",
        siglaPartido == "NOVO" ~ "Direita",
        siglaPartido == "PAN" ~ "Direita",
        siglaPartido == "PEN" ~ "Direita",
        siglaPartido == "PATRIOTA" ~ "Direita",
        siglaPartido == "PC DO B" ~ "Esquerda",
        siglaPartido == "PCdoB" ~ "Esquerda",
        siglaPartido == "PDT" ~ "Esquerda",
        siglaPartido == "PHS" ~ "Direita",
        siglaPartido == "PR" ~ "Direita",
        siglaPartido == "PL" ~ "Direita",
        siglaPartido == "PMN" ~ "Direita",
        siglaPartido == "PTN" ~ "Direita",
        siglaPartido == "PODE" ~ "Direita",
        siglaPartido == "PPB" ~ "Direita",
        siglaPartido == "PP" ~ "Direita",
        siglaPartido == "PP**" ~ "Direita",
        siglaPartido == "PL*" ~ "Direita",
        siglaPartido == "PPL" ~ "Direita",
        siglaPartido == "PRB" ~ "Direita",
        siglaPartido == "PMR" ~ "Direita",
        siglaPartido == "REPUBLICANOS" ~ "Direita",
        siglaPartido == "PRONA" ~ "Direita",
        siglaPartido == "PROS" ~ "Direita",
        siglaPartido == "PRP" ~ "Direita",
        siglaPartido == "PRTB" ~ "Direita",
        siglaPartido == "PSB" ~ "Centro",
        siglaPartido == "PSC" ~ "Direita",
        siglaPartido == "PSD" ~ "Direita",
        siglaPartido == "PSDB" ~ "Direita",
        siglaPartido == "PSL" ~ "Direita",
        siglaPartido == "PSOL" ~ "Esquerda",
        siglaPartido == "PST" ~ "Direita",
        siglaPartido == "PT" ~ "Esquerda",
        siglaPartido == "PTB" ~ "Direita",
        siglaPartido == "PTC" ~ "Direita",
        siglaPartido == "PTN" ~ "Direita",
        siglaPartido == "PV" ~ "Centro",
        siglaPartido == "REDE" ~ "Centro",
        siglaPartido == "SD" ~ "Direita",
        siglaPartido == "SDD" ~ "Direita",
        siglaPartido == "SOLIDARIEDADE" ~ "Direita",
        siglaPartido == "PRD" ~ "Direita",
        siglaPartido == "PMB" ~ "Direita",
        siglaPartido == "PATRI" ~ "Direita",
        siglaPartido == "UNIÃO" ~ "Direita"
      ),
      tamanho = case_when(
        # === LEGISLATURA 56 (2019-2023) ===
        # Grandes (50+)
        siglaPartido %in% c("PT", "PSL", "PL") & idLegislatura == 56 ~ "Grande",
        
        # Médios (21-49)
        siglaPartido %in% c("PP", "PSD", "MDB", "PR", "PSB", "PRB", "DEM", "PSDB", "PDT", "UNIÃO") & idLegislatura == 56 ~ "Médio",
        
        # Pequenos (5-20)
        siglaPartido %in% c("SOLIDARIEDADE", "PODE", "PSOL", "PTB", "PCdoB", "NOVO", "PPS", "PROS", "PSC", "AVANTE", "PHS") & idLegislatura == 56 ~ "Pequeno",
        
        # Nanicos (até 4)
        siglaPartido %in% c("PATRI", "PRP", "PV", "PMN", "PTC", "DC", "PPL", "REDE") & idLegislatura == 56 ~ "Nanico",
        
        # === LEGISLATURA 55 (2015-2019) ===
        # Grandes (50+)
        siglaPartido %in% c("PT", "PMDB", "PSDB", "MDB") & idLegislatura == 55 ~ "Grande",
        
        # Médios (21-49)
        siglaPartido %in% c("PP**", "PSD", "PSB", "PR", "PTB", "PRB", "DEM", "PDT", "UNIÃO") & idLegislatura == 55 ~ "Médio",
        
        # Pequenos (5-20)
        siglaPartido %in% c("SD", "PSC", "PROS", "PPS", "PCdoB", "PV", "PSOL", "PHS", "SOLIDARIEDADE", "PMB", "REDE") & idLegislatura == 55 ~ "Pequeno",
        
        # Nanicos (até 4)
        siglaPartido %in% c("PTN", "PRP", "PMN", "PEN", "PSDC", "PTC", "PTdoB", "PSL", "PRTB", "AVANTE", "PODE", "PATRI", "PPL") & idLegislatura == 55 ~ "Nanico",
        
        # === LEGISLATURA 54 (2011-2015) ===
        # Grandes (50+)
        siglaPartido %in% c("PT", "PMDB", "PSDB", "PSD") & idLegislatura == 54 ~ "Grande",
        
        # Médios (21-49)
        siglaPartido %in% c("PP**", "DEM", "PR", "PSB", "PDT", "PTB") & idLegislatura == 54 ~ "Médio",
        
        # Pequenos (5-20)
        siglaPartido %in% c("PSC", "PCdoB", "PV", "PPS", "PRB", "SD", "SDD", "PROS") & idLegislatura == 54 ~ "Pequeno",
        
        # Nanicos (até 4)
        siglaPartido %in% c("PMN", "PTdoB", "PSOL", "PHS", "PRTB", "PRP", "PTC", "PSL", "PEN", "PSDC") & idLegislatura == 54 ~ "Nanico",
        
        # === LEGISLATURA 53 (2007-2011) ===
        # Grandes (50+)
        siglaPartido %in% c("PMDB", "PT", "PSDB", "PFL", "DEM") & idLegislatura == 53 ~ "Grande",
        
        # Médios (21-49)
        siglaPartido %in% c("PP**", "PR", "PSB", "PDT", "PTB") & idLegislatura == 53 ~ "Médio",
        
        # Pequenos (5-20)
        siglaPartido %in% c("PPS", "PV", "PCdoB", "PSC") & idLegislatura == 53 ~ "Pequeno",
        
        # Nanicos (até 4)
        siglaPartido %in% c("PAN", "PSOL", "PMN", "PTC", "PHS", "PTdoB", "PRB", "PRTB") & idLegislatura == 53 ~ "Nanico",
        
        # === LEGISLATURA 52 (2003-2007) ===
        # Grandes (50+)
        siglaPartido %in% c("PT", "PFL", "PMDB", "PSDB") & idLegislatura == 52 ~ "Grande",
        
        # Médios (21-49)
        siglaPartido %in% c("PPB", "PTB", "PL*", "PSB", "PPS", "PP**", "PR") & idLegislatura == 52 ~ "Médio",
        
        # Pequenos (5-20)
        siglaPartido %in% c("PDT", "PCdoB", "PRONA", "PV") & idLegislatura == 52 ~ "Pequeno",
        
        # Nanicos (até 4)
        siglaPartido %in% c("PMN", "PSC", "PSL", "PHS", "PSOL", "PST", "PTC", "PMR", "PRB", "PRP") & idLegislatura == 52 ~ "Nanico",
        
        # === LEGISLATURA 51 (1999-2003) ===
        # Grandes (50+)
        siglaPartido %in% c("PFL", "PSDB", "PMDB", "PPB", "PT") & idLegislatura == 51 ~ "Grande",
        
        # Médios (21-49)
        siglaPartido %in% c("PTB", "PDT") & idLegislatura == 51 ~ "Médio",
        
        # Pequenos (5-20)
        siglaPartido %in% c("PSB", "PL", "PCdoB") & idLegislatura == 51 ~ "Pequeno",
        
        # Nanicos (até 4)
        siglaPartido %in% c("PPS", "PSD", "PMN", "PSC", "PRONA", "PSL", "PST", "PV") & idLegislatura == 51 ~ "Nanico",
        
        # === CASOS ESPECIAIS DE MUDANÇA DE CATEGORIA ===
        
        # PP: Médio até leg. 54, depois volta para Médio na 56
        siglaPartido == "PP" & idLegislatura %in% c(51, 52, 53, 54, 56) ~ "Médio",
        siglaPartido == "PP" & idLegislatura == 55 ~ "Médio", # 38 deputados
        
        # PSB: cresce de Pequeno para Médio
        siglaPartido == "PSB" & idLegislatura %in% c(51, 52) ~ "Pequeno", # 17 e 28
        siglaPartido == "PSB" & idLegislatura >= 53 ~ "Médio", # 28, 34, 34, 32
        
        # PDT: Médio nas primeiras, depois Médio mantido
        siglaPartido == "PDT" & idLegislatura %in% c(51, 52) ~ "Médio", # 25, 17
        siglaPartido == "PDT" & idLegislatura >= 53 ~ "Médio", # 23, 26, 20, 28
        
        # PTB: varia de Médio para Médio/Pequeno
        siglaPartido == "PTB" & idLegislatura %in% c(51, 52) ~ "Médio", # 31, 41
        siglaPartido == "PTB" & idLegislatura == 53 ~ "Médio", # 21
        siglaPartido == "PTB" & idLegislatura == 54 ~ "Médio", # 22
        siglaPartido == "PTB" & idLegislatura == 55 ~ "Médio", # 25
        siglaPartido == "PTB" & idLegislatura == 56 ~ "Pequeno", # 10
        
        # PL: Médio até fusão em 2006, depois vira PR
        siglaPartido == "PL" & idLegislatura %in% c(51, 52) ~ "Médio", # 12, 33
        siglaPartido == "PL" & idLegislatura == 53 ~ "Médio", # virou PR em 2006
        
        # PSDB: Grande nas primeiras, depois oscila
        siglaPartido == "PSDB" & idLegislatura %in% c(51, 52, 53) ~ "Grande", # 99, 63, 64
        siglaPartido == "PSDB" & idLegislatura >= 54 ~ "Grande", # 53, 54, mantém como Grande
        siglaPartido == "PSDB" & idLegislatura == 56 ~ "Médio", # 29 deputados
        
        # DEM/PFL: Grande até 2007, depois Médio
        siglaPartido == "PFL" & idLegislatura %in% c(51, 52, 53) ~ "Grande", # 105, 75, 62
        siglaPartido == "DEM" & idLegislatura >= 54 ~ "Médio", # 43, 21, 29
        
        # Mudanças de nome que devem manter classificação
        siglaPartido == "MDB" & idLegislatura == 56 ~ "Médio", # era PMDB
        siglaPartido == "REPUBLICANOS" & idLegislatura == 56 ~ "Médio", # era PRB
        siglaPartido == "CIDADANIA" & idLegislatura == 56 ~ "Pequeno", # era PPS
        siglaPartido == "PATRIOTA" & idLegislatura == 56 ~ "Nanico", # era PEN/PATRI
        siglaPartido == "PROGRESSISTAS" & idLegislatura == 56 ~ "Médio", # era PP
        siglaPartido == "AVANTE" & idLegislatura == 56 ~ "Pequeno", # era PTdoB
        siglaPartido == "MOBILIZA" & idLegislatura >= 57 ~ "Nanico", # era PMN
        
        # Padrão para partidos não listados ou legislaturas futuras
        TRUE ~ "Indefinido"
      )
      )
  }
  
  #Aplicação dos filtros
  {
    #Filtrar por data, verificar dados repetidos e em períodos muito próximos, plotar gráficos pela data
    #Adicionar se é reeleito ou não
    #Selecionar partidos que fundiram para adicionar como condição de não migração
    #Verificar se quando muda nome do partido considera-se migração, se sim, filtrar
    {
      migrações_filtrado <- migrações_filtrado |>
        mutate(
          # 1. Coluna apenas com o ano
          ano = as.numeric(substr(dataHora, 1, 4)),
          
          # 2. Coluna com a data completa (sem horário)
          data_completa = as.Date(substr(dataHora, 1, 10)),
          
          # 3. Coluna apenas com o horário
          horario = substr(dataHora, 12, 16),
          
          # 4. Coluna com data completa e horário (convertida para POSIXct)
          data_hora_completa = as.POSIXct(dataHora, format = "%Y-%m-%dT%H:%M")
        )
      
      migrações_teste <- migrações_filtrado |>
        filter(idLegislatura > 51 & idLegislatura < 57) |> 
        arrange(id, data_completa) |>
        group_by(id) |>
        mutate(
          # Calcular diferença em meses entre registros consecutivos
          diff_meses = ifelse(row_number() > 1,
                              as.numeric(difftime(as.Date(data_completa), 
                                                  lag(as.Date(data_completa)), 
                                                  units = "days")) / 30,
                              NA),
          
          # Identificar mudança de partido (excluindo fusões/mudanças de nome)
          mudou_partido_real = ifelse(row_number() > 1 & siglaPartido != lag(siglaPartido), 
                                      # Verificar se não é fusão/mudança de nome
                                      !(
                                        # === MUDANÇAS DE NOME ===
                                        # PSN -> PHS (2000)
                                        (lag(siglaPartido) == "PSN" & siglaPartido == "PHS") |
                                          # PRN -> PTC (2001) 
                                          (lag(siglaPartido) == "PRN" & siglaPartido == "PTC") |
                                          # PPB -> PP (2003)
                                          (lag(siglaPartido) == "PPB" & siglaPartido == "PP") |
                                          # PFL -> DEM (2007)
                                          (lag(siglaPartido) == "PFL" & siglaPartido == "DEM") |
                                          # PMR -> PRB (2009)
                                          (lag(siglaPartido) == "PMR" & siglaPartido == "PRB") |
                                          # PTN -> PODE (2017)
                                          (lag(siglaPartido) == "PTN" & siglaPartido == "PODE") |
                                          # PT do B -> Avante (2017)
                                          (lag(siglaPartido) == "PT do B" & siglaPartido == "Avante") |
                                          # PMDB -> MDB (2018)
                                          (lag(siglaPartido) == "PMDB" & siglaPartido == "MDB") |
                                          # PEN -> Patriota (2018)
                                          (lag(siglaPartido) == "PEN" & siglaPartido == "Patriota") |
                                          # PSDC -> DC (2018)
                                          (lag(siglaPartido) == "PSDC" & siglaPartido == "DC") |
                                          # PP -> Progressistas (2018)
                                          (lag(siglaPartido) == "PP" & siglaPartido == "Progressistas") |
                                          # PATRI -> Patriota (2019)
                                          (lag(siglaPartido) == "PATRI" & siglaPartido == "Patriota") |
                                          # PRB -> Republicanos (2019)
                                          (lag(siglaPartido) == "PRB" & siglaPartido == "Republicanos") |
                                          # PPS -> Cidadania (2019)
                                          (lag(siglaPartido) == "PPS" & siglaPartido == "Cidadania") |
                                          # PR -> PL (2019)
                                          (lag(siglaPartido) == "PR" & siglaPartido == "PL") |
                                          # PTC -> Agir (2022)
                                          (lag(siglaPartido) == "PTC" & siglaPartido == "Agir") |
                                          # PMN -> MOBILIZA (2023)
                                          (lag(siglaPartido) == "PMN" & siglaPartido == "MOBILIZA") |
                                          
                                          # === FUSÕES E INCORPORAÇÕES ===
                                          # PDS/PDC -> PPR (1993)
                                          (lag(siglaPartido) %in% c("PDS", "PDC") & siglaPartido == "PPR") |
                                          # PPR/PP -> PPB (1995)
                                          (lag(siglaPartido) %in% c("PPR", "PP") & siglaPartido == "PPB") |
                                          # PL/PRONA -> PR (2006)
                                          (lag(siglaPartido) %in% c("PL", "PRONA") & siglaPartido == "PR") |
                                          # DEM/PSL -> UNIÃO (2022)
                                          (lag(siglaPartido) %in% c("DEM", "PSL") & siglaPartido == "UNIÃO") |
                                          # Patriota/PTB -> PRD (2023)
                                          (lag(siglaPartido) %in% c("Patriota", "PTB") & siglaPartido == "PRD") |
                                          # PROS -> Solidariedade (2023 - incorporação)
                                          (lag(siglaPartido) == "PROS" & siglaPartido == "Solidariedade") |
                                          # PSC -> PODE (2023 - incorporação)
                                          (lag(siglaPartido) == "PSC" & siglaPartido == "PODE") |
                                          # PHS -> PODE (2019 - incorporação)
                                          (lag(siglaPartido) == "PHS" & siglaPartido == "PODE") |
                                          # PRP -> Patriota (2019 - incorporação)
                                          (lag(siglaPartido) == "PRP" & siglaPartido == "Patriota") |
                                          # PPL -> PC do B (2019 - incorporação)
                                          (lag(siglaPartido) == "PPL" & siglaPartido == "PC do B") |
                                          # PSD (antigo) -> PTB (2003 - incorporação)
                                          (lag(siglaPartido) == "PSD" & siglaPartido == "PTB")
                                      ), 
                                      FALSE),
          
          # Condição robusta para não retorno (A->B->A em período curto)
          nao_retorno = ifelse(row_number() > 2,
                               # Condição original: não volta para o mesmo partido de 2 posições atrás
                               siglaPartido != lag(siglaPartido, 2) |
                                 # OU se voltou, mas não foi em período curto (mais de 3 meses entre as mudanças)
                                 (siglaPartido == lag(siglaPartido, 2) & 
                                    (is.na(lag(diff_meses)) | lag(diff_meses) > 1 | diff_meses > 1)),
                               TRUE),
          
          # Manter diff_meses como condição separada (período curto entre mudanças consecutivas)
          mudanca_periodo_curto = abs(diff_meses - 1) <= 3,
          
          # Extrair mês e ano
          mes = as.numeric(substr(data_completa, 6, 7)),
          ano = as.numeric(substr(data_completa, 1, 4)),
          
          # Identificar anos eleitorais (2002, 2006, 2010, 2014, 2018, 2022, etc.)
          ano_eleitoral = (ano >= 2002) & ((ano - 2002) %% 4 == 0),
          
          # Identificar anos de assunção de mandato (2003, 2007, 2011, 2015, 2019, 2023, etc.)
          ano_assuncao = (ano >= 2003) & ((ano - 2003) %% 4 == 0),
          
          # Identificar janela (março a abril em anos eleitorais)
          na_janela = mes >= 3 & mes <= 4 & ano_eleitoral == TRUE,
          
          # Classificar migração
          SituaçãoMigrantes = case_when(
            situaçãoReclassificada == "Migrou" & 
              mudou_partido_real == TRUE & 
              #mudanca_periodo_curto == TRUE & 
              nao_retorno == TRUE & 
              na_janela == TRUE ~ "Migrou na janela",
            
            situaçãoReclassificada == "Migrou" & 
              mudou_partido_real == TRUE & 
              #mudanca_periodo_curto == TRUE & 
              nao_retorno == TRUE & 
              na_janela == FALSE & 
              ano_assuncao == TRUE ~ "Migrou ao assumir",
            
            situaçãoReclassificada == "Migrou" & 
              mudou_partido_real == TRUE & 
              #mudanca_periodo_curto == TRUE & 
              nao_retorno == TRUE & 
              na_janela == FALSE & 
              ano_assuncao == FALSE ~ "Migrou fora da janela",
            
            TRUE ~ "Não Migrou"
          ),
          
          # Criar coluna Migrantes
          Migrantes = case_when(
            SituaçãoMigrantes != "Não Migrou" ~ "Migrante",
            SituaçãoMigrantes == "Não Migrou" ~ "Não Migrante"
          )
        ) |>
        select(-mudou_partido_real, -mudanca_periodo_curto, -nao_retorno, -na_janela, -ano_eleitoral, -ano_assuncao) |>
        ungroup()
    }
    migrações_teste$nome <- str_to_title(migrações_teste$nome, "LATIN-ASCII")
    migrações_teste <- unique(migrações_teste)
    write_xlsx(migrações_teste, "versão do livro jamovi.xlsx")
    
    migrantes <- migrações_teste |> 
      group_by(nome, idLegislatura) |> 
      summarise(
        Migrantes = ifelse(any(Migrantes == "Migrante"), "Sim", "Não"),
        .groups = "drop"
      )
    
    migrantes$nome <- stri_trans_general(migrantes$nome, "LATIN-ASCII")
    
    write_xlsx(migrantes, "migrantes.xlsx")
    
    # Preparar dados: extrair ano da data_completa e filtrar apenas migrações
    dados_migracoes <- migrações_teste |>
      filter(Migrantes == "Migrante") |>  # apenas migrações reais
      mutate(
        ano_migracao = year(data_completa)  # extrair ano da data
      )
  }
  
  {
    # === GRÁFICO 1: MIGRAÇÕES POR TAMANHO DE PARTIDO ===
    grafico_tamanho <- ggplot(
      dados_migracoes |>
        group_by(ano_migracao, tamanho) |>
        summarise(n_migracoes = n(), .groups = "drop") |>
        filter(!is.na(tamanho) & tamanho != "Indefinido"), # remover casos indefinidos
      aes(x = ano_migracao, y = n_migracoes, color = tamanho, group = tamanho)
    ) +
      geom_line(size = 1.5) +
      geom_point(shape = 21, size = 3.5, aes(fill = tamanho), color = "white", stroke = 1.2) +
      geom_text(aes(label = n_migracoes), 
                vjust = -1.5, size = 3, family = "Garamond", color = "black") +
      scale_x_continuous(
        breaks = seq(2002, 2022, by = 1),
        expand = c(0.04, 0.04)
      ) +
      scale_y_continuous(
        breaks = scales::pretty_breaks(n = 8),
        expand = c(0.1, 0.1)
      ) +
      scale_color_manual(values = c(
        "Grande"  = "#d73027",
        "Médio"   = "#fc8d59", 
        "Pequeno" = "#4575b4",
        "Nanico"  = "#91bfdb"
      )) +
      scale_fill_manual(values = c(
        "Grande"  = "#d73027",
        "Médio"   = "#fc8d59",
        "Pequeno" = "#4575b4", 
        "Nanico"  = "#91bfdb"
      )) +
      labs(
        title = "Evolução das Migrações Partidárias por Tamanho de Partido",
        subtitle = "Número absoluto de migrações por ano",
        x = "Ano",
        y = "Número de Migrações",
        color = expression(bold("Tamanho do Partido")),
        fill = expression(bold("Tamanho do Partido"))
      ) +
      theme_classic(base_family = "Garamond") +
      theme(
        plot.title = element_text(face = "bold", size = 16, margin = margin(b = 10)),
        plot.subtitle = element_text(size = 12, margin = margin(b = 20)),
        axis.title = element_text(size = 12, margin = margin(b = 10)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "bottom"
      ) +
      guides(fill = "none") # remover legenda duplicada
    
    # === GRÁFICO 2: MIGRAÇÕES POR IDEOLOGIA PARTIDÁRIA ===
    grafico_ideologia <- ggplot(
      dados_migracoes |>
        group_by(ano_migracao, Ideologia) |>
        summarise(n_migracoes = n(), .groups = "drop") |>
        filter(!is.na(Ideologia)), # remover casos sem ideologia
      aes(x = ano_migracao, y = n_migracoes, color = Ideologia, group = Ideologia)
    ) +
      geom_line(size = 1.5) +
      geom_point(shape = 25, size = 3.5, aes(fill = Ideologia), show.legend = FALSE) +
      geom_text(aes(label = n_migracoes), 
                vjust = -1.7, size = 3.5, family = "Garamond", color = "black") +
      scale_x_continuous(
        breaks = seq(1999, 2023, by = 2),
        expand = c(0.02, 0.02)
      ) +
      scale_y_continuous(
        breaks = scales::pretty_breaks(n = 8),
        expand = c(0.05, 0.05)
      ) +
      scale_color_manual(values = c(
        "Esquerda" = "#e2030a",
        "Centro"   = "#0a6b00", 
        "Direita"  = "#0073e6"
      )) +
      scale_fill_manual(values = c(
        "Esquerda" = "#e2030a",
        "Centro"   = "#0a6b00",
        "Direita"  = "#0073e6"
      )) +
      labs(
        title = "Evolução das Migrações Partidárias por Ideologia",
        subtitle = "Número absoluto de migrações por ano",
        x = "Ano",
        y = "Número de Migrações",
        color = expression(bold("Ideologia"))
      ) +
      theme_classic(base_family = "Garamond") +
      theme(
        plot.title = element_text(face = "bold", size = 16, margin = margin(b = 10)),
        plot.subtitle = element_text(size = 12, margin = margin(b = 20)),
        axis.title = element_text(size = 12, margin = margin(b = 10)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "bottom"
      )
    
    # === GRÁFICO 3: PERCENTUAIS POR TAMANHO (alternativa) ===
    grafico_tamanho_pct <- ggplot(
      dados_migracoes |>
        group_by(ano_migracao, tamanho) |>
        summarise(n = n(), .groups = "drop") |>
        filter(!is.na(tamanho) & tamanho != "Indefinido") |>
        group_by(ano_migracao) |>
        mutate(porcentagem = 100 * n / sum(n)),
      aes(x = ano_migracao, y = porcentagem, color = tamanho, group = tamanho)
    ) +
      geom_line(size = 1.5) +
      geom_point(shape = 21, size = 3.5, aes(fill = tamanho), color = "white", stroke = 1.2) +
      geom_text(aes(label = paste0(round(porcentagem, 1), "%")), 
                vjust = -1.5, size = 3, family = "Garamond", color = "black") +
      scale_x_continuous(
        breaks = seq(2002, 2022, by = 2),
        expand = c(0.02, 0.02)
      ) +
      scale_y_continuous(
        limits = c(0, 100),
        breaks = seq(0, 100, by = 20),
        expand = c(0.02, 0.02)
      ) +
      scale_color_manual(values = c(
        "Grande"  = "#d73027",
        "Médio"   = "#fc8d59",
        "Pequeno" = "#4575b4",
        "Nanico"  = "#91bfdb"
      )) +
      scale_fill_manual(values = c(
        "Grande"  = "#d73027", 
        "Médio"   = "#fc8d59",
        "Pequeno" = "#4575b4",
        "Nanico"  = "#91bfdb"
      )) +
      labs(
        title = "Percentual das Migrações por Tamanho de Partido",
        subtitle = "Distribuição percentual das migrações por ano",
        x = "Ano",
        y = "Percentual de Migrações (%)",
        color = expression(bold("Tamanho do Partido"))
      ) +
      theme_classic(base_family = "Garamond") +
      theme(
        plot.title = element_text(face = "bold", size = 16, margin = margin(b = 10)),
        plot.subtitle = element_text(size = 12, margin = margin(b = 20)),
        axis.title = element_text(size = 12, margin = margin(b = 10)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "bottom"
      ) +
      guides(fill = "none")
    
    #Categorização de 3 tamanhos de partido
    {
      tamanho = case_when(
        # === PARTIDOS GRANDES (50+ cadeiras) ===
        # PT - Sempre grande desde 2003 (91 dep) até hoje
        siglaPartido == "PT" ~ "Grande",
        
        # PMDB/MDB - Sempre grande, maior bancada em várias legislaturas
        siglaPartido %in% c("PMDB", "MDB") ~ "Grande",
        
        # PSDB - Grande até 2018, depois médio
        siglaPartido == "PSDB" & idLegislatura <= 55 ~ "Grande", # até 2018 (54 dep em 2015)
        siglaPartido == "PSDB" & idLegislatura >= 56 ~ "Médio",  # 2019+ (29 dep em 2019)
        
        # PFL/DEM - Grande até 2010, depois médio, depois pequeno
        siglaPartido %in% c("PFL", "DEM") & idLegislatura <= 53 ~ "Grande", # até 2010
        siglaPartido == "DEM" & idLegislatura %in% c(54, 55) ~ "Médio",     # 2011-2018
        siglaPartido == "DEM" & idLegislatura >= 56 ~ "Pequeno",            # 2019+
        
        # PP/PPB/Progressistas - Médio/Grande dependendo da legislatura
        siglaPartido %in% c("PP", "PPB", "Progressistas") & idLegislatura <= 52 ~ "Grande", # até 2006
        siglaPartido %in% c("PP", "PPB", "Progressistas") & idLegislatura >= 53 ~ "Médio",  # 2007+
        
        # PL (atual) - Grande desde 2019
        siglaPartido == "PL" & idLegislatura >= 56 ~ "Grande", # 2019+ (76 dep em 2023)
        
        # === PARTIDOS MÉDIOS (21-49 cadeiras) ===
        # PSD - Médio desde criação (2011)
        siglaPartido == "PSD" & idLegislatura >= 54 ~ "Médio", # 2011+
        
        # PSB - Tradicionalmente médio
        siglaPartido == "PSB" ~ "Médio",
        
        # PR/PL (antigo) - Médio até virar PL atual
        siglaPartido %in% c("PR", "PL") & idLegislatura <= 55 ~ "Médio", # até 2018
        
        # PTB - Médio na maioria das legislaturas
        siglaPartido == "PTB" ~ "Médio",
        
        # PDT - Médio tradicionalmente
        siglaPartido == "PDT" ~ "Médio",
        
        # UNIÃO - Médio (fusão DEM+PSL em 2022)
        siglaPartido == "UNIÃO" ~ "Médio",
        
        # PODE/PTN - Médio
        siglaPartido %in% c("PODE", "PTN") ~ "Médio",
        
        # Republicanos/PRB - Médio
        siglaPartido %in% c("Republicanos", "PRB") ~ "Médio",
        
        # Solidariedade - Médio
        siglaPartido == "Solidariedade" ~ "Médio",
        
        # === PARTIDOS PEQUENOS (5-20 cadeiras) ===
        # PSL - Pequeno até 2018, depois grande em 2019, depois pequeno
        siglaPartido == "PSL" & idLegislatura <= 55 ~ "Pequeno", # até 2018
        siglaPartido == "PSL" & idLegislatura == 56 ~ "Grande",  # 2019 (52 dep)
        siglaPartido == "PSL" & idLegislatura >= 57 ~ "Pequeno", # 2023+ (pós-fusão)
        
        # Partidos tradicionalmente pequenos
        siglaPartido %in% c("PSOL", "PC do B", "PCdoB", "PV", "REDE", "NOVO") ~ "Pequeno",
        siglaPartido %in% c("Cidadania", "PPS", "Avante", "PT do B") ~ "Pequeno",
        siglaPartido %in% c("Patriota", "PEN", "PATRI") ~ "Pequeno",
        siglaPartido %in% c("DC", "PSDC", "Agir", "PTC") ~ "Pequeno",
        siglaPartido %in% c("MOBILIZA", "PMN", "PMB") ~ "Pequeno",
        siglaPartido %in% c("PROS", "PSC", "PHS", "PRP", "PPL") ~ "Pequeno",
        siglaPartido %in% c("PRTB", "PST", "PRONA") ~ "Pequeno",
        
        # Casos específicos históricos
        siglaPartido == "PPB" & idLegislatura == 52 ~ "Médio", # Mantendo seu caso específico
        
        # === PARTIDOS EXTINTOS/TRANSFORMADOS ===
        # Partidos que mudaram de nome/se fundiram
        siglaPartido %in% c("PRN", "PSN", "PMR") ~ "Pequeno",
        
        # Casos não especificados - assumir pequeno
        TRUE ~ "Pequeno"
      )
      }
    
    # Preparação dos dados
    migrações_plot <- migrações_teste |>
      filter(Migrantes == "Migrante") |>  # Apenas migrantes
      mutate(
        ano = year(data_completa),  # Extrair ano da data
        mes_ano = floor_date(data_completa, "month")  # Para granularidade mensal
      )
    
    # ============================================================================
    # 1. GRÁFICO LONGITUDINAL POR TAMANHO DE PARTIDO (Line Plot)
    # ============================================================================
    
    graph_tamanho <- ggplot(
      migrações_plot |>
        group_by(ano, tamanho) |>
        summarise(n = n(), .groups = "drop") |>
        group_by(ano) |>
        mutate(porcentagem = 100 * n / sum(n)),
      aes(x = ano, y = porcentagem, color = tamanho, group = tamanho)
    ) +
      geom_line(size = 1.5) +
      scale_x_continuous(
        limits = c(1995, 2026),
        breaks = seq(1998, 2026, by = 4),
        expand = c(0.01, 0.01)
      ) +
      scale_y_continuous(
        limits = c(0, 100),
        breaks = seq(0, 100, by = 10)
      ) +
      geom_point(shape = 25, size = 3.5, aes(fill = tamanho), show.legend = FALSE) +
      geom_text(aes(label = paste0(round(porcentagem, 1), "%")), 
                vjust = -1.7, size = 3.5, family = "Garamond", color = "black") +
      scale_color_manual(values = c(
        "Nanico" = "#ff6b6b",
        "Pequeno" = "#ffa726", 
        "Médio" = "#66bb6a",
        "Grande" = "#42a5f5"
      )) +
      scale_fill_manual(values = c(
        "Nanico" = "#ff6b6b",
        "Pequeno" = "#ffa726", 
        "Médio" = "#66bb6a",
        "Grande" = "#42a5f5"
      )) +
      labs(
        title = "Percentual de Migrações por Tamanho de Partido ao Longo do Tempo",
        x = "Ano",
        y = "Percentual de Migrações (%)",
        color = expression(bold("Tamanho"))
      ) +
      theme_classic(base_family = "Garamond") +
      theme(
        plot.title = element_text(face = "bold", size = 16, margin = margin(b = 20)),
        axis.title = element_text(size = 12, margin = margin(b = 10)),
        legend.title = element_text(face = "bold"),
        legend.text = element_text(size = 11)
      )
    
    # ============================================================================
    # 2. GRÁFICO LONGITUDINAL POR IDEOLOGIA PARTIDÁRIA (Line Plot)
    # ============================================================================
    
    graph_ideologia <- ggplot(
      migrações_plot |>
        group_by(ano, Ideologia) |>
        summarise(n = n(), .groups = "drop") |>
        group_by(ano) |>
        mutate(porcentagem = 100 * n / sum(n)),
      aes(x = ano, y = porcentagem, color = Ideologia, group = Ideologia)
    ) +
      geom_line(size = 1.5) +
      scale_x_continuous(
        limits = c(1995, 2026),
        breaks = seq(1998, 2026, by = 4),
        expand = c(0.01, 0.01)
      ) +
      scale_y_continuous(
        limits = c(0, 80),
        breaks = seq(0, 80, by = 10)
      ) +
      geom_point(shape = 25, size = 3.5, aes(fill = Ideologia), show.legend = FALSE) +
      geom_text(aes(label = paste0(round(porcentagem, 1), "%")), 
                vjust = -1.7, size = 3.5, family = "Garamond", color = "black") +
      scale_color_manual(values = c(
        "Esquerda" = "#e2030a",
        "Centro"   = "#0a6b00",
        "Direita"  = "#0073e6"
      )) +
      scale_fill_manual(values = c(
        "Esquerda" = "#e2030a",
        "Centro"   = "#0a6b00",
        "Direita"  = "#0073e6"
      )) +
      labs(
        title = "Percentual de Migrações por Ideologia Partidária ao Longo do Tempo",
        x = "Ano",
        y = "Percentual de Migrações (%)",
        color = expression(bold("Ideologia"))
      ) +
      theme_classic(base_family = "Garamond") +
      theme(
        plot.title = element_text(face = "bold", size = 16, margin = margin(b = 20)),
        axis.title = element_text(size = 12, margin = margin(b = 10)),
        legend.title = element_text(face = "bold"),
        legend.text = element_text(size = 11)
      )
    
    # ============================================================================
    # 3. GRÁFICOS DE BARRAS MENSAIS POR LEGISLATURA
    # ============================================================================
    
    # Função para criar gráfico mensal por legislatura
    criar_grafico_mensal <- function(leg_num) {
      dados_leg <- migrações_plot |>
        filter(idLegislatura == leg_num) |>
        group_by(mes_ano) |>
        summarise(n_migracoes = n(), .groups = "drop")
      
      ggplot(dados_leg, aes(x = mes_ano, y = n_migracoes)) +
        geom_col(fill = "#42a5f5", alpha = 0.8, width = 20) +
        scale_x_date(
          date_labels = "%b\n%Y",
          date_breaks = "6 months",
          expand = c(0.02, 0.02)
        ) +
        scale_y_continuous(
          breaks = function(x) seq(0, max(x), by = max(1, ceiling(max(x)/10)))
        ) +
        geom_text(aes(label = n_migracoes), 
                  vjust = -0.5, size = 3, family = "Garamond") +
        labs(
          title = paste0("Migrações Mensais - Legislatura ", leg_num),
          x = "Mês/Ano",
          y = "Número de Migrações"
        ) +
        theme_classic(base_family = "Garamond") +
        theme(
          plot.title = element_text(face = "bold", size = 14, margin = margin(b = 20)),
          axis.title = element_text(size = 11),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 9)
        )
    }
    
    # Criar gráficos para cada legislatura
    legislaturas <- sort(unique(migrações_plot$idLegislatura))
    
    # Lista para armazenar todos os gráficos
    graficos_mensais <- list()
    
    for(leg in legislaturas) {
      graficos_mensais[[paste0("leg_", leg)]] <- criar_grafico_mensal(leg)
    }
    
    # Exemplo de como exibir os gráficos
    # graficos_mensais$leg_51  # Legislatura 51
    # graficos_mensais$leg_52  # Legislatura 52
    # etc...
    
    # ============================================================================
    # EXIBIR OS GRÁFICOS PRINCIPAIS
    # ============================================================================
    
    print(graph_tamanho)
    print(graph_ideologia)
    
    # Para visualizar um gráfico mensal específico (exemplo: legislatura 54)
    if("leg_52" %in% names(graficos_mensais)) {
      print(graficos_mensais$leg_52)
    }
    if("leg_53" %in% names(graficos_mensais)) {
      print(graficos_mensais$leg_53)
    }
    if("leg_54" %in% names(graficos_mensais)) {
      print(graficos_mensais$leg_54)
    }
    if("leg_55" %in% names(graficos_mensais)) {
      print(graficos_mensais$leg_55)
    }
    if("leg_56" %in% names(graficos_mensais)) {
      print(graficos_mensais$leg_56)
    }
    
    # ============================================================================
    # ESTATÍSTICAS COMPLEMENTARES
    # ============================================================================
    
    # Resumo das migrações por legislatura
    resumo_legislaturas <- migrações_plot |>
      group_by(idLegislatura) |>
      summarise(
        total_migracoes = n(),
        periodo_inicio = min(data_completa),
        periodo_fim = max(data_completa),
        .groups = "drop"
      )
    
    print("Resumo das migrações por legislatura:")
    print(resumo_legislaturas)
  }
  
  {
    deps_migrantes <- migrações_teste |> 
      filter(Migrantes == "Migrante") |> 
      select(id, siglaPartido, siglaUf, idLegislatura, Migrantes)
    
    deps_não_migrantes <- migrações_teste |> 
      filter(Migrantes == "Não Migrante") |> 
      select(id, siglaPartido, siglaUf, idLegislatura, Migrantes)
    
    deps_não_migrantes <-  unique(deps_não_migrantes)
    
    deps_verificação <- bind_rows(deps_migrantes, deps_não_migrantes)
    
    deps_verificação_corrigido <- deps_verificação %>%
      arrange(id, idLegislatura) %>%
      group_by(id, idLegislatura) %>%
      mutate(
        # Debug: mostrar o que está acontecendo
        linha = row_number(),
        partido_anterior = lag(siglaPartido),
        mudou_partido_simples = linha > 1 & siglaPartido != lag(siglaPartido),
        
        # Aplicar lógica de fusão completa
        mudou_partido_real = ifelse(linha > 1 & siglaPartido != lag(siglaPartido), 
                                    # Verificar se NÃO é fusão/mudança de nome
                                    !(
                                      # === MUDANÇAS DE NOME ===
                                      (lag(siglaPartido) == "PSN" & siglaPartido == "PHS") |
                                        (lag(siglaPartido) == "PRN" & siglaPartido == "PTC") |
                                        (lag(siglaPartido) == "PPB" & siglaPartido == "PP") |
                                        (lag(siglaPartido) == "PFL" & siglaPartido == "DEM") |
                                        (lag(siglaPartido) == "PMR" & siglaPartido == "PRB") |
                                        (lag(siglaPartido) == "PTN" & siglaPartido == "PODE") |
                                        (lag(siglaPartido) == "PT do B" & siglaPartido == "Avante") |
                                        (lag(siglaPartido) == "PMDB" & siglaPartido == "MDB") |
                                        (lag(siglaPartido) == "PEN" & siglaPartido == "Patriota") |
                                        (lag(siglaPartido) == "PSDC" & siglaPartido == "DC") |
                                        (lag(siglaPartido) == "PP" & siglaPartido == "Progressistas") |
                                        (lag(siglaPartido) == "PATRI" & siglaPartido == "Patriota") |
                                        (lag(siglaPartido) == "PRB" & siglaPartido == "Republicanos") |
                                        (lag(siglaPartido) == "PPS" & siglaPartido == "Cidadania") |
                                        (lag(siglaPartido) == "PR" & siglaPartido == "PL") |
                                        (lag(siglaPartido) == "PTC" & siglaPartido == "Agir") |
                                        (lag(siglaPartido) == "PMN" & siglaPartido == "MOBILIZA") |
                                        
                                        # === FUSÕES E INCORPORAÇÕES ===
                                        (lag(siglaPartido) %in% c("PDS", "PDC") & siglaPartido == "PPR") |
                                        (lag(siglaPartido) %in% c("PPR", "PP") & siglaPartido == "PPB") |
                                        (lag(siglaPartido) %in% c("PL", "PRONA") & siglaPartido == "PR") |
                                        (lag(siglaPartido) %in% c("DEM", "PSL") & siglaPartido == "UNIÃO") |
                                        (lag(siglaPartido) %in% c("Patriota", "PTB") & siglaPartido == "PRD") |
                                        (lag(siglaPartido) == "PROS" & siglaPartido == "Solidariedade") |
                                        (lag(siglaPartido) == "PSC" & siglaPartido == "PODE") |
                                        (lag(siglaPartido) == "PHS" & siglaPartido == "PODE") |
                                        (lag(siglaPartido) == "PRP" & siglaPartido == "Patriota") |
                                        (lag(siglaPartido) == "PPL" & siglaPartido == "PC do B") |
                                        (lag(siglaPartido) == "PSD" & siglaPartido == "PTB")
                                    ), 
                                    FALSE),
        
        # Status original
        status_original = Migrantes,
        
        # Nova classificação
        Migrantes_novo = case_when(
          status_original == "Migrante" ~ "Migrante",
          mudou_partido_real == TRUE ~ "Migrante",
          TRUE ~ status_original
        ),
        
        # Foi reclassificado?
        foi_reclassificado = status_original != Migrantes_novo
      ) %>%
      ungroup()
    
    deps_não_migrantes$nome <- stri_trans_general(deps_não_migrantes$nome, "LATIN-ASCII")
    deps_não_migrantes <- unique(deps_não_migrantes)
  }
  
  #Verificação das médias das bancadas
  {
    bancadas_2003 <- c(90, 75, 69, 63, 43, 41, 33, 28, 21, 17, 12, 6, 6, 2, 1, 1)
    bancadas_2007 <- c(90, 83, 64, 62, 41, 34, 28, 23, 21, 17, 13, 13, 7, 4, 3, 3, 3, 2, 1, 1)
    bancadas_2011 <- c(87, 78, 53, 44, 43, 40, 34, 26, 22, 17, 15, 14, 12, 8, 4, 4, 3, 2, 2, 2, 1, 1)
    bancadas_2015 <- c(69, 65, 54, 38, 36, 34, 34, 25, 21, 21, 20, 15, 13, 11, 10, 10, 8, 5, 5, 4, 3, 3, 2, 2, 2, 1, 1, 1)
    bancadas_2019 <- c(54, 51, 38, 35, 34, 33, 32, 30, 29, 29, 28, 13, 11, 10, 10, 9, 8, 8, 8, 8, 7, 6, 5, 4, 4, 3, 2, 1, 1, 1)
    bancadas_posse <- c(bancadas_2003, bancadas_2007, bancadas_2011, bancadas_2015, bancadas_2019)
    
    quantile(bancadas_posse)
    median(bancadas_posse)
    mean(bancadas_posse)
    mean(Winsorize(bancadas_2019, probs = c(0.7, 1)))
  }
  
  {migrações_filtrado <- migrações_filtrado |>
      mutate(
        # 1. Coluna apenas com o ano
        ano = as.numeric(substr(dataHora, 1, 4)),
        
        # 2. Coluna com a data completa (sem horário)
        data_completa = as.Date(substr(dataHora, 1, 10)),
        
        # 3. Coluna apenas com o horário
        horario = substr(dataHora, 12, 16),
        
        # 4. Coluna com data completa e horário (convertida para POSIXct)
        data_hora_completa = as.POSIXct(dataHora, format = "%Y-%m-%dT%H:%M")
      )
    
    migrações_teste <- migrações_filtrado |>
      arrange(id, data_completa) |>
      group_by(id) |>
      mutate(
        # Calcular diferença em meses entre registros consecutivos
        diff_meses = ifelse(row_number() > 1,
                            as.numeric(difftime(as.Date(data_completa), 
                                                lag(as.Date(data_completa)), 
                                                units = "days")) / 30,
                            NA),
        
        # Identificar mudança de partido
        mudou_partido = ifelse(row_number() > 1 & siglaPartido != lag(siglaPartido), TRUE, FALSE),
        
        # Verificar se não é retorno ao mesmo partido
        nao_retorno = ifelse(row_number() > 2,
                             siglaPartido != lag(siglaPartido, 2),
                             TRUE),
        
        
        # Extrair mês e ano
        mes = as.numeric(substr(data_completa, 6, 7)),
        ano = as.numeric(substr(data_completa, 1, 4)),
        
        # Identificar janela (fevereiro a maio)
        na_janela = mes >= 3 & mes <= 4 & ano <= 2010,
        
        # Identificar anos eleitorais (2002, 2006, 2010, 2014, 2018, 2022, etc.)
        ano_eleitoral = (ano >= 2002) & ((ano - 2002) %% 4 == 0),
        
        # Identificar anos de assunção de mandato (2003, 2007, 2011, 2015, 2019, 2023, etc.)
        ano_assuncao = (ano >= 2003) & ((ano - 2003) %% 4 == 0),
        
        # Classificar migração
        SituaçãoMigrantes = case_when(
          situaçãoReclassificada == "Migrou" & 
            mudou_partido == TRUE & 
            abs(diff_meses - 1) <= 1 & 
            nao_retorno == TRUE & 
            na_janela == TRUE ~ "Migrou na janela",
          
          situaçãoReclassificada == "Migrou" & 
            mudou_partido == TRUE & 
            abs(diff_meses - 1) <= 1 & 
            nao_retorno == TRUE & 
            na_janela == FALSE & 
            ano_assuncao == FALSE & 
            ano_eleitoral == FALSE ~ "Migrou fora da janela",
          
          situaçãoReclassificada == "Migrou" & 
            mudou_partido == TRUE & 
            abs(diff_meses - 1) <= 1 & 
            nao_retorno == TRUE & 
            #na_janela == FALSE & 
            ano_assuncao == TRUE ~ "Migrou ao assumir",
          
          situaçãoReclassificada == "Migrou" & 
            mudou_partido == TRUE & 
            abs(diff_meses - 1) <= 1 & 
            nao_retorno == TRUE & 
            #na_janela == FALSE & 
            ano_eleitoral == TRUE ~ "Migrou antes da eleição",
          
          TRUE ~ "Não Migrou"
        ),
        
        # Criar coluna Migrantes
        Migrantes = case_when(
          SituaçãoMigrantes != "Não Migrou" ~ "Migrante",
          SituaçãoMigrantes == "Não Migrou" ~ "Não Migrante"
        )
      ) |>
      select(-diff_meses, -mudou_partido, -nao_retorno, -mes, -ano, -na_janela, -ano_eleitoral, -ano_assuncao) |>
      ungroup()}
}

# Correção do Capítulo
{
  Banco_Nilton_Codato <- BD_Sainz_Codato_ClassePolíticadoBrasil_INCT_ReDem_2025_1_
  rm(BD_Sainz_Codato_ClassePolíticadoBrasil_INCT_ReDem_2025_1_)
  
  banco_arrumado <- right_join(banco, Banco_Nilton_Codato, by = "ID_BD")
  banco_arrumado <- banco_arrumado %>% 
    select(ID_BD, Número_CPF, Nome_Deputado.x, Nome_Urna_Deputado, Nome_Social_Deputado,
           Data_Nascimento, Idade_Posse, Gênero, Cor_Raça_TSE, Estado_Civil,
           Ano_Eleição.x, idLegislatura, Sigla_Partido.x, Ideologia.x, Tamanho, Migrantes, Reeleitos,
           Sigla_Estado, Região, Escolaridade, Ocupação_Profissional,
           Resultado_Eleição, Tipo_agremiação, Nome_coligação, Composição_Coligação) %>% 
    rename(Nome_Deputado = Nome_Deputado.x, Ano_Eleição = Ano_Eleição.x,
           Sigla_Partido = Sigla_Partido.x, Ideologia = Ideologia.x)
  
  anyNA(banco_arrumado)
  
  # Modelagem com gênero
  {
  modelo_gênero <- banco_arrumado |>
    filter(Migrantes != "Primeiro mandato" & Ideologia != "Esquerda")
  modelo_gênero$Migrantes <- as.factor(modelo_gênero$Migrantes)
  
  regressão_gênero_ideo <- glm(Migrantes ~ Gênero * Ideologia + Tamanho, 
        data = modelo_gênero, family = "binomial")
  summary(regressão_gênero_ideo)
  
  regressão_gênero_tam <- glm(Migrantes ~ Gênero * Tamanho + Ideologia, 
                               data = modelo_gênero, family = "binomial")
  summary(regressão_gênero_tam)
  
  regressão_gênero <- glm(Migrantes ~ Gênero + Tamanho + Ideologia, 
                          data = modelo_gênero, family = "binomial")
  summary(regressão_gênero)
  
  regressão_gênero_final <- glm(Migrantes ~ Gênero * Tamanho * Ideologia, 
                          data = modelo_gênero, family = "binomial")
  summary(regressão_gênero_final)
  }
  
  # Modelagem com raça
  {
    modelo_raça <- banco_arrumado |>
      filter(Migrantes != "Primeiro mandato" & Ideologia != "Esquerda")
    modelo_raça$Migrantes <- as.factor(modelo_raça$Migrantes)
    
    regressão_raça_ideo <- glm(Migrantes ~ Cor_Raça_TSE * Ideologia + Tamanho, 
                                 data = modelo_raça, family = "binomial")
    summary(regressão_raça_ideo)
    
    regressão_raça_tam <- glm(Migrantes ~ Cor_Raça_TSE * Tamanho + Ideologia, 
                                data = modelo_raça, family = "binomial")
    summary(regressão_raça_tam)
    
    regressão_raça <- glm(Migrantes ~ Cor_Raça_TSE + Tamanho + Ideologia, 
                            data = modelo_raça, family = "binomial")
    summary(regressão_raça)
    
    regressão_raça_final <- glm(Migrantes ~ Cor_Raça_TSE * Tamanho * Ideologia, 
                                  data = modelo_raça, family = "binomial")
    summary(regressão_raça_final)
  }
  
  # Modelagem com idade
  {
    modelo_idade <- banco_arrumado |>
      filter(Migrantes != "Primeiro mandato" & Ideologia != "Esquerda")
    modelo_idade$Migrantes <- as.factor(modelo_idade$Migrantes)
    
    regressão_idade_ideo <- glm(Migrantes ~ Idade_Posse * Ideologia + Tamanho, 
                               data = modelo_idade, family = "binomial")
    summary(regressão_idade_ideo)
    
    regressão_idade_tam <- glm(Migrantes ~ Idade_Posse * Tamanho + Ideologia, 
                              data = modelo_idade, family = "binomial")
    summary(regressão_idade_tam)
    
    regressão_idade <- glm(Migrantes ~ Idade_Posse + Tamanho + Ideologia, 
                          data = modelo_idade, family = "binomial")
    summary(regressão_idade)
    
    regressão_idade_final <- glm(Migrantes ~ Idade_Posse * Tamanho * Ideologia, 
                                data = modelo_idade, family = "binomial")
    summary(regressão_idade_final)
  }
}


