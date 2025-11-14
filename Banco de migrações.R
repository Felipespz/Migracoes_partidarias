library(readxl)
library(writexl)
library(tidyverse)
library(patchwork)
library(officer)     # Para limpeza de texto na junção das variáveis
library(stringi)     # Para limpeza de texto na junção das variáveis
library(lmtest)      # Para aplicação do General Linear Model "glm()"

#Puxando e transformando os dados
  banco_de_migrações_cap <- read_xlsx("C:/Users/felip/Downloads/BD_Sainz&Codato_ClassePolíticadoBrasil_INCT_ReDem_2025.xlsx")
  
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
  # 1. Filtrar e preparar os dados
  sankey <- banco_final_cap |>
    filter(Reeleito == "Sim") |>
    arrange(Nome_Deputado, Ano_Eleição)
    
    print(gerar_sankey_ideologia_plotly(sankey))
    
    print(gerar_sankey_tamanho_plotly(sankey))

  #Recategorização dos dados
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


