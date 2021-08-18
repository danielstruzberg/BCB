

library(tidyverse)
library(dplyr)
library(here)
library(gt)
library(tsibble)

caminhos<-str_glue('https://www3.bcb.gov.br/rdrweb/rest/ext/ranking/arquivo?ano={periodos_ano$ano}&periodicidade=TRIMESTRAL&periodo={periodos_ano$periodo}&tipo=Bancos%20e%20financeiras')


purrr::walk2(.x=caminhos,.y=periodos_ano$arquivo_destino,.f=~download.file(url=.x,destfile = .y))

Arquivos <- list.files(here('Dados'),full.names = TRUE)

Dados_Cliente_Banco <- map_df(
    .x = Arquivos, 
    .f = ~read_csv2(
        file = .x, 
        locale = locale(encoding = "latin1"),
        name_repair = janitor::make_clean_names,
        col_types = cols(cnpj_if = col_character())

    )
)   %>% 
    select(c(
        ano,
        trimestre,
        categoria,
        tipo,
        instituicao_financeira,
        quantidade_total_de_reclamacoes,
        quantidade_de_clientes_u_0096_fgc,
        quantidade_total_de_clientes_u_0096_ccs_e_scr,
        quantidade_de_clientes_u_0096_ccs
    )) %>% 
    mutate(
        across(everything(), ~replace_na(.x, 0))
    ) %>% 
    filter(tipo=="Conglomerado")



Analise_Trimestral <- Dados_Cliente_Banco %>%  
    mutate(instituicao_financeira=str_remove(instituicao_financeira," \\(conglomerado\\)"),
           trimestre=str_extract(trimestre,"[0-9]"),
           Trimestre_Ano=str_glue("{ano}Q{trimestre}") %>% yearquarter(),
           Ano_Anterior=ano-1,
           Delta_Ano=str_glue("{ano}-{Ano_Anterior}",),
           Banco=if_else(instituicao_financeira=='INTERMEDIUM','INTER',instituicao_financeira)
    ) %>%
    mutate(
        Banco = if_else(Banco == "BTG PACTUAL/BANCO PAN","PAN", Banco)
    ) %>% 
    rename(        
           quantidade_clientes_fgc=quantidade_de_clientes_u_0096_fgc,
           quantidade_reclamacoes=quantidade_total_de_reclamacoes,
           quantidade_clientes=quantidade_de_clientes_u_0096_ccs) %>%
    select(!c(tipo,trimestre,Ano_Anterior,instituicao_financeira)) %>% 
    group_by(Banco) %>% 
    mutate(delta_qtd_clientes=quantidade_clientes/lag(quantidade_clientes)-1,
           delta_qtd_clientes_fgc=quantidade_clientes_fgc/lag(quantidade_clientes_fgc)-1,
           delta_qtd_reclamacoes=quantidade_reclamacoes/lag(quantidade_reclamacoes)-1,
           qtd_clientes_media_periodo = (quantidade_clientes + lag(quantidade_clientes))/2,
           reclamacoes_por_cliente = quantidade_reclamacoes / qtd_clientes_media_periodo
           ) %>% 
    ungroup() %>% 
    filter(
        quantidade_clientes != 0
    ) %>% 
    as_tsibble(
        key = Banco,
        index = Trimestre_Ano
    )




# Para Ajustar a Analisa Anual deve fazer um filtro que tenha o ultimo trimestre de cada ano
# Resolvi Fazer Dessa forma para separ mas vai ter que separar os resultados entre anual e trimestral

Analise_Anual <- Dados_Cliente_Banco %>%  
    mutate(instituicao_financeira=str_remove(instituicao_financeira," \\(conglomerado\\)"),
           trimestre=str_extract(trimestre,"[0-9]"),
           Trimestre_Ano=str_glue("{ano}Q{trimestre}") %>% yearquarter(),
           Ano_Anterior=ano-1,
           Delta_Ano=str_glue("{ano}-{Ano_Anterior}",),
           Banco=if_else(instituicao_financeira=='INTERMEDIUM','INTER',instituicao_financeira)
    ) %>% 
    mutate(
        Banco = if_else(Banco == "BTG PACTUAL/BANCO PAN","PAN", Banco)
    ) %>% 
    rename(        
        quantidade_clientes_fgc=quantidade_de_clientes_u_0096_fgc,
        quantidade_reclamacoes=quantidade_total_de_reclamacoes,
        quantidade_clientes=quantidade_de_clientes_u_0096_ccs) %>%
    select(!c(tipo,Ano_Anterior,instituicao_financeira)) %>% 
    filter(trimestre == "4") %>% 
    select(-trimestre) %>% 
    group_by(Banco) %>% 
    mutate(delta_qtd_clientes = quantidade_clientes/lag(quantidade_clientes)-1,
           delta_qtd_clientes_fgc = quantidade_clientes_fgc/lag(quantidade_clientes_fgc)-1,
           delta_qtd_reclamacoes = quantidade_reclamacoes/lag(quantidade_reclamacoes)-1,
           qtd_clientes_media_periodo = (quantidade_clientes + lag(quantidade_clientes))/2,
           reclamacoes_por_cliente = quantidade_reclamacoes / qtd_clientes_media_periodo
    ) %>% 
    ungroup() %>% 
    filter(
        quantidade_clientes != 0
    ) %>% 
    as_tsibble(
        key = Banco,
        index = ano
    )



write_rds(Analise_Anual, "dados_tratados/analise_anual.rds")
write_rds(Analise_Trimestral, "dados_tratados/analise_trimestral.rds")












