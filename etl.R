

library(tidyverse)
library(dplyr)
library(here)


ano<-tibble(ano=c(2017:2021))
periodo<-tibble(periodo=c(1:4))
periodos_ano <- crossing(ano,periodo) %>% 
    mutate(arquivo_destino=str_glue("Dados/{ano}-{periodo}") %>% 
               here()) 


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
)
    

