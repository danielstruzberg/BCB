

library(tidyverse)
library(dplyr)


ano<-tibble(ano=c(2017:2021))
periodo<-tibble(periodo=c(1:4))
periodos_ano <- crossing(ano,periodo)


caminhos<-str_glue('https://www3.bcb.gov.br/rdrweb/rest/ext/ranking/arquivo?ano={periodos_ano$ano}&periodicidade=TRIMESTRAL&periodo={periodos_ano$periodo}&tipo=Bancos%20e%20financeiras')


purrr::walk(caminhos,.f=download.file)

