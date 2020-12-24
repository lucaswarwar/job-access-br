# A0: Creates data.frame with Metro areas of the project --------------------------------------------

source('fun.R')

# Metros in the project
metros <- 
  c("RM Belo Horizonte","RM Curitiba","RM Fortaleza","RM Porto Alegre",
    "RM Rio de Janeiro","RM Recife","RM Salvador","RM São Paulo",
    "RIDE - Região Integrada de Desenvolvimento do Distrito Federal e Entorno")

# Brazilian metro areas
metros_br <- geobr::read_metro_area() %>% 
  dplyr::filter(name_metro %in% metros) %>% 
  dplyr::select(cd_ibge=code_muni,
                name_city=name_muni,
                cd_uf=code_state,
                name_uf=abbrev_state,
                name_metro) %>% 
  data.table::setDT(key = 'cd_ibge')

metros_br <- metros_br[, name_metro := data.table::fifelse(
  name_metro == 'RIDE - Região Integrada de Desenvolvimento do Distrito Federal e Entorno','Brasília',
  gsub('RM ','',name_metro))]

metros_br <- metros_br[, cd_6 := stringr::str_sub(cd_ibge,1,6)]

metros_br <- metros_br[, ab_metro := data.table::fcase(
  name_metro == 'Belo Horizonte', 'bho',
  name_metro == 'Brasília', 'bsb',
  name_metro == 'Curitiba', 'cur',
  name_metro == 'Fortaleza', 'for',
  name_metro == 'Porto Alegre', 'poa',
  name_metro == 'Rio de Janeiro', 'rjo',
  name_metro == 'Recife', 'rec',
  name_metro == 'Salvador', 'sal',
  name_metro == 'São Paulo', 'spo')]

metros_br %>% 
  dplyr::select(cd_ibge,
                cd_6,
                name_city,
                cd_uf,
                name_uf,
                name_metro,
                ab_metro,
                geom) %>% 
  readr::write_rds(here::here('data-raw','metros_br.rds'))