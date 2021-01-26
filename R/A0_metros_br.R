# A0: Creates data.frame with Metro areas of the project --------------------------------------------

source('fun.R')

# Metros in the project
metros <- 
  c("RM Belo Horizonte","RM Curitiba","RM Fortaleza","RM Porto Alegre",
    "RM Rio de Janeiro","RM Recife","RM Salvador","RM São Paulo",
    "RIDE - Região Integrada de Desenvolvimento do Distrito Federal e Entorno",
    "RM Belém", "RM Campinas", "RM Manaus", "RM Goiânia",
    "RM do Vale do Paraíba e Litoral Norte", "RM de Sorocaba",
    "RM Grande Vitória", "RM Baixada Santista", "RM Ribeirão Preto",
    "RM Natal", "RM Grande São Luís")

# Brazilian metro areas
metros_br <- geobr::read_metro_area() %>% 
  dplyr::filter(name_metro %in% metros) %>% 
  dplyr::select(cd_ibge=code_muni,
                name_city=name_muni,
                cd_uf=code_state,
                name_uf=abbrev_state,
                name_metro) %>% 
  data.table::setDT(key = 'cd_ibge')

metros_br <- metros_br[, name_metro := data.table::fcase(
  name_metro == 'RIDE - Região Integrada de Desenvolvimento do Distrito Federal e Entorno','Brasília',
  name_metro == 'RM do Vale do Paraíba e Litoral Norte','Vale do Paraíba',
  name_metro == 'RM de Sorocaba','Sorocaba',
  name_metro == 'RM Grande São Luís','São Luís',
  name_metro == 'RM Grande Vitória','Vitória',
  name_metro == 'RM Belo Horizonte', 'Belo Horionte',
  name_metro == 'RM Curitiba', 'Curitiba',
  name_metro == 'RM Fortaleza', 'Fortaleza',
  name_metro == 'RM Porto Alegre', 'Porto Alegre',
  name_metro == 'RM Rio de Janeiro', 'Rio de Janeiro',
  name_metro == 'RM Recife', 'Recife',
  name_metro == 'RM Salvador', 'Salvador',
  name_metro == 'RM São Paulo', 'São Paulo',
  name_metro == 'RM Manaus', 'Manaus',
  name_metro == 'RM Belém', 'Belém',
  name_metro == 'RM Campinas', 'Campinas',
  name_metro == 'RM Baixada Santista', 'Baixada Santista',
  name_metro == 'RM Ribeirão Preto', 'Ribeirão Preto',
  name_metro == 'RM Goiânia', 'Goiânia',
  name_metro == 'RM Natal', 'Natal')]

metros_br <- metros_br[, cd_6 := stringr::str_sub(cd_ibge,1,6)]

metros_br <- metros_br[, ab_metro := data.table::fcase(
  name_metro == 'Belo Horizonte', 'bho',
  name_metro == 'Brasília', 'bsb',
  name_metro == 'Curitiba', 'cur',
  name_metro == 'Fortaleza', 'for',
  name_metro == 'Porto Alegre', 'poa',
  name_metro == 'Rio de Janeiro', 'rio',
  name_metro == 'Recife', 'rec',
  name_metro == 'Salvador', 'sal',
  name_metro == 'São Paulo', 'spo',
  name_metro == 'Sorocaba', 'sor',
  name_metro == 'Vitória', 'vit',
  name_metro == 'Manaus', 'man',
  name_metro == 'Belém', 'bel',
  name_metro == 'Campinas', 'cps',
  name_metro == 'Vale do Paraíba', 'sjc',
  name_metro == 'Baixada Santista', 'san',
  name_metro == 'Ribeirão Preto', 'rbp',
  name_metro == 'Goiânia', 'goi',
  name_metro == 'São Luís', 'slz',
  name_metro == 'Natal', 'nat')]

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