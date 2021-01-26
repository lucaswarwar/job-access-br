Sys.setenv(TZ='UTC') # Local Time Zone

# load libraries --------------------------------------

library(here)         # manage directories
library(janitor)      # data cleaning
library(modelsummary) # freq tables
library(skimr)        # skim data
library(ggplot2)      # data viz
library(ggthemes)     # data viz themes
library(hrbrthemes)   # data viz themes
library(sf)           # read and manipulate spatial data
library(data.table)   # fast data wrangling
library(collapse)     # insanely fast data transformation
library(foreign)      # read data in strange formats
library(magrittr)     # pipe operator
library(ggmap)        # Google API
library(dodgr)        # calculate distance between points
library(r5r)          # calculate travel distance and intineraries 
library(geobr)        # Brazil's spatial data
library(pbapply)      # progress bar
library(readr)        # rapid data read 
library(tidyr)        # data manipulating
library(stringr)      # strings operations
library(lubridate)    # handle date formats  
library(maptools)
library(mapview)      # interactive maps
library(fixest)       # fast fixed effects
library(sandwich)     # fast estimators
library(broom)        # model summary
library(RColorBrewer) # color palettes
library(paletteer)    # color palettes
library(extrafont)    # text fonts
library(ggtext)       # text tool for data viz
library(knitr)        # knit documents
library(furrr)        # vectorize in parallel
library(purrr)        # funcional programming
library(forcats)      # handle factors
library(parallel)     # optimize operations
library(future.apply) # more optimization
library(dplyr)        # better than data.table!
library(beepr)        # tells me when work is done
library(patchwork)    # plot composition
library(Hmisc)        # calculate weighted quantiles
library(osmdata)      # Download OpenStreetMaps data (transit networks)
library(opentripplanner) # Use OTP from R: https://github.com/ITSLeeds/opentripplanner
library(h3jsr)        # h3 hexagonons
library(bit64)        # viz large numbers
library(gtfs2gps)     # Easy work with GTFS files

# Set some options and functions --------------------

source('fun.R')

options(scipen = 99999)

# Use GForce Optimisations in data.table operations
# details > https://jangorecki.gitlab.io/data.cube/library/data.table/html/datatable-optimize.html

options(datatable.optimize=Inf)

# set number of threads used in data.table
data.table::setDTthreads(percent = 100)


# Set files' paths at Ipea's directories ----------------------------------------

path_cadunico_v7 <- "//Storage6/bases/DADOS/RESTRITO/CADASTRO_UNICO/csv/"
path_cadunico_v6 <- "//STORAGE6/bases/DADOS/RESTRITO/CADASTRO_UNICO/originais/v6/"

cols_familia_v7 <- c("cd_ibge_cadastro", "co_familiar_fam", "dt_cadastro_fam", "dt_atualizacao_fam",
                     "vl_renda_media_fam", "no_localidade_fam", "no_tip_logradouro_fam",'no_tit_logradouro_fam',
                     "no_logradouro_fam", "nu_logradouro_fam","ds_complemento_fam","nu_cep_logradouro_fam",
                     "co_agua_canalizada_fam","co_abaste_agua_domic_fam","co_escoa_sanitario_domic_fam" ,
                     "co_destino_lixo_domic_fam","co_iluminacao_domic_fam","co_calcamento_domic_fam",
                     'vl_desp_aluguel_fam','in_parc_mds_fam')

cols_pessoa_v7 <- c("co_familiar_fam","nu_nis_pessoa","no_pessoa","co_sexo_pessoa",
                    "dt_nasc_pessoa", "co_raca_cor_pessoa","nu_cpf_pessoa","co_ibge_munic_nasc_pessoa",
                    'co_sabe_ler_escrever_memb',"in_frequenta_escola_memb","no_escola_memb","co_censo_inep_memb",
                    'co_curso_frequenta_memb','co_ano_serie_frequenta_memb','co_curso_freq_pessoa_memb',
                    'co_ano_serie_frequentou_memb'  ,'co_concluiu_frequentou_memb',   
                    "vl_remuner_emprego_memb","co_trabalho_12_meses_memb","qt_meses_12_meses_memb",       
                    "vl_renda_bruta_12_meses_memb","vl_renda_seguro_desemp_memb",
                    "co_cart_assinada_memb","in_dinh_const_memb","in_dinh_flanelhinha_memb",     
                    "in_dinh_carregador_memb","in_dinh_catador_memb","in_dinh_servs_gerais_memb",    
                    "in_dinh_pede_memb","in_dinh_vendas_memb","in_dinh_outro_memb")


cols_familia_v6 <- c("CD_DOMICILIAR","CD_FAMILIAR","CD_IDENTIFICACAO_DOMICILIO","CD_CEP_LOGRADOURO","DS_LOGRADOURO",
                     "NM_LOGRADOURO","NU_LOGRADOURO","DS_COMPLEMENTO_LOGRADOURO","NM_BAIRRO_LOGRADOURO",         
                     "CD_IBGE_LOGRADOURO","CD_TIPO_LOCALIDADE","CD_TIPO_COBERTURA", "CD_SITUACAO_DOMICILIO" ,    
                     "CD_TIPO_DOMICILIO","NU_COMODOS","CD_CONSTRUCAO","CD_ABASTECIMENTO_AGUA",
                     "QT_PESSOAS_INFORMADA","DT_INCLUSAO_DOMICILIO","DT_ALTERACAO_DOMICILIO")

cols_pessoa_v6 <- c("CD_FAMILIAR","NU_ORDEM_PESSOA","NM_PESSOA","DT_NASCIMENTO","CD_SEXO",  
                    "CD_IBGE_NASCIMENTO","NM_PAI","NM_MAE","CD_ESTADO_CIVIL","NU_ORDEM_ESPOSO",  
                    "CD_RACA_COR" ,"NU_NIS_PESSOA", "NU_CPF","CD_SERIE_ESCOLAR" ,"NM_ESCOLA", "CD_CENSO_INEP" ,                
                    "CD_MERCADO_TRABALHO","VL_REMUNERACAO_EMPREGO", "VL_RENDA_APOSENTADORIA" ,       
                    "VL_RENDA_SEGURO_DESEMPREGO","VL_RENDA_PENSAO_ALIMENTICIA","VL_OUTRAS_RENDAS",
                    "NU_ORDEM_PAI","NU_ORDEM_MAE","IN_PARTICIPA_PETI","DT_INCLUSAO_PETI",              
                    "CD_BENEFICIO_PETI","IN_PARTICIPA_AGENTE_JOVEM","IN_PARTICIPA_BOLSA_ESCOLA",
                    "IN_PARTICIPA_BOLSA_ALIMENTACAO","IN_NENHUM_PROGRAMA","IN_PARTICIPA_LOAS_BPC",
                    "IN_PARTICIPA_PREV_RURAL" ,"IN_PARTICIPA_PRONAF","IN_PARTICIPA_PROGER",
                    "IN_OUTRO_PROGRAMA","IN_BENEFICIARIO_BAL","IN_BENEFICIARIO_BES")


path_rais <- "//Storage6/bases/DADOS/RESTRITO/RAIS/"
path_caged <- "//Storage6/bases/DADOS/RESTRITO/CAGED_ID/"
path_fies <- "//storage1/bases4/RESTRITO/FIES/"
path_gtfs <- "D:/data/gtfs/spo/"

# Loads metros_br df
metros_br <- readr::read_rds(here::here('data-raw','metros_br.rds'))