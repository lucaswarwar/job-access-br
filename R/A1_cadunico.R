### A1 - Get CADUnico identified data

# Load libraries --------

source('setup.R')

### 1. Read data of families (cad_familia.csv) ###

# Function to read CADUnico's raw data
# Selects variables that will be use and save 1 df per year (people families merged)

# It's necessary to read available files from multiple years, to capture possible changes of address

read_cadunico <- function(year, metros = 'all'){

  message('Working on ', year)
  
  read_cadunico_un <- function(metro) {
  
    message('Working on ', metro)
    
    df_metro <- metros_br %>% dplyr::filter(ab_metro == metro)
    
    # Load dataset for given year 
  
    df_familia <- data.table::fread(
      file = paste0(path_cadunico,'cad_familia_12',year,'.csv'), #nrows = 1000)
      select = c("cd_ibge_cadastro", "co_familiar_fam", "dt_cadastro_fam", "dt_atualizacao_fam",
                 "vl_renda_media_fam", "no_localidade_fam", "no_tip_logradouro_fam",'no_tit_logradouro_fam',
                 "no_logradouro_fam", "nu_logradouro_fam","ds_complemento_fam","nu_cep_logradouro_fam"))
  
    # Filter: families living in the metro
    df_familia <- df_familia[as.character(cd_ibge_cadastro) %in% as.character(df_metro$cd_ibge)]  
  
    # Recode and remove address columns
    df_familia <- df_familia[, no_logradouro_fam := ifelse(no_tit_logradouro_fam=="",no_logradouro_fam,
                                                           paste(no_tit_logradouro_fam,no_logradouro_fam))]
  
    df_familia <- df_familia[, nu_logradouro_fam := ifelse(!is.na(nu_logradouro_fam),as.character(nu_logradouro_fam),
                                                           ds_complemento_fam)]
  
    df_familia <- df_familia[, !c('ds_complemento_fam','no_tit_logradouro_fam')]
  
    # Set key
    data.table::setkey(df_familia, key = 'co_familiar_fam')
  
    ### 2. Read data of people (cad_pessoa.csv) ###

    # Load full dataset
    df_pessoa <- data.table::fread(
    file = paste0(path_cadunico,'cad_pessoa_12', year,'.csv'), #nrows = 1000)
    select = c("cd_ibge_cadastro", "co_familiar_fam","nu_nis_pessoa","no_pessoa","co_sexo_pessoa",
               "dt_nasc_pessoa", "co_raca_cor_pessoa","nu_cpf_pessoa","co_ibge_munic_nasc_pessoa",
               "in_frequenta_escola_memb","no_escola_memb","co_censo_inep_memb",
               "vl_remuner_emprego_memb","co_trabalho_12_meses_memb","qt_meses_12_meses_memb",       
               "vl_renda_bruta_12_meses_memb","vl_renda_seguro_desemp_memb",
               "co_cart_assinada_memb","in_dinh_const_memb","in_dinh_flanelhinha_memb",     
               "in_dinh_carregador_memb","in_dinh_catador_memb","in_dinh_servs_gerais_memb",    
               "in_dinh_pede_memb","in_dinh_vendas_memb","in_dinh_outro_memb"))
  
    # Filter people in RMSP
    df_pessoa <- df_pessoa[as.character(cd_ibge_cadastro) %in% as.character(df_metro$cd_ibge)]
  
    # Informal market
    df_pessoa <- df_pessoa[, in_wkinf := ifelse(
      !is.na(in_dinh_const_memb) | !is.na(in_dinh_flanelhinha_memb) | !is.na(in_dinh_carregador_memb) | !is.na(in_dinh_catador_memb) |
      !is.na(in_dinh_servs_gerais_memb) | !is.na(in_dinh_pede_memb) | !is.na(in_dinh_vendas_memb) | !is.na(in_dinh_outro_memb),1,0)]
  
    # Filter people with working age in given year
    df_pessoa <- df_pessoa[,age := as.numeric(year) - as.numeric(stringr::str_sub(dt_nasc_pessoa,1,4))]
    df_pessoa <- df_pessoa[age >= 14] 
  
  # Recode Variables ----------

  #df_pessoa <- df_pessoa[, co_sexo_pessoa := data.table::fifelse(co_sexo_pessoa == '1', 'Homem', 'Mulher')]

  #df_pessoa <- df_pessoa[, co_raca_cor_pessoa := data.table::fcase(co_raca_cor_pessoa == '1', 'Branca',
  #                                                   co_raca_cor_pessoa == '2', 'Preta',
  #                                                   co_raca_cor_pessoa == '3', 'Amarela',
  #                                                   co_raca_cor_pessoa == '4', 'Parda',
  #                                                   co_raca_cor_pessoa == '5', 'Indigena')]
  
  data.table::setkey(df_pessoa, key = 'co_familiar_fam')

  ### 3. Create final dataset (merge people and families)  ###

  # Merge datasets
    df_cad <- data.table::merge.data.table(df_pessoa,
                                          df_familia[,!c('cd_ibge_cadastro')],
                                          all.x = TRUE, 
                                          by = 'co_familiar_fam')
  
   rm(df_pessoa,df_familia)
  
    df_cad <- df_cad %>% dplyr::select(cpf = nu_cpf_pessoa,
                                       nis = nu_nis_pessoa,
                                       name = no_pessoa,
                                       gend = co_sexo_pessoa,
                                       race = co_raca_cor_pessoa,
                                       dtbirth = dt_nasc_pessoa,
                                       age,
                                       cd_ibge_nasc=co_ibge_munic_nasc_pessoa,
                                       in_school=in_frequenta_escola_memb,
                                       no_school=no_escola_memb,
                                       cd_inep=co_censo_inep_memb,
                                       in_wk12mo=co_trabalho_12_meses_memb,
                                       qt_wk12mo=qt_meses_12_meses_memb,       
                                       remtot=vl_renda_bruta_12_meses_memb,
                                       rememp=vl_remuner_emprego_memb,
                                       rem_unemp_in=vl_renda_seguro_desemp_memb,
                                       in_wkform=co_cart_assinada_memb,
                                       in_wkinf,
                                       cd_fam = co_familiar_fam,
                                       dt_cad=dt_cadastro_fam,
                                       dt_upd=dt_atualizacao_fam,
                                       remmd_fam=vl_renda_media_fam,
                                       cd_ibge = cd_ibge_cadastro,
                                       tp_logradouro = no_tip_logradouro_fam,
                                       no_logradouro = no_logradouro_fam,
                                       nu_logradouro = nu_logradouro_fam,
                                       localidade = no_localidade_fam,
                                       cep = nu_cep_logradouro_fam) %>% 
    data.table::setDT(df_cad, key = 'cpf')
  
    # Save
    readr::write_rds(df_cad, here::here('data-raw',metro,'CADUnico', paste0('cadunico_',metro,'_',year,'.rds')), compress = 'gz')
  }
  
  # 2. Apply function
  
  if (metros == "all") {
    
    x = unique(metros_br$ab_metro)
    
  } else (x = metros)
  
  
  purrr::walk(.x = x,.f = read_cadunico_un)
}

### 4. Apply functions
years <- as.character(seq(2011,2019,1))
purrr::walk(.x = years, .f = read_cadunico)
#read_cadunico(year = '2019',metros = c('rjo','spo','poa','cur'))
