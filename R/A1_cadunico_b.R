### A1 - Get CADUnico identified data

# Load libraries --------

source('setup.R')


rm2 <- c("Belém", "Campinas", "Manaus", "Goiânia",
         "Vale do Paraíba", "Sorocaba",
         "Vitória", "Baixada Santista", "Ribeirão Preto",
         "Natal", "São Luís")

metros_br <- metros_br[name_metro %in% rm2]

### 1. Read data of families (cad_familia.csv) ###

# Function to read CADUnico's raw data
# Selects variables that will be use and save 1 df per year (people families merged)

# It's necessary to read available files from multiple years, to capture possible changes of address

read_cadunico <- function(year){
  
  message('Working on ', year)
    
    # Load dataset for given year 
    
    df_familia <- data.table::fread(
      file = paste0(path_cadunico,'cad_familia_12',year,'.csv'), #nrows = 1000)
      select = c("cd_ibge_cadastro", "co_familiar_fam", "dt_cadastro_fam", "dt_atualizacao_fam",
                 "vl_renda_media_fam", "no_localidade_fam", "no_tip_logradouro_fam",'no_tit_logradouro_fam',
                 "no_logradouro_fam", "nu_logradouro_fam","ds_complemento_fam","nu_cep_logradouro_fam",
                 "co_agua_canalizada_fam","co_abaste_agua_domic_fam","co_escoa_sanitario_domic_fam" ,
                 "co_destino_lixo_domic_fam","co_iluminacao_domic_fam","co_calcamento_domic_fam"))
    
    # Filter: families living in the metro
    df_familia <- df_familia[as.character(cd_ibge_cadastro) %in% as.character(metros_br$cd_ibge)]  
    
    # Recode and remove address columns
    df_familia <- df_familia[, no_logradouro_fam := ifelse(no_tit_logradouro_fam=="",no_logradouro_fam,
                                                           paste(no_tit_logradouro_fam,no_logradouro_fam))]
    
    df_familia <- df_familia[, nu_logradouro_fam := ifelse(!is.na(nu_logradouro_fam),as.character(nu_logradouro_fam),
                                                           ds_complemento_fam)]
    
    df_familia <- df_familia[, !c('ds_complemento_fam','no_tit_logradouro_fam')]
    
    # Set key
    data.table::setkey(df_familia, key = 'co_familiar_fam')
    
    message('df_familia done!')
    
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
    df_pessoa <- df_pessoa[as.character(cd_ibge_cadastro) %in% as.character(metros_br$cd_ibge)]
    
    # Informal market
    df_pessoa <- df_pessoa[, in_wkinf := ifelse(
      !is.na(in_dinh_const_memb) | !is.na(in_dinh_flanelhinha_memb) | !is.na(in_dinh_carregador_memb) | !is.na(in_dinh_catador_memb) |
        !is.na(in_dinh_servs_gerais_memb) | !is.na(in_dinh_pede_memb) | !is.na(in_dinh_vendas_memb) | !is.na(in_dinh_outro_memb),1,0)]
    
    # Filter people with working age in given year
    #df_pessoa <- df_pessoa[,age := as.numeric(year) - as.numeric(stringr::str_sub(dt_nasc_pessoa,1,4))]
    #df_pessoa <- df_pessoa[age >= 14] 
    
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
    
    df_cad <- df_cad %>% dplyr::select(cd_fam = co_familiar_fam,
                                       cpf = nu_cpf_pessoa,
                                       nis = nu_nis_pessoa,
                                       name = no_pessoa,
                                       gend = co_sexo_pessoa,
                                       race = co_raca_cor_pessoa,
                                       dtbirth = dt_nasc_pessoa,
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
                                       dt_cad=dt_cadastro_fam,
                                       dt_upd=dt_atualizacao_fam,
                                       remmd_fam=vl_renda_media_fam,
                                       cd_ibge = cd_ibge_cadastro,
                                       in_h2o_can=co_agua_canalizada_fam,
                                       in_h20_abas=co_abaste_agua_domic_fam,
                                       in_esgoto=co_escoa_sanitario_domic_fam,
                                       in_lixo=co_destino_lixo_domic_fam,
                                       in_ilum=co_iluminacao_domic_fam,
                                       in_calc=co_calcamento_domic_fam,
                                       tp_logradouro = no_tip_logradouro_fam,
                                       no_logradouro = no_logradouro_fam,
                                       nu_logradouro = nu_logradouro_fam,
                                       localidade = no_localidade_fam,
                                       cep = nu_cep_logradouro_fam) %>% 
      data.table::setDT(df_cad, key = c('cd_fam','cpf'))
    
    message('merge done!')
    
    # 1. Padronize address
    # Fix numbers
    df_cad <- df_cad[, nu_logradouro := gsub("00000000000000","",nu_logradouro)] 
    df_cad <- df_cad[, nu_logradouro := gsub("0000000000000","",nu_logradouro)] 
    df_cad <- df_cad[, nu_logradouro := gsub("000000000000","",nu_logradouro)] 
    df_cad <- df_cad[, nu_logradouro := gsub("00000000000","",nu_logradouro)] 
    df_cad <- df_cad[, nu_logradouro := gsub("0000000000","",nu_logradouro)] 
    df_cad <- df_cad[, nu_logradouro := gsub("000000000","",nu_logradouro)] 
    df_cad <- df_cad[, nu_logradouro := gsub("00000","",nu_logradouro)] 
    df_cad <- df_cad[, nu_logradouro := gsub("^0000","",nu_logradouro)]
    df_cad <- df_cad[, nu_logradouro := gsub("^000","",nu_logradouro)] 
    df_cad <- df_cad[, nu_logradouro := gsub("^00","",nu_logradouro)] 
    df_cad <- df_cad[, nu_logradouro := gsub("S/N","SN",nu_logradouro)]
    df_cad <- df_cad[, nu_logradouro := gsub('^0',"",nu_logradouro)]
    df_cad <- df_cad[, nu_logradouro := ifelse(nu_logradouro == '','SN',nu_logradouro)]
    
    # Fix type of way
    df_cad <- df_cad[, tp_logradouro := data.table::fcase(
      tp_logradouro == "RUA" | tp_logradouro == "10A" |tp_logradouro == "R" | 
        tp_logradouro == "R." | stringr::str_detect(tp_logradouro,"RUA"), "RUA",
      tp_logradouro == "AVENIDA" | tp_logradouro == "AV" |tp_logradouro == "AV." | 
        tp_logradouro == "AVE" | stringr::str_detect(tp_logradouro,"AVENIDA"), "AVENIDA",
      tp_logradouro == 'ALA' | tp_logradouro == 'ALAMEDA','ALAMEDA',
      tp_logradouro == 'BEC' | tp_logradouro == 'BECO','BECO',
      tp_logradouro == 'TRA' | tp_logradouro == 'TRAVESSA','TRAVESSA',
      tp_logradouro == 'DIS' | tp_logradouro == 'DISTRITO','DISTRITO',
      tp_logradouro == 'ACE' | stringr::str_detect(tp_logradouro,"ACESSO"),'ACESSO',
      tp_logradouro == 'BL' | tp_logradouro == 'BLO' | tp_logradouro == 'BLOCO','BLOCO',
      stringr::str_detect(tp_logradouro,"CONJUNTO"),'CONJUNTO',
      tp_logradouro == 'EST' | stringr::str_detect(tp_logradouro,"ESTRADA"),'ESTRADA',
      tp_logradouro == 'SIT' | stringr::str_detect(tp_logradouro,"SITIO"),'SITIO',
      tp_logradouro == 'FAZ' | stringr::str_detect(tp_logradouro,"FAZENDA"),'FAZENDA',
      tp_logradouro == 'CHA' | stringr::str_detect(tp_logradouro,"CHACARA"),'CHACARA',
      tp_logradouro == 'JAR' | tp_logradouro == 'JD' | stringr::str_detect(tp_logradouro,"JARDIM"),'JARDIM',
      tp_logradouro == 'LOT' | stringr::str_detect(tp_logradouro,"LOTEAMENTO"),'LOTEAMENTO',
      tp_logradouro == 'PRA' | tp_logradouro == 'PC' | stringr::str_detect(tp_logradouro,"PRACA"),'PRACA',
      tp_logradouro == 'QUA' | stringr::str_detect(tp_logradouro,"QUADRA"),'QUADRA',
      tp_logradouro == 'RES' | stringr::str_detect(tp_logradouro,"RESIDENCIAL"),'RESIDENCIAL',
      tp_logradouro == 'ROD' | stringr::str_detect(tp_logradouro,"RODOVIA"),'RODOVIA',
      stringr::str_detect(tp_logradouro,"CONDOMINIO"),'CONDOMINIO',
      tp_logradouro == 'VIL' | stringr::str_detect(tp_logradouro,"VILA"),'VILA',
      tp_logradouro == 'VIE' | stringr::str_detect(tp_logradouro,"VIELA"),'VIELA',
      tp_logradouro == 'COL' | stringr::str_detect(tp_logradouro,"COLONIA"),'COLONIA')]
    
    # Padronize street names
    df_cad <- df_cad[, no_logradouro := gsub("01","1",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("02","2",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("03","3",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("04","4",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("05","5",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("06","6",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("07","7",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("08","8",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("09","9",no_logradouro)] 
    
    df_cad <- df_cad[, no_logradouro := gsub("II","2",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("III","3",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("VII","7",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("VIII","8",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("XII","12",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("XIII","13",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("XIV","14",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("XV","15",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("XVI","16",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("XVII","17",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("XVIII","18",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("XIX","19",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("XXI","21",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("XXII","22",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("XXIII","23",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("XXIV","24",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("XXV","25",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("XXVI","26",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("XXVII","27",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("XXVIII","28",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("XXIX","29",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("XXX","30",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("XX","20",no_logradouro)] 
    
    df_cad <- df_cad[, no_logradouro := gsub("^CEL ","CORONEL ",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("^MAJ ","MAJOR ",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("^CAP ","CAPITAO ",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("^TN ","TENENTE ",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("^TEN ","TENENTE ",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("^MAL ","MARECHAL ",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("^GAL ","GENERAL ",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("^DR  ","DOUTOR ",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("^PROF ","PROFESSOR ",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("^ENG ","ENGENHEIRO ",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("^PE ","PADRE ",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("^FR ","FREI ",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("^DES ","DESEMBARGADOR ",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("^VISC ","VISCONDE ",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("^DUQ ","DUQUE ",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("^BR ","BARAO ",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("^DQ ","DUQUE ",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("^COM ","COMENDADOR ",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("^STO ","SANTO ",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("^STA ","SANTA ",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("^S ","SAO ",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("^PRES ","PRESIDENTE ",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("^GOV ","GOVERNADOR ",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("^PREF ","PREFEITO ",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("^SEN ","SENADOR ",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("^DEP ","DEPUTADO ",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("^VER ","VEREADOR ",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("^BRIG ","BRIGADEIRO ",no_logradouro)] 
    df_cad <- df_cad[, no_logradouro := gsub("SDO","SEM DENOMINACAO",no_logradouro)] %>% unique()
    
    message('address fix done!')
    
    # Save
    readr::write_rds(df_cad, here::here('data-raw','CADUnico', paste0('cadunico2_',year,'.rds')), compress = 'gz')

}

### 4. Apply functions
years <- as.character(seq(2011,2019,1))
purrr::walk(.x = years, .f = read_cadunico)
#read_cadunico(year = '2019',metros = c('rjo','spo','poa','cur'))

#### Base V6 - 2007

df_familia <- data.table::fread(
  file = '//STORAGE6/bases/DADOS/RESTRITO/CADASTRO_UNICO/originais/v6/20100802_fita_754_jul_2010/csv/mycad_dom_201007.csv',
  select = c("CD_DOMICILIAR","CD_FAMILIAR","CD_IDENTIFICACAO_DOMICILIO","CD_CEP_LOGRADOURO","DS_LOGRADOURO",
             "NM_LOGRADOURO","NU_LOGRADOURO","DS_COMPLEMENTO_LOGRADOURO","NM_BAIRRO_LOGRADOURO",         
             "CD_IBGE_LOGRADOURO","CD_TIPO_LOCALIDADE","CD_TIPO_COBERTURA", "CD_SITUACAO_DOMICILIO" ,    
             "CD_TIPO_DOMICILIO","NU_COMODOS","CD_CONSTRUCAO","CD_ABASTECIMENTO_AGUA",
             "QT_PESSOAS_INFORMADA","DT_INCLUSAO_DOMICILIO","DT_ALTERACAO_DOMICILIO"))

df_familia <- df_familia[CD_IBGE_LOGRADOURO %in% metros_br$cd_ibge]

df_pessoa <- data.table::fread(
  file = '//STORAGE6/bases/DADOS/RESTRITO/CADASTRO_UNICO/originais/v6/20100802_fita_754_jul_2010/csv/mycad_pes_201007.csv',
  #nrows = 100000)
  select = c("CD_FAMILIAR","NU_ORDEM_PESSOA","NM_PESSOA","DT_NASCIMENTO","CD_SEXO",  
             "CD_IBGE_NASCIMENTO","NM_PAI","NM_MAE","CD_ESTADO_CIVIL","NU_ORDEM_ESPOSO",  
             "CD_RACA_COR" ,  "NU_NIS_PESSOA", "NU_CPF","CD_SERIE_ESCOLAR" , "NM_ESCOLA", "CD_CENSO_INEP" ,                
             "CD_MERCADO_TRABALHO" ,"VL_REMUNERACAO_EMPREGO", "VL_RENDA_APOSENTADORIA" ,       
             "VL_RENDA_SEGURO_DESEMPREGO", "VL_RENDA_PENSAO_ALIMENTICIA" , "VL_OUTRAS_RENDAS",
             "NU_ORDEM_PAI" , "NU_ORDEM_MAE","IN_PARTICIPA_PETI", "DT_INCLUSAO_PETI",              
            "CD_BENEFICIO_PETI", "IN_PARTICIPA_AGENTE_JOVEM" ,"IN_PARTICIPA_BOLSA_ESCOLA", "IN_PARTICIPA_BOLSA_ALIMENTACAO",
            "IN_NENHUM_PROGRAMA" , "IN_PARTICIPA_LOAS_BPC"  , "IN_PARTICIPA_PREV_RURAL" ,"IN_PARTICIPA_PRONAF" ,
            "IN_PARTICIPA_PROGER","IN_OUTRO_PROGRAMA", "IN_BENEFICIARIO_BAL"  ,  "IN_BENEFICIARIO_BES")
  )

df_pessoa <- df_pessoa[CD_FAMILIAR %in% df_familia$CD_FAMILIAR]

df_pessoa$CD_FAMILIAR <- as.integer(df_pessoa$CD_FAMILIAR)
  
df_pessoa <- merge.data.table(df_pessoa, df_familia, all.x = TRUE, by = 'CD_FAMILIAR') %>% 
  setDT(key = 'CD_FAMILIAR')

readr::write_rds(df_pessoa, here::here('data-raw', 'CADUnico','v6','cadunico_2010.rds'), compress = 'gz')
rm(df_familia,df_pessoa)
