source('setup.R')

df1 <- stringr::str_split_fixed(locations_bho$address, ", ",n=2) %>% as.data.frame()
df2 <- stringr::str_split_fixed(df1$V2, " - ",n=2) %>% as.data.frame()

locations_bho <- data.table::data.table(
  NO_LOGRADOURO = df1$V1,
  NU_LOGRADOURO = df2$V1,
  LOCALIDADE = df2$V2,
  CEP = locations_bho$cep,
  CD_MUN = locations_bho$codemun)

locations_bho <- locations_bho[, TP_LOGRADOURO := stringr::str_extract(NO_LOGRADOURO,"[^ ]+")]
locations_bho <- locations_bho[, NO_LOGRADOURO := stringr::str_trim(stringr::str_remove(NO_LOGRADOURO,"[^ ]+"))]
locations_bho <- locations_bho[, .(TP_LOGRADOURO,NO_LOGRADOURO,NU_LOGRADOURO,LOCALIDADE,CEP,CD_MUN)]

tipos <- locations_bho[,.(n = .N), by = .(TP_LOGRADOURO)]
locations_bho <- locations_bho[, TP_LOGRADOURO := data.table::fcase(
  TP_LOGRADOURO == "RUA" | TP_LOGRADOURO == "10A" |TP_LOGRADOURO == "R" | TP_LOGRADOURO == "R.", "RUA",
  TP_LOGRADOURO == "AVENIDA" | TP_LOGRADOURO == "AV" |TP_LOGRADOURO == "AV." | TP_LOGRADOURO == "AVE", "RUA",
)]

tipos <- tipos[, TP_LOGRADOURO2 := stringr::str_extract(TP_LOGRADOURO,"[^.]+")]
tipos <- tipos[, NOME := stringr::str_remove(TP_LOGRADOURO,"[^.]+")]
