#####################################################
#####################################################
##### Autor: Neale Ahmed El-Dash
##### Data: 19/09/2019
##### Código: Esse código, escrito para o software R,
##### tem o objetivo de enfatizar como é possível replicar 
##### as taxas anuais do PRODES divulgadas pelo INPE.
##### Primeiramente, as bases de dados do INPE têm  
##### que ser baixadas do servidor (um arquivo para 
##### cada ano, de 2002 a 2018). 
##### A função get_data_saida é utilizada para todos
##### os anos, e os formatos mudam um pouco, ela está 
##### longe de ser considerada elegante.
#####################################################
#####################################################
#####################################################

#carregando bibliotecas
require(tidyverse)
require(rvest)

#definindo função

get_data_saida <- function(url){
  
  print(url)
  pag <- read_html(url)
  df <- pag %>% html_table(header=TRUE)
  df <- df[[1]]
  if ("" %in% names(df) & !("Outliers" %in% names(df))){
    pos <- grep("^$",names(df))
    names(df)[pos] <- "Outliers"
  }
  if ("" %in% names(df)){
    df <- df[,!(names(df) == "")]
  }
  if ("Outliers" %in% names(df)){
    df <- df %>% mutate_at(vars(-Sta,-Outliers),list(~as.numeric(.)))
    df$Outliers <- str_replace(df$Outliers,"rate$","rates")
    df$Outliers <- str_replace(df$Outliers,"(cloud$|clds|clould)","clouds")
  } else {
    df <- df %>% mutate_at(vars(-Sta),list(~as.numeric(.)))
  }
  df$ano <- as.numeric(str_extract(url,"[0-9]{4}"))
  df <- df %>% filter(str_detect(PathRow,"^[0-9]{3,5}$"))
  df <- df %>% rename(pathrow=PathRow,uf=Sta)
  
  
  return(df)
  
}

#executando código

urls <- "http://www.obt.inpe.br/OBT/assuntos/programas/amazonia/prodes/taxas-calculadas-por-imagem-de-satelite-pos-2002"
pag <- read_html(urls)

links = pag %>% html_nodes(xpath="//a[@class='external-link']") %>% html_attr("data-mce-href")
links = links[!is.na(links)]

df.saida <- map_df(links,~get_data_saida(.))

#ajuste manual para um caso com informações faltantes no site
df.saida$Outliers[df.saida$pathrow == "22263" & df.saida$uf == "MA" & df.saida$ano==2005] <- "rates"


#alterando nomes das variáveis que serão reproduzidas
df.fim <- df.saida
df.fim <- df.fim %>% rename(rate.inpe=Rate,PercRate.inpe=PercRate,DRate2.inpe=DRate2,nd2r.inpe=nd2r,nd1.inpe=nd1,DRate1.inpe=DRate1,nd1r.inpe=nd1r,ndnxtY.inpe=ndnxtY,Outliers.inpe=Outliers)

#ordenando a base de dados.
df.fim <- df.fim %>% arrange(pathrow,uf,ano)

#reproduzindo resultados
df.fim <- df.fim %>% mutate(
  nd1a = case_when(
    Jul0 > EndClim ~ 0,
    Jul0 < StClim ~ EndClim - StClim + 1,
    TRUE ~ EndClim -  Jul0 + 1
  ),
  nd2a = case_when(
    Jul1 > EndClim ~ EndClim - StClim + 1,
    Jul1 < StClim ~ 0,
    TRUE ~ Jul1 - StClim + 1
  ),
  nd2 = case_when(
    Jul2 > EndClim ~ EndClim - StClim + 1,
    Jul2 < StClim ~ 0,
    TRUE ~ Jul2 - StClim + 1
  ),
  nd1_ = case_when(
    Jul1 > EndClim ~ 0,
    Jul1 < StClim ~ EndClim - StClim + 1,
    TRUE ~ pmax(EndClim - Jul1 + 1,0)
  ),
  nd1 = case_when(
    Jul1 > EndClim ~ 0,
    Jul1 <= StClim & EndClim < 211 ~ 211 - StClim + 1,
    Jul1 <= StClim & StClim < 211 ~ EndClim - 211 + 1,
    Jul1 <= StClim & EndClim >= 211 ~ EndClim - StClim + 1,
    Jul1 > StClim & Jul1 <= 211 ~ pmax(EndClim - 211 + 1,0),
    TRUE ~ pmax(EndClim - Jul1 + 1,0)
  ),
  nd2r = case_when(
    EndClim < 211 ~ pmax(EndClim - StClim + 1,0),
    EndClim >= 211 ~ pmax(211 - StClim + 1,0)
  ),
  nd1r = case_when(
    Jul1 <= 211 ~ 0,
    211 < StClim & Jul1 < StClim ~ 0,
    211 < StClim & Jul1 == StClim ~ 1,
    Jul1 > StClim & Jul1 <= EndClim & StClim > 211 ~ Jul1 - StClim + 1,
    Jul1 > StClim & Jul1 <= EndClim & StClim <= 211 ~ Jul1 - 211 + 1,
    Jul1 > EndClim & EndClim >= 211 & StClim > 211 ~ EndClim - StClim + 1,
    Jul1 > EndClim & EndClim >= 211 & StClim <= 211 ~ EndClim - 211 + 1,
    Jul1 > EndClim & EndClim < 211 ~ 0,
    Jul1 <= StClim ~ EndClim - StClim + 1
  ),
  DRate2 = ifelse(is.infinite(CorrInc / (nd1_ + nd2)),0, CorrInc / (nd1_ + nd2)),
  DRate2 = ifelse(nd1_ + nd2 == 0,0,DRate2),
  DRate2.w = ifelse(is.infinite(CorrInc / (nd1 + nd2)),0, CorrInc / (nd1 + nd2)),
  DRate2.w = ifelse(nd1 + nd2 == 0,0,DRate2.w),
  DRate1 = ifelse(is.infinite(CorrLstY / (nd1a + nd2a)),0,CorrLstY / (nd1a + nd2a)),
  DRate1 = ifelse(nd1a + nd2a == 0,0,DRate1),
  RATE = DRate2 * (nd1 + nd2r) + DRate1 * (nd1r),
  RATE.w = DRate2.w * (nd1 + nd2r) + DRate1 * (nd1r),
  PercRate =    round(100*((RATE / CorrInc)-1)),
  outliers = case_when(
    (PercClds >100 & Increm > 50) | (lag(PercClds) >100 & lag(Increm) > 50) ~ "cloulds",
    100 * (((RATE - DRate1*nd1r)  - CorrInc) / CorrInc) > 50 ~ "rates"
  ),
  final = ifelse(Outliers.inpe %in% c("rates","clouds"),Increm,rate.inpe),
  status.check = ifelse(abs(rate.inpe - RATE) <= 0.02,"OK","ERRADO"),
  status.w.check = ifelse(abs(rate.inpe - RATE.w) <= 0.02,"OK","ERRADO"),
  rate.check = round(rate.inpe - RATE,3),
  rate1.check = round(DRate1.inpe - DRate1,3),
  rate2.check = round(DRate2.inpe - DRate2,3),
  rate2w.check = round(DRate2.inpe - DRate2.w,3),
  nd1.check = nd1.inpe - nd1,
  nd1r.check = nd1r.inpe - nd1r,
  nd2r.check = nd2r.inpe - nd2r,
  out.check = Outliers.inpe ==outliers
)

xtabs(~df.fim$outliers + df.fim$Outliers.inpe ,addNA = TRUE)

df.ano <- df.fim %>% group_by(ano) %>% summarise(
  replicado = round(sum(final,na.rm = TRUE),0)
)

df.prodes <- tibble(
  ano = c(1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997,
          1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
          2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017,
          2018),
  prodes = c(21050, 17770, 13730, 11030, 13786, 14896, 14896, 29059,
             18161, 13227, 17383, 17259, 18226, 18165, 21651, 25396,
             27772, 19014, 14286, 11651, 12911, 7464, 7000, 6418, 4571, 5891,
             5012, 6207, 7893, 6947, 7536)
)
df.ano <- df.ano %>% left_join(df.prodes)
df.ano$dif <- df.ano$replicado - df.ano$prodes
df.ano$perc <- round(100 * (df.ano$dif / df.ano$prodes),1)

