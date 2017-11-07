if (!require("pacman")) install.packages("pacman")
# run sudo apt-get install -y libtesseract-dev libleptonica-dev tesseract-ocr-eng tesseract-ocr-spa from terminal for tesseract
p_load(dplyr,tidyr,readr,stringr,RCurl,XML,pdftools,tidytext,devtools,tesseract)
p_load_gh("ropensci/tabulizer")

if(!dir.exists("01_1_raw_documents")) {
  try(dir.create("01_1_raw_documents"))
  
  url = "https://www.servel.cl/programas-de-candidaturas-a-presidente-de-la-republica/"
  url2 = getURL(url)
  parsed = htmlParse(url2)
  
  links_programas = as_tibble(xpathSApply(parsed, path = "//a", xmlGetAttr, "href")) %>% 
    filter(str_detect(value, 'pdf'))
  
  archivos_programas = links_programas %>% 
    mutate(value = gsub(".*/","",value))
  
  for(j in 1:nrow(links_programas)) {
    archivo = archivos_programas$value[[j]]
    if(!file.exists(paste0("01_1_raw_documents/",archivo))) {
      url = links_programas$value[[j]]
      try(download.file(url, paste0("01_1_raw_documents/",archivo)))
    }
  }
  
  if(!file.exists("stopwords-es.txt")){
    try(download.file("https://raw.githubusercontent.com/stopwords-iso/stopwords-es/master/stopwords-es.txt","stopwords-es.txt"))
  }
}

try(dir.create("01_2_processed_documents"))
stopwords_es = read_csv("stopwords-es.txt", col_names = F) %>% setNames(., "word")

#############

if(!file.exists("01_2_processed_documents/words_guillier.csv")) {
  text_guillier = pdf_text("01_1_raw_documents/Programa_Alejandro_Guillier_Alvarez.pdf")
  
  words_guillier = text_guillier %>% 
    iconv() %>%
    tibble() %>%
    setNames(., "text") %>%
    mutate(text = gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", text, perl=TRUE),
           candidato =  "guillier") %>% 
    unnest_tokens(word, text) %>% 
    anti_join(stopwords_es)
  
  top_words_guillier = words_guillier %>%
    count(word, sort = TRUE)
  
  write_csv(words_guillier,"01_2_processed_documents/words_guillier.csv")
} else {
  words_guillier = read_csv("01_2_processed_documents/words_guillier.csv")
  
  top_words_guillier = words_guillier %>%
    count(word, sort = TRUE)
}

#############

if(!file.exists("01_2_processed_documents/words_navarro.csv")) {
  text_navarro = pdf_text("01_1_raw_documents/Programa_Alejandro_Navarro_Brain.pdf")
  
  words_navarro = text_navarro %>% 
    iconv() %>%
    tibble() %>%
    setNames(., "text") %>%
    mutate(text = gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", text, perl=TRUE),
           candidato =  "navarro") %>% 
    unnest_tokens(word, text) %>% 
    anti_join(stopwords_es)
  
  top_words_navarro = words_navarro %>%
    count(word, sort = TRUE)
  
  write_csv(words_navarro,"01_2_processed_documents/words_navarro.csv")
} else {
  words_navarro = read_csv("01_2_processed_documents/words_navarro.csv")
  
  top_words_navarro = words_navarro %>%
    count(word, sort = TRUE)
}

#############

if(!file.exists("01_2_processed_documents/words_sanchez.csv")) {
  text_sanchez = pdf_text("01_1_raw_documents/Programa_Beatriz_Sanchez_Munoz.pdf")
  
  words_sanchez = text_sanchez %>% 
    iconv() %>%
    tibble() %>%
    setNames(., "text") %>%
    mutate(text = gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", text, perl=TRUE),
           candidato =  "sanchez") %>% 
    unnest_tokens(word, text) %>% 
    anti_join(stopwords_es)
  
  top_words_sanchez = words_sanchez %>%
    count(word, sort = TRUE)
  
  write_csv(words_sanchez,"01_2_processed_documents/words_sanchez.csv")
} else {
  words_sanchez = read_csv("01_2_processed_documents/words_sanchez.csv")
  
  top_words_sanchez = words_sanchez %>%
    count(word, sort = TRUE)
}

#############

if(!file.exists("01_2_processed_documents/words_goic.csv")) {
  text_goic = pdf_text("01_1_raw_documents/Programa_Carolina_Goic_Boroevic.pdf")
  
  words_goic = text_goic %>% 
    iconv() %>%
    tibble() %>%
    setNames(., "text") %>%
    mutate(text = gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", text, perl=TRUE),
           candidato =  "goic") %>% 
    unnest_tokens(word, text) %>% 
    anti_join(stopwords_es)
  
  top_words_goic = words_goic %>%
    count(word, sort = TRUE)
  
  write_csv(words_goic,"01_2_processed_documents/words_goic.csv")
} else {
  words_goic = read_csv("01_2_processed_documents/words_goic.csv")
  
  top_words_goic = words_goic %>%
    count(word, sort = TRUE)
}

#############

if(!file.exists("01_2_processed_documents/words_enriquez.csv")) {
  text_enriquez = pdf_text("01_1_raw_documents/Programa_Gobierno_Marco_Enriquez-Ominami.pdf")
  
  words_enriquez = text_enriquez %>% 
    iconv() %>%
    tibble() %>%
    setNames(., "text") %>%
    mutate(text = gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", text, perl=TRUE),
           candidato =  "enriquez") %>% 
    unnest_tokens(word, text) %>% 
    anti_join(stopwords_es)
  
  top_words_enriquez = words_enriquez %>%
    count(word, sort = TRUE)
  
  write_csv(words_enriquez,"01_2_processed_documents/words_enriquez.csv")
} else {
  words_enriquez = read_csv("01_2_processed_documents/words_enriquez.csv")
  
  top_words_enriquez = words_enriquez %>%
    count(word, sort = TRUE)
}

#############

if(!file.exists("01_2_processed_documents/words_enriquez.csv")) {
  text_enriquez = pdf_text("01_1_raw_documents/Programa_Gobierno_Marco_Enriquez-Ominami.pdf")
  
  words_enriquez = text_enriquez %>% 
    iconv() %>%
    tibble() %>%
    setNames(., "text") %>%
    mutate(text = gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", text, perl=TRUE),
           candidato =  "enriquez") %>% 
    unnest_tokens(word, text) %>% 
    anti_join(stopwords_es)
  
  top_words_enriquez = words_enriquez %>%
    count(word, sort = TRUE)
  
  write_csv(words_enriquez,"01_2_processed_documents/words_enriquez.csv")
} else {
  words_enriquez = read_csv("01_2_processed_documents/words_enriquez.csv")
  
  top_words_enriquez = words_enriquez %>%
    count(word, sort = TRUE)
}

#############

if(!file.exists("01_2_processed_documents/words_artes.csv")) {
  text_artes = pdf_text("01_1_raw_documents/Programa_Eduardo_Artes_Brichetti.pdf")
  
  words_artes = text_artes %>% 
    iconv() %>%
    tibble() %>%
    setNames(., "text") %>%
    mutate(text = gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", text, perl=TRUE),
           candidato =  "artes") %>% 
    unnest_tokens(word, text) %>% 
    anti_join(stopwords_es)
  
  top_words_artes = words_artes %>%
    count(word, sort = TRUE)
  
  write_csv(words_artes,"01_2_processed_documents/words_artes.csv")
} else {
  words_artes = read_csv("01_2_processed_documents/words_artes.csv")
  
  top_words_artes = words_artes %>%
    count(word, sort = TRUE)
}

#############

if(!file.exists("01_2_processed_documents/words_kast.csv")) {
  text_kast = pdf_text("01_1_raw_documents/Programa_Jose_Antonio_Kast_Rist.pdf")
  
  words_kast = text_kast %>% 
    iconv() %>%
    tibble() %>%
    setNames(., "text") %>%
    mutate(text = gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", text, perl=TRUE),
           candidato =  "kast") %>% 
    unnest_tokens(word, text) %>% 
    anti_join(stopwords_es)
  
  top_words_kast = words_kast %>%
    count(word, sort = TRUE)
  
  write_csv(words_enriquez,"01_2_processed_documents/words_kast.csv")
} else {
  words_kast = read_csv("01_2_processed_documents/words_kast.csv")
  
  top_words_kast = words_kast %>%
    count(word, sort = TRUE)
}

#############

if(!file.exists("01_2_processed_documents/words_pinera.csv")) {
  info_pinera = pdf_info("01_1_raw_documents/Programa_Sebastian_Pinera_Echenique.pdf")
  
  try(dir.create("01_2_processed_documents/pinera/"))
  
  for(i in 1:info_pinera$pages) {
    bitmap = pdf_render_page("01_1_raw_documents/Programa_Sebastian_Pinera_Echenique.pdf", page = i)
    png::writePNG(bitmap, paste0("01_2_processed_documents/pinera/page",i,".png"))
    rm(bitmap)
  }
  
  lista_archivos_pinera = list.files(path = "01_2_processed_documents/pinera/", pattern = "png", recursive = F) %>% 
    paste0("01_1_processed_documents/pinera/", .)
  
  for(i in 1:length(lista_archivos_pinera)) {
    page = ocr(paste0("01_2_processed_documents/pinera/page",i,".png"), engine = tesseract("spa"))
    envir = as.environment(1)
    assign(paste0("page_",i), page, envir = envir)
    rm(page)
  }
  
  words_pinera = mget(ls(pattern = "page_")) %>% 
    tibble() %>% 
    bind_rows() %>% 
    setNames(., "text") %>%
    mutate(text = gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", text, perl=TRUE),
           candidato =  "pinera") %>% 
    unnest_tokens(word, text) %>% 
    anti_join(stopwords_es)
  
  top_words_pinera = words_pinera %>%
    count(word, sort = TRUE)
  
  rm(list = ls(pattern = "page_[0-9]"))
  write_csv(words_pinera,"01_2_processed_documents/words_pinera.csv")
} else {
  words_pinera = read_csv("01_2_processed_documents/words_pinera.csv")
  
  top_words_pinera = words_pinera %>%
    count(word, sort = TRUE)
}

#############

words_all = mget(ls(pattern = "^words_")) %>% 
  bind_rows() %>% 
  group_by(candidato) %>% 
  count(word, sort = TRUE) %>% 
  bind_tf_idf(word, candidato, n)

write_csv(words_all,"01_2_processed_documents/words_all.csv")
