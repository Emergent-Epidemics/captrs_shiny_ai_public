#libraries
library(shiny)
library(openai)
library(elmer)
library(markdown)
library(jsonlite)
library(XML)
library(shinycssloaders)
library(bibliometrix)
library(pubmedR)
library(RefManageR)
library(reticulate)
library(readxl)
library(writexl)
library(DT)

#globals
url_kg <- "https://outbreak-kg.indra.bio/v1/" #API base
py_install("openai")

#reticulate
OpenAI <- import("openai")
os <- import("os")

#data
load("data/world_dev_indicator_data.RData") #dev indicators

load("data/world_health_indicator_data.RData") #health indicators

locations <- unique(world_dev_ind$`Country Name`)

secrets <- read.table("data/secrets.csv", header = TRUE,row.names = 1) #API keys

Sys.setenv(
  OPENAI_API_KEY = secrets["open_ai", "key"]
)

client = OpenAI$OpenAI(api_key = secrets["assist", "key"])
assistant = client$beta$assistants$retrieve("asst_5lEHOihNNucxvALwy6cnRZeB")

#acc functions
get_citations <- function(pubmed_id){ #gets citations from NCBI
  url_raw <- URLencode(pubmed_id)
  url_use <- paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?retmode=xml&db=pubmed&id=", url_raw)
  tmp <- tempfile()
  download.file(url_use, tmp)
  citation <- xmlTreeParse(tmp)
  citation
}

get_geoloc_info <- function(annotates){ #annotates with geolocation and matches to indicators
  use_geo_loc <- which(annotates$type == "geoloc")
  if(length(use_geo_loc) == 0){
    return()
  }
  use_health <- which(toupper(world_health_ind$`Country Name`) == toupper(annotates$name[use_geo_loc][1]))
  
  use_geo <- which(toupper(world_dev_ind$`Country Name`) == toupper(annotates$name[use_geo_loc]))
  
  list("health" = world_health_ind[use_health,], "geo" = world_dev_ind[use_geo, ])
}

get_ontology <- function(text){ #gets ontology results
  text_sub_pound <- gsub("[[:punct:]]", replacement = "", x = text)
  text_prompt <- URLencode(text_sub_pound)
  if(nchar(text_prompt) > 4000){
    text_prompt <- substr(x = text_prompt, start = 1, stop = 4000)
  }
  url_use <- paste0(url_kg, "text_relations?text=", text_prompt)
  tmp <- tempfile()
  download.file(url_use, tmp)
  text_raw <- readLines(tmp)
  text_raw_inf <- gsub(pattern = "-Infinity", replacement = "-9999", x = text_raw)
  json_out <- fromJSON(text_raw_inf)
  if(length(json_out$annotations) == 0){
    json_out$annotations <- matrix(NA, ncol = 3)
    colnames(json_out$annotations) <- c("name", "type",  "curie")
    json_out$annotations  <- as.data.frame(json_out$annotations)
  }
  annotates <- json_out$annotations[,c("name", "type",  "curie")]
  use_annote <- duplicated(annotates$name)
  annote_use <- annotates[which(use_annote == FALSE),]
  ord <- order(annote_use$name)
  annote_out <- annote_use[ord,]
  colnames(annote_out) <- c("Term", "Type", "CURIE ID")
  
  realism <- json_out$realism_score$scores

  if(length(realism) > 0){
    realism <- as.data.frame(realism)
    names_1 <- rep(NA, nrow(realism))
    names_2 <- rep(NA, nrow(realism))
    for(i in 1:nrow(realism)){
      mt.1.i <- which(annote_out$`CURIE ID` == paste0("MESH:",realism[i,1]))
      mt.2.i <- which(annote_out$`CURIE ID` == paste0("MESH:",realism[i,2]))
      
      if(length(mt.1.i) == 1){
        names_1[i] <- annote_out$Term[mt.1.i]
      }else{
        names_1[i] <- paste0("MESH:",realism[i,1])
      }
      
      if(length(mt.2.i) == 1){
        names_2[i] <- annote_out$Term[mt.2.i]
      }else{
        names_2[i] <- paste0("MESH:",realism[i,2])
      }
    }
    realism[,1] <- names_1
    realism[,2] <- names_2
    colnames(realism) <- c("Term 1", "Term 2", "Realism Score")
    qual_score <- rep("Low", nrow(realism))
    qual_score[which(realism$`Realism Score`>-7)] <- "Medium"
    qual_score[which(realism$`Realism Score`>-4.5)] <- "High"
    realism$`Realism Score` <- qual_score
    realism_cat <- json_out$realism_score$classification
  }else{
    realism <- matrix(NA, ncol = 3)
    colnames(realism) <- c("Term 1", "Term 2", "Realism Score")
    realism <- as.data.frame(realism)
    realism_cat <- NA
  }
  geo_info <- get_geoloc_info(annotates)
  
  promed_ids <- json_out$alerts$alert$name
  if(length(promed_ids) == 0){
    promed_ids_out <- NA
  }else{
    promed_ids_out <- promed_ids[length(promed_ids)]
  }
  list("annotations" = annote_out, "promed" = promed_ids_out, "geo_info" = geo_info, "realism_cat" = realism_cat, "realism" = realism)
}

make_health_prompt <- function(country, inds = c("Urban population", "UHC service coverage index", "Rural population (% of total population)", "Physicians (per 1,000 people)", "Nurses and midwives (per 1,000 people)", "Mortality rate attributed to unsafe water, unsafe sanitation and lack of hygiene (per 100,000 population)", "Life expectancy at birth, total (years)", "Immunization, measles second dose (% of children by the nationally recommended age)", "Current health expenditure (% of GDP)")){ #makes health indicator prompt
  use_world_health_ind <- which(world_health_ind$`Country Name` == country & world_health_ind$`Series Name` %in% inds)
  health_df_use <- world_health_ind[use_world_health_ind,c("2019 [YR2019]", "2020 [YR2020]", "2021 [YR2021]", "2022 [YR2022]", "2023 [YR2023]")]
  for(i in 1:ncol(health_df_use)){
    mk.num.i <- as.numeric(unlist(health_df_use[,i]))
    health_df_use[,i] <- mk.num.i
  }
  vals <- rowMeans(health_df_use, na.rm = T)
  prompt_rows <- paste(world_health_ind$`Series Name`[use_world_health_ind], vals, collapse = " is ")
  prompt <- paste(prompt_rows, collapse = " and ")
  return(prompt)
}

make_dev_prompt <- function(country, inds = c("Military expenditure (% of GDP)", "Land area (sq. km)", "Investment in water and sanitation with private participation (current US$)", "Internally displaced persons, total displaced by conflict and violence (number of people)", "Intentional homicides (per 100,000 people)", "Inflation, consumer prices (annual %)", "Income share held by highest 10%", "Income share held by lowest 10%", "Incidence of malaria (per 1,000 population at risk)", "Gross national expenditure (current US$)", "Government Effectiveness: Percentile Rank", "Gini index", "GDP per capita (current US$)", "Forest area (% of land area)", "Control of Corruption: Percentile Rank", "Cause of death, by communicable diseases and maternal, prenatal and nutrition conditions (% of total)", "Birth rate, crude (per 1,000 people)", "Armed forces personnel (% of total labor force)", "Gross national expenditure (% of GDP)", "Internally displaced persons, total displaced by conflict and violence (number of people")){ #makes dev indicator prompt
  use_world_dev_ind <- which(world_dev_ind$`Country Name` == country & world_dev_ind$`Series Name` %in% inds)
  dev_df_use <- world_dev_ind[use_world_dev_ind,c("2019 [YR2019]", "2020 [YR2020]", "2021 [YR2021]", "2022 [YR2022]", "2023 [YR2023]")]
  for(i in 1:ncol(dev_df_use)){
    mk.num.i <- as.numeric(unlist(dev_df_use[,i]))
    dev_df_use[,i] <- mk.num.i
  }
  vals <- rowMeans(dev_df_use, na.rm = T)
  prompt_rows <- paste(world_dev_ind$`Series Name`[use_world_dev_ind], vals, sep = " is ")
  prompt <- paste(prompt_rows, collapse = " and ")
  return(prompt)
}

make_prompt <- function(prompt){ #makes prompt
  split_pathogen <- strsplit(x = prompt, split = "PATHOGEN_HERE")
  split_severity <- strsplit(x = split_pathogen[[1]][2], split = "SEVERITY_HERE")
  split_geopolitical <- strsplit(x = split_severity[[1]][2], split = "LOCATION_HERE")
  split_location <- strsplit(x = split_geopolitical[[1]][2], split = "HEALTH_IND")
  split_health <- strsplit(x = split_location[[1]][2], split = "DEV_IND")
  split_dev <- strsplit(x = split_health[[1]][2], split = "DEV_IND")
  prompt_pieces <- list("start_pathogen" = split_pathogen[[1]][1], "severity" = split_severity[[1]][1], "geopolitical" = split_geopolitical[[1]][1], "location" = split_location[[1]][1], "health_ind" =  split_health[[1]][1], "dev_ind_end" = split_dev[[1]][1])
  return(prompt_pieces)
}

format_rag_response <- function(messages){
  split1 <- strsplit(messages, split = "value=")
  use1 <- split1[[1]][2]
  split2 <- strsplit(use1, split = "), type='text')]")
  use2 <- split2[[1]][1]
  return(use2)
}