library(tidyverse)

verbs <- readr::read_tsv(file = "https://raw.githubusercontent.com/gederajeg/afiksasi-verba-bahasa-indonesia/main/verbs_main.txt")

gdrive_path <- "/Users/Primahadi/Library/CloudStorage/GoogleDrive-primahadi_wijaya@unud.ac.id/My Drive/2022-07-30-tatabahasa-indonesia-kontemporer/"

genre_levels <- c("Surat_Resmi", "Perundang_undangan", "Jurnal", 
                  "Disertasi_Tesis_Skripsi", "Buku_Teks", "Laman_Resmi", 
                  "Populer", "Biografi", "Majalah", 
                  "Koran", "Cerpen", "Novel")

verbs <- verbs %>% 
  mutate(genres = factor(genres, levels = genre_levels))

genressizes <- readr::read_tsv(paste(gdrive_path, "genressizes.txt", sep = "")) %>% 
  mutate(genres = factor(genres, levels = genre_levels))

yearsizes <- readr::read_tsv(paste(gdrive_path, "yearsizes.txt", sep = "")) %>% 
  mutate(year = as.character(year))

corpussizes_all <- as.numeric(readr::read_lines(paste(gdrive_path, "corpussizes_all.txt", sep = "")))
                              