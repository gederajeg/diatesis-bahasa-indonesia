# Ber- code

source("codes/verbind-retrieval.R")
ber <- verbs |>
  filter(pref_morphind=='ber-', suff_morphind=='0') |>
  group_by(morphind, root_morphind, root_pos_morphind, 
           affix_morphind, affix_morphind_wclass, pref_morphind, suff_morphind, 
           verb_tagged) |>
  summarise(n = sum(n)) |>
  arrange(desc(root_pos_morphind), desc(n))
ber

# Kemmer's (1993) appendix =====
naturally_reciprocal_event <- c("\\b(gelut|gulat|perang|tempur|tarung|cumbu|tikai|damai|temu|tegur|kelahi|tengkar|gaduh|hubungan|bicara|bincang|cakap|omong|gabung|satu|padu|pisah|dempet|cerai|tanding|kawan|teman)\\b")
ber |> 
  filter(str_detect(root_morphind, naturally_reciprocal_event)) |> 
  group_by(root_morphind, root_pos_morphind, affix_morphind_wclass, affix_morphind, verb_tagged) |> 
  summarise(n = sum(n), .groups = "drop") |> 
  arrange(desc(n))

naturally_collective_action <- c("\\b(kawanan|kerumun|kumpul|ramai|gombal|cokol|sarang|jalin|baur|campur|buyar|gerili?ya|gerombol|kelompok)\\b")
ber |> 
  filter(str_detect(root_morphind, naturally_collective_action)) |> 
  group_by(root_morphind, root_pos_morphind, affix_morphind_wclass, affix_morphind, verb_tagged) |> 
  summarise(n = sum(n), .groups = "drop") |> 
  arrange(desc(n))

grooming <- c("\\b(mandi|cukur|kuris|cuci|dandan|pakaian|sisir|sabuk|lipstik|pupur|bedak|baju|peci|anting|sanggul)\\b")
ber |> 
  filter(str_detect(root_morphind, grooming)) |> 
  group_by(root_morphind, root_pos_morphind, affix_morphind_wclass, affix_morphind, verb_tagged) |> 
  summarise(n = sum(n), .groups = "drop") |> 
  arrange(desc(n))

# especially when non-volitional
non_translational_motion <- c("\\b(balik|putar|kitar|belit|belok|bengkok|geliat|geliang|kelok|lenggok|lengkok|lengkung|papah|sender|sandar|sangga|sendeng|topang|pijak|tumpu|angguk|ayun|gerak|oleng|getar|gemetar|geletar|geligis|gentar|gigil|gonjang|goyang|gunjang|kedut|kedat|keletar|ketar|ketir|kijai|gecar|gelatuk|gemere?tak|gertak|gerumit|gesek|gosok|geser|gilir|gontai|guit|gulir|ingsut|alih|kalih|kelit|kiblat|kisar|putar|klesot|kucak|kutik|(se)?laju|selancar|langkah|larat|langsar|layap|gelayut)\\b")
ber |> 
  filter(str_detect(root_morphind, non_translational_motion)) |> 
  group_by(root_morphind, root_pos_morphind, affix_morphind_wclass, affix_morphind, verb_tagged) |> 
  summarise(n = sum(n), .groups = "drop") |> 
  arrange(desc(n))

# translational motion (Kemmer 1993: 18)
translational_motion <- c("\\b(pergi|datang|jalan|lari|terbang|lompat|renang|loncat|jatuh|tumbang|gugur|ungsep|runtuh|sungkur|geblak|urai|urug|ambrol|ambruk|ambyar|anjlok|anjlog|boya|lonjak|tanjak|tomplok|cucur|kabur|panjat|rangkak)\\b")
ber |> 
  filter(str_detect(root_morphind, translational_motion)) |> 
  group_by(root_morphind, root_pos_morphind, affix_morphind_wclass, affix_morphind, verb_tagged) |> 
  summarise(n = sum(n), .groups = "drop") |> 
  arrange(desc(n))

body_posture <- c("\\b(lutut|jongkok|dekam|rangkung|tiarap|bangkit|gidi[kg]|diam|sujud|sembah|pose)\\b")
ber |> 
  filter(str_detect(root_morphind, body_posture)) |> 
  group_by(root_morphind, root_pos_morphind, affix_morphind_wclass, affix_morphind, verb_tagged) |> 
  summarise(n = sum(n), .groups = "drop") |> 
  arrange(desc(n))

# positional
positional <- c("\\b(s[ae]n[ae]r|g(el)?antung|diri|rebah|baring|sila|duduk|simpuh|selonjor|pangku)\\b")
ber |> 
  filter(str_detect(root_morphind, positional)) |> 
  group_by(root_morphind, root_pos_morphind, affix_morphind_wclass, affix_morphind, verb_tagged) |> 
  summarise(n = sum(n), .groups = "drop") |> 
  arrange(desc(n))

# other body actions
body_action <- c("\\b(garuk|batuk|na[pf]as|masturbasi)\\b")
ber |> 
  filter(str_detect(root_morphind, body_action)) |> 
  group_by(root_morphind, root_pos_morphind, affix_morphind_wclass, affix_morphind, verb_tagged) |> 
  summarise(n = sum(n), .groups = "drop") |> 
  arrange(desc(n))

# self-protection
self_protection <- c("\\b(lindung|sembunyi|tahan|jaga)")

self_control <- c("\\b(tingkah|laku|perilaku|etika|lagak|sifat|watak)\\b")

# readiness
readiness <- c("\\b(siap)\\b")

ber |> write_tsv("data/ber.tsv")


# =================
library(tidyverse)
library(readxl)
# = THE FOLLOWING CODES HAVE BEEN RUN =
# ber <- readxl::read_xlsx('data/ber.xlsx', sheet = 1) |>
#   filter(!is.na(semantic_types)) |>
#   mutate(semantics = str_replace_all(semantics, "^emotion-(speech)", "\\1"),
#          semantics = str_replace_all(semantics, "\\?$", ""))
# ber |>
#   select(ID, morphind, root_morphind, semantic_types, root_pos_morphind, n) |>
#   slice_sample(n = 1000) |>
#   write_tsv("data/ber1000.tsv")
# = THE ABOVE CODES HAVE BEEN RUN =
ber <- readr::read_tsv("data/ber1000.tsv")
ber <- ber |> 
  group_by(semantic_types) |> 
  mutate(is_hapax = if_else(n == 1, TRUE, FALSE)) |> 
  ungroup()
ber_stats <- ber |> 
  group_by(semantic_types) |> 
  summarise(n_type = n_distinct(root_morphind),
            n_token = sum(n),
            n_hapax = sum(is_hapax),
            .groups = "drop") |> 
  mutate(# ttr = n_type/n_token,
         perc_type = round(n_type/sum(n_type) * 100, 1),
         perc_token = round(n_token/sum(n_token) * 100, 1),
         perc_hapax = round(n_hapax/sum(n_hapax) * 100, 1)) |> 
  arrange(desc(n_type))
ber_stats

