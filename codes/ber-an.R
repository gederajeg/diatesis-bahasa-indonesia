# ber- -an codes

# ==== codes below have been run! ====

# source("codes/verbind-retrieval.R")
# ber_an <- verbs |> 
#   filter(pref_morphind=='ber-', suff_morphind=='an') |> 
#   group_by(morphind, root_morphind, root_pos_morphind, affix_morphind, affix_morphind_wclass, pref_morphind, suff_morphind, verb_tagged) |> 
#   summarise(n = sum(n)) |> 
#   arrange(desc(n))
  # ber_an |> write_tsv("data/ber_an.tsv")

# verbs |> 
#   filter(pref_morphind=='ber-', suff_morphind=='an', !word_form %in% c("bergan-", "bersan-", "bermian", "bersaan")) |> 
#   pull(word_form) |> 
#   unique() |> 
#   write_lines("data/ber_an_wordform.txt")

# ==== codes above have been run! ====

ber_an0 <- read_tsv("data/ber_an.tsv") 
ber_an <- ber_an0 |> 
  filter(ok) |> 
  group_by(root_morphind, root_pos_morphind, affix_morphind_wclass) |> 
  summarise(n = sum(n)) |> 
  arrange(desc(n))
ber_an_type_by_pos <- ber_an |> 
  group_by(root_pos_morphind) |> 
  summarise(n_type = n_distinct(root_morphind), .groups = "drop") |> 
  arrange(desc(n_type))
ber_an_hapax_by_pos <- ber_an |> 
  filter(n == 1) |> 
  group_by(root_pos_morphind) |> 
  summarise(n_hpx = n_distinct(root_morphind), .groups = "drop") |> 
  arrange(desc(n_hpx))
ber_an_tokens_by_pos <- ber_an |> 
  group_by(root_pos_morphind) |> 
  summarise(tokens = sum(n), .groups = "drop") |> 
  arrange(desc(tokens))
ber_an_productivity <- ber_an_tokens_by_pos |> 
  left_join(ber_an_type_by_pos) |> 
  left_join(ber_an_hapax_by_pos)

ber_an_words <- read_lines('data/ber_an_wordform.txt')
ber_an_words_rgx <- ber_an_words |> paste(collapse = "|") %>% paste("(", ., ")\\b", sep = "")
source('codes/word_use_in_corpus.R')
saling_ber_an <- concord_tbik(rgx = paste("\\bsaling ", ber_an_words_rgx, sep = ""))
ber_an_conc <- concord_tbik(rgx = paste("\\b", ber_an_words_rgx, sep = ""))

# get lines where "ber-/-an" is not preceded immediately by "saling"
ber_an_saling_around_left <- ber_an_conc |> 
  filter(str_detect(LEFT, "saling$", negate = TRUE)) |> 
  # and from those lines, check if "saling" still present around the left context
  filter(str_detect(LEFT, "\\bsaling [a-z]+"))
# ber_an_saling_around_left |> write_tsv("data/ber_an_saling_around_left.tsv")
