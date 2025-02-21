kegg_parse_json = function(kegg) {
  # Reads the kegg json (https://www.kegg.jp/kegg-bin/download_htext?htext=ko00001.keg&format=json&filedir=) and outputs a neat table
  
  unnest = kegg$children %>% unnest(children, names_repair = "universal") %>% # AB 57
    unnest(children, names_repair = "universal") %>% # C 551
    unnest(children, names_repair = "universal") %>% # D 59868
    
    rename(
      a_class = 1, # E.g. "09100 Metabolism"
      b_class = 2, # E.g. "09101 Carbohydrate metabolism"
      pathway = 3, 
      reaction = 4
    ) %>%
    separate_wider_regex(pathway, # E.g. "00010 Glycolysis \/ Gluconeogenesis [PATH:ko00010]"
                         c(
                           pathway = "^[^\\[]+", # Anything that isn't a [
                           pathway_id = "\\[.+\\]$"),
                         too_few = "align_start"
    ) %>%
    separate_wider_regex(reaction, # E.g. "K00844  HK; hexokinase [EC:2.7.1.1]"
                         c( # "name":"K04022  eutG; alcohol dehydrogenase"
                           reaction_ko = "^K[0-9]+",
                           "  ",
                           reaction_abbreviation = "[^;]+", # Anything that isn't a ;
                           "; ",
                           reaction_name = "[^\\[]+", # Anything that isn't a [
                           " \\[",
                           reaction_ec = ".+",
                           "\\]$"), 
                         too_few = "align_start"
    ) %>% 
    mutate(reaction_ec = str_trim(reaction_ec)) %>% 
    mutate(illegal_reaction = is.na(reaction_ko))
  
  # Check and report on the number of reactions with illegal names.
  illegals = unnest %>% filter(illegal_reaction)
  if (nrow(illegals) > 0) {
    message(paste("\nWarning:", nrow(illegals), "reaction(s) have an illegal name."))
    message(illegals$reaction %>% head())
  }
  
  # Return everything that isn't illegal.
  unnest %>% filter(!illegal_reaction) %>% 
    select(-illegal_reaction)
  
}

# found this at https://gist.github.com/cmkobel/c2f76359eb17e0c00ddd4e83e90bdf99