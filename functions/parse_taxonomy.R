# Custom function to parse rownames and assign taxonomic levels
parse_taxonomy <- function(tax_string) {
  # Split the string by '|'
  tax_levels <- str_split(tax_string, "\\|")[[1]]
  
  # Initialize a named list with all taxonomic levels as NA
  tax_list <- list(Domain = NA, Phylum = NA, Class = NA, Order = NA, Family = NA, Genus = NA, Species = NA)
  
  # Loop through each taxonomic level and assign to the correct place
  for (level in tax_levels) {
    if (str_detect(level, "^d__")) {
      tax_list$Domain <- level
    } else if (str_detect(level, "^p__")) {
      tax_list$Phylum <- level
    } else if (str_detect(level, "^c__")) {
      tax_list$Class <- level
    } else if (str_detect(level, "^o__")) {
      tax_list$Order <- level
    } else if (str_detect(level, "^f__")) {
      tax_list$Family <- level
    } else if (str_detect(level, "^g__")) {
      tax_list$Genus <- level
    } else if (str_detect(level, "^s__")) {
      tax_list$Species <- level
    }
  }
  
  # Return the parsed taxonomy as a data frame
  return(as_tibble(tax_list))
}