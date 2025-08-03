###########################
###  Primary Crops studied      ##
target_crops <- c(
  "Wheat", "Grape", "Tomato", "Olive", "Barley", "Maize", "Almond", "Lettuce",
  "Chickpea", "Faba bean", "Cauliflower", "Fennel", "Proso millet", "Canola",
  "Alfalfa", "Lupin", "Cowpea", "Cullen", "Soybean", "Mugwort",
  "Apricot", "Lentil", "Blueberry", "Camelina", "Carob", "Zucchini", "Peppers",
  "Clover", "Savoy cabbage", "Glass wort", "Grapefruit", "Sorghum",
  "Moringa", "Orange", "Peach", "French serradella", "Pistachio", "Potato",
  "Quinoa", "Rooibos", "Safflower", "Saffron", "St Johns wort",
  "Sulla", "Teff", "Oat", "Ryegrass", "Cocksfoot", "Tall fescue", "Artemisia",
  "Mandarin", "Triticale", "Cardoon", "Sunflower", "Rapeseed", "Carrot"
)
target_crops_clean <- tools::toTitleCase(tolower(target_crops))
#mandarin triticale cardoon sunflower rapeseed carrot 

crop_type <- tribble(
  ~crop_Type,        ~crop,
  "Beverage crop",   "Rooibos",
  "Cereal",          "Barley",
  "Cereal",          "Proso Millet",
  "Cereal",          "Sorghum",
  "Cereal",          "Quinoa",
  "Cereal",          "Teff",
  "Cereal",          "Oat",
  "Cereal",      "Triticale",
  "Cereal",          "Wheat",
  "Cereal",          "Maize",
  "Fruit",           "Grape",
  "Fruit",           "Olive",
  "Fruit",       "Mandarin",
  "Fruit",           "Apricot",
  "Fruit",           "Blueberry",
  "Fruit",           "Orange",
  "Fruit",           "Peach",
  "Fruit",           "Grapefruit",
  "Grass",           "Ryegrass",
  "Grass",           "Cocksfoot",
  "Grass",           "Tall Fescue",
  "Herb",            "Artemisia",
  "Herb",            "Mugwort",
  "Herb",            "St Johns Wort",
  "Herb",            "Sulla",
  "Legume",          "Chickpea",
  "Legume",          "Faba Bean",
  "Legume",          "Alfalfa",
  "Legume",          "Lupin",
  "Legume",          "Cowpea",
  "Legume",          "Cullen",
  "Legume",          "Soybean",
  "Legume",          "Lentil",
  "Legume",          "Carob",
  "Legume",          "Moringa",
  "Legume",          "French Serradella",
  "Legume",          "Clover",
  "Nuts",            "Almond",
  "Nuts",            "Pistachio",
  "Oil",             "Camelina",
  "Oil",             "Safflower",
  "Oil",             "Canola",
  "Oil",         "Rapeseed",
  "Oil",         "Sunflower",
  "Root crop",       "Potato",
  "Spice",           "Saffron",
  "Spice",           "Fennel",
  "Vegetable",       "Cauliflower",
  "Vegetable",       "Zucchini",
  "Vegetable",       "Peppers",
  "Vegetable",       "Savoy Cabbage",
  "Vegetable",       "Glass Wort",
  "Vegetable",       "Tomato",
  "Vegetable",       "Lettuce",
  "Vegetable",   "Carrot",
  "Vegetable",   "Cardoon",
)

latin_crop <- tribble(
  ~crop,                ~latin_name,
  "Barley",             "Hordeum vulgare",
  "Proso millet",       "Panicum miliaceum",
  "Sorghum",            "Sorghum bicolor",
  "Quinoa",             "Chenopodium quinoa",
  "Teff",               "Eragrostis tef",
  "Oat",                "Avena sativa",
  "Wheat",              "Triticum spp",
  "Maize",              "Zea mays",
  "Grape",              "Vitis vinifera",
  "Apricot",            "Prunus armeniaca",
  "Blueberry",          "Vaccinium corymbosum",
  "Peach",              "Prunus persica",
  "Olive",              "Olea europaea",
  "Orange",             "Citrus x sinensis",
  "Grapefruit",         "Citrus x paradisi",
  "Ryegrass",           "Lolium spp",
  "Cocksfoot",          "Dactylis glomerata",
  "Tall fescue",        "Festuca arundinacea",
  "Rooibos",            "Aspalathus linearis",
  "Artemisia",           "Artemisia absinthium",
  "Mugwort",            "Artemisia vulgaris",
  "St Johns wort",      "Hypericum perforatum",
  "Sulla",              "Sulla coronaria",
  "Chickpea",           "Cicer arietinum",
  "Faba bean",          "Vicia faba",
  "Alfalfa",            "Medicago sativa",
  "Lupin",              "Lupinus spp",
  "Cowpea",             "Vigna unguiculata",
  "Cullen",             "Cullen spp",
  "Soybean",            "Glycine max",
  "Lentil",             "Lens culinaris",
  "Carob",              "Ceratonia siliqua",
  "Moringa",            "Moringa oleifera",
  "French serradella",  "Ornithopus sativus",
  "Clover",             "Trifolium spp",
  "Almond",             "Prunus dulcis",
  "Pistachio",          "Pistacia vera",
  "Camelina",           "Camelina sativa",
  "Safflower",          "Carthamus tinctorius",
  "Canola",             "Brassica napus",
  "Potato",             "Solanum tuberosum",
  "Saffron",            "Crocus sativus",
  "Fennel",             "Foeniculum vulgare",
  "Cauliflower",         "Brassica oleracea",
  "Zucchini",           "Cucurbita pepo",
  "Peppers",            "Capsicum spp",
  "Savoy cabbage",      "Brassica oleracea var. sabauda",
  "Glass wort",         "Salicornia spp",
  "Tomato",              "Solanum lycopersicum",
  "Lettuce",            "Lactuca sativa",
  "Mandarin",	           "Citrus reticulata",
  "Cardoon",            "Cynara cardunculus",
  "Carrot",             "Daucus carota",
  "Sunflower",          "Helianthus annuus",
  "Rapeseed",           "Brassica napus",
  "Triticale",         "× Triticosecale"    
)


# Step 1: Join crop_type and latin_crop to get a complete crop table
crop_data <- crop_type %>%
  full_join(latin_crop, by = "crop")

# Step 2: Filter to only keep crops in your target_crops list
crop_table <- crop_data %>%
  filter(crop %in% target_crops_clean) %>%
  arrange(crop_Type, crop)

# View the result
print(crop_table)

n <- nrow(crop_table)
half <- ceiling(n / 2)

left <- crop_table %>%
  slice(1:half) %>%
  rename(
    `Crop Type` = crop_Type,
    `Crop` = crop,
    ` ` = latin_name
  )

right <- crop_table %>%
  slice((half + 1):n) %>%
  rename(
    `Crop Type` = crop_Type,
    `Crop` = crop,
    `  ` = latin_name   # two spaces so headers are distinct blanks
  )

if (nrow(right) < nrow(left)) {
  n_pad <- nrow(left) - nrow(right)
  # Create a tibble of NAs with same columns as right
  pad <- right[rep(1, n_pad), ]
  pad[,] <- NA
  right <- bind_rows(right, pad)
}

double_col_table <- bind_cols(left, right)


ft <- double_col_table %>%
  flextable() %>%
  italic(j = c(3, 6)) %>%
  fontsize(size = 8, part = "all") %>%
  padding(padding = 1, part = "all") %>%
  autofit()

# Export to Word in landscape mode
read_docx() %>%
  body_add_flextable(value = ft) %>%
  print(target = "crop_table.docx")

#### Crop Count#########
crop_counts <- database %>%
  filter(!is.na(primary.crop.of.focus)) %>%
  mutate(
    matched_crops = map(primary.crop.of.focus, function(text) {
      text_lower <- tolower(text)
      found <- target_crops_clean[
        str_detect(text_lower, regex(tolower(target_crops_clean), ignore_case = TRUE))
      ]
      return(found)
    })
  ) %>%
  unnest(matched_crops) %>%
  count(matched_crops, name = "count") %>%
  arrange(desc(count)) %>%
  mutate(crop = str_to_title(matched_crops)) %>%
  select(crop, count)


#Crop count per country: 

crop_counts_by_country <- database %>%
  filter(!is.na(primary.crop.of.focus), !is.na(location.country)) %>%
  # Step 1: Standardize country names to prevent incorrect splitting
  mutate(
    location.country = str_replace_all(location.country, "South Africa", "South_Africa"),
    location.country = str_replace_all(location.country, "United States", "United_States"),
    location.country = str_replace_all(location.country, "Spain \\(not stated\\)", "Spain"),
    location.country = str_replace_all(location.country, "Spain Palestinian territories", "Spain, Palestinian territories"),
    location.country = str_replace_all(location.country, "\\(not stated\\)", "(not stated)"),
    location.country = str_replace_all(location.country, "Palestinian territories", "Palestinian_territories"),
    location.country = str_replace_all(location.country, "Global Mediterranean", "Global_Mediterranean"),
    matched_crops = map(primary.crop.of.focus, function(text) {
      text_lower <- tolower(text)
      found <- target_crops_clean[
        str_detect(text_lower, regex(tolower(target_crops_clean), ignore_case = TRUE))
      ]
      if (length(found) == 0) NA_character_ else found
    })
  ) %>%
  unnest(matched_crops, keep_empty = FALSE) %>%
  filter(!is.na(matched_crops)) %>%
  # Step 2: Split combined country names
  separate_rows(location.country, sep = ",|;| and | & |\\s(?=[A-Z])") %>%
  # Step 3: Clean and restore country names
  mutate(
    location.country = str_trim(location.country),
    location.country = str_replace_all(location.country, "_", " "),
    crop = str_to_title(matched_crops)
  ) %>%
  # Step 4: Remove empty or invalid entries
  filter(location.country != "") %>%
  # Step 5: Ensure unique combinations to avoid double-counting
  distinct(article.ID, crop, location.country, .keep_all = TRUE) %>%
  # Step 6: Count crops by country
  count(location.country, crop, name = "count") %>%
  # Step 7: Add total studies per country
  group_by(location.country) %>%
  mutate(total_studies = sum(count)) %>%
  ungroup() %>%
  arrange(location.country, desc(count)) %>%
  select(location.country, crop, count, total_studies)