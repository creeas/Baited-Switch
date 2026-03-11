# title: 03_decompose_artis

# load packages
library(data.table)
library(tidyverse)

# load ARTIS consumption data filtered to chondrichthyes 
# (generated in 01_filter_artis)
consumption <- fread("data/consumption_v2_0_sharks.csv")

# load ARTIS taxa table
sciname <- read.csv("data/sciname.csv")

# load taxa table from Chris Mull
taxa_add <- read.csv("data/taxonomy_20240205.csv") %>%
  select(-EDGE.Scientific.name) %>%
  rename("sciname" = "species_binomial") %>%
  mutate(sciname = tolower(sciname),
         Genus = tolower(Genus),
         Family = tolower(Family),
         Order = tolower(Order),
         Superorder = tolower(Superorder),
         Class = tolower(Class)) %>%
  mutate(Order = case_match(Order,
                            "myliobatiformes" ~ "rajiformes",
                            "rhinopristiformes" ~ "rajiformes",
                            "torpediniformes" ~ "rajiformes",
                            .default = Order))

# load augmented landings data from Chris Mull
landings_resolved <- read.csv("data/spp_land_trade.csv") %>%
  mutate(species = tolower(species)) %>%
  left_join(taxa_add,
            by = c("species" = "sciname")) %>%
  rename("source_country_iso3c" = "country", "sciname" = "species") %>% 
  filter(landings >= 0.1)


# sequentially decompose ARTIS data
consumption_resolved <- consumption %>%
  filter(year %in% 2012:2019) %>%
  group_by(source_country_iso3c, exporter_iso3c, consumer_iso3c, 
           consumption_source, sciname) %>%
  summarise(consumption_live_t = sum(consumption_live_t)/length(2012:2019)) %>%
  filter(consumption_live_t >= 1)

# Check totals for ARTIS average vs landings resolved
test <- consumption_resolved %>%
  group_by(source_country_iso3c) %>%
  summarise(artis_total = sum(consumption_live_t)) %>%
  full_join(landings_resolved %>%
              group_by(source_country_iso3c) %>%
              summarise(landings_total = sum(landings)),
            by = c("source_country_iso3c")) %>%
  mutate(diff = abs(artis_total-landings_total))

# For each level: 
# 1. Filter ARTIS consumption to observations occurring at that level
# 2. Join to disaggregated landings
# 3. Subtract volume already accounted for in ARTIS from landings

# Species
consumption_sp <- consumption_resolved %>%
  filter(sciname %in% taxa_add$sciname)

landings_resolved_adj_sp <- landings_resolved %>%
  left_join(consumption_sp %>%
              group_by(source_country_iso3c, sciname) %>%
              summarise(consumption_live_t = sum(consumption_live_t)),
            by = c("source_country_iso3c", "sciname")) %>%
  replace_na(list(consumption_live_t = 0)) %>%
  mutate(landings_adj = landings - consumption_live_t) %>%
  filter(landings_adj > 0) %>%
  select(-consumption_live_t, -landings) %>%
  rename("landings" = "landings_adj")

paste("unallocated resolved landings total = ", sum(landings_resolved_adj_sp$landings))

total_to_disaggregate <- consumption_resolved %>% 
  filter(!(sciname %in% taxa_add$sciname), 
         source_country_iso3c != "unknown") %>%
  ungroup() %>%
  summarise(total = sum(consumption_live_t)) %>%
  pull(total)

paste("total volume to disaggregate = ", total_to_disaggregate)

# Genus
landings_resolved_adj_props <- landings_resolved_adj_sp %>%
  group_by(source_country_iso3c, Genus) %>%
  mutate(prop = landings/sum(landings))

consumption_genus <- consumption_resolved %>%
  filter(sciname %in% taxa_add$Genus) %>%
  left_join(landings_resolved_adj_props %>%
              select(source_country_iso3c, Genus, species = sciname, prop), 
            by = c("source_country_iso3c", "sciname" = "Genus"),
            relationship = "many-to-many") 

consumption_genus_na <- consumption_genus %>%
  filter(is.na(prop))

consumption_genus <- consumption_genus %>%
  mutate(consumption_live_t = consumption_live_t*prop) %>%
  select(-sciname) %>%
  rename("sciname" = "species") %>%
  filter(!is.na(prop))

landings_resolved_adj_genus <- landings_resolved_adj_sp %>%
  left_join(consumption_genus %>%
              group_by(source_country_iso3c, sciname) %>%
              summarise(consumption_live_t = sum(consumption_live_t)),
            by = c("source_country_iso3c", "sciname")) %>%
  replace_na(list(consumption_live_t = 0)) %>%
  mutate(landings_adj = landings - consumption_live_t) %>%
  filter(landings_adj > 0) %>%
  select(-consumption_live_t, -landings) %>%
  rename("landings" = "landings_adj")

paste("unallocated resolved landings total = ", sum(landings_resolved_adj_genus$landings))

# Family
landings_resolved_adj_props <- landings_resolved_adj_genus %>%
  group_by(source_country_iso3c, Family) %>%
  mutate(prop = landings/sum(landings))

consumption_family <- consumption_resolved %>%
  filter(sciname %in% taxa_add$Family) %>%
  left_join(landings_resolved_adj_props %>%
              select(source_country_iso3c, Family, species = sciname, prop), 
            by = c("source_country_iso3c", "sciname" = "Family"),
            relationship = "many-to-many") 

consumption_family_na <- consumption_family %>%
  filter(is.na(prop))

consumption_family <- consumption_family %>%
  mutate(consumption_live_t = consumption_live_t*prop)  %>%
  select(-sciname) %>%
  rename("sciname" = "species") %>%
  filter(!is.na(prop))

landings_resolved_adj_family <- landings_resolved_adj_genus %>%
  left_join(consumption_family %>%
              group_by(source_country_iso3c, sciname) %>%
              summarise(consumption_live_t = sum(consumption_live_t)),
            by = c("source_country_iso3c", "sciname")) %>%
  replace_na(list(consumption_live_t = 0)) %>%
  mutate(landings = landings - consumption_live_t) %>%
  filter(landings > 0) %>%
  select(-consumption_live_t)

paste("unallocated resolved landings total = ", sum(landings_resolved_adj_family$landings))

# Order
landings_resolved_adj_props <- landings_resolved_adj_family %>%
  group_by(source_country_iso3c, Order) %>%
  mutate(prop = landings/sum(landings))

consumption_order <- consumption_resolved %>%
  filter(sciname %in% taxa_add$Order) %>%
  left_join(landings_resolved_adj_props %>%
              select(source_country_iso3c, Order, species = sciname, prop), 
            by = c("source_country_iso3c", "sciname" = "Order"),
            relationship = "many-to-many") 

consumption_order_na <- consumption_order %>%
  filter(is.na(prop))

consumption_order <- consumption_order %>%
  mutate(consumption_live_t = consumption_live_t*prop)%>%
  select(-sciname) %>%
  rename("sciname" = "species")%>%
  filter(!is.na(prop))

landings_resolved_adj_order <- landings_resolved_adj_family %>%
  left_join(consumption_order %>%
              group_by(source_country_iso3c, sciname) %>%
              summarise(consumption_live_t = sum(consumption_live_t)),
            by = c("source_country_iso3c", "sciname")) %>%
  replace_na(list(consumption_live_t = 0)) %>%
  mutate(landings = landings - consumption_live_t) %>%
  filter(landings > 0) %>%
  select(-consumption_live_t)

paste("unallocated resolved landings total = ", sum(landings_resolved_adj_order$landings))

# Class
landings_resolved_adj_props <- landings_resolved_adj_order %>%
  group_by(source_country_iso3c, Class) %>%
  mutate(prop = landings/sum(landings))

consumption_class <- consumption_resolved %>%
  filter(sciname %in% taxa_add$Class) %>%
  left_join(landings_resolved_adj_props %>%
              select(source_country_iso3c, Class, species = sciname, prop), 
            by = c("source_country_iso3c", "sciname" = "Class"),
            relationship = "many-to-many") 

consumption_class_na <- consumption_class %>%
  filter(is.na(prop))

consumption_class <- consumption_class %>%
  mutate(consumption_live_t = consumption_live_t*prop) %>%
  select(-sciname) %>%
  rename("sciname" = "species") %>%
  filter(!is.na(prop))

landings_resolved_adj_class <- landings_resolved_adj_order %>%
  left_join(consumption_class %>%
              group_by(source_country_iso3c, sciname) %>%
              summarise(consumption_live_t = sum(consumption_live_t)),
            by = c("source_country_iso3c", "sciname")) %>%
  replace_na(list(consumption_live_t = 0)) %>%
  mutate(landings = landings - consumption_live_t) %>%
  filter(landings > 0) %>%
  select(-consumption_live_t)

paste("unallocated resolved landings total = ", sum(landings_resolved_adj_class$landings))

# Stitch consumption together
consumption_resolved_out <- consumption_sp %>%
  bind_rows(consumption_genus) %>%
  bind_rows(consumption_family) %>%
  bind_rows(consumption_order) %>%
  bind_rows(consumption_class) %>%
  select(-prop) %>%
  mutate(data_source = "disaggregated from ARTIS") %>%
  bind_rows(landings_resolved_adj_class %>%
              mutate(
                exporter_iso3c = NA,
                consumer_iso3c = source_country_iso3c, 
                consumption_source = "domestic", 
                data_source = "added from latent landings") %>%
              select(source_country_iso3c, exporter_iso3c, consumer_iso3c,
                     consumption_source, sciname, "consumption_live_t" = "landings",
                     data_source
              )) %>%
  ungroup()

consumption_resolved_na <- consumption_genus_na %>%
  bind_rows(consumption_family_na) %>%
  bind_rows(consumption_order_na) %>%
  bind_rows(consumption_class_na) %>%
  select(-prop) %>%
  ungroup()

write.csv(consumption_resolved_out, "data/consumption_resolved.csv", row.names = FALSE)