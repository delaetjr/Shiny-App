# Load necessary libraries
library(nflreadr)
library(dplyr)
library(purrr)
library(readr)
library(stringr)

# Step 1: Load Roster Data --------------------------------------------------

# Load annual rosters with `season` column
roster_data <- load_rosters(2016:2024) %>%
  select(season, pff_id, full_name, rookie_year, draft_club, draft_number, position, jersey_number, team, smart_id) %>%
  rename(Player = full_name, team_name = team)  # Standardizing column names

roster_data <- roster_data %>%
  filter(!is.na(smart_id))

# Standardize `pff_teams` to match `roster_teams`
roster_data <- roster_data %>%
  mutate(team_name = recode(team_name, 
                            "BAL" = "BLT", 
                            "ARI" = "ARZ", 
                            "HOU" = "HST", 
                            "CLE" = "CLV"
  ))


# **Step 1: Rename "Player" to "player" in `roster_data` for Matching**
roster_data <- roster_data %>%
  rename(player = Player)  # Ensure Consistent Column Name

# Convert pff_id to numeric
roster_data <- roster_data %>%
  mutate(pff_id = as.numeric(pff_id))


players_metadata <- load_players()

players_metadata <- players_metadata %>%
  filter(!is.na(smart_id))

#Read PFF Data Into List

# Function to Read NFL Data and Add "Year" Column
read_nfl_data <- function(base_path = "NFL") {
  
  nfl_data <- list()  # Initialize empty list to store data frames
  
  # Read defense files
  defense_path <- file.path(base_path, "Defense")
  defense_files <- list.files(defense_path, pattern = "^Defense Grades \\d{4}\\.csv$", full.names = TRUE)
  
  for (file in defense_files) {
    year <- gsub(".*?(\\d{4})\\.csv$", "\\1", file)
    df_name <- paste0("Defense_", year)
    nfl_data[[df_name]] <- read_csv(file) %>% 
      mutate(Year = as.numeric(year), Category = "Defense")
  }
  
  # Read offense files from subfolders
  offense_subfolders <- c("Passing Grades", "Rushing Grades", "Receiving Grades", "Blocking Grades")
  offense_path <- file.path(base_path, "Offense")
  
  for (subfolder in offense_subfolders) {
    subfolder_path <- file.path(offense_path, subfolder)
    offense_files <- list.files(subfolder_path, pattern = paste0("^", subfolder, " \\d{4}\\.csv$"), full.names = TRUE)
    
    for (file in offense_files) {
      year <- gsub(".*?(\\d{4})\\.csv$", "\\1", file)
      df_name <- paste0(gsub(" ", "_", subfolder), "_", year)  # Convert spaces to underscores
      nfl_data[[df_name]] <- read_csv(file) %>% 
        mutate(Year = as.numeric(year), Category = subfolder)
    }
  }
  
  return(nfl_data)
}

# Load Data
nfl_data_list <- read_nfl_data()

# Function to Standardize Player Names
standardize_names <- function(name) {
  name %>%
    str_to_lower() %>%  # Convert to lowercase for consistency
    str_remove_all("[\\.\\',]") %>%  # Remove periods, apostrophes, commas
    str_remove_all("\\s+(jr|sr|ii|iii|iv|v|vi|vii|viii|ix|x)$") %>%  # Remove suffixes
    str_squish()  # Remove extra spaces
}

# Function to Standardize Player Names in a DataFrame
standardize_names_df <- function(df, name_col) {
  df %>%
    mutate(!!sym(name_col) := standardize_names(!!sym(name_col)))
}

# Standardize player names before merging
roster_data <- standardize_names_df(roster_data, "player")

# Apply standardization to each dataset in the list
nfl_data_list <- map(nfl_data_list, ~ standardize_names_df(.x, "player"))

roster_data <- roster_data %>%
  filter(!(player == "tj carter" & position == "DB"))

roster_data <- roster_data %>%
  filter(!(player == "anthony johnson" & smart_id == 	
             "32004a4f-4805-7904-a7a8-8cd8295cb244"))

roster_data <- roster_data %>%
  mutate(pff_id = if_else(player == "tyler conklin", 47124, pff_id))

# Convert to DF and Merge -------------------------------------------------

# Bind all datasets for each category
defense_data   <- bind_rows(nfl_data_list[grep("^Defense_", names(nfl_data_list))], .id = "Source") %>% select(-Source)
passing_data   <- bind_rows(nfl_data_list[grep("^Passing_", names(nfl_data_list))], .id = "Source") %>% select(-Source)
rushing_data   <- bind_rows(nfl_data_list[grep("^Rushing_", names(nfl_data_list))], .id = "Source") %>% select(-Source)
receiving_data <- bind_rows(nfl_data_list[grep("^Receiving_", names(nfl_data_list))], .id = "Source") %>% select(-Source)
blocking_data  <- bind_rows(nfl_data_list[grep("^Blocking_", names(nfl_data_list))], .id = "Source") %>% select(-Source)




# Match smart_id using PFF ID
defense_data <- defense_data %>%
  left_join(
    roster_data %>% select(season, pff_id, smart_id),
    by = c("Year" = "season", "player_id" = "pff_id")
  )

passing_data <- passing_data %>%
  left_join(
    roster_data %>% select(season, pff_id, smart_id),
    by = c("Year" = "season", "player_id" = "pff_id")
  )

rushing_data <- rushing_data %>%
  left_join(
    roster_data %>% select(season, pff_id, smart_id),
    by = c("Year" = "season", "player_id" = "pff_id")
  )

receiving_data <- receiving_data %>%
  left_join(
    roster_data %>% select(season, pff_id, smart_id),
    by = c("Year" = "season", "player_id" = "pff_id")
  )

blocking_data <- blocking_data %>%
  left_join(
    roster_data %>% select(season, pff_id, smart_id),
    by = c("Year" = "season", "player_id" = "pff_id")
  )


# Separate matched and unmatched players for each dataset
pff_matched_defense <- filter(defense_data, !is.na(smart_id))
unmatched_defense <- filter(defense_data, is.na(smart_id))

unmatched_defense <- unmatched_defense %>%
  filter(!(player == "tj carter" & position == "S"))


pff_matched_passing <- filter(passing_data, !is.na(smart_id))
unmatched_passing <- filter(passing_data, is.na(smart_id))

pff_matched_rushing <- filter(rushing_data, !is.na(smart_id))
unmatched_rushing <- filter(rushing_data, is.na(smart_id))

pff_matched_receiving <- filter(receiving_data, !is.na(smart_id))
unmatched_receiving <- filter(receiving_data, is.na(smart_id))

pff_matched_blocking <- filter(blocking_data, !is.na(smart_id))
unmatched_blocking <- filter(blocking_data, is.na(smart_id))

roster_data <- roster_data %>%
  filter(is.na(pff_id))

# Match smart_id using Player Name, Team, and Year, without overwriting existing smart_id
newly_matched_defense <- unmatched_defense %>%
  left_join(
    roster_data %>% select(season, team_name, player, smart_id),
    by = c("Year" = "season", "team_name" = "team_name", "player" = "player")
  ) %>%
  mutate(smart_id = coalesce(smart_id.x, smart_id.y)) %>%
  select(-smart_id.x, -smart_id.y)

newly_matched_passing <- unmatched_passing %>%
  left_join(
    roster_data %>% select(season, team_name, player, smart_id),
    by = c("Year" = "season", "team_name" = "team_name", "player" = "player")
  ) %>%
  mutate(smart_id = coalesce(smart_id.x, smart_id.y)) %>%
  select(-smart_id.x, -smart_id.y)

newly_matched_rushing <- unmatched_rushing %>%
  left_join(
    roster_data %>% select(season, team_name, player, smart_id),
    by = c("Year" = "season", "team_name" = "team_name", "player" = "player")
  ) %>%
  mutate(smart_id = coalesce(smart_id.x, smart_id.y)) %>%
  select(-smart_id.x, -smart_id.y)

newly_matched_receiving <- unmatched_receiving %>%
  left_join(
    roster_data %>% select(season, team_name, player, smart_id),
    by = c("Year" = "season", "team_name" = "team_name", "player" = "player")
  ) %>%
  mutate(smart_id = coalesce(smart_id.x, smart_id.y)) %>%
  select(-smart_id.x, -smart_id.y)

newly_matched_blocking <- unmatched_blocking %>%
  left_join(
    roster_data %>% select(season, team_name, player, smart_id),
    by = c("Year" = "season", "team_name" = "team_name", "player" = "player")
  ) %>%
  mutate(smart_id = coalesce(smart_id.x, smart_id.y)) %>%
  select(-smart_id.x, -smart_id.y)


# Separate still unmatched players
still_unmatched_defense <- filter(newly_matched_defense, is.na(smart_id))
still_unmatched_passing <- filter(newly_matched_passing, is.na(smart_id))
still_unmatched_rushing <- filter(newly_matched_rushing, is.na(smart_id))
still_unmatched_receiving <- filter(newly_matched_receiving, is.na(smart_id))
still_unmatched_blocking <- filter(newly_matched_blocking, is.na(smart_id))

still_unmatched_blocking <- still_unmatched_blocking %>%
  filter(snap_counts_offense >= 50)

still_unmatched_defense <- still_unmatched_defense %>%
  filter(snap_counts_defense >= 100)

still_unmatched_rushing <- still_unmatched_rushing %>%
  filter(attempts >= 50)

final_matched_rushing <- still_unmatched_rushing %>%
  mutate(smart_id = if_else(player == "audric estime", "32004553-5461-0772-74c0-fa722e2922d5", smart_id)) %>%
  filter(!is.na(smart_id))

# Create a new dataframe for final matched receiving
final_matched_receiving <- still_unmatched_receiving %>%
  mutate(smart_id = case_when(
    player == "walter powell" ~ "3200504f-5757-0795-6fe7-b1331845bc1d",
    player == "bryce treggs" ~ "32005452-4567-4214-7d01-a585ad3bb558",
    player == "michael woods" ~ "3200574f-4f54-5683-661d-2c7b56b7d9fb",
    player == "rodney williams" ~ "32005749-4c50-6729-e5da-0244dc363828",
    player == "zonovan knight" ~ "32004b4e-4976-4772-52fc-5e9b627ab7bb",
    player == "chigoziem okonkwo" ~ "32004f4b-4f28-3611-fc64-d952b8d6cb17",
    player == "tyquan thornton" ~ "32005448-4f79-7153-e52f-fb28090a0034",
    player == "john samuel shenker" ~ "32005348-4546-8281-eba8-359b99bfbf02",
    player == "audric estime" ~ "32004553-5461-0772-74c0-fa722e2922d5",
    TRUE ~ smart_id  # Keep existing smart_id values unchanged
  ))

#Verify
final_matched_receiving <- final_matched_receiving %>%
  filter(!is.na(smart_id))



# Create a new dataframe for final matched defense
final_matched_defense <- still_unmatched_defense %>%
  mutate(smart_id = case_when(
    player == "anthony johnson" ~ "32004a4f-4878-9936-25b4-d6b1428e60fb",
    player == "austin edwards" ~ "32004544-5711-3688-df0f-b3c007c0c917",
    player == "chris smith" ~ "3200534d-4982-9490-6b43-3a3a8d668113",
    player == "damarcus fields" ~ "32004649-4548-4543-73ca-b0707dedd244",
    player == "darius hillary" ~ "32004849-4c65-9979-dafc-f24334e55b10",
    player == "dax hill" ~ "32004849-4c16-4994-7b99-e93ebbf2d838",
    player == "delontae scott" ~ "32005343-4f23-1185-e3c1-49ea440f32d8",
    player == "elerson smith" ~ "3200534d-4925-2880-8ea6-bc381ac064de",
    player == "iman lewis-marshall" ~ "32004d41-5241-6178-2ecb-70bae5d48123",
    player == "jake hummel" ~ "32004855-4d24-3174-5f1c-e6d317cf4f4b",
    player == "jayden peevy" ~ "32005045-4568-9214-fd31-f91b793dc1b1",
    player == "josh onujiogu" ~ "32004f4e-5520-7645-e297-db9f33425eef",
    player == "josh paschal" ~ "32005041-5306-8486-93fc-0cae1c9cca7f",
    player == "juju brents" ~ "32004252-4551-0110-2798-7a9b05aa2760",
    player == "keenan isaac" ~ "32004953-4113-8430-eaa4-199d1e0b0ab0",
    player == "kj henry" ~ "32004845-4e62-4398-758e-3f565d3dc34c",
    player == "kyu blu kelly" ~ "32004b45-4c67-6898-ac08-b6c85f2a9a6b",
    player == "louis young" ~ "3200594f-5537-9662-f884-0b83c63c40a9",
    player == "michael hall" ~ "32004841-4c64-8337-1c7b-c1256a64f35a",
    player == "michael morgan" ~ "32004d4f-5219-6620-e088-8ffd63338f63",
    player == "nesta jade silvera" ~ "32005349-4c70-1377-3876-3fd27dd61a2c",
    player == "olakunle fatukasi" ~ "32004641-5466-3851-f29e-937133376c79",
    player == "prince emili" ~ "3200454d-4941-5291-9038-7082e5700b9f",
    player == "sam webb" ~ "3200454d-4941-5291-9038-7082e5700b9f",
    player == "sauce gardner" ~ "32004741-5215-2629-6c74-87ce99a3689f",
    player == "stefan mcclure" ~ "32004d43-4345-2788-a8fc-c5525d0f80a3",
    player == "tariq woolen" ~ "3200574f-4f71-3124-55f0-e6f0816e959a",
    player == "tony fields" ~ "32004649-4582-3499-ef09-e1ca0d751c1f",
    player == "zach carter" ~ "32004341-5278-6801-508d-115b8e175a05",
    player == "zachary orr" ~ "32004f52-5257-9972-8ad4-0d0b2802e740",
    TRUE ~ smart_id  # Keep existing smart_id values unchanged
  ))


final_matched_defense <- final_matched_defense %>%
  filter(!is.na(smart_id))



# Create a new dataframe for final matched blocking
final_matched_blocking <- still_unmatched_blocking %>%
  mutate(smart_id = case_when(
    player == "ron leary" ~ "32004c45-4161-0932-4559-796cee221ae8",
    player == "cam fleming" ~ "3200464c-4515-7731-f0ee-1683da2b3966",
    player == "adam redmond" ~ "32005245-4459-8410-a500-7ba56e4d0924",
    player == "audric estime" ~ "32004553-5461-0772-74c0-fa722e2922d5",
    player == "bobby evans" ~ "32004556-4123-7310-44e4-5ca5223f4b63",
    player == "brett toth" ~ "3200544f-5416-9395-9e76-e35c46db51e2",
    player == "bryce treggs" ~ "32005452-4567-4214-7d01-a585ad3bb558",
    player == "chigoziem okonkwo" ~ "32004f4b-4f28-3611-fc64-d952b8d6cb17",
    player == "chris hubbard" ~ "32004855-4217-4074-41a2-2926a9fc4a4b",
    player == "deion calhoun" ~ "32004341-4c42-2355-5cfd-4380f9d6e470",
    player == "jared veldheer" ~ "32005645-4c11-6302-6665-c63598a1ecdf",
    player == "joe noteboom" ~ "32004e4f-5405-9326-71bf-6ca4d7f0dd6d",
    player == "john samuel shenker" ~ "32005348-4546-8281-eba8-359b99bfbf02",
    player == "john simpson" ~ "32005349-4d53-5559-6e58-1567c1c85840",
    player == "leroy watson" ~ "32005741-5445-8313-edef-c03fa6cef6ed",
    player == "michael jordan" ~ "32004a4f-5214-6382-8d60-9ad32b12a1f6",
    player == "michael woods" ~ "3200574f-4f54-5683-661d-2c7b56b7d9fb",
    player == "mike jerrell" ~ "32004a45-5247-7236-6333-a1bdf940c65e",
    player == "mike onwenu" ~ "32004f4e-5741-5291-a74a-040f32c558e0",
    player == "olisaemeka udoh" ~ "32005544-4f48-9451-1839-5671f0b17981",
    player == "olusegun oluwatimi" ~ "32004f4c-5559-3274-eef4-acb200e9e905",
    player == "rodney williams" ~ "32005749-4c50-6729-e5da-0244dc363828",
    player == "sam cosmi" ~ "3200434f-5319-6802-cc94-36bf0ee8edce",
    player == "shaq mason" ~ "32004d41-5323-7768-d4ec-0e86b8431bf1",
    player == "sua opeta" ~ "32004f50-4550-4350-155c-6d278d826be0",
    player == "trent brown" ~ "32004252-4f73-9325-7786-18cc8995e060",
    player == "trent scott" ~ "32005343-4f71-8957-ee1a-311a16c8d5d0",
    player == "trystan colon" ~ "3200434f-4c74-7687-5343-cc39a681538c",
    player == "tyquan thornton" ~ "32005448-4f79-7153-e52f-fb28090a0034",
    player == "walter powell" ~ "3200504f-5757-0795-6fe7-b1331845bc1d",
    player == "xavier newman" ~ "32004e45-5750-1295-56ca-e2e2aa6b55de",
    player == "yosh nijman" ~ "32004e49-4a39-4486-5ad3-455d5d7e45d9",
    TRUE ~ smart_id  # Keep existing smart_id values unchanged
  ))

final_matched_blocking <- final_matched_blocking %>%
  filter(!is.na(smart_id))

newly_matched_blocking <- newly_matched_blocking %>%
  filter(!is.na(smart_id))

newly_matched_defense <- newly_matched_defense %>%
  filter(!is.na(smart_id))

newly_matched_receiving <- newly_matched_receiving %>%
  filter(!is.na(smart_id))

newly_matched_rushing <- newly_matched_rushing %>%
  filter(!is.na(smart_id))


# Bind all matched data for each category
final_defense <- bind_rows(pff_matched_defense, newly_matched_defense, final_matched_defense)
final_passing <- bind_rows(pff_matched_passing, newly_matched_passing)
final_rushing <- bind_rows(pff_matched_rushing, newly_matched_rushing, final_matched_rushing)
final_receiving <- bind_rows(pff_matched_receiving, newly_matched_receiving, final_matched_receiving)
final_blocking <- bind_rows(pff_matched_blocking, newly_matched_blocking, final_matched_blocking)



final_defense <- final_defense %>%
  filter(position %in% c("ED", "LB","CB", "S", "DI"))

final_passing <- final_passing %>%
  filter(position == "QB")

final_receiving <- final_receiving %>%
  filter(position %in% c("WR", "TE", "HB"))

final_blocking <- final_blocking %>%
  filter(position %in% c("WR", "TE", "HB", "T", "G", "C"))

final_rushing <- final_rushing %>%
  filter(position %in% c("WR", "HB", "QB"))


# Define the relevant columns from players_metadata
metadata_cols <- c("smart_id", "college_conference", "college_name", "draft_club", 
                   "draftround", "draft_number", "rookie_year")



roster_data <- roster_data %>%
  filter(smart_id != "3200474f-5204-5716-a75e-4ec8e313a651")

final_blocking <- final_blocking %>%
  filter(smart_id != "3200474f-5204-5716-a75e-4ec8e313a651")

final_receiving <- final_receiving %>%
  filter(smart_id != "3200474f-5204-5716-a75e-4ec8e313a651")

# Merge player metadata into each dataset
final_defense <- final_defense %>%
  left_join(players_metadata %>% select(all_of(metadata_cols)), by = "smart_id")

final_passing <- final_passing %>%
  left_join(players_metadata %>% select(all_of(metadata_cols)), by = "smart_id")

final_rushing <- final_rushing %>%
  left_join(players_metadata %>% select(all_of(metadata_cols)), by = "smart_id")

final_receiving <- final_receiving %>%
  left_join(players_metadata %>% select(all_of(metadata_cols)), by = "smart_id")

final_blocking <- final_blocking %>%
  left_join(players_metadata %>% select(all_of(metadata_cols)), by = "smart_id")



final_blocking <- final_blocking %>%
  filter(Year >= 2017)

final_passing <- final_passing %>%
  filter(Year >= 2017)

final_rushing <- final_rushing %>%
  filter(Year >= 2017)

final_receiving <- final_receiving %>%
  filter(Year >= 2017)

final_defense <- final_defense %>%
  filter(Year >= 2017)


missing_rookie_blocking <- final_blocking %>%
  filter(is.na(rookie_year))


# Update Ryan Bates' missing rookie information
missing_rookie_blocking <- missing_rookie_blocking %>%
  mutate(
    rookie_year = if_else(player == "ryan bates", 2019, rookie_year),
    college_name = if_else(player == "ryan bates", "Penn State", college_name),
    college_conference = if_else(player == "ryan bates", "Big Ten Conference", college_conference)
  )


final_blocking <- final_blocking %>%
  filter(!is.na(rookie_year))

final_blocking <- final_blocking %>%
  bind_rows(missing_rookie_blocking)



# Function to Add Season Column
add_season_column <- function(df) {
  df %>%
    mutate(season = (Year - rookie_year) + 1)
}

# Apply Function to Each Dataset
final_defense   <- add_season_column(final_defense)
final_passing   <- add_season_column(final_passing)
final_rushing   <- add_season_column(final_rushing)
final_receiving <- add_season_column(final_receiving)
final_blocking  <- add_season_column(final_blocking)


# Define the file paths for saving
write_csv(final_defense, "final_defense.csv")
write_csv(final_passing, "final_passing.csv")
write_csv(final_rushing, "final_rushing.csv")
write_csv(final_receiving, "final_receiving.csv")
write_csv(final_blocking, "final_blocking.csv")


# Load Csvs ---------------------------------------------------------------

final_defense <- read_csv("final_defense.csv")
final_passing <- read_csv("final_passing.csv")
final_rushing <- read_csv("final_rushing.csv")
final_receiving <- read_csv("final_receiving.csv")
final_blocking <- read_csv("final_blocking.csv")


final_defense <- final_defense %>%
  mutate(
  position = case_when(
    position == "DI" & snap_counts_dl_a_gap >= 0.33 * snap_counts_defense ~ "NT",
    position == "CB" & snap_counts_slot >= 0.50 * snap_counts_defense ~ "SLOT",
    position == "S" & snap_counts_fs >= 0.50 * snap_counts_defense ~ "FS",
    TRUE ~ position  # Keep existing position if conditions aren't met
  )
)


all_data <- bind_rows(
  mutate(final_defense, Category = "Defense"),
  mutate(final_passing, Category = "Passing"),
  mutate(final_rushing, Category = "Rushing"),
  mutate(final_receiving, Category = "Receiving"),
  mutate(final_blocking, Category = "Blocking")
)

write_csv(all_data, "all_data.csv")
