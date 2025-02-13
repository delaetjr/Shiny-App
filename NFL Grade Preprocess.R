library(nflfastR)
library(nflreadr)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(stringr)
library(stringdist)

# Get NFL Rookie and Draft Data -------------------------------------------


# Step 1: Load Roster Data (includes drafted & undrafted players) ------------------------

roster_data <- load_rosters(2016:2024) %>%
  select(pff_id, full_name, rookie_year, draft_club, draft_number, position) %>%
  rename(Player = full_name)

# Filter to include distinct entries for each player and team
roster_data_distinct <- roster_data %>%
  group_by(Player, draft_club, position) %>%
  slice_min(rookie_year, with_ties = FALSE) %>%
  ungroup()

roster_data_distinct <- roster_data_distinct[-9210,]

# Load Draft Data (Expanding to Pre-2016 Drafts) -----------------------------------

draft_data <- load_draft_picks(1994:2024) %>%
  select(season, round, pick, pfr_player_name, team, position) %>%
  rename(Draft_Year = season, Draft_Round = round, Draft_Pick = pick, Player = pfr_player_name, Draft_Team = team)

# Step 2: Matching Function (Handles Duplicates Internally) ----------------------------

# Step 1: Standardize Player Names in `draft_data`
draft_data_clean <- draft_data %>%
  mutate(
    Player = tolower(Player),                       # Standardize player names
    Player = str_replace_all(Player, "[\\.']", ""),      # Remove periods and apostrophes
    Player = str_replace_all(Player, "\\s+jr|sr\\.?$", ""), # Remove "Jr."
    Player = str_replace_all(Player, "\\s+(ii|iii|iv|v|vi|vii|viii|ix|x)$", ""), # Remove Roman numerals
    Player = str_squish(Player)                         # Remove extra spaces
  ) %>%
  select(Draft_Year, Draft_Round, Draft_Pick, Player, Draft_Team)

roster_data_distinct <- roster_data_distinct %>%
  mutate(
    Player = tolower(Player),                       # Standardize player names
    Player = str_replace_all(Player, "[\\.']", ""),      # Remove periods and apostrophes
    Player = str_replace_all(Player, "\\s+jr|sr\\.?$", ""), # Remove "Jr."
    Player = str_replace_all(Player, "\\s+(ii|iii|iv|v|vi|vii|viii|ix|x)$", ""), # Remove Roman numerals
    Player = str_squish(Player)                         # Remove extra spaces
  )

roster_data_distinct <- roster_data_distinct %>%
  group_by(rookie_year, draft_club, Player, position) %>%
  slice_min(rookie_year, with_ties = FALSE) %>%  # Keep the first row (ties unlikely)
  ungroup()


# Step 2: Merge Cleaned `roster_data_distinct` with `draft_data_clean`
players_final <- roster_data_distinct %>%
  left_join(draft_data_clean, by = c("Player", "rookie_year" = "Draft_Year")) %>%
  mutate(
    draft_club = coalesce(draft_club, Draft_Team),        # Fill missing draft club
    draft_number = coalesce(draft_number, Draft_Pick),    # Fill missing draft pick
    Draft_Round = coalesce(Draft_Round, NA)              # Ensure draft round is included
  ) %>%
  select(pff_id, Player, rookie_year, draft_club, draft_number, Draft_Round, position) %>%
  distinct()  # Remove duplicate rows if any



missing_draft_info <- players_final  %>%
  filter(!is.na(draft_club) & (is.na(draft_number) | is.na(Draft_Round))) 


# Manually update Draft_Round and draft_number for the specified players
players_final <- players_final %>%
  mutate(
    Draft_Round = case_when(
      Player == "temuchin hodges" & rookie_year == 2017 ~ 6,
      Player == "dj pumphrey" & rookie_year == 2017 ~ 4,
      Player == "chris campbell" & rookie_year == 2018 ~ 6,
      Player == "ced wilson" & rookie_year == 2018 ~ 6,
      Player == "dave williams" & rookie_year == 2018 ~ 7,
      Player == "josh frazier" & rookie_year == 2018 ~ 7,
      Player == "alize mack" & rookie_year == 2019 ~ 7,
      TRUE ~ Draft_Round  # Keep existing values unchanged
    ),
    
    draft_number = case_when(
      Player == "temuchin hodges" & rookie_year == 2017 ~ 201,
      Player == "dj pumphrey" & rookie_year == 2017 ~ 132,
      Player == "chris campbell" & rookie_year == 2018 ~ 182,
      Player == "ced wilson" & rookie_year == 2018 ~ 208,
      Player == "dave williams" & rookie_year == 2018 ~ 226,
      Player == "josh frazier" & rookie_year == 2018 ~ 246,
      Player == "alize mack" & rookie_year == 2019 ~ 231,
      TRUE ~ draft_number  # Keep existing values unchanged
    ),
    
    draft_club = case_when(
      Player == "sam beal" & rookie_year == 2018 ~ NA_character_,  # Remove draft club
      Player == "isaiah searight" & rookie_year == 2019 ~ NA_character_,  # Remove draft club
      TRUE ~ draft_club  # Keep existing values unchanged
    )
  )

# Manually update Draft_Round for the specified players
players_final <- players_final %>%
  mutate(Draft_Round = case_when(
    Player == "matt dayes" & rookie_year == 2017 ~ 7,
    Player == "deangelo henderson sr" & rookie_year == 2017 ~ 6,
    Player == "julien davenport" & rookie_year == 2017 ~ 4,
    Player == "mike tyson" & rookie_year == 2017 ~ 6,
    Player == "kam moore" & rookie_year == 2018 ~ 6,
    Player == "carlos basham" & rookie_year == 2021 ~ 2,
    Player == "mike woods" & rookie_year == 2022 ~ 6,
    Player == "tre tomlinson" & rookie_year == 2023 ~ 6,
    Player == "mike hall" & rookie_year == 2024 ~ 2,
    Player == "audric estimé" & rookie_year == 2024 ~ 5,
    Player == "dj glaze" & rookie_year == 2024 ~ 3,
    TRUE ~ Draft_Round  # Keep existing values unchanged
  ))



missing_draft_info <- players_final  %>%
  filter(!is.na(draft_club) & (is.na(draft_number) | is.na(Draft_Round))) 

players_final <- players_final %>%
  mutate(
    draft_club = case_when(
      Player %in% c("terrelle pryor", "josh gordon", "jalen thompson",
                    "isaiah battle", "adonis alexander", "ahmad brooks") ~ NA_character_,  # Remove draft team
      TRUE ~ draft_club
    )
  )



# Manually update Draft_Round for the specified players
players_final <- players_final %>%
  mutate(Draft_Round = case_when(
    Player == "mike vick" & rookie_year == 2001 ~ 1,
    Player == "benjamin watson" & rookie_year == 2004 ~ 1,
    Player == "matt slater" & rookie_year == 2008 ~ 5,
    Player == "ziggy hood" & rookie_year == 2009 ~ 1,
    Player == "ricky jean francois" & rookie_year == 2009 ~ 7,
    Player == "matt bosher" & rookie_year == 2011 ~ 6,
    Player == "mike person" & rookie_year == 2011 ~ 7,
    Player == "trevor graham" & rookie_year == 2012 ~ 3,
    Player == "charles d johnson" & rookie_year == 2013 ~ 7,
    Player == "nicholas williams" & rookie_year == 2013 ~ 7,
    Player == "pj dawson" & rookie_year == 2015 ~ 3,
    Player == "shaquille mason" & rookie_year == 2015 ~ 4,
    Player == "lt walton" & rookie_year == 2015 ~ 6,
    Player == "trenton brown" & rookie_year == 2015 ~ 7,
    Player == "matthew judon" & rookie_year == 2016 ~ 5,
    Player == "lachlan edwards" & rookie_year == 2016 ~ 7,
    Player == "dan vitale" & rookie_year == 2016 ~ 6,
    Player == "matt ioannidis" & rookie_year == 2016 ~ 5,
    Player == "takk mckinley" & rookie_year == 2017 ~ 1,
    Player == "nate gerry" & rookie_year == 2017 ~ 5,
    Player == "delano hill" & rookie_year == 2017 ~ 3,
    Player == "foye oluokun" & rookie_year == 2018 ~ 6,
    Player == "darius leonard" & rookie_year == 2018 ~ 2,
    Player == "kahlil mckenzie" & rookie_year == 2018 ~ 6,
    Player == "joe noteboom" & rookie_year == 2018 ~ 3,
    Player == "obo okoronkwo" & rookie_year == 2018 ~ 5,
    Player == "sebastian joseph" & rookie_year == 2018 ~ 6,
    Player == "christopher herndon" & rookie_year == 2018 ~ 4,
    Player == "foley fatukasi" & rookie_year == 2018 ~ 6,
    Player == "jake martin" & rookie_year == 2018 ~ 6,
    Player == "mike jordan" & rookie_year == 2019 ~ 4,
    Player == "mike jackson" & rookie_year == 2019 ~ 5,
    Player == "josh allen" & rookie_year == 2019 ~ 1,
    Player == "bisi johnson" & rookie_year == 2019 ~ 7,
    Player == "olisaemeka udoh" & rookie_year == 2019 ~ 6,
    Player == "chauncey gardner-johnson" & rookie_year == 2019 ~ 4,
    Player == "ugochukwu amadi" & rookie_year == 2019 ~ 4,
    Player == "justin madubuike" & rookie_year == 2020 ~ 3,
    Player == "gabe davis" & rookie_year == 2020 ~ 4,
    Player == "kenneth murray," & rookie_year == 2020 ~ 1,
    Player == "joshua uche" & rookie_year == 2020 ~ 2,
    Player == "mike onwenu" & rookie_year == 2020 ~ 6,
    Player == "cameron brown" & rookie_year == 2020 ~ 6,
    Player == "kam curl" & rookie_year == 2020 ~ 7,
    Player == "greg rousseau" & rookie_year == 2021 ~ 1,
    Player == "cam sample" & rookie_year == 2021 ~ 4,
    Player == "pat surtain" & rookie_year == 2021 ~ 1,
    Player == "will sherman" & rookie_year == 2021 ~ 6,
    Player == "elerson smith" & rookie_year == 2021 ~ 4,
    Player == "dee eskridge" & rookie_year == 2021 ~ 2,
    Player == "joe tryon" & rookie_year == 2021 ~ 1,
    Player == "sam cosmi" & rookie_year == 2021 ~ 2,
    Player == "will bradley-king" & rookie_year == 2021 ~ 7,
    Player == "zach thomas" & rookie_year == 2022 ~ 6,
    Player == "dax hill" & rookie_year == 2022 ~ 1,
    Player == "zach carter" & rookie_year == 2022 ~ 3,
    Player == "joshua paschal" & rookie_year == 2022 ~ 2,
    Player == "ahmad gardner" & rookie_year == 2022 ~ 1,
    Player == "tariq woolen" & rookie_year == 2022 ~ 5,
    Player == "chigoziem okonkwo" & rookie_year == 2022 ~ 4,
    Player == "kyu kelly" & rookie_year == 2023 ~ 5,
    Player == "chris smith" & rookie_year == 2023 ~ 5,
    Player == "olu oluwatimi" & rookie_year == 2023 ~ 5,
    Player == "chig okonkwo" & rookie_year == 2022 ~ 4,
    Player == "elerson g smith" & rookie_year == 2021 ~ 4,
    Player == "reginald mckenzie" & rookie_year == 2018 ~ 6,
    TRUE ~ Draft_Round  # Keep existing values unchanged
  ))

players_final <- players_final %>%
  mutate(
    Draft_Round = case_when(
      Player == "vladimir ducasse" & rookie_year == 2010 ~ 2,
      Player == "phillip taylor" & rookie_year == 2011 ~ 1,
      Player == "nate stupar" & rookie_year == 2012 ~ 7,
      Player == "janoris jenkins" & rookie_year == 2012 ~ 2,
      Player == "rick wagner" & rookie_year == 2013 ~ 5,
      Player == "jon bostic" & rookie_year == 2013 ~ 2,
      Player == "dan mccullers" & rookie_year == 2014 ~ 6,
      Player == "christian bryant" & rookie_year == 2014 ~ 7,
      Player == "owa odighizuwa" & rookie_year == 2015 ~ 3,
      Player == "lt walton" & rookie_year == 2015 ~ 6,
      TRUE ~ Draft_Round  # Keep existing values unchanged
    ),
    draft_number = case_when(
      Player == "vladimir ducasse" & rookie_year == 2010 ~ 61,
      Player == "phillip taylor" & rookie_year == 2011 ~ 21,
      Player == "nate stupar" & rookie_year == 2012 ~ 230,
      Player == "janoris jenkins" & rookie_year == 2012 ~ 39,
      Player == "rick wagner" & rookie_year == 2013 ~ 168,
      Player == "jon bostic" & rookie_year == 2013 ~ 50,
      Player == "dan mccullers" & rookie_year == 2014 ~ 215,
      Player == "christian bryant" & rookie_year == 2014 ~ 241,
      Player == "owa odighizuwa" & rookie_year == 2015 ~ 74,
      Player == "lt walton" & rookie_year == 2015 ~ 199,
      TRUE ~ draft_number  # Keep existing values unchanged
    )
  )

  
# Merge the manually created draft data with `players_final`
players_final <- players_final %>%
  left_join(missing_draft_info, by = c("Player", "rookie_year")) %>%
  mutate(
  draft_club = coalesce(draft_club.x, draft_club.y),
  draft_number = coalesce(draft_number.x, draft_number.y),
  Draft_Round = coalesce(Draft_Round.x, Draft_Round.y),
  position = coalesce(position.x, position.y)
  ) %>%
  select(pff_id, Player, rookie_year, draft_club, draft_number, Draft_Round, position)  # Keep only necessary columns
  

# Step 5: Save Cleaned Dataset ------------------------------------------------------

#write_csv(players_final, "roster_with_complete_draft_data.csv")

# Load in CSV -------------------------------------------------------------

players_final <- read.csv("roster_with_complete_draft_data.csv")

# Read NFL Grades In ------------------------------------------------------


#Read PFF Data Into List
read_nfl_data <- function(base_path = "NFL") {
  
  nfl_data <- list()  # Initialize empty list to store data frames
  
  # Read defense files
  defense_path <- file.path(base_path, "Defense")
  defense_files <- list.files(defense_path, pattern = "^Defense Grades \\d{4}\\.csv$", full.names = TRUE)
  
  for (file in defense_files) {
    year <- gsub(".*?(\\d{4})\\.csv$", "\\1", file)
    df_name <- paste0("Defense_", year)
    nfl_data[[df_name]] <- read_csv(file) %>% mutate(Year = as.numeric(year), Category = "Defense")
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
      nfl_data[[df_name]] <- read_csv(file) %>% mutate(Year = as.numeric(year), Category = subfolder)
    }
  }
  
  return(nfl_data)
}

# Example Usage
nfl_data_list <- read_nfl_data()

# Inspect column names from one of the PFF datasets
colnames(nfl_data_list[[1]])


# Update positions for defensive players based on snap count thresholds
for (year in names(nfl_data_list)) {
  if (grepl("^Defense_", year)) {  # Process only defensive files
    nfl_data_list[[year]] <- nfl_data_list[[year]] %>%
      mutate(
        position = case_when(
          position == "DI" & snap_counts_dl_a_gap >= 0.33 * snap_counts_defense ~ "NT",
          position == "CB" & snap_counts_slot >= 0.50 * snap_counts_defense ~ "SLOT",
          position == "S" & snap_counts_fs >= 0.50 * snap_counts_defense ~ "FS",
          TRUE ~ position  # Keep existing position if conditions aren't met
        )
      )
  }
}

sum(nfl_data_list[["Defense_2018"]]$position == "NT")

sum(nfl_data_list[["Defense_2018"]]$position == "SLOT")

sum(nfl_data_list[["Defense_2018"]]$position == "FS")

# Define relevant columns for defensive files
defense_columns <- c("player", "player_id", "position", "team_name", "grades_defense", "Year", 
                     "snap_counts_box",	"snap_counts_corner",	"snap_counts_coverage",	"snap_counts_defense",	"snap_counts_dl",
                     "grades_coverage_defense", "penalties", "grades_pass_rush_defense", "grades_run_defense", "missed_tackle_rate",
                     "snap_counts_dl_a_gap",	"snap_counts_dl_b_gap",	"snap_counts_dl_outside_t",	"snap_counts_dl_over_t",	"snap_counts_fs",	
                     "snap_counts_offball",	"snap_counts_pass_rush",	"snap_counts_run_defense",	"snap_counts_slot",
                     "hits", "hurries", "sacks", "qb_rating_against", "targets", "total_pressures", 
                     "yards_after_catch", "yards_per_reception"
)

# Modify `nfl_data_list` in place for only defensive datasets
for (year in names(nfl_data_list)) {
  if (grepl("^Defense_", year)) {  # Check if the entry corresponds to a defensive dataset
    nfl_data_list[[year]] <- select(nfl_data_list[[year]], any_of(defense_columns))  # Keep only relevant columns
  }
}

# Save the modified list (optional, if you need to reload later)
saveRDS(nfl_data_list, "nfl_data_list_trimmed.rds")

defensive_positions <- c("ED", "CB", "DI", "S", "LB", "NT", "SLOT", "FS")

# Filter defensive datasets in `nfl_data_list`
for (year in names(nfl_data_list)) {
  if (grepl("^Defense_", year)) {  # Process only defensive files
    nfl_data_list[[year]] <- nfl_data_list[[year]] %>%
      filter(position %in% defensive_positions)  # Keep only specified positions
  }
}

# Save the modified list (optional, for future use)
saveRDS(nfl_data_list, "nfl_data_list_filtered_positions.rds")


# Define the relevant columns for passing files
passing_columns <- c("player", "player_id", "team_name", "grades_offense", "Year", "dropbacks",
                     "grades_pass",	"grades_run", "twp_rate", "ypa", "pressure_to_sack_rate", "scrambles",
                     "passing_snaps", "btt_rate", "avg_depth_of_target",	"avg_time_to_throw"
)

# Filter passing datasets in `nfl_data_list`
for (year in names(nfl_data_list)) {
  if (grepl("^Passing_", year)) {  # Process only passing files
    nfl_data_list[[year]] <- nfl_data_list[[year]] %>%
      filter(position == "QB") %>%  # Keep only QBs
      select(any_of(passing_columns))  # Retain only relevant columns
  }
}


# Define the relevant columns for rushing files
rushing_columns <- c("player", "player_id", "position", "team_name", "grades_offense", "Year", "attempts",
                     "gap_attempts", "zone_attempts", "grades_pass_block",	"grades_pass_route",	"grades_run",
                     "penalties", "routes", "avoided_tackles", "breakaway_percent", "elusive_rating", "yco_attempt",	"ypa"

)

rushing_positions <- c("HB", "WR")

# Filter rushing datasets in `nfl_data_list`
for (year in names(nfl_data_list)) {
  if (grepl("^Rushing_", year)) {  # Process only passing files
    nfl_data_list[[year]] <- nfl_data_list[[year]] %>%
      select(any_of(rushing_columns)) %>% # Retain only relevant columns
      filter(position %in% rushing_positions)
  }
}




# Define the relevant positions and columns for receiving files
receiving_positions <- c("WR", "TE", "RB")
receiving_columns <- c("player", "player_id", "position", "team_name", 
                       "grades_offense", "pass_plays", "pass_blocks", "inline_snaps",
                       "slot_snaps", "wide_snaps", "Year", "avg_depth_of_target",	"avoided_tackles",
                       "caught_percent",	"contested_catch_rate", "drop_rate", "penalties", 
                       "yards_after_catch_per_reception",	"yards_per_reception",	"yprr", 
                       "pass_block_rate", "route_rate"
)

# Filter receiving datasets in `nfl_data_list`
for (year in names(nfl_data_list)) {
  if (grepl("^Receiving_", year)) {  # Process only receiving files
    nfl_data_list[[year]] <- nfl_data_list[[year]] %>%
      filter(position %in% receiving_positions) %>%  # Keep only WR, TE, RB
      select(any_of(receiving_columns))  # Retain only relevant columns
  }
}



blocking_positions <- c("G", "C", "T", "TE", "HB")
blocking_columns <- c("player", "player_id", "position", "team_name",
                      "grades_offense", "grades_pass_block", "grades_run_block", 
                      "Year", "snap_counts_block", "hits_allowed",	"hurries_allowed",
                      "non_spike_pass_block", "pbe", "penalties",	"pressures_allowed",
                      "sacks_allowed",	"snap_counts_ce",	"snap_counts_lg",
                      "snap_counts_lt",	"snap_counts_offense",	"snap_counts_pass_block",
                      "snap_counts_pass_play",	"snap_counts_rg",	"snap_counts_rt",	"snap_counts_run_block",
                      "snap_counts_te"
)


# Filter blocking datasets in `nfl_data_list`
for (year in names(nfl_data_list)) {
  if (grepl("^Blocking_", year)) {  # Process only blocking files
    nfl_data_list[[year]] <- nfl_data_list[[year]] %>%
      filter(position %in% blocking_positions) %>%  # Keep relevant positions
      select(any_of(blocking_columns))  # Retain only relevant columns
  }
}




# Merge Draft and PFF Info  -----------------------------------------------


# Bind all defensive years into a single dataframe
defense_data <- bind_rows(nfl_data_list[grep("^Defense_", names(nfl_data_list))], .id = "Source") %>%
  mutate(Year = as.numeric(str_extract(Source, "\\d{4}"))) %>%  # Extract year from source name
  select(-Source)  # Remove identifier column

passing_data <- bind_rows(nfl_data_list[grep("^Passing_", names(nfl_data_list))], .id = "Source") %>%
  mutate(Year = as.numeric(str_extract(Source, "\\d{4}"))) %>%  # Extract year from source name
  select(-Source)  # Remove identifier column

receiving_data <- bind_rows(nfl_data_list[grep("^Receiving_", names(nfl_data_list))], .id = "Source") %>%
  mutate(Year = as.numeric(str_extract(Source, "\\d{4}"))) %>%  # Extract year from source name
  select(-Source)  # Remove identifier column

rushing_data <- bind_rows(nfl_data_list[grep("^Rushing_", names(nfl_data_list))], .id = "Source") %>%
  mutate(Year = as.numeric(str_extract(Source, "\\d{4}"))) %>%  # Extract year from source name
  select(-Source)  # Remove identifier column

blocking_data <- bind_rows(nfl_data_list[grep("^Blocking_", names(nfl_data_list))], .id = "Source") %>%
  mutate(Year = as.numeric(str_extract(Source, "\\d{4}"))) %>%  # Extract year from source name
  select(-Source)  # Remove identifier column


# Clean PFF Data ----------------------------------------------------------

clean_pff_data <- function(df) {
  df %>%
    mutate(
      player = tolower(player),  # Convert to lowercase
      player = str_replace_all(player, "[\\.']", ""),  # Remove periods & apostrophes
      player = str_replace_all(player, "\\s+jr\\.?$", ""),  # Remove "Jr."
      player = str_replace_all(player, "\\s+sr\\.?$", ""),  # Remove "Sr."
      player = str_replace_all(player, "\\s+(ii|iii|iv|v|vi|vii|viii|ix|x)$", ""),  # Remove Roman numerals
      player = str_squish(player)  # Remove extra spaces
    )
}

defense_data <- clean_pff_data(defense_data)
blocking_data <- clean_pff_data(blocking_data)
passing_data <- clean_pff_data(passing_data)
receiving_data <- clean_pff_data(receiving_data)
rushing_data <- clean_pff_data(rushing_data)


# Merge PFF and Roster Data -----------------------------------------------
players_final <- players_final %>%
  group_by(Player, rookie_year, draft_club, draft_number) %>% 
  slice_min(draft_number, with_ties = FALSE) %>%  # Keep one row per draft number
  ungroup()

# Merge defense data with players_final using both `player_id` and `player`
merged_defense <- defense_data %>%
  left_join(players_final, by = c("player_id" = "pff_id", "player" = "Player"))

ambiguous_defense <- merged_defense %>%
  group_by(player, Year) %>%
  filter(n() > 1) %>%
  ungroup()

# View ambiguous cases
print(ambiguous_defense)

merged_defense <- merged_defense %>%
  # Remove Derrick Johnson with specific draft statistics
  filter(!(player == "derrick johnson" & draft_number == 205 & Draft_Round == 6)) %>%
  
  # Mutate rookie_year for specific player_ids
  mutate(
    rookie_year = case_when(
      player_id == 44081 ~ 2020,  # Update rookie year for player_id 44081
      player_id == 56037 ~ 2022,  # Update rookie year for player_id 56037
      TRUE ~ rookie_year  # Keep other rookie_year values unchanged
    )
  )



merged_passing <- passing_data %>%
  left_join(players_final, by = c("player_id" = "pff_id", "player" = "Player"))

ambiguous_passing <- merged_passing %>%
  group_by(player, Year) %>%
  filter(n() > 1) %>%
  ungroup()

# View ambiguous cases
print(ambiguous_passing)


merged_receiving <- receiving_data %>%
  left_join(players_final, by = c("player_id" = "pff_id", "player" = "Player"))
            
ambiguous_receiving <- merged_receiving %>%
  group_by(player, Year) %>%
  filter(n() > 1) %>%
  ungroup()
            
# View ambiguous cases
print(ambiguous_receiving)

merged_rushing <- rushing_data %>%
  left_join(players_final, by = c("player_id" = "pff_id", "player" = "Player"))

ambiguous_rushing <- merged_rushing %>%
  group_by(player, Year) %>%
  filter(n() > 1) %>%
  ungroup()

# View ambiguous cases
print(ambiguous_rushing)


merged_blocking <- blocking_data %>%
  left_join(players_final, by = c("player_id" = "pff_id", "player" = "Player"))

ambiguous_blocking <- merged_blocking %>%
  group_by(player, Year) %>%
  filter(n() > 1) %>%
  ungroup()

# View ambiguous cases
print(ambiguous_blocking)
