library(tidyr)

# Function to augment the data by adding information about conversation timing
augment <- function (data) {
    times <- group_by(data, experiment, convo) %>%
        summarize(amt = min(min_time), bmt = max(min_time)) %>%
        ungroup()

    # Join the timing information back to the original data
    # and add a 'second' column to indicate if it's the second message in a conversation
    left_join(data, times, by = c("experiment", "convo")) %>%
        mutate(second = (min_time == bmt))
}

# Function to convert treatment numbers to descriptive factor levels
treatments <- function (data) {
    mutate(data,
           treat = factor(treatment,
                          levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                          labels = c("base", "project", "enemy", "competition",
                                     "war", "labour", "stats", "couple",
                                     "procurement", "discretion"))
    )
}

# Function to convert wide format data to long format
to_long <- function (with_human, slug, second_) {
    with_human[, endsWith(colnames(with_human), slug) | colnames(with_human) == "participant.label"] %>%
        pivot_longer(
                     cols = starts_with("ego_human."),   # Select all columns starting with "ego_human."
                     names_to = "round_",         # Create a new column for the variable names
                     values_to = "choice",         # Create a new column for the values
                     ) %>%
    mutate(round_ = rep(1:10, 304), second = second_, coop = (choice == 1)) %>%
    left_join(with_human[, c("participant.label", "treat", "gpt4", "human")], by = "participant.label")
}

# Load and process data for GPT-3.5 model
data35 <- read.csv("data/data35_new.csv") %>%
    augment() %>%
    treatments() %>%
    mutate(gpt4 = F, human = F, coop = (choice == 1))

# Load and process data for GPT-4 model
data4 <- read.csv("data/data4_new.csv") %>%
    augment() %>%
    treatments() %>%
    mutate(gpt4 = T, human = F, coop = (choice == 1))

# Combine GPT-3.5 and GPT-4 data
combined34 <- rbind(data35, data4)

# Filter main treatments (base, enemy, competition) for GPT-3.5 and GPT-4
data35_main <- filter(data35, treat == "base" | treat == "enemy" | treat == "competition")
data4_main <- filter(data4, treat == "base" | treat == "enemy" | treat == "competition")
combined34_main <- rbind(data35_main, data4_main)

# Load and process human data from multiple CSV files
with_human <- bind_rows(
                        read.csv("data/all_apps_wide-2023-08-11.csv"),
                        read.csv("data/all_apps_wide-2023-08-15.csv"),
                        read.csv("data/all_apps_wide-2023-08-16.csv"),
                        read.csv("data/all_apps_wide-2023-08-17.csv")
                        ) %>% 
filter(!is.na(ego_human.10.player.other_choice)) %>% # Filter out incomplete data (leaves 304 pairs)
mutate(treatment = ego_human.1.player.treatment, gpt4 = T, human = T) %>%
    treatments()

# Convert human data to long format for both player and AI choices
with_human_long_human <- to_long(with_human, "player.choice", T)
with_human_long_llm <- to_long(with_human, "player.other_choice", F)

# Combine human and AI choice data
with_human_long <- rbind(with_human_long_human, with_human_long_llm)

# Combine all data (GPT-3.5, GPT-4, and human) into a single dataset
all_data <- bind_rows(combined34, with_human_long) %>%
    mutate(convo = ifelse(is.na(convo), participant.label, convo), ldef = NA, lsdef = NA)

cat("Please wait until the counter reaches ~86000.\n")

# Calculate last defection (ldef) and sum of defections (lsdef) for each conversation
for (i in 1:nrow(all_data)) {
    if (!all_data[i, "second"]) {
        previous <- arrange(
                            filter(all_data,
                                   convo == all_data[i, "convo"],
                                   round_ < all_data[i, "round_"],
                                   second),
                            round_)

        if (nrow(previous) > 0) {
            all_data[i, "ldef"] <- tail(previous, 1)$choice != 1
            all_data[i, "lsdef"] <- sum(previous$choice != 1)
        }
    }

    # Print progress every 1000 rows
    if (i %% 1000 == 0) {
        print(i)
    }
}

# Filter main treatments (base, enemy, competition) for the final dataset
all_data_main <- filter(all_data, treat == "base" | treat == "enemy" | treat == "competition")
gptreact <- filter(all_data_main, !is.na(ldef), gpt4)

cat("Calculating standard errors takes a while. Be patient.\n")
