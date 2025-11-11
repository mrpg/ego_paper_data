library(plm)

source("lib/preload.R")
source("lib/data.R")

# Table A1: Lower and upper bounds of cooperation rates ----
# that a binomial test rejects at the 5% level per platform and frame

## Helper function to compute bounds for a subset ----
compute_bounds <- function(data, subset_condition, rounds = 1:10, levels = 0:10) {
    bounds <- data.frame()

    for (round in rounds) {
        subset_data <- data[subset_condition & data$round_ == round, ]
        n_coop <- sum(subset_data$coop)
        n_total <- length(subset_data$coop)

        # Find lower bound (test alternative = "greater")
        # Find the lowest p where we reject that true proportion > p
        lower_bound <- NA
        for (level in rev(levels[levels > 0])) {
            p_val <- binom.test(n_coop, n_total, p = level / 10, alternative = "greater")$p.value
            if (p_val < 0.05) {
                lower_bound <- level * 10
                break
            }
        }

        # Find upper bound (test alternative = "less")
        # Find the highest p where we reject that true proportion < p
        upper_bound <- NA
        for (level in levels[levels < 10]) {
            p_val <- binom.test(n_coop, n_total, p = level / 10, alternative = "less")$p.value
            if (p_val < 0.05) {
                upper_bound <- level * 10
                break
            }
        }

        bounds <- rbind(bounds, data.frame(
                                           Round = round,
                                           Lower = lower_bound,
                                           Upper = upper_bound,
                                           N = n_total,
                                           Cooperation_Rate = round(n_coop / n_total * 100, 1)
                                           ))
    }

    return(bounds)
}

## GPT-3.5 bounds ----
cat("GPT-3.5 Base:\n")
gpt35_base <- compute_bounds(
                             data35_main,
                             data35_main$treat == "base"
)
print(gpt35_base)

cat("\nGPT-3.5 Enemy:\n")
gpt35_enemy <- compute_bounds(
                              data35_main,
                              data35_main$treat == "enemy"
)
print(gpt35_enemy)

cat("\nGPT-3.5 Competition:\n")
gpt35_competition <- compute_bounds(
                                    data35_main,
                                    data35_main$treat == "competition"
)
print(gpt35_competition)

## GPT-4 bounds ----
cat("\nGPT-4 Base:\n")
gpt4_base <- compute_bounds(
                            data4_main,
                            data4_main$treat == "base"
)
print(gpt4_base)

cat("\nGPT-4 Enemy:\n")
gpt4_enemy <- compute_bounds(
                             data4_main,
                             data4_main$treat == "enemy"
)
print(gpt4_enemy)

cat("\nGPT-4 Competition:\n")
gpt4_competition <- compute_bounds(
                                   data4_main,
                                   data4_main$treat == "competition"
)
print(gpt4_competition)

## Human bounds (pooled across all rounds) ----
# For humans, compute a single bound pooled across all rounds
compute_bounds_pooled <- function(data, subset_condition, levels = 0:10) {
    subset_data <- data[subset_condition, ]
    n_coop <- sum(subset_data$coop)
    n_total <- length(subset_data$coop)

    # Find lower bound (test alternative = "greater")
    lower_bound <- NA
    for (level in rev(levels[levels > 0])) {
        p_val <- binom.test(n_coop, n_total, p = level / 10, alternative = "greater")$p.value
        if (p_val < 0.05) {
            lower_bound <- level * 10
            break
        }
    }

    # Find upper bound (test alternative = "less")
    upper_bound <- NA
    for (level in levels[levels < 10]) {
        p_val <- binom.test(n_coop, n_total, p = level / 10, alternative = "less")$p.value
        if (p_val < 0.05) {
            upper_bound <- level * 10
            break
        }
    }

    return(data.frame(
        Lower = lower_bound,
        Upper = upper_bound,
        N = n_total,
        Cooperation_Rate = round(n_coop / n_total * 100, 1)
    ))
}

cat("\nHuman Base (pooled):\n")
human_base <- compute_bounds_pooled(
                                    with_human_long_human,
                                    with_human_long_human$treat == "base"
)
print(human_base)

cat("\nHuman Enemy (pooled):\n")
human_enemy <- compute_bounds_pooled(
                                     with_human_long_human,
                                     with_human_long_human$treat == "enemy"
)
print(human_enemy)

cat("\nHuman Competition (pooled):\n")
human_competition <- compute_bounds_pooled(
                                           with_human_long_human,
                                           with_human_long_human$treat == "competition"
)
print(human_competition)

## Create combined summary table (Table A1) ----
create_summary_table <- function(gpt35_base, gpt35_enemy, gpt35_competition,
                                 gpt4_base, gpt4_enemy, gpt4_competition,
                                 human_base, human_enemy, human_competition) {

    # Helper to format bounds
    format_bounds <- function(lower, upper) {
        lower_str <- ifelse(is.na(lower), "", as.character(lower))
        upper_str <- ifelse(is.na(upper) | upper >= 100, "-", as.character(upper))
        return(paste0(lower_str, "\n", upper_str))
    }

    cat("\n\nTable A1: Lower and upper bounds of cooperation rates\n")
    cat("that a binomial test rejects at the 5% level per platform and frame\n\n")

    platforms <- c("GPT-3.5", "GPT-4", "Human")
    frames <- c("base", "enemy", "competition")

    # Get all data frames in a nested list
    bounds_list <- list(
                        `GPT-3.5` = list(base = gpt35_base, enemy = gpt35_enemy, competition = gpt35_competition),
                        `GPT-4` = list(base = gpt4_base, enemy = gpt4_enemy, competition = gpt4_competition),
                        Human = list(base = human_base, enemy = human_enemy, competition = human_competition)
    )

    for (platform in platforms) {
        cat(sprintf("\n%s:\n", platform))
        for (frame in frames) {
            df <- bounds_list[[platform]][[frame]]
            if (platform == "Human") {
                # For humans, show single pooled value
                cat(sprintf("  %s: Lower = %s, Upper = %s (pooled)\n",
                            frame,
                            ifelse(is.na(df$Lower), "NA", as.character(df$Lower)),
                            ifelse(is.na(df$Upper) | df$Upper >= 100, "-", as.character(df$Upper))))
            } else {
                # For GPT platforms, show per-round values
                cat(sprintf("  %s: Lower bounds by round: %s\n",
                            frame, paste(ifelse(is.na(df$Lower), "NA", df$Lower), collapse = ", ")))
                cat(sprintf("  %s: Upper bounds by round: %s\n",
                            frame, paste(ifelse(is.na(df$Upper) | df$Upper >= 100, "-", df$Upper), collapse = ", ")))
            }
        }
    }
}

create_summary_table(gpt35_base, gpt35_enemy, gpt35_competition,
                     gpt4_base, gpt4_enemy, gpt4_competition,
                     human_base, human_enemy, human_competition)
