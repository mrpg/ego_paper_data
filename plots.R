library(ggplot2)
library(Cairo)
library(patchwork)

source("lib/preload.R")
source("lib/data.R")


# plot 1 ("Figure 3")

machinedata_main <- all_data_main %>%
    filter(!human)

bsummary <- machinedata_main %>%
    group_by(gpt4, treat, round_) %>%
    summarize(mcoop = mean(coop), lower = ci(coop, 1), upper = ci(coop, 2))

# Generate Table 3 (Mean Percentage of Cooperative Choices per Platform and Frame)
table3 <- bsummary %>%
    group_by(gpt4, treat) %>%
    summarize(mmcoop = mean(mcoop) * 100) %>%
    ungroup()

cat("\n=== Table 3: Mean Percentage of Cooperative Choices ===\n")
print(table3, n = Inf)
cat("\n")

cairo_pdf("output/plot1.pdf", width = 7, height = 4)

ggplot(bsummary, aes(x = round_, y = mcoop, ymin = lower, ymax = upper, color = treat)) +
    geom_point() +
    geom_line() +
    scale_x_continuous(breaks = 1:10, labels = 1:10, minor_breaks = F) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    geom_ribbon(alpha = 0.2) +
    facet_wrap(~ gpt4, labeller = labeller(gpt4 = c("FALSE" = "GPT-3.5", "TRUE" = "GPT-4"))) +
    labs(x = "Round", y = "Cooperation", color = "Treatment")

dev.off()


# plot 2 ("Figure 4")

humandata <- all_data_main %>%
    filter(human)

ll <- c(1, 4, 7, 10)

cairo_pdf("output/plot2.pdf", width = 9.5, height = 4)

subplot1_1 <- humandata %>%
    group_by(treat, round_) %>%
    summarize(mcoop = mean(coop), lower = ci(coop, 1), upper = ci(coop, 2)) %>%
    ggplot(aes(x = round_, y = mcoop, ymin = lower, ymax = upper)) +
    geom_point() + 
    geom_line() +
    scale_x_continuous(breaks = ll, labels = ll, minor_breaks = F) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    geom_ribbon(alpha = 0.2) +
    facet_wrap(~ treat) +
    labs(x = "Round", y = "Cooperation") +
    ggtitle("Pooled")

subplot1_2 <- humandata %>%
    filter(!second) %>%
    group_by(treat, round_) %>%
    summarize(mcoop = mean(coop), lower = ci(coop, 1), upper = ci(coop, 2)) %>%
    ggplot(aes(x = round_, y = mcoop, ymin = lower, ymax = upper)) +
    geom_point() + 
    geom_line() +
    scale_x_continuous(breaks = ll, labels = ll, minor_breaks = F) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    geom_ribbon(alpha = 0.2) +
    facet_wrap(~ treat) +
    labs(x = "Round", y = "Cooperation") +
    ggtitle("Machine")

subplot1_3 <- humandata %>%
    filter(second) %>%
    group_by(treat, round_) %>%
    summarize(mcoop = mean(coop), lower = ci(coop, 1), upper = ci(coop, 2)) %>%
    ggplot(aes(x = round_, y = mcoop, ymin = lower, ymax = upper)) +
    geom_point() + 
    geom_line() +
    scale_x_continuous(breaks = ll, labels = ll, minor_breaks = F) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    geom_ribbon(alpha = 0.2) +
    facet_wrap(~ treat) +
    labs(x = "Round", y = "Cooperation") +
    ggtitle("Human")

subplot1_1 + subplot1_2 + subplot1_3

dev.off()

# plot A1

lookuptbl <- all_data_main %>%
    arrange(round_) %>%
    filter(human, second) %>%
    group_by(convo) %>%
    summarize(coop1 = first(coop))

lookup <- Vectorize(function (convo_) {
                        filter(lookuptbl, convo == convo_) %>%
                            pull(coop1)
})

# get second-mover's behavior in first round
humandata <- humandata %>%
    mutate(coop1 = lookup(convo))

movers <- humandata %>%
    group_by(second, treat, round_, coop1) %>%
    summarize(mcoop = mean(coop), lower = ci(coop, 1), upper = ci(coop, 2))

subplot2_1 <- ggplot(filter(movers, !second, coop1), aes(x = round_, y = mcoop, ymin = lower, ymax = upper)) + 
    geom_point() + 
    geom_line() +
    scale_x_continuous(breaks = 1:10, labels = 1:10, minor_breaks = F) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    geom_ribbon(alpha = 0.2) +
    facet_wrap(~ treat) +
    labs(x = "Round", y = "Cooperation") +
    ggtitle("AI, human initially cooperative")

subplot2_2 <- ggplot(filter(movers, second, coop1), aes(x = round_, y = mcoop, ymin = lower, ymax = upper)) + 
    geom_point() + 
    geom_line() +
    scale_x_continuous(breaks = 1:10, labels = 1:10, minor_breaks = F) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    geom_ribbon(alpha = 0.2) +
    facet_wrap(~ treat) +
    labs(x = "Round", y = "Cooperation") +
    ggtitle("Human, initially cooperative")

subplot2_3 <- ggplot(filter(movers, !second, !coop1), aes(x = round_, y = mcoop, ymin = lower, ymax = upper)) + 
    geom_point() + 
    geom_line() +
    scale_x_continuous(breaks = 1:10, labels = 1:10, minor_breaks = F) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    geom_ribbon(alpha = 0.2) +
    facet_wrap(~ treat) +
    labs(x = "Round", y = "Cooperation") +
    ggtitle("AI, human initially not cooperative")

subplot2_4 <- ggplot(filter(movers, second, !coop1), aes(x = round_, y = mcoop, ymin = lower, ymax = upper)) + 
    geom_point() + 
    geom_line() +
    scale_x_continuous(breaks = 1:10, labels = 1:10, minor_breaks = F) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    geom_ribbon(alpha = 0.2) +
    facet_wrap(~ treat) +
    labs(x = "Round", y = "Cooperation") +
    ggtitle("Human, initially not cooperative")

cairo_pdf("output/plotA1.pdf", width = 10, height = 8)

(subplot2_1 + subplot2_2) / (subplot2_3 + subplot2_4)

dev.off()


# plot A2

summary35 <- data35 %>%
    group_by(treat, round_) %>%
    summarize(mcoop = mean(coop), lower = ci(coop, 1), upper = ci(coop, 2))

cairo_pdf("output/plotA2.pdf", width = 8, height = 6)

ggplot(summary35, aes(x = round_, y = mcoop, ymin = lower, ymax = upper)) +
    geom_point() +
    geom_line() +
    scale_x_continuous(breaks = 1:10, labels = 1:10, minor_breaks = F) +
    scale_y_continuous(labels = scales::percent) +
    geom_ribbon(alpha = 0.2) +
    labs(x = "Round", y = "Cooperation") +
    facet_wrap(~ treat)

dev.off()


# plot A3

summary4 <- data4 %>%
    group_by(treat, round_) %>%
    summarize(mcoop = mean(coop), lower = ci(coop, 1), upper = ci(coop, 2))

cairo_pdf("output/plotA3.pdf", width = 8, height = 6)

ggplot(summary4, aes(x = round_, y = mcoop, ymin = lower, ymax = upper)) +
    geom_point() +
    geom_line() +
    scale_x_continuous(breaks = 1:10, labels = 1:10, minor_breaks = F) +
    scale_y_continuous(labels = scales::percent) +
    geom_ribbon(alpha = 0.2) +
    labs(x = "Round", y = "Cooperation") +
    facet_wrap(~ treat)

dev.off()
