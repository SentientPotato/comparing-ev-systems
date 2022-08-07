## Load packages
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

## Define vote allocation function
## (see https://en.wikipedia.org/wiki/D%27Hondt_method)
dhondt = function(parties, votes, seats) {
    total   = sum(votes)
    votemat = outer(votes, 1:seats, "/")
    topvals = sort(votemat, decreasing = TRUE)[seats]
    indices = which(votemat >= topvals, arr.ind = TRUE)
    return(sapply(parties, function(x) sum(parties[indices[ , "row"]] == x)))
}

## Read in raw data
## Election return data from MIT's Election Lab,
## https://doi.org/10.7910/DVN/42MVDX
## Electoral vote distribution from Wikipedia,
## https://en.wikipedia.org/wiki/United_States_Electoral_College
## All datasets used in this file should be in the GitHub gist except the
## MIT Election Lab data, which should be downloaded at the link above
pop = read_csv("1976-2020-president.csv")
EC  = read_csv("EVs-all.csv")
WTA = read_csv("winner-take-all.csv")

## Reshape electoral vote data and prepare state & year variables for merging
EC  = EC %>%
    pivot_longer(!State, names_to = "year", values_to = "EVs") %>%
    rename(state = State) %>%
    mutate(state = toupper(state)) %>%
    mutate(state = ifelse(state == "D.C.", "DISTRICT OF COLUMBIA", state)) %>%
    mutate(year  = as.numeric(year))

## Summarize popular vote data to the year/state/candidate level
## (In some states, candidates ran under multiple party labels)
pop = pop %>%
    group_by(year, candidate) %>%
    mutate(party = party_detailed[1]) %>%
    group_by(year, state, party) %>%
    summarize(votes = sum(candidatevotes), total_votes = sum(totalvotes))

## Apportion electoral votes
dat = pop %>%
    left_join(EC, by = c("year", "state")) %>%
    group_by(year, state) %>%
    mutate(Votes = dhondt(party, votes, unique(EVs)))

## Aggregate third party votes
dat = dat %>%
    rename(Year = year) %>%
    mutate(Party = case_when(
        party == "DEMOCRAT" ~ "Democrat",
        party == "REPUBLICAN" ~ "Republican",
        TRUE ~ "Other"
    )) %>%
    group_by(Year, Party) %>%
    summarise(PopularVote = sum(votes), Proportional = sum(Votes, na.rm = TRUE))

## Add in winner-take-all results
dat = dat %>% full_join(WTA, by = c("Year", "Party"))

## Put vote totals in terms of percentages and reshape to long format
dat = dat %>%
    group_by(Year) %>%
    mutate(
        PopularVote   = 100 * PopularVote / sum(PopularVote),
        WinnerTakeAll = 100 * WinnerTakeAll / sum(WinnerTakeAll),
        Proportional  = 100 * Proportional / sum(Proportional)
    ) %>%
    pivot_longer(
        cols = c("WinnerTakeAll", "Proportional"),
        names_to = "System",
        values_to = "ElectoralVotes"
    )

## Plot results
map = aes(x = PopularVote, y = ElectoralVotes, color = Party, shape = Party)
pal = c(Democrat = "#0072b2", Other = "#cc79a7", Republican = "#d55e00")
url = "https://github.com/SentientPotato/comparing-ev-systems"
cap = "Figure by Sentient Potato (Twitter: @SentientPotato6)"
cap = paste(cap, paste("Code to reproduce at", url), sep = "\n")
ttl = "Correspondence between electoral vote & popular vote in proportional &"
ttl = paste(ttl, "winner-take-all systems, 1976-2020 elections", sep = "\n")
plt = ggplot(data = dat, mapping = map) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", alpha = 1/4) +
    geom_point(size = 3, alpha = 3/4) +
    facet_wrap(~System) +
    coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
    xlab("Popular Vote (Percent)") +
    ylab("Electoral Votes (Percent)") +
    labs(title = ttl, caption = cap) +
    scale_color_manual(values = pal) +
    theme_bw() +
    theme(plot.caption = element_text(color = "#5f5f5f", hjust = 0))
ggsave(
    filename = "ev-systems.png", plot = plt,
    height = 3.76, width = 6.684, units = "in", dpi = 180
)
