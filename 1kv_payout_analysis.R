# title: "1kv Payout Analysis"
# author: "Jonas Gehrlein @ Web3 Foundation"
# date: 31/01/2022

library(tidyverse)
# Specify chain: either "polkadot" or "kusama"
chain = "kusama"
# Current era - 1 as the data is written at the end of an era
current_era = 3302

if(chain=="polkadot"){
  normalization = 1/10000000000
}else{
  normalization = 1/1000000000000
}

# Define the 1kv Stashes
kusama_1kv_stash <- c('EX9uchmfeSqKTM7cMMg8DkH49XV8i4R7a7rqCn8btpZBHDP', 'G1rrUNQSk7CjjEmLSGcpNu72tVtyzbWdUvgmSer9eBitXWf', 'HgTtJusFEn2gmMmB5wmJDnMRXKD6dzqCpNR7a99kkQ7BNvX')
kusama_1kv_names <- c("1kv-stash-1", "1kv-stash-2", "1kv-stash-3")
kusama_1kv <- data.frame(kusama_1kv_stash, kusama_1kv_names)

# Import Data for T
validators <- read.csv(url(paste("https://storage.googleapis.com/watcher-csv-exporter/", chain , "_validators_era_", current_era, ".csv", sep=(""))),stringsAsFactors = FALSE)

# Import Data from T+1 and only extract the rewards paid to validators.
validators_next <- read.csv(url(paste("https://storage.googleapis.com/watcher-csv-exporter/", chain , "_validators_era_", current_era + 1, ".csv", sep=(""))),stringsAsFactors = FALSE)
validator_rewards <- validators_next$validator_rewards_previous_era[1]

# Only take active validators
validators <- subset(validators, active==1)

# Very rare bug where the "stakers" variable is empty - should investigate this later.
validators <- subset(validators, stakers != "")

decode <- function(code) {
  # return a list of data frames for the decoded string vector code
  code %>% 
    str_split(",") %>% 
    map(str_split, ";", simplify = TRUE) %>% 
    map(as.data.frame) %>% 
    map(set_names, c("name", "value")) %>%
    map(~mutate_all(.,as.character)) %>%
    map(mutate, value = as.numeric(value))
}

sum_these <- function(df1, to_sum1) {
  # for one data frame of name, value (df1), sum the values for name in to_sum1
  df1 %>% filter(name %in% to_sum1) %>% pull(value) %>% as.numeric() %>% sum()
}

# Set counter variable
validators$votes_of_1kv <- 0
validators$our_stash <- "none"

# Loop to determine if one of the stash addresses is among the stakers, which essentially means that the validator is in 1kv
for(i in 1:nrow(validators)){
  for(x in 1:length(kusama_1kv_stash)){
    find <- grepl(kusama_1kv$kusama_1kv_stash[x], validators$stakers[i])
    if(find==TRUE){
      validators$votes_of_1kv[i] <- validators$votes_of_1kv[i] + 1
      validators$our_stash[i] <- kusama_1kv_stash[x]
    }
  }
}

# Take subset of only 1kv validators
validators_use <- subset(validators, votes_of_1kv>=1)

# Generate dataset which extracts the amount of our stake with the validator. In the case that more 1kv-stashes stake with a validator, we take the sum (this might never be the case)
df <- tibble(
  validators_use$name, validators_use$commission_percent, validators_use$self_stake, validators_use$total_stake, validators_use$era_points, validators_use$our_stash,
  val = decode(validators_use$stakers), 
  to_sum = list(kusama_1kv_stash), 
  our_stake = map2_dbl(val, to_sum, sum_these)
)

# TO-DO Payoff Analysis



