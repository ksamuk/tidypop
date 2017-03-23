# simulate some population genetic data

# maybe one day we can use MS for this, for now, fake data

# tidyverse, natch
library("tidyverse")

# need a function to create fake snp data someday
# for now, an artisinal fake SNP data set
# 2 pops (A and B) 10 individuals per pop, 2 chromosomes with 100 loci per individual
# all SNPs biallelic

list.files("functions", full.names = TRUE) %>% sapply(source, echo = FALSE)

# simulate some two populations
pop_A <- simulate_population("A", avg_alt_allele_freq = 0.1)
pop_B <- simulate_population("B", avg_alt_allele_freq = 0.2)

pop_dat <- bind_rows(pop_A, pop_B)

# compute per-locus wright's FST

pop_dat %>%
  group_by(pop, chr, pos) %>%
  summarize(allele_freq = sum(genotype) / length(genotype))

