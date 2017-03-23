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

# compute per-locus weir and cockerham's FST

wc_fst <- function(pop_dat){
  
  # compute (alt) allele frequencies
  allele_freq_dat <- pop_dat %>%
    group_by(pop, chr, pos) %>%
    summarize(allele_freq = sum(genotype) / length(genotype)) %>%
    spread(key = pop, value = allele_freq)
  
  names(allele_freq_dat)[3:4] <- c("frq_A", "frq_B")
  
  # compute observed heterozygosity
  het_obs_dat <- pop_dat %>%
    group_by(pop, chr, pos) %>%
    summarize(het_obs = sum(genotype == 1)/ length(genotype)) %>%
    spread(key = pop, value = het_obs)
  
  names(het_obs_dat)[3:4] <- c("h_obs_A", "h_obs_B")
  
  # compute sample sizes 
  sample_size_dat <- pop_dat %>%
    group_by(pop, chr, pos) %>%
    tally %>% 
    ungroup %>%
    group_by(chr, pos) %>%
    spread(key = pop, value = n)
  
  names(sample_size_dat)[3:4] <- c("n_A", "n_B")
  
  # join three data types
  frq_dat <- left_join(allele_freq_dat, het_obs_dat) %>%
    left_join(sample_size_dat)
  
  # compute all wc fst parameters including wc FST
  frq_dat %>%
    rowwise %>%
    mutate(n_bar = sum(n_A, n_B) / 2) %>%
    mutate(n_c = (2 * n_bar - (((n_A^2)/(2 * n_bar)) + (n_B^2)/(2 * n_bar))) / 2 - 1) %>%
    mutate(p_bar = ((n_A * frq_A) + (n_B * frq_B))/ (2 * n_bar)) %>%
    mutate(s_2 = ((n_A * (frq_A - p_bar)^2) / ((2 - 1) * n_bar)) + ((n_B * (frq_B - p_bar)^2) / ((2 - 1) * n_bar))) %>%
    mutate(h_bar = ((n_A * h_obs_A) + (n_B * h_obs_B))/ (2 * n_bar)) %>%
    mutate(a = (n_bar/n_c) * (s_2 - ((1 / (n_bar - 1)) * (p_bar * (1 - p_bar) - (1/2) * s_2 - (1/4) * h_bar)))) %>%
    mutate(b = (n_bar / (n_bar - 1)) * (p_bar * (1 - p_bar) - (1/2) * s_2 - (((2 * n_bar) - 1 )/(4 * n_bar)) * h_bar)) %>%
    mutate(c = (1/2) * h_bar) %>%
    mutate(wc_fst = a / (a + b + c)) %>%
    select(chr, pos, wc_fst)
  
}



# compute weir and cockerham's FST
# truly a horrendous formula

# FST ("theta hat") = a / a + b + c

# PRIMITIVE TERMS:

# ni, the sample size for population i
# pi, the frequency of allele "A" in population i
# r, the number of populations
r <- 2

# COMPONENT TERMS:

# n_bar, the average sample size 
# sum num samples divided by num pops

# n_c, a scaling factor for sample size variance 
# (r * n_bar - (sum(ni^2)/r*n_bar)) / r - 1 

# p_bar, the average sample freq of allele A: 
# sum(ni * pi) / r * n_bar

# s_squared, the sample variance of allele A freq over pops
# sum(ni(pi - p_bar)^2) / ((r - 1) * n_bar)

# h_bar, the average heterzygote frequency of allele A

