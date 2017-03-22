# simulate some population genetic data

# maybe one day we can use MS for this, for now, fake data

# tidyverse, natch
library("tidyverse")

# need a function to create fake snp data someday
# for now, an artisinal fake SNP data set
# 2 pops (A and B) 10 individuals per pop, 2 chromosomes with 100 loci per individual
# all SNPs biallelic

make_fake_individual <- function(pop_id, ind_id, num_chromosomes, num_sites, avg_alt_allele_freq){
  
  # total length of dataset
  len_dat <- num_chromosomes * num_sites
  
  # initialize data frame (no genotypes)
  pop <- rep(pop_id, len_dat)
  ind <- rep(ind_id, len_dat)
  pos <- rep(1:num_sites, num_chromosomes)
  chr <- rep(1:num_chromosomes, len_dat) %>% sort
  
  # add in genotypes
  # genotypes are a binomial draw based on average frequency of the alt allele
  # 0 = reference allele (major allele if alt freq < 0.5), 
  # 1 = alterantive allele (minor allele if alt freq < 0.5)
  data.frame(pop, ind, chr, pos) %>%
    rowwise %>%
    mutate(genotype = rbinom(1, size = 2, prob = avg_alt_allele_freq))
    
}

make_fake_individual("A", 1, 2, 100, 0.2)



make_fake_population <- function(pop_id, num_ind, num_chromosomes, num_sites, avg_alt_allele_freq = 0.1){
  
  lapply(1:num_ind, make_fake_individual, 
         pop_id = pop_id, num_chromosomes = 2, num_sites = 100, 
         avg_alt_allele_freq = 0.1) %>% bind_rows
  
  make_fake_individual(pop_id, ind_id, num_chromosomes, num_sites, avg_alt_allele_freq)
  
}

# make a fake dataset (no genotypes)
pop_dat <- 


