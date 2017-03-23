simulate_individual <- function(pop_id, ind_id, num_chromosomes, num_sites, avg_alt_allele_freq){
  
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