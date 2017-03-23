simulate_population <- function(pop_id, num_ind = 10, num_chromosomes = 2, num_sites = 100, avg_alt_allele_freq = 0.1){
  
  lapply(1:num_ind, simulate_individual, 
         pop_id = pop_id, num_chromosomes = num_chromosomes, num_sites = num_sites, 
         avg_alt_allele_freq = avg_alt_allele_freq) %>% bind_rows
  
}