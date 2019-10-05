# Useful snippets of code

# git remote add origin https://github.com/rlavelli/RUtility.git

# git config remote.origin.url git@github.com:rlavelli/RUtility.git
# git pull -u origin master
# git push -u origin master

# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# use of possibly() which helps dealing with errors
possible_my_function <- possibly(my_function, otherwise = NA_real_)
numbers_with_error <- list(1, 2, 3, "spam", 4) # wrong data type

map(numbers_with_error, possible_my_function) # executes with NA

# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# nice plot for visualize missing data
missing_data <- df %>% summarise_all(funs(sum(is.na(.))/n())) # use all variables
missing_data <- gather(missing_data, key = "variables", value = "percent_missing")
ggplot(missing_data, aes(x = reorder(variables, percent_missing), y = percent_missing)) +
  geom_bar(stat = "identity", fill = "red", aes(color = I('white')), size = 0.3)+
  xlab('variables')+
  coord_flip()+ 
  theme_bw()


# ------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------
# complete dates
id <- paste0(letters[1:3], 1:3)

anni1 <- c(2015, 2015, 2015, 2016, 2018)
mesi1 <- c(2, 5, 7, 3, 4)
anni2 <- c(2014, 2014, 2016, 2018)
mesi2 <- c(1, 4, 5, 1)
anni3 <- c(2016, 2018, 2018)
mesi3 <- c(1, 5, 12)

set.seed(123)
dat <- data.frame(id = rep(id, c(5,4,3)),
                  anno = c(anni1, anni2, anni3),
                  mese = c(mesi1, mesi2, mesi3),
                  qta = sample(50:100, 12))

library(dplyr)
library(tidyr)

dat_r <- dat %>% 
  complete(id, anno, mese) # completo i casi possibili

# per ogni id salvo solo anno min/max
# dat_r <- dat_r %>% 
#   group_by(id) %>% 
#   mutate(anno_min = min(anno[!is.na(qta)]),
#          anno_max = max(anno[!is.na(qta)])) %>% 
#   filter(anno >= anno_min & anno <= anno_max) %>% 
#   ungroup() %>% 
#   mutate(qta = ifelse(is.na(qta), 0, qta))

complete_missing_dates <- function(dat, id_var = "id") {
  #' Input: data.frame con anno/mese incompleti dei mancanti
  #' variabile id prodotto
  #' Output: data.frame con valori nulli per i mesi mancanti
  #' nel periodo di vendita del prodotto, basato su min/max anni
  id_var <- rlang::sym(id_var)

  dat_c <- dat_c %>% 
    group_by(!!id_var) %>% 
    mutate(anno_min = min(anno[!is.na(qta)]),
           anno_max = max(anno[!is.na(qta)])) %>% 
    filter(anno >= anno_min & anno <= anno_max) %>% 
    ungroup() %>% 
    mutate(qta = ifelse(is.na(qta), 0, qta))
  dat_c %>% 
    select(-anno_max, -anno_min)
}

complete_missing_dates(dat, id_var = "id")


