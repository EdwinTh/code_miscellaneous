library(ggplot2)
library(dplyr)
library(magrittr)
library(data.table)

# functie om een ontwerp te maken van de tegeltjes
# produceert een random patroon

tegels <- function(
  kleuren  = c('darkblue', 'cyan3', 'lightblue1', 'white'),   # vector van kleuren
  aantal   = rep(40, 4),     # aantal tegels er kleur
  aantal_h = 8, # aantal tegels in de hoogte
  aantal_b = 20, # aantal tegels in de breedte
  max_naast_elkaar = 2) {
  
  if(length(kleuren) != length(aantal)){
    stop('Lengte van kleuren en lengte van aantal niet gelijk')
  }
  if(sum(aantal) != aantal_h * aantal_b){
    stop('Som van aantal moet gelijk zijn aan aantal_h * aantal_b')
  }
  
  plot_data <- expand.grid(1:aantal_h, 1:aantal_b) %>% as.data.table
  colnames(plot_data) <- c('Hoogte', 'Breedte')
  plot_data$Kleur <- integer(nrow(plot_data))
  
  kleuren_to_sample <- rep(1:length(p_vec), p_vec * aantal_h * aantal_b)
  
  # functie die een kleur samplet uit de resterende opties
  # geeft de kleur terug en de kleuren_to_sample met de kleur eruit verwijderd
  sample_kleur <- function(uitsluiten = 0, 
                           kts = kleuren_to_sample){
    if(kts  %>% is_in(uitsluiten) %>% all){
      stop('Geen geldige oplossing gevonden, start opnieuw')
    }
    
    geldige_kleur <- FALSE
    while(geldige_kleur == FALSE){
      kleur <- sample(kts, 1)
      if(kleur %>% is_in(uitsluiten) %>% not) {
        geldige_kleur <- TRUE
        return_list <- list(kleur = kleur,
                            kts = 
                              kts[-((kts == kleur) %>% which.max)])
      }
    }
    return(return_list)
  }
  
  # functie voor de te selecteren kleur kijkt of er een kleur moet
  # worden uitgesloten
  check_kleuren <- function(cur_breedte,
                            cur_hoogte){
    if(cur_hoogte > max_naast_elkaar){
      kleuren_hoogte <- 
        plot_data[Hoogte %in% (cur_hoogte-(max_naast_elkaar)):(cur_hoogte-1) & 
                    Breedte == cur_breedte, Kleur] %>% unique
    } else {
      kleuren_hoogte <- NULL
    }
    
    if(cur_breedte > max_naast_elkaar){
      kleuren_breedte <- 
        plot_data[Breedte %in% (cur_breedte-(max_naast_elkaar)):(cur_breedte-1) & 
                    Hoogte == cur_hoogte, Kleur] %>% unique
    } else {
      kleuren_breedte <- NULL
    }
    
    if(length(kleuren_hoogte) > 1) kleuren_hoogte <- NULL
    if(length(kleuren_breedte) > 1) kleuren_breedte <- NULL
    uitsluiten <- c(kleuren_hoogte, kleuren_breedte)
    if(length(uitsluiten) == 0) uitsluiten <- 0
    
    return(uitsluiten)
  }
  
  for(i in 1:(aantal_b-1)){
    for(j in 1:aantal_h){
      uitsluiten_iter <- check_kleuren(i, j)
      kleur_iter      <- sample_kleur(uitsluiten_iter, kleuren_to_sample)
      plot_data[Hoogte == j & Breedte == i, Kleur := kleur_iter$kleur]
      kleuren_to_sample <- kleur_iter$kts
    }
  }
  
  plot_data[ ,Kleur := Kleur %>% as.character]
  
  ggplot(plot_data, aes(Breedte, Hoogte)) +
    geom_tile(aes(fill = Kleur), col = 'white') +
    scale_fill_manual(values = c('1' = kleuren[1],
                                 '2' = kleuren[2],
                                 '3' = kleuren[3],
                                 '4' = kleuren[4])) + 
    xlab('') + 
    ylab('') + 
    guides(fill = FALSE) + 
    theme_bw() +
    theme(
      plot.background = element_blank()
      ,panel.grid.major = element_blank()
      ,panel.grid.minor = element_blank()
      ,panel.border = element_blank()
      ,axis.ticks.x = element_blank()
      ,axis.ticks.y = element_blank()
      ,axis.text.x = element_blank()
      ,axis.text.y = element_blank()
    ) +
    coord_fixed()
  
}

tegels(kleuren = paste('deepskyblue', 1:3, sep = ''),
       aantal  = c(59, 59, 29),
       aantal_h =  7,
       aantal_b =  21,
       max_naast_elkaar = 2)