library(ggplot2)

# functie om een ontwerp te maken van de tegeltjes
# produceert een random patroon

tegels <- function(
  kleuren,   # vector van kleuren
  p_vec,     # vector probabilites van de kleuren, gelijke lengte als kleuren en som = 1
  aantal_h = 8, # aantal tegels in de hoogte
  aantal_b = 50, # aantal tegels in de breedte
  max_naast_elkaar = 3
  ) {
  
  if(length(kleuren) != length(p_vec)){
    stop('Lengte van kleuren en p_vec niet gelijk')
  }
  if(sum(p_vec) != 1){
    stop('Som van p_vec moet precies 1 zijn')
  }
  
  plot_data <- expand.grid(1:aantal_h, 1:aantal_b)
  colnames(plot_data) <- c('Hoogte', 'Breedte')
  plot_data$Kleur <- character(nrow(plot_data))
  ltr <- LETTERS[1:4]
  
  laatste_hoogte <- character(max_naast_elkaar)
  kleur_vermijden <- NULL

  for(i in 1:nrow(plot_data)){
    if(is.null(kleur_vermijden)) {
      plot_data$Kleur[i] <- sample(ltr, 1, ,p_vec)
    } else {
      plot_data$Kleur[i] <- sample(ltr[!(ltr %in% kleur_vermijden)] , 1, ,
                                   p_vec[!(ltr %in% kleur_vermijden)])
    }
      
    if(i %% 8 != 0){
      laatste_hoogte <- c(plot_data$Kleur[i], laatste_hoogte[1:2])
    } else {
      laatste_hoogte <- character(max_naast_elkaar)
    }
    laatste_breedte_ind <- c( (i+1) - c(aantal_h,
                                          aantal_h * 2,
                                          aantal_h * 3))
    if( all(laatste_breedte_ind > 1) ){
      laatste_breedte <- plot_data$Kleur[laatste_breedte_ind]
    } else {
      laatste_breedte <- LETTERS[1:3]
    }
      
    if(length(unique(laatste_hoogte)) == 1) {
       kleur_vermijden <- laatste_hoogte[1]
      }
    if(length(unique(laatste_breedte)) == 1) {
      kleur_vermijden <- c(kleur_vermijden, laatste_breedte[1])
    }
    if(length(unique(laatste_hoogte)) != 1 & 
         length(unique(laatste_breedte)) != 1){
      kleur_vermijden <- NULL
    }
    
  }

  ggplot(plot_data, aes(Breedte, Hoogte)) +
    geom_tile(aes(fill = Kleur), col = 'white') +
    scale_fill_manual(values = c("A" = kleuren[1],
                                 "B" = kleuren[2],
                                 "C" = kleuren[3],
                                 "D" = kleuren[4])) + 
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

beneden <- c('darkblue', 'cyan3', 'lightblue1', 'lightyellow')
boven <- c('red', 'gold', 'tan1', 'white')
boven_alt <- c('black', 'grey', 'red', 'white')

tegels(beneden, c(.3,.15,.4,.15), max_naast_elkaar = 3)
tegels(boven, c(.3,.2,.3,.2))
tegels(boven_alt, c(.3,.2,.3,.2))

boven_alt2 <- c('darkblue', 'tan4', 'beige', 'burlywood3')
tegels(boven_alt2, c(.3,.2,.3,.2))
