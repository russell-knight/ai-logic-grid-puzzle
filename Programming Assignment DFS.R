library(combinat)

makes = c("Toyota", "Nissan", "Honda", "Hyundai", "Holden")
nationalities = c("British", "Canadian", "French", "Indian", "Chinese")
destinations = c("Gold Coast", "Sydney", "Tamworth", "Port Macquarie", "Newcastle")
colours = c("Red", "Green", "Black", "White", "Blue")
times = c("5am", "6am", "7am", "8am", "9am")

# fix names
makePermutations <- permn(makes)
nationalitiesPermutations <- permn(nationalities)
destinationsPermutations <- permn(destinations)
coloursPermutations <- permn(colours)
timesPermutations <- permn(times)

# bestSolution stores the solution that has returned the highest fitness value. A solution will be found
# once all 22 constraints are passed
bestSolution <- 0

bestFitness <- 0
bestSolution <- data.frame
count <- 0

for (m in makePermutations) {
  # Make positions
  toyota_position <- match("Toyota", m)
  hyundai_position <- match("Hyundai", m)
  holden_position <- match("Holden", m)
  nissan_position <- match("Nissan", m)
  honda_position <- match("Honda", m)
  for (n in nationalitiesPermutations) {
    # Nationality positions
    british_position <- match("British", n)
    french_position <- match("French", n)
    chinese_position <- match("Chinese", n)
    indian_position <- match("Indian", n)
    canadian_position <- match("Canadian", n)
    
    # Toyota Camry hired by a British couple
    if (toyota_position != british_position) { next }
    # Holden Barina to the left of car hired by British couple
    if (british_position - holden_position != 1) { next }
    # Last (5th) car hired by Indian man
    if (indian_position != 5) { next }
    # Indian car to the right of Chinese car
    if (indian_position - chinese_position != 1) { next }
    
    for (d in destinationsPermutations) {
      # Destination positions
      goldcoast_position <- match("Gold Coast", d)
      newcastle_position <- match("Newcastle", d)
      tamworth_position <- match("Tamworth", d)
      portMacquarie_position <- match("Port Macquarie", d)
      sydney_position <- match("Sydney", d)
      
      # Gold Coast car to right of French car
      if (goldcoast_position - french_position != 1) { next }
      # Nissan car heading to Sydney
      if (nissan_position != sydney_position) { next }
      # Honda car to right of Gold Coast car
      if (honda_position - goldcoast_position != 1) { next }
      
      for (c in coloursPermutations) {
        # Color positions
        blue_position <- match("Blue", c)
        green_position <- match("Green", c)
        white_position <- match("White", c)
        black_position <- match("Black", c)
        red_position <- match("Red", c)
        
        # Middle (3rd) car was black
        if (black_position != 3) { next }
        # Holden was blue
        if (holden_position != blue_position) { next }
        # Blue car was left of British
        if (british_position - blue_position != 1) { next }
        # Green car to right of Chinese
        if (green_position - chinese_position != 1) { next }
        # Red car went to Tamworth
        if (red_position != tamworth_position) { next }
        for (t in timesPermutations) {
          # Time positions
          fivenAm_position <- match("5am", t)
          sixAm_position <- match("6am", t)
          sevenAm_position <- match("7am", t)
          eightAm_position <- match("8am", t)
          nineAm_position <- match("9am", t)
          
          # Toyota hired at 6am by British
          if (toyota_position != sixAm_position) { next }
          if (british_position != sixAm_position) { next }
          # Hyundai left at 9am
          if (hyundai_position != nineAm_position) { next }
          # Newcastle left at 5am
          if (newcastle_position != fivenAm_position) { next }
          # White car to left of car that left at 7am
          if (sevenAm_position - white_position != 1) { next }
          # Black car left at 8am
          if (black_position != eightAm_position) { next }
          # Tamworth car left at 6am
          if (tamworth_position != sixAm_position) { next }
          
          # if this point is reached all constraints have passed so a solution
          # has been found
          solution <- data.frame(
            Make = m,
            Nationality = n,
            Destination = d,
            Colour = c,
            Time = t
          )
          print(solution)

        }
      }
    }
  }
}

  
  
  
  
  
  
  

