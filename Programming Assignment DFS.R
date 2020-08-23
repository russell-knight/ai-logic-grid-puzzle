library(combinat)

# Create vectors for each of the car attributes
# The index of each vector represents the position the car was in
makes = c("Toyota", "Nissan", "Honda", "Hyundai", "Holden")
nationalities = c("British", "Canadian", "French", "Indian", "Chinese")
destinations = c("Gold Coast", "Sydney", "Tamworth", "Port Macquarie", "Newcastle")
colours = c("Red", "Green", "Black", "White", "Blue")
times = c("5am", "6am", "7am", "8am", "9am")

# For each attribute we create a list containing every possible permutation
makePermutations <- permn(makes)
nationalitiesPermutations <- permn(nationalities)
destinationsPermutations <- permn(destinations)
coloursPermutations <- permn(colours)
timesPermutations <- permn(times)


# From here the we begin a Depth-First Search to try and find a valid solution
# to the logic-grid puzzle. As the tree is searched, the constraints given in
# the puzzle are tested. If a constraint fails, we know that particular branch
# is no longer worth exploring so we move on to the next branch.
for (m in makePermutations) {
  # Make positions for this permutation
  toyota_position <- match("Toyota", m)
  hyundai_position <- match("Hyundai", m)
  holden_position <- match("Holden", m)
  nissan_position <- match("Nissan", m)
  honda_position <- match("Honda", m)
  
  for (n in nationalitiesPermutations) {
    # Nationality positions for this permutation
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
      # Destination positions for this permutation
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
        # Color positions for this permutation
        blue_position <- match("Blue", c)
        green_position <- match("Green", c)
        white_position <- match("White", c)
        black_position <- match("Black", c)
        red_position <- match("Red", c)
        
        # Middle (3rd) car was black
        if (black_position != 3) { next }
        # The Holden was blue
        if (holden_position != blue_position) { next }
        # Blue car was to the left of the British couple
        if (british_position - blue_position != 1) { next }
        # Green car was to the right of the Chinese man
        if (green_position - chinese_position != 1) { next }
        # The red car went to Tamworth
        if (red_position != tamworth_position) { next }
        
        for (t in timesPermutations) {
          # Time positions for this permutation
          fivenAm_position <- match("5am", t)
          sixAm_position <- match("6am", t)
          sevenAm_position <- match("7am", t)
          eightAm_position <- match("8am", t)
          nineAm_position <- match("9am", t)
          
          # Toyota hired at 6am by British couple
          if (toyota_position != sixAm_position) { next }
          if (british_position != sixAm_position) { next }
          # Hyundai left at 9am
          if (hyundai_position != nineAm_position) { next }
          # The car going to Newcastle left at 5am
          if (newcastle_position != fivenAm_position) { next }
          # White car to the left of car that left at 7am
          if (sevenAm_position - white_position != 1) { next }
          # The black car left at 8am
          if (black_position != eightAm_position) { next }
          # The Tamworth car left at 6am
          if (tamworth_position != sixAm_position) { next }
          
          # By reaching this point all constraints have passed so a valid solution
          # has been found
          solution <- data.frame(
            Make = m,
            Nationality = n,
            Destination = d,
            Colour = c,
            Time = t
          )
          # print(solution)
          cat(sprintf("The car in position %s was a %s %s hired by an %s at %s whose destination was %s.\n",
                      portMacquarie_position,
                      solution[portMacquarie_position,4],
                      solution[portMacquarie_position,1],
                      solution[portMacquarie_position,2],
                      solution[portMacquarie_position,5],
                      solution[portMacquarie_position,3]))
          cat(sprintf("The car in position %s was a %s %s hired by an %s at %s whose destination was %s.\n",
                      canadian_position,
                      solution[canadian_position,4],
                      solution[canadian_position,1],
                      solution[canadian_position,2],
                      solution[canadian_position,5],
                      solution[canadian_position,3]))

        }
      }
    }
  }
}

  
  
  
  
  
  
  

