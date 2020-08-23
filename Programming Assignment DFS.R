# COSC350 Programming Assignment
# Author: Russell

options(warn=-1) # used to supress messy warning messages
library(combinat)

cat(sprintf("This program was written by Russell Knight for COSC350: Artificial Intelligence.\nIts purpose is to solve the logic-grid puzzle specified in the COSC350 Programming Assignment. In order to achieve this, a Depth-First Search algorithm has been implemented. This algorithm traverses the tree of solutions, starting at the root node, then exploring as far down the tree as it can before reaching a leaf node. Leaf nodes represent possible solutions. As there are quadzillions of possible solutions, it is unfeasible to exhasutively search the entire tree. Instead, as the algorithm searches each branch it checks the potential solution against the given constraints in the puzzle. If a constraint fails, the algorithm knows it is a waste of time to continue down the current branch and moves on to the next. In this manner the algorithm quickly converges to a solution.\n\n"))

cat("Press [enter] to continue")
readLines(file("stdin"), n=1)


# Create vectors for each of the car attributes
# The index of each vector represents the position of the car
# e.g Honda is in the 3rd position
makes = c("Toyota", "Nissan", "Honda", "Hyundai", "Holden")
nationalities = c("British", "Canadian", "French", "Indian", "Chinese")
destinations = c("Gold Coast", "Sydney", "Tamworth", "Port Macquarie", "Newcastle")
colours = c("Red", "Green", "Blue", "White", "Black")
times = c("5am", "6am", "7am", "8am", "9am")

# Timer to slow execution speed for better readability of program
cat("Creating tree solution space")
Sys.sleep(1)
cat(".")
Sys.sleep(1)
cat(".")
Sys.sleep(1)
message(".")
Sys.sleep(1)


# For each attribute we create a list containing every possible permutation
makesPermutations <- permn(makes)
nationalitiesPermutations <- permn(nationalities)
destinationsPermutations <- permn(destinations)
coloursPermutations <- permn(colours)
timesPermutations <- permn(times)

# The rationale here is that permutations of each attribute can be combined to
# create every possible solution in the solution space.
# In this way we can use the Depth-First Search algorithm to try and find a
# valid solution to the logic-grid puzzle. In this approach, each layer of the
# tree represents the position of a different attribute.
# e.g the 1st layer represents the make of car found in the 1st position, the 2nd
# layer represents the make of car found in the 2nd position as so on. This
# produces the following tree structure:

# Layer 1: Make in 1st position
# Layer 2: Make in 2nd position
# Layer 3: Make in 3rd position
# Layer 4: Make in 4th position
# Layer 5: Make in 5th position
# Layer 6: Nationality in 1st position
# Layer 7: Nationality in 2nd position
# Layer 8: Nationality in 3rd position
# Layer 9: Nationality in 4th position
# Layer 10: Nationality in 5th position
# Layer 11: Destination in 1st position
# Layer 12: Destination in 2nd position
# Layer 13: Destination in 3rd position
# Layer 14: Destination in 4th position
# Layer 15: Destination in 5th position
# Layer 16: Colour in 1st position
# Layer 17: Colour in 2nd position
# Layer 18: Colour in 3rd position
# Layer 19: Colour in 4th position
# Layer 20: Colour in 5th position
# Layer 21: Time in 1st position
# Layer 22: Time in 2nd position
# Layer 23: Time in 3rd position
# Layer 24: Time in 4th position
# Layer 25: Time in 5th position

# Looking at this structure it is clear that only the leaf nodes contain
# possible solutions. It is also clear that there are quadzillions of possible
# solutions. As such, exhaustively searching the tree is unfeasible as it would
# take hundreds of years to search the entire solution space.

# In order to avoid wasting time on unnecessary branches in the tree, the algorithm
# tests each permutation against the given constraints. If the constraint is not
# passed, the algorithm knows that remainder of this branch is not worth exploring
# and moves on to the next branch.


count <- 0

# Loop through every combination of permutation
for (m in makesPermutations) {
  count <- count + 1
  # Make positions for this permutation
  toyota_position <- match("Toyota", m)
  hyundai_position <- match("Hyundai", m)
  holden_position <- match("Holden", m)
  nissan_position <- match("Nissan", m)
  honda_position <- match("Honda", m)
  
  for (n in nationalitiesPermutations) {
    count <- count + 1
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
      count <- count + 1
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
        count <- count + 1
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
          count <- count + 1
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
          
          # At this point all constraints have passed so we have a valid solution
          solution <- data.frame(
            Make = m,
            Nationality = n,
            Destination = d,
            Colour = c,
            Time = t
          )
          message("Solution: ")
          print(solution)
          cat("Number of branches searched: ")
          message(count)
          message("")
          cat(sprintf("The car in position %s was a %s %s hired by an %s at %s whose destination was %s.\n",
                      portMacquarie_position,
                      solution[portMacquarie_position,4],
                      solution[portMacquarie_position,1],
                      solution[portMacquarie_position,2],
                      solution[portMacquarie_position,5],
                      solution[portMacquarie_position,3]))
          cat(sprintf("The car in position %s was a %s %s hired by a %s at %s whose destination was %s.\n",
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

  
  
  
  
  
  
  

