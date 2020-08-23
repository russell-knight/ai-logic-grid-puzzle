# COSC350 Programming Assignment
# Russell Knight

# Set up vectors containing attributes related to each car
makes = c("Toyota", "Nissan", "Honda", "Hyundai", "Holden")
nationalities = c("British", "Canadian", "French", "Indian", "Chinese")
destinations = c("Gold Coast", "Sydney", "Tamworth", "Port Macquarie", "Newcastle")
colours = c("Red", "Green", "Blue", "White", "Black")
times = c("5am", "6am", "7am", "8am", "9am")

# Initialise solution state data frame
solutionFrame = data.frame(Make = vector(),
                           Nationality = vector(),
                           Destination = vector(),
                           Colour = vector(),
                           Time = vector()
                           )

# bestSolution stores the solution that has returned the highest fitness value. A solution will be found
# once all 22 constraints are passed
bestSolution <- 0

# function that takes a solution state as an argument and tests it against known constraints
# returning a fitness value
test_constraints <- function(solution) {
  # Extract positional information from data frame
  toyota_position <- match("Toyota", solution$Make)
  hyundai_position <- match("Hyundai", solution$Make)
  holden_position <- match("Holden", solution$Make)
  nissan_position <- match("Nissan", solution$Make)
  honda_position <- match("Honda", solution$Make)
  
  sevenAm_position <- match("7am", solution$Time)
  
  british_position <- match("British", solution$Nationality)
  french_position <- match("French", solution$Nationality)
  chinese_position <- match("Chinese", solution$Nationality)
  indian_position <- match("Indian", solution$Nationality)
  canadian_position <- match("Canadian", solution$Nationality)
  
  blue_position <- match("Blue", solution$Colour)
  green_position <- match("Green", solution$Colour)
  white_position <- match("White", solution$Colour)
  black_position <- match("Black", solution$Colour)
  
  goldcoast_position <- match("Gold Coast", solution$Destination)
  newcastle_position <- match("Newcastle", solution$Destination)
  tamworth_position <- match("Tamworth", solution$Destination)
  portMacquarie_position <- match("Port Macquarie", solution$Destination)
  
  # Fitness value, will store Boolean values of based on constraints passed and failed
  # e.g if solution has a black car in the middle position TRUE will be appended to fitness
  fitness <- logical(length = 0)
  
  # Known constraints listed here
  # The Toyota Camry was hired at 6:00am by a British couple.
  fitness <- append(fitness, solution$Nationality[toyota_position] == "British") # Toyota && British
  fitness <- append(fitness, solution$Time[toyota_position] == "6am") # Toyota && 6am
  fitness <- append(fitness, solution$Time[british_position] == "6am") # British && 6am
  
  # The car in the middle (3rd) had a black colour.
  fitness <- append(fitness, solution$Colour[3] == "Black")
  
  # The Hyundai Accent left the depot at 9:00am.
  fitness <- append(fitness, solution$Time[hyundai_position] == "9am") # Hyundai && 9am
  
  # The Holden Barina with a blue colour was to the left of the car that carries the British couple.
  fitness <- append(fitness, solution$Colour[holden_position] == "Blue") # Holden && Blue
  fitness <- append(fitness, british_position - holden_position == 1) # Holden leftOf British
  fitness <- append(fitness, british_position - blue_position == 1) # Blue leftOf British
  
  # To the right of the car hired by a French lady was the car going to Gold Coast.
  fitness <- append(fitness, goldcoast_position - french_position == 1) # French LeftOf Gold Coast
  
  # The Nissan X-Trail was heading for Sydney.
  fitness <- append(fitness, solution$Destination[nissan_position] == "Sydney") # Nissan && Sydney
  
  # To the right of the car carrying a Chinese businessman was the car with a green colour.
  fitness <- append(fitness, green_position - chinese_position == 1) # Chinese LeftOf Green
  
  # The car going to Newcastle left at 5:00am.
  fitness <- append(fitness, solution$Time[newcastle_position] == "5am") # Newcastle && 5am
  
  # The Honda Civic left at 7:00am and was on the right of the car heading for Gold Coast.
  fitness <- append(fitness, solution$Time[honda_position] == "7am") # Honda && 7am
  fitness <- append(fitness, honda_position - goldcoast_position == 1) # GoldCoast LeftOf Honda
  fitness <- append(fitness, sevenAm_position - goldcoast_position == 1) # GoldCoast LeftOf 7am
  
  # The car with a red colour was going to Tamworth.
  fitness <- append(fitness, solution$Colour[tamworth_position] == "Red") # Tamworth && Red
  
  # To the left of the car that left at 7:00am was the car with a white colour.
  fitness <- append(fitness, sevenAm_position - white_position == 1) # White LeftOf 7am
  
  # The last (5th) car was hired by an Indian man.
  fitness <- append(fitness, indian_position == 5) # Indian && 5th
  
  # The car with a black colour left at 8:00am.
  fitness <- append(fitness, solution$Time[black_position] == "8am") # Black && 8am
  
  # The car carrying an Indian man was to the right of the car hired by a Chinese businessman.
  fitness <- append(fitness, indian_position - chinese_position == 1) # Chinese LeftOf Indian
  
  # The car heading for Tamworth left at 6:00am.
  fitness <- append(fitness, solution$Time[tamworth_position] == "6am") # Tamworth && 6am
  
  # Sum fitness values will return the number of TRUE values, i.e the number of constraints 
  # that this solution has passed
  return(sum(fitness))
} 

# turn this into a while loop while(bestSolution != 22)
for (i in 1:100000) {
  # Randomise a data frame by sampling each attribute
  car.data <- data.frame(
    Make = sample(makes),
    Nationality = sample(nationalities),
    Destination = sample(destinations),
    Colour = sample(colours),
    Time = sample(times)
  )
  #print(car.data)
  currentFitness <- test_constraints(car.data)
  
  if (currentFitness > bestSolution){
    solutionFrame = car.data
    bestSolution = currentFitness
    print("New best")
    print(bestSolution)
  }
}
# print results
print(bestSolution)
print(solutionFrame)