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
# fitness function tests number of constraints that equate to TRUE
fitness <- logical(length = 0)
bestSolution = 0

for (i in 1:1000000) {
  # Randomise a data frame by sampling each attribute
  car.data <- data.frame(
    Make = sample(makes),
    Nationality = sample(nationalities),
    Destination = sample(destinations),
    Colour = sample(colours),
    Time = sample(times)
  )
  #print(car.data)
  
  # Extract positional information from data frame
  toyota_position <- match("Toyota", car.data$Make)
  hyundai_position <- match("Hyundai", car.data$Make)
  holden_position <- match("Holden", car.data$Make)
  nissan_position <- match("Nissan", car.data$Make)
  honda_position <- match("Honda", car.data$Make)
  
  sevenAm_position <- match("7am", car.data$Time)
  
  british_position <- match("British", car.data$Nationality)
  french_position <- match("French", car.data$Nationality)
  chinese_position <- match("Chinese", car.data$Nationality)
  indian_position <- match("Indian", car.data$Nationality)
  canadian_position <- match("Canadian", car.data$Nationality)
  
  blue_position <- match("Blue", car.data$Colour)
  green_position <- match("Green", car.data$Colour)
  white_position <- match("White", car.data$Colour)
  black_position <- match("Black", car.data$Colour)
  
  goldcoast_position <- match("Gold Coast", car.data$Destination)
  newcastle_position <- match("Newcastle", car.data$Destination)
  tamworth_position <- match("Tamworth", car.data$Destination)
  portMacquarie_position <- match("Port Macquarie", car.data$Destination)
  
  # Known constraints listed here
  # The Toyota Camry was hired at 6:00am by a British couple.
  fitness <- append(fitness, car.data$Nationality[toyota_position] == "British") # Toyota && British
  fitness <- append(fitness, car.data$Time[toyota_position] == "6am") # Toyota && 6am
  fitness <- append(fitness, car.data$Time[british_position] == "6am") # British && 6am
  
  # The car in the middle (3rd) had a black colour.
  fitness <- append(fitness, car.data$Colour[3] == "Black")
  
  # The Hyundai Accent left the depot at 9:00am.
  fitness <- append(fitness, car.data$Time[hyundai_position] == "9am") # Hyundai && 9am
  
  # The Holden Barina with a blue colour was to the left of the car that carries the British couple.
  fitness <- append(fitness, car.data$Colour[holden_position] == "Blue") # Holden && Blue
  fitness <- append(fitness, british_position - holden_position == 1) # Holden leftOf British
  fitness <- append(fitness, british_position - blue_position == 1) # Blue leftOf British
  
  # To the right of the car hired by a French lady was the car going to Gold Coast.
  fitness <- append(fitness, goldcoast_position - french_position == 1) # French LeftOf Gold Coast
  
  # The Nissan X-Trail was heading for Sydney.
  fitness <- append(fitness, car.data$Destination[nissan_position] == "Sydney") # Nissan && Sydney
  
  # To the right of the car carrying a Chinese businessman was the car with a green colour.
  fitness <- append(fitness, green_position - chinese_position == 1) # Chinese LeftOf Green
  
  # The car going to Newcastle left at 5:00am.
  fitness <- append(fitness, car.data$Time[newcastle_position] == "5am") # Newcastle && 5am
  
  # The Honda Civic left at 7:00am and was on the right of the car heading for Gold Coast.
  fitness <- append(fitness, car.data$Time[honda_position] == "7am") # Honda && 7am
  fitness <- append(fitness, honda_position - goldcoast_position == 1) # GoldCoast LeftOf Honda
  fitness <- append(fitness, sevenAm_position - goldcoast_position == 1) # GoldCoast LeftOf 7am
  
  # The car with a red colour was going to Tamworth.
  fitness <- append(fitness, car.data$Colour[tamworth_position] == "Red") # Tamworth && Red
  
  # To the left of the car that left at 7:00am was the car with a white colour.
  fitness <- append(fitness, sevenAm_position - white_position == 1) # White LeftOf 7am
  
  # The last (5th) car was hired by an Indian man.
  fitness <- append(fitness, indian_position == 5) # Indian && 5th
  
  # The car with a black colour left at 8:00am.
  fitness <- append(fitness, car.data$Time[black_position] == "8am") # Black && 8am
  
  # The car carrying an Indian man was to the right of the car hired by a Chinese businessman.
  fitness <- append(fitness, indian_position - chinese_position == 1) # Chinese LeftOf Indian
  
  # The car heading for Tamworth left at 6:00am.
  fitness <- append(fitness, car.data$Time[tamworth_position] == "6am") # Tamworth && 6am
  
  # Check if this solution's fitness is better than previous best

  if (sum(fitness) > bestSolution){
    solutionFrame = car.data
    bestSolution = sum(fitness)
    print("New best")
    print(bestSolution)
  }
  # Reset fitness value
  fitness <- logical(length = 0)
}
print(bestSolution)
print(solutionFrame)