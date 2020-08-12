# COSC350 Programming Assignment
# Russell Knight

# Set up vectors containing attributes related to each car
makes = c("Toyota", "Nissan", "Honda", "Hyundai", "Holden")
nationalities = c("British", "Canadian", "French", "Indian", "Chinese")
destinations = c("Gold Coast", "Sydney", "Tamworth", "Port Macquarie", "Newcastle")
colours = c("Red", "Green", "Blue", "White", "Black")
times = c("5am", "6am", "7am", "8am", "9am")

# Randomise a data frame by sampling each attribute
car.data <- data.frame(
  Make = sample(makes),
  Nationality = sample(nationalities),
  Destination = sample(destinations),
  Colour = sample(colours),
  Time = sample(times)
)

print(car.data)

# fitness function tests number of constraints that equate to TRUE
# TO DO
fitness <- logical(length = 0)
print(fitness)

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

blue_position <- match("Blue", car.data$Colour)
green_position <- match("Green", car.data$Colour)
white_position <- match("White", car.data$Colour)
black_position <- match("Black", car.data$Colour)

goldcoast_position <- match("Gold Coast", car.data$Destination)
newcastle_position <- match("Newcastle", car.data$Destination)
tamworth_position <- match("Tamworth", car.data$Destination)

# Known constraints listed here
# The Toyota Camry was hired at 6:00am by a British couple.
print(car.data$Nationality[toyota_position] == "British") # Toyota && British
print(car.data$Time[toyota_position] == "6am") # Toyota && 6am
print(car.data$Time[british_position] == "6am") # British && 6am

# The car in the middle (3rd) had a black colour.
print(car.data$Colour[3] == "Black")

# The Hyundai Accent left the depot at 9:00am.
print(car.data$Time[hyundai_position] == "9am") # Hyundai && 9am

# The Holden Barina with a blue colour was to the left of the car that carries the British couple.
print(car.data$Colour[holden_position] == "Blue") # Holden && Blue
print(holden_position != 5) # Holden && ~5th
print(british_position != 1) # British && ~1st
print(british_position - holden_position == 1) # Holden leftOf British
print(british_position - blue_position == 1) # Blue leftOf British

# To the right of the car hired by a French lady was the car going to Gold Coast.
print(goldcoast_position != 1) # GoldCoast && ~1st
print(french_position != 5) # French && ~5th
print(goldcoast_position - french_position == 1) # French LeftOf Gold Coast

# The Nissan X-Trail was heading for Sydney.
print(car.data$Destination[nissan_position] == "Sydney") # Nissan && Sydney

# To the right of the car carrying a Chinese businessman was the car with a green colour.
print(chinese_position != 5) # Chinese && ~5th
print(green_position != 1) # Green && ~1st
print(green_position - chinese_position == 1) # Chinese LeftOf Green

# The car going to Newcastle left at 5:00am.
print(car.data$Time[newcastle_position] == "5am") # Newcastle && 5am

# The Honda Civic left at 7:00am and was on the right of the car heading for Gold Coast.
print(goldcoast_position != 5) # GoldCoast && ~5th
print(honda_position != 1) # Honda && ~1st
print(sevenAm_position != 1) # 7am && ~1st
print(car.data$Time[honda_position] == "7am") # Honda && 7am
print(honda_position - goldcoast_position == 1) # GoldCoast LeftOf Honda
print(sevenAm_position - goldcoast_position == 1) # GoldCoast LeftOf 7am

# The car with a red colour was going to Tamworth.
print(car.data$Colour[tamworth_position] == "Red") # Tamworth && Red

# To the left of the car that left at 7:00am was the car with a white colour.
print(white_position != 5) # White && ~5th
# 7am && ~1st already checked previously
print(sevenAm_position - white_position == 1) # White LeftOf 7am

# The last (5th) car was hired by an Indian man.
print(indian_position == 5) # Indian && 5th

# The car with a black colour left at 8:00am.
print(car.data$Time[black_position] == "8am") # Black && 8am

# The car carrying an Indian man was to the right of the car hired by a Chinese businessman.
print(indian_position != 1) # Indian && ~1st
print(chinese_position != 5) # Chinese && ~5th
print(indian_position - chinese_position == 1) # Chinese LeftOf Indian

# The car heading for Tamworth left at 6:00am.
print(car.data$Time[tamworth_position] == "6am") # Tamworth && 6am