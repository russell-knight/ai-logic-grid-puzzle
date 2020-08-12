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
print(car.data$Colours[3])

# Known constraints listed here

# The Toyota Camry was hired at 6:00am by a British couple.
toyota_position <- print(match("Toyota", car.data$Make))
british_position <- print(match("British", car.data$Nationality))
print(car.data$Nationality[toyota_position] == "British") # Toyota && British
print(car.data$Time[toyota_position] == "6am") # Toyota && 6am
print(car.data$Time[british_position] == "6am") # British && 6am

# The car in the middle (3rd) had a black colour.
print(car.data$Colour[3] == "Black")

# The Hyundai Accent left the depot at 9:00am.
hyundai_position <- print(match("Hyundai", car.data$Make))
print(car.data$Time[hyundai_position] == "9am") # Hyundai && 9am

# The Holden Barina with a blue colour was to the left of the car that carries the British couple.
holden_position <- print(match("Holden", car.data$Make))
print(car.data$Colour[holden_position] == "Blue") # Holden && Blue
print(holden_position != 5) # Holden && ~5th

# To the right of the car hired by a French lady was the car going to Gold Coast.
# The Nissan X-Trail was heading for Sydney.
# To the right of the car carrying a Chinese businessman was the car with a green colour.
# The car going to Newcastle left at 5:00am.
# The Honda Civic left at 7:00am and was on the right of the car heading for Gold Coast.
# The car with a red colour was going to Tamworth.
# To the left of the car that left at 7:00am was the car with a white colour.
# The last car was hired by an Indian man.
# The car with a black colour left at 8:00am.
# The car carrying an Indian man was to the right of the car hired by a Chinese businessman.
# The car heading for Tamworth left at 6:00am.