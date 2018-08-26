library("lpSolve")
library("lpSolveAPI")

#Reading in database
dat = read.csv("Math 381 Diet Problem Food Item Database.csv", header = TRUE)

#Data Wrangling
foodName = dat$Food
foodName = as.character(foodName)
foodType = dat$Food.type
foodType = as.character(foodType)
fruits = numeric(171)
veggiesandlegs = numeric(171)
meat = numeric(171)
beverage = numeric(171)
fatsandoil = numeric(171)
dairy = numeric(171)

#Making vectors to indicate foodtype via binary variable
for (i in 1:171) {
  if (foodType[i] == "FRUITS"){
    fruits[i] = 1
  }
  else if (foodType[i] == "MEAT"){
    meat[i] = 1
  }
  else if (foodType[i] == "BEVERAGES"){
    beverage[i] = 1
  }
  else if (foodType[i] == "VEGETABLES AND LEGUMES"){
    veggiesandlegs[i] = 1
  }
  else if (foodType[i] == "FATS AND OILS"){
    fatsandoil[i] = 1
  }
  else if (foodType[i] == "DAIRY"){
    dairy[i] = 1
  }
}

#More data wrangling
price = dat$Price.Per.Serving....
food = dat$Variable.Name
food = as.character(food)
fat = dat$Total.Fat..g.
protein = dat$Protein..g.
carb = dat$Carbohydrate..g.
calcium = dat$Calcium..mg.
fiber = dat$Fiber..g.
cholestrol = dat$Cholesterol..mg.
iron = dat$Iron..mg.
magnesium = dat$Magnesium..mg.
calories = dat$Energy..kcal.
sodium = dat$Sodium..mg.
phosphorus = dat$Phosphorous..mg.
potassium = dat$Potassium..mg.
zinc = dat$Zinc..mg.
niacin = dat$Niacin..mg.
thiamine = dat$Thiamin..mg.
riboflavin = dat$Riboflavin..mg.
vitb6 = dat$Vit.B6..mg.
vitc = dat$Vit.C..mg.
vita = dat$Vit.A..RE. 
type = dat$Food.type
type = as.character(type)

#Setting upper and lower bounds for number of servings of each food item
lowerbound = numeric(171)
upperbound = numeric(171)
for (i in 1:171){
   upperbound[i] = 5
}

#Initializing Linear Program Model
lprec <- make.lp(0, 171)
set.objfn(lprec, price)

#Adding nutritional and variety constraints
add.constraint(lprec, protein, ">=", 45)
add.constraint(lprec, protein, "<=", 65)
add.constraint(lprec, carb, ">=", 100)
add.constraint(lprec, carb, "<=", 160)
add.constraint(lprec, calcium, ">=", 800)
add.constraint(lprec, calcium, "<=", 1200)
add.constraint(lprec, fiber, ">=", 30)
add.constraint(lprec, fiber, "<=", 46)
add.constraint(lprec, fat, ">=", 16)
add.constraint(lprec, fat, "<=", 28)
add.constraint(lprec, cholestrol, ">=", 220)
add.constraint(lprec, cholestrol, "<=", 360)
add.constraint(lprec, iron, ">=", 5)
add.constraint(lprec, iron, "<=", 10)
add.constraint(lprec, magnesium, ">=", 300)
add.constraint(lprec, magnesium, "<=", 500)
add.constraint(lprec, calories, ">=", 1000) 
add.constraint(lprec, calories, "<=", 2500)
add.constraint(lprec, sodium, ">=", 1150)
add.constraint(lprec, sodium, "<=", 1850)
add.constraint(lprec, phosphorus, ">=", 400)
add.constraint(lprec, phosphorus, "<=", 1000)
add.constraint(lprec, potassium, ">=", 3000)
add.constraint(lprec, potassium, "<=", 5000)
add.constraint(lprec, zinc, ">=", 8)
add.constraint(lprec, zinc, "<=", 16)
add.constraint(lprec, niacin, ">=", 12)
add.constraint(lprec, niacin, "<=", 20)
add.constraint(lprec, thiamine, ">=", 1)
add.constraint(lprec, thiamine, "<=", 1.4)
add.constraint(lprec, riboflavin, ">=", 1.1)
add.constraint(lprec, riboflavin, "<=", 1.5)
add.constraint(lprec, vitb6, ">=", 1)
add.constraint(lprec, vitb6, "<=", 1.5)
add.constraint(lprec, vitc, ">=", 65)
add.constraint(lprec, vitc, "<=", 150)
add.constraint(lprec, vita, ">=", 700)
add.constraint(lprec, vita, "<=", 1500)
add.constraint(lprec, fruits, ">=", 1)
add.constraint(lprec, veggiesandlegs, ">=", 1)
add.constraint(lprec, meat, ">=", 1)
add.constraint(lprec, beverage, ">=", 1)
add.constraint(lprec, fatsandoil, ">=", 1)
add.constraint(lprec, dairy, ">=", 1)

#Setting row names in LP Model
RowNames <- c("Min Protein", "Max Protein", "Min Carbohydrates", "Max Carbohydrate", "Min Calcium", 
              "Max Calcium", "Min Fiber", "Max Fiber", "Min Fat", "Max Fat", "Min Cholestrol", 
              "Max Cholestrol", "Min Iron", "Max Iron","Min Magnesium", "Max Magnesium",
              "Min Calories", "Max Calories", "Min Sodium", "Max Sodium", "Min Phosphorus", "Max Phosphorus",
              "Min Potassium", "Max Potassium", "Min Zinc", "Max Zinc", "Min Niacin", "Max Niacin", "Min Thiamine",
              "Max Thiamine", "Min Riboflavin", "Max Riboflavin", "Min Vitamin B6", "Max Vitamin B6",
              "Min Vitamin C", "Max Vitamin C", "Min Vitamin A", "Max Vitamin A", "Min Fruits",
              "Min Veggies and Legumes", "Min Meat", "Min Beverage", "Min Fats and Oils", "Min Dairy")

#Setting upper and lower bounds for number of servings of each food item in LP Model 
set.bounds(lprec, lower = lowerbound, columns = 1:171)
set.bounds(lprec, upper = upperbound, columns = 1:171)

ColNames <- food

#Setting type of solution to real numbers
set.type(lprec, 1:171, "real")

dimnames(lprec) <- list(RowNames, ColNames)

#Solving the LP
solve(lprec)
solution = get.variables(lprec)
totalCost = sum(solution * price)

#Rounding results to quarter servings
for (i in 1:171){ 
  if(solution[i] != 0){
    rounded = solution[i] * 100
    toMultiply = rounded / 25.0
    toMultiply = round(toMultiply)
    solution[i] = toMultiply * 0.25
  }
}
totalCostAfterRounding = sum(solution * price)

count = 0
for (i in 1:171){
  if(solution[i] != 0) {
    count = count + 1
  }
}

#Gathering results (Food items, serving size, price)
foodResults = numeric(count) 
categoriesResult = numeric(count)
priceResult = numeric(count)
servingsResult = numeric(count)
index = 1
for (i in 1:171){
  if(solution[i] != 0){
    foodResults[index] = foodName[i] 
    categoriesResult[index] = foodType[i]
    priceResult[index] = price[i] * solution[i]
    servingsResult[index] = solution[i]
    index = index + 1
  }
}

#Making table and pie chart of results
resultTable = data.frame(foodResults, priceResult, servingsResult, categoriesResult)
colNames = c("Food Name", "Price", "Serving Size", "Food Type")
names(resultTable) = colNames
pct = round(priceResult/sum(priceResult)*100)
foodResults = paste(foodResults, pct) 
foodResults = paste(foodResults,"%",sep="")
pie(priceResult, foodResults)

#Writing LP to text file
write.lp(lprec, filename="test.txt")

