# Import ggplot library
library(ggplot2)

# Read in data
df = read.csv("mammals.csv")

# Boxplot of diet type vs adjusted BMR
ggplot(df, aes(x=diet_type, y=adjusted_bmr))+
  geom_boxplot()+
  labs(title="Adjusted BMR by Diet",
       x="Diet",
       y="Adjusted BMR")+
  theme_classic()

# Boxplot of diet type vs body mass
ggplot(df, aes(x=diet_type, y=adult_mass_g))+
  geom_boxplot()+
  labs(title="Body Mass by Diet",
       x="Diet",
       y="Body Mass (g)")+
  theme_classic()

# Scatterplot of % Plants in Diet vs Adjusted BMR
ggplot(df, aes(x=dphy_plant, y=adjusted_bmr))+
  geom_point()+
  labs(title="Herbivory and BMR",
       x="% Plants in Diet",
       y="Adjusted BMR")+
  theme_classic()

# Scatterplot of log-transformed Body Mass vs log-transformed 
# BMR Color-coded by Diet Type
ggplot(df, aes(x=log(adult_mass_g),
               y=log(BasalMetRate_mLO2hr),
               color=factor(diet_type)))+
  geom_point()+
  labs(title="Body Mass vs BMR",
       x="log(Body Mass)",
       y="log(BMR)")+
  theme_classic()
