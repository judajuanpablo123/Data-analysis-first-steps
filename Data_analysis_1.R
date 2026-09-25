#Title: Data analyst 1
# Author: Juda Juan Pablo
# Date: 29-09-2026

# fist we need charge our library tidyverse
library(tidyverse)
library(palmerpenguins)

data <- palmerpenguins::penguins_raw
data2 <- palmerpenguins::penguins
class(data)
## first we need know about our data
dim(data)
glimpse(data)
names(data)

## how many species are in the dataset?
data %>%
      summarise(num_species = n_distinct(Species)
                )
## cuantos individuos pertenecen a cada especie?
data %>%
    count(Species) ## we compute the frecuency in the variable species

n_specie <- data %>%
    group_by(Species) %>%
    mutate(Frecuencia_specie = n()) %>% 
    ungroup()

ggplot(data = n_specie, aes(x = Species, y= Frecuencia_specie, fill = Species)) + 
  geom_bar(stat = "identity") +
  theme_minimal()
## how many missing values are
sum(is.na(data))


result_na <-c() ## vector vacio
for (columna in data)
{
  n_na <- sum(is.na(columna)) ## guardamos un vector para los resultados de cada iteracion
  ## guardamos el resultado en este vector
  result_na = c(result_na, n_na) ## agregamos estos resultados al vector vacio
}
result_na

data_na <- data.frame(Variable = colnames(data),
                      na = result_na)
data_na

data_na %>%
        mutate(porcentaje_na = na/nrow(data)* 100)






























## aqui comenzamos aver correlaciones entre las variables solamente numericas

data_numeric <- data %>%
                select(where(is.numeric)) %>%
                drop_na() ## quitamos valores na

cor <- cor(data_numeric)
library(GGally)
ggpairs(data_numeric)
