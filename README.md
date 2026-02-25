# Background
This README is dedicated to the 2026 BIOE 276 group project by Eli Allen, Anik Grearson, and Mikaela Salvador.

### Introduction 
Anthocyanins are a group of pigments that contribute to petal color intensity across many species of flowers.<sup>1</sup> These pigments also are important for providing tolerance to abiotic stressors, such as high UV radiation, extreme soil conditions, and low precipitation.<sup>2</sup>

*Leptosiphon minimus* is a native plant species that occurs across the northwestern coast of North America in California, Oregon, Washington, and British Columbia. *L. minimus* is known for being polymorphic, exhibiting two or more petal colors, such as pink and white. Within these color morphs, elevated levels of anthocyanins produce a pink pigment. 

In our project, we are interested in assessing if certain abiotic stressors, specifically harsh, more saline soils, can predict the frequency of pink flower color for *L. minimus* at the population level. We will be using iNaturalist observations taken across the northwestern coast of USA and Canada to identify the color morphs of *L. minimus* and define different populations. We will also use distance to shoreline as a proxy for soil salinity.

Sources: 
<sup>1</sup>Mekapogu *et al.* (2025). Anthocyanins in Floral Colors: Biosynthesis and Regulation in Chrysanthemum Flowers. (https://doi.org/10.3390/ijms21186537)[https://doi.org/10.3390/ijms21186537]

<sup>2</sup>Grossenbacher *et al.* (2025).Soil and climate contribute to maintenance of a flower color polymorphism.[https://doi.org/10.1002/ajb2.70018](https://doi.org/10.1002/ajb2.70018)

### Research Question

> Are pink flowers of *Leptosiphon minimus* with elevated anthocyanins more likely to occur closer to the shoreline?

### Hypothesis

> Soils closer to the shoreline have higher salinity, exposing *Leptosiphon minimus* to higher levels of stress, resulting in higher expression of anthocyanins (i.e., pink pigment).

### Target Audience

### Metadata

------------------------------------------------------------------------

## Set up Working Directory and Install/Library Packages

```{r}
#install.packages("dbscan")
#install.packages("sf")
#install.packages(("ggspatial"))
#install.packages("rnaturalearthdata")
#install.packages("scatterpie")
#install.packages("rnaturalearth")
#install.packages("sf")

library(scatterpie)
library(dbscan)
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggspatial)
library(rnaturalearth)
```

\

------------------------------------------------------------------------

# *Leptosiphon minimus*

------------------------------------------------------------------------

## Read in CSV from iNat (*Leptosiphon minimus*)

```{r}
#Read in CSV
LM <- read.csv("data/LMini_iNat.csv")

#Check out names of columns
names(LM)
```

------------------------------------------------------------------------

## Convert from Degrees to Meters, set up Pops

```{r}
#Convert to sf object
LM_sf <- st_as_sf(LM, coords = c("longitude", "latitude"), crs = 4326)

#Project to meters
LM_utm <- st_transform(LM_sf, 32611)  # change UTM zone if needed
LM_coords <- st_coordinates(LM_utm)
```

------------------------------------------------------------------------

## Run dbscan and add Pops column to CSV

```{r}
#Run DBSCAN, 1000 m radius populations with 4 or more observations per pop
db <- dbscan(LM_coords, eps = 1000, minPts = 4)  

#Add population column
LM$population_id <- db$cluster

```

------------------------------------------------------------------------

## Check if it worked

```{r}
table(LM$population_id)
sum(LM$population_id == 0)

```

------------------------------------------------------------------------

## Save CSV

```{r}
write.csv(LM, "data/LMini_iNat_Pops.csv", row.names = FALSE)
```

------------------------------------------------------------------------

## Visualize data

### NOTE: All observations beloning to population '0' are singelton observations, that did not meet our population criteria (4 obs within 1000 m)

```{r}
#List pop sizes
LM_pop_sizes <- LM %>%
  count(population_id, name = "n_obs") %>%
  arrange(desc(n_obs))

LM_pop_sizes

#Bar plot of pop sizes
ggplot(LM_pop_sizes, aes(x = factor(population_id), y = n_obs)) +
  geom_col() +
  labs(x = "Population ID",
       y = "Number of Observations",
       title = "L. minimus Observation Counts per Population") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 5))


```

------------------------------------------------------------------------

## Visualize pops on a map

```{r}
# Get world basemap
world <- ne_countries(scale = "medium", returnclass = "sf")

# Bounding box around our data
bbox <- c(
  xmin = min(LM$longitude) - 0.5,
  xmax = max(LM$longitude) + 0.5,
  ymin = min(LM$latitude) - 0.5,
  ymax = max(LM$latitude) + 0.5
)

# Plot
ggplot() +
  geom_sf(data = world, fill = "gray95", color = "gray80") +
  geom_point(data = LM,
             aes(x = longitude, y = latitude,
                 color = factor(population_id)),
             size = 2, alpha = 0.85) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"]),
           expand = FALSE) +
  labs(color = "Population") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 5))

```

## Lets remove noise from 'pop 0' singelton observation

```{r}
# Remove noise points (population_id == 0)
LM_clean <- LM %>%
  filter(population_id != 0)

# Quick check
table(LM_clean$population_id)

# Bounding box around our data
bbox <- c(
  xmin = min(LM_clean$longitude) - 0.5,
  xmax = max(LM_clean$longitude) + 0.5,
  ymin = min(LM_clean$latitude) - 0.5,
  ymax = max(LM_clean$latitude) + 0.5
)

#Plot cleaned data
ggplot() +
  geom_sf(data = world, fill = "gray95", color = "gray80") +
  geom_point(data = LM_clean,
             aes(x = longitude, y = latitude,
                 color = factor(population_id)),
             size = 2, alpha = 0.85) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"]),
           expand = FALSE) +
  labs(color = "Population") +
  theme_minimal() +
   theme(axis.text.x = element_text(size = 5))

```

------------------------------------------------------------------------

## Finish and Save

```{r}
# Save to new CSV
write.csv(LM_clean, "data/LMini_iNat_Pops_filtered.csv", row.names = FALSE)
```

------------------------------------------------------------------------

# Curating Data

------------------------------------------------------------------------

### Now that we have our data set, we need to mark iNat observations based on flower color

Lets make a copy of LMini_iNat_Pops_filtered.csv called LMini_iNat_Pops_filtered_curated.csv, and add some new columns: **polymorphic** and **color**

```{r}
## add code to do that here
```

Next, we will go through each iNat observation in our LMini_iNat_Pops_filtered_metadata.csv by clicking the hyperlink in each respective row. Then we will note the flower color in the observation's photo, choosing **pink** or **white**, and writing that down in the cell of the color column for each respective row. However, if the observation has no flowers or all flowers are still closed, we will instead write **closed** in that cell instead. Additionally, we will mark whether the observation show more than one flower color in the polymorphic column. This will be a binary variable, with **polymorphic observations receiving a 1** and **monomorphic observations receiving a 0.**

Once the **polymorphic** and **color** columns are filled out we can move on to the next step. Make sure to save it as a CSV after editing.\

------------------------------------------------------------------------

## Filtering out closed flowers

```{r}
#Read in curated CSV 
LMC <- read.csv("data/LMini_iNat_Pops_filtered_curated.csv")

LMCF <- LMC %>%
  filter(!(color1 == "closed" & color2 == "closed"))
```

```{r}
#Combine remaining color columns into one column and remove empty rows
LMCF_clean <- LMCF %>%
  pivot_longer(
    cols = starts_with("color"),
    values_to = "color",
    names_to = "slot"
  ) %>%
  filter(
    !is.na(color),
    color != "",
    color != "closed"
  )
```

------------------------------------------------------------------------

## Group colors by population

```{r}
#Grouping colors by population for pie charts
pop_pies <- LMCF_clean %>%
  group_by(population_id) %>%
  summarise(
    latitude  = mean(latitude),
    longitude = mean(longitude),
    white = sum(color == "white"),
    pink  = sum(color == "pink"),
    total_n = n(),
    .groups = "drop"
  )
```

------------------------------------------------------------------------

## Mapping populations as pie charts

```{r}
# World basemap
world <- ne_countries(scale = "medium", returnclass = "sf")

# Bounding box from population-level data
bbox <- c(
  xmin = min(pop_pies$longitude) - 0.5,
  xmax = max(pop_pies$longitude) + 0.5,
  ymin = min(pop_pies$latitude) - 0.5,
  ymax = max(pop_pies$latitude) + 0.5
)

# Plot
ggplot() +
  geom_sf(data = world, fill = "gray95", color = "gray80") +
  geom_scatterpie(
    data = pop_pies,
    aes(x = longitude, y = latitude),
    cols = c("white", "pink"),
    pie_scale = 1.5,
    color = "black"
  ) +
  coord_sf(
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"], bbox["ymax"]),
    expand = FALSE
  ) +
  scale_fill_manual(
    values = c(white = "white", pink = "#d81b60"),
    name = "Flower color"
  ) +
  theme_minimal()
```

\
