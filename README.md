# Background
This repository is dedicated to the 2026 BIOE 276 group project by Eli Allen, Anik Grearson, and Mikaela Salvador.

## Introduction 
Anthocyanins are a group of pigments that contribute to petal color intensity across many species of flowers.<sup>1</sup> These pigments also are important for providing tolerance to abiotic stressors, such as high UV radiation, extreme soil conditions, and low precipitation.<sup>2</sup>

*Leptosiphon minimus* is a native plant species that occurs across the northwestern coast of North America in California, Oregon, Washington, and British Columbia. *L. minimus* is known for being polymorphic, exhibiting two petal colors, pink and white. Within these color morphs, elevated levels of anthocyanins produce a pink pigment. 

In our project, we are interested in assessing if certain abiotic stressors, specifically harsh, more saline soils, can predict the frequency of pink flowers for *L. minimus* at the population level. We will be using iNaturalist observations taken across the northwestern coast of USA and Canada to identify the pink vs. white color morphs of *L. minimus* and define different populations. We will also use distance to shoreline as a proxy for soil salinity.

Sources: 

<sup>1</sup>Mekapogu *et al.* (2025). Anthocyanins in Floral Colors: Biosynthesis and Regulation in Chrysanthemum Flowers. *International Journal of Molecular Sciences*, *21*(18), 6537. [https://doi.org/10.3390/ijms21186537](https://doi.org/10.3390/ijms21186537)

<sup>2</sup>Grossenbacher *et al.* (2025). Soil and climate contribute to maintenance of a flower color polymorphism. *American Journal of Botany*, e70018. [https://doi.org/10.1002/ajb2.70018](https://doi.org/10.1002/ajb2.70018)

## Research Question

> Are pink flowers of *Leptosiphon minimus* with elevated anthocyanins more likely to occur closer to the shoreline?

## Hypothesis

> Soils closer to the shoreline have higher salinity, exposing *Leptosiphon minimus* to higher levels of abiotic stress, which will result in elevated levels of anthocyanins (i.e., pink pigment).

## Target Audience

Our target audience for this study is for other scientists within the field of Ecology and Evolutionary Biology, specifically botanists, plant ecologists, and evolutionary biologists. 

## Repositories
We have two sub-repositories: ```data``` and ```output```. 

```data``` contains the following files: 
- ```LMini_iNat.csv```: all raw, unfiltered iNaturalist observations of *Leptosiphon minimus*
- ```LMini_iNat_Pops.csv```: all iNaturalist observations of *L. minimus* grouped by populations 0 - 6 
- ```LMini_iNat_Pops_filtered.csv```: iNaturalist observations of *L. minimus* grouped by populations 1 - 6, removing population 0 for a singleton observation
- ```LMini_iNat_Pops_filtered_curated.csv```: iNaturalist observations of *L. minimus* grouped by populations 1 - 6, with blank columns donating if an observation is ```polymorphic```, their primary color ```color1```, and their secondary color if applicable ```color2```
- ```LMini_iNat_Pops_filtered_curated_color.csv```: iNaturalist observations of *L. minimus* grouped by populations 1 - 6 with filled in columns for ```polymorphic```, ```color1```, and ```color2```
- ```LMini_iNat_Pops_filtered_curated_metadata.xlsx```: metadata for each of our iNaturalist observations
- ```LMCF_clean_dist.csv```: distance to shore for each of the observations in populations 1-6, excluding population 0 and any closed observations 
- ```West```: West region shape file from NOAA used to calculate distance to shore

```output``` contains our model output for testing if pink flower color is more likely closer to shore: m.dist.color.pop

## Metadata

Our metadata ```LMini_iNat_Pops_filtered_curated_metadata``` is produced after step 2 (Curating Data) and contains the following columns:

- scientific name for the species observed (i.e., *Leptosiphon minimus*)
- url and hyperlink columns linking to the iNaturalist observation
- latitude and longitude coordinates for the observation
- population identified (1, 2, 3, 4, or 5)
- presence of polymorphism (0 for monomorphic, 1 for polymorphic, closed for unidentifiable)
- flower color (pink or white)

------------------------------------------------------------------------

# Data Curation, Wrangling, and Exploratory Visualization

In the following sections, we will wrangle our data to generate our metadata sheet ```LMini_iNat_Pops_filtered_curated_metadata.xlsx``` and our final filtered datasheet ```LMCF_clean_dist.csv``` that contains individual iNaturalist observations of *L. minimus* grouped by populations, identified flower color (pink or white), and distance to shoreline. We will also produce an exploratory map, showing the proportion of pink to white flowers for each defined population as a pie chart on a map. 

<details><summary>0. Setup of R Packages</summary>
<p>

## Install and load necessary R Packages

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
</p>
</details>

------------------------------------------------------------------------
<details><summary>1. Data Wrangling and Visualization to Define *Leptosiphon minimus* Populations</summary>
<p> 

In this section, we will use the dbscan package to define populations of *L.minimus* from iNaturalist observations. We will define populations as groups of 4 or more observations within a 1000 m radius. 

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

We will use ggplot to make a bar plot of population sizes.

### NOTE: All observations belonging to population '0' are singleton observations, that did not meet our population criteria (4 observations within 1000 m)

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
We will use the sf package to visualize the proportion of pink to white flowers as pie charts on a map of our populations. 

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

## Let's remove noise from 'pop 0' singleton observation

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

</p>
</details>

------------------------------------------------------------------------

<details><summary>2. Curating Data to Identify Flower Color of *Leptosiphon minimus* Populations</summary>
<p> 

# Curating Data

Now that we have our data set, we need to mark our iNaturalist observations based on flower color.

## Let's make a copy of LMini_iNat_Pops_filtered.csv called LMini_iNat_Pops_filtered_curated.csv, and add some new columns: **polymorphic** and **color**

```{r}
# Read in the csv file from Step 1
LMini_pops_fil <- read.csv("LMini_iNat_Pops_filtered.csv", header = TRUE)

# Let's create new blank columns called polymorphic and color
# Set it to NA for numerical/logical data
LMini_pops_fil$polymorphic <- NA
LMini_pops_fil$color <- NA

# Save the CSV
write.csv(LMini_pops_fil, "LMini_iNat_Pops_filtered_curated.csv", row.names = FALSE)
```
------------------------------------------------------------------------

## Identify color and degree of polymorphism for each iNaturalist Observation

Next, we will go through each iNaturalist observation in our LMini_iNat_Pops_filtered_curated.csv by clicking the hyperlink in each respective row.

Then, we will note the flower color in the observation's photo, choosing **pink** or **white**, and writing that down in the cell of the color column for each respective row. However, if the observation has no flowers or all flowers are still closed, we will instead write **closed** in that cell instead. Additionally, we will mark whether the observation show more than one flower color in the polymorphic column. This will be a binary variable, with **polymorphic observations receiving a 1** and **monomorphic observations receiving a 0.**

Once the **polymorphic** and **color** columns are filled out we can move on to the next step. Make sure to save it to the same CSV ```LMini_iNat_Pops_filtered_curated.csv```after editing.

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
</p>
</details>

<details><summary>3. Generating Shoreline Distance for Individual iNaturalist Observations </summary>
<p>

In order to calculate the distance of each observation from the water (i.e., to the closest shore), we will compute the minimum distance from each observation (point) to the shoreline using vectors from the NOAA National Shoreline CUSP (West region) dataset. We downloaded the West region shape file from the NOAA portal (<https://nsde.ngs.noaa.gov/>), and imported this into R. 

**Note: This only works for observations in the US, so we had to manually calculate the distance from shore for the Canadian observations (population 1) using the app Avenza. For each of the Canadian observations, we measured the minimum distance from shore using the Draw and Measure Tool.**

```{r}

# Import shape file of Western US shoreline
shoreline <- st_read("data/West/West.shp")

# To view the imported shape file
tmap_mode("view")  # interactive mode
tm_shape(shoreline) + tm_lines()

# Import coordinate data for observations
points_sf <- st_as_sf(LMCF_clean,
                      coords = c("longitude", "latitude"),
                      crs = 4326)

# Project to a suitable projection
shoreline_proj <- st_transform(shoreline, 32610) # UTM 10N
points_proj    <- st_transform(points_sf, 32610)

# Compute minimum distances to shore - add new column called dist_m
LMCF_clean$dist_m <- apply(st_distance(points_proj, shoreline_proj), 1, min)

# Manually overwrite data for observations in Canada (Population 1)
LMCF_clean$dist_m[6] <- 200 # Observation 121793064
LMCF_clean$dist_m[7] <- 210 # Observation 121793096
LMCF_clean$dist_m[8] <- 190 # Observation 121793133

# Change pink to 1 and white to 0, add a new column called color_binom
LMCF_clean <- LMCF_clean %>%
  mutate(color_binom = case_when(
    color == "pink"  ~ 1,
    color == "white" ~ 0,
TRUE ~ NA_real_
  ))

# Write to csv
write.csv(LMCF_clean,"data/LMCF_clean_dist.csv", row.names = FALSE)
```
  
</p>
</details>

------------------------------------------------------------------------

# Modeling our Data

Since we are interested in if shoreline distance predicts the outcome of flower color (pink or white), we will be using a Bernoulli distribution to model our data. We will use individual populations (i.e., 1, 2, 3, 4, 5, 6) as a random effect since it will contribute to clustering in our data. We will be using the brm() within the ```brms``` R package for our model. 

<details><summary>0. Making a ggplot for Our Potential Model </summary>
<p> 

```{r}
# Change color to a factor - will default 
#LCMF_clean$color <- as.factor(LCMF_clean$color$)

# Plot 
ggplot(data=LMCF_clean, aes(x=dist_m, y=color)) +
  geom_point() +
  theme_bw()

```
</p>
</details>

<details><summary>1.  Running and Assessing the Model using brms() </summary>
<p> 

```
# Run the model
m.dist.color.pop <-
  brm(data = LMCF_clean, # Give the model the data
      # Use a binomial distribution
      family = bernoulli(link = "logit"),
      # Specify the model 
      color_binom ~ 1 + dist_m + (1|population_id),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      file = "output/m.dist.color.pop")

print(m.dist.color.pop, digits = 3)
plot(m.dist.color.pop)
```
</p>
</details>

<details><summary>2.  Estimate and visualize the probability that pink increases closer to shore </summary>
<p> 

```{r}
#  Extract all posteriors from 4 MCMC chains into a table
draws <- as_draws_df(m.dist.color.pop) 

# What proportion of posteriors have estimate for slope > 0?
sum(draws$b_dist_m>0)/length(draws$b_dist_m) 

# Plot predicted response
preds <- predict_response(m.dist.color.pop)
plot(preds)
```
</p>
</details>
