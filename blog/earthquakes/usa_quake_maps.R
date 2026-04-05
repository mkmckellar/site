
# load libraries and data -------------------------------------------------

library(pacman)
p_load(dplyr, ggplot2, plotly, sf)

usa_quakes <- readr::read_csv("blog/earthquakes/all_quakes2025.csv")

glimpse(usa_quakes)


# add geographical information --------------------------------------------

# add geographical information to usa_quakes
# shape files, nearest USA state

## add shape information to usa_quakes ----
# this code should add a geometry column
# the geometry type added is POINT
usa_quakes <- st_as_sf(
  usa_quakes,
  coords = c("longitude", "latitude"),
  # crs 4326 = WGS 84 -- very widely used ref system
  crs = 4326, 
  remove = FALSE
)

glimpse(usa_quakes) # geometry column added to df

## get state and territory map shape info ----
# load USA polygons
# download shapefile for all states
states_sf <- tigris::states(cb = TRUE) %>%
  sf::st_transform(4326) %>% # transform or covert coordinates of simple feature
  select(state = NAME)

# note the geometry column, includes territories
# the geometry column is a multipolygon, this should be the outline of the state or territory
# territories will not be included in 
glimpse(states_sf) 


## spatial join and drop geometry ----
# st_join == spatial join
# st_within == geometric binary predicates on pairs of simple geometry sets
usa_quakes <- sf::st_join(usa_quakes, states_sf, join = st_within)

glimpse(usa_quakes) # joined on geo column, new state column added

usa_quakes <- usa_quakes |> 
  relocate(state, .after = place)


# data assessment prior to mapping ----------------------------------------
# additional cleaning and wrangling may be required

## add missing state information ----
# check for missing info and states with earthquakes recorded
sum(is.na(usa_quakes$state)) # 5931 rows without states
length(unique(usa_quakes$state)) # 37 states represented

table(usa_quakes$state)

# let's look at some rows without states
# some quakes are near california islands
# others are off the coast of british columbia or Mexico's baja ca
usa_quakes |> 
  dplyr::filter(is.na(state)) |> 
  select(place, state) |> 
  head(10)

# identify points with no state
na_points <- usa_quakes %>% filter(is.na(state))
glimpse(na_points) # you can use this to demonstrate earthquakes that are
# off the coast or just across the border in MX or Canada.

# find nearest state for each NA point
nearest_state_idx <- st_nearest_feature(na_points, states_sf)

usa_quakes_complete <- usa_quakes

# assign nearest state name
# adding the nearest state because quakes can be felt across borders
usa_quakes_complete$state[is.na(usa_quakes_complete$state)] <- states_sf$state[nearest_state_idx]

sum(is.na(usa_quakes$state)) # 5931 without state
sum(is.na(usa_quakes_complete$state))

# peak at na state entries
usa_quakes |> 
  select(place, state) |> 
  dplyr::filter(is.na(state)) |> 
  head(10)

length(unique(usa_quakes$state)) # 37 states represented
table(usa_quakes$state)

## add US census region information
# add US census regions to usa_quakes
usa_quakes <- usa_quakes |> 
  mutate(region = case_when(state %in% c("Connecticut", "Maine",
                                         "Massachusetts", "New Hampshire",
                                         "Rhode Island", "Vermont") ~ "New England",
                            state %in% c("New Jersey", "New York", "Pennsylvania") ~ "Middle Atlantic",
                            state %in% c("Illinois", "Indiana",
                                         "Michigan", "Ohio",
                                         "Wisconsin") ~ "East North Central",
                            state %in% c("Iowa", "Kansas",
                                         "Minnesota", "Missouri",
                                         "Nebraska", "North Dakota",
                                         "South Dakota") ~ "West North Central",
                            state %in% c("Delaware", "District of Columbia",
                                         "Florida", "Georgia",
                                         "Maryland", "North Carolina",
                                         "South Carolina", "Virginia",
                                         "West Virginia") ~ "South Atlantic",
                            state %in% c("Alabama", "Kentucky",
                                         "Mississippi", "Tennessee") ~ "East South Central",
                            state %in% c("Arkansas", "Louisiana",
                                         "Oklahoma", "Texas") ~ "West South Central",
                            state %in% c("Arizona", "Colorado", 
                                         "Idaho", "Montana", 
                                         "Nevada", "New Mexico",
                                         "Utah", "Wyoming") ~ "Mountain",
                            state %in% c("Alaska", "California",
                                         "Hawaii", "Oregon",
                                         "Washington") ~ "Pacific")
  )

# add US census regions to states shape file
states_sf <- states_sf |> 
  mutate(region = case_when(state %in% c("Connecticut", "Maine",
                                         "Massachusetts", "New Hampshire",
                                         "Rhode Island", "Vermont") ~ "New England",
                            state %in% c("New Jersey", "New York", "Pennsylvania") ~ "Middle Atlantic",
                            state %in% c("Illinois", "Indiana",
                                         "Michigan", "Ohio",
                                         "Wisconsin") ~ "East North Central",
                            state %in% c("Iowa", "Kansas",
                                         "Minnesota", "Missouri",
                                         "Nebraska", "North Dakota",
                                         "South Dakota") ~ "West North Central",
                            state %in% c("Delaware", "District of Columbia",
                                         "Florida", "Georgia",
                                         "Maryland", "North Carolina",
                                         "South Carolina", "Virginia",
                                         "West Virginia") ~ "South Atlantic",
                            state %in% c("Alabama", "Kentucky",
                                         "Mississippi", "Tennessee") ~ "East South Central",
                            state %in% c("Arkansas", "Louisiana",
                                         "Oklahoma", "Texas") ~ "West South Central",
                            state %in% c("Arizona", "Colorado", 
                                         "Idaho", "Montana", 
                                         "Nevada", "New Mexico",
                                         "Utah", "Wyoming") ~ "Mountain",
                            state %in% c("Alaska", "California",
                                         "Hawaii", "Oregon",
                                         "Washington") ~ "Pacific")
  )

## gather information about number of earthquakes by state and region
quake_count <- usa_quakes |> 
  group_by(state, region) |> 
  summarise(n = n(),
            smallest_quake = min(mag),
            largest_quake = max(mag)) |> 
  ungroup() |> 
  tidyr::drop_na()

glimpse(quake_count)


# mapping earthquakes -----------------------------------------------------

## plotly ----

# this plot should color the states by US census region
# when a user hoovers their mouse over a state there should be a pop up
# text box that contains the census region, state name, number of quakes
# smallest magnitude and largest magnitude

# join quake count with states_sf
# keep all states without a an earthquake present
# remove territories -- not present in quake data set
# need to add magnitude column that's character and replace NA with none
# otherwise the hoover text won't render correctly?
quake_count <- st_join(states_sf, quake_count) |> 
  select(-state.y, -region.y) |> 
  tidyr::drop_na(region.x) |> 
  tidyr::replace_na(list(n = 0)) |> 
  rename(state = state.x, region = region.x) |> 
  dplyr::mutate(smallest_mag = as.character(smallest_quake) |> tidyr::replace_na("None"),
                largest_mag = as.character(largest_quake) |> tidyr::replace_na("None")
  ) |> 
  dplyr::mutate(hover_text = paste(region, state, n, smallest_mag, largest_mag, sep = "<br />"))

# shift hawaii and alaska
quake_count <- tigris::shift_geometry(quake_count)

region_colors <- viridisLite::viridis(9, option = "D")

fig <- plot_ly(
  data = quake_count,
  split = ~state,
  color = ~region,
  colors = region_colors,
  text = ~paste(region, state, paste("Number of quakes", n, sep = ": "),
                paste("Smallest Magnitude", smallest_mag, sep = ": "),
                paste("Largest Magnitude", largest_mag, sep = ": "),
                sep = "<br />"),
  hoverinfo = "text",
  stroke = I("white"),
  span = I(1)
) |> 
  layout(title = "US Earthquakes by State (2025)",
         showlegend = FALSE)

fig

## claude recommendation ----
# --- Build the plotting data ---
# Start from states_sf so ALL states are included (even those with no quakes)
# Drop geometry from quake_count first — states_sf provides the geometry
quake_count_plot <- st_join(
  states_sf,
  quake_count |> sf::st_drop_geometry()  # 👈 prevents geometry collision
) |>
  select(-state.y, -region.y) |>
  rename(state = state.x, region = region.x) |>
  tidyr::drop_na(region) |>          # drop territories (no census region assigned)
  tidyr::replace_na(list(n = 0)) |>  # states with no quakes get count of 0
  dplyr::mutate(                     # 👈 dplyr::mutate, not plotly::mutate
    smallest_mag = as.character(smallest_quake) |> tidyr::replace_na("None"),
    largest_mag  = as.character(largest_quake)  |> tidyr::replace_na("None"),
    # Build hover text as a proper column
    hover_text = paste0(
      "<b>", state, "</b>",
      "<br>Region: ", region,
      "<br>Earthquakes: ", n,
      "<br>Smallest magnitude: ", smallest_mag,
      "<br>Largest magnitude: ", largest_mag
    )
  )

## MY EDITS ##
# Drop geometry from quake_count first — states_sf provides the geometry
quake_count_plot <- st_join(
  states_sf,
  sf::st_drop_geometry(quake_count)  # 👈 prevents geometry collision
)

#error  
quake_count2 <- sf::st_drop_geometry(quake_count)
#error
quake_count3 <- st_join(quake_count2, states_sf)

# no error
join_test <- st_join(states_sf, quake_count)
join_test_swap <- st_join(quake_count, states_sf)

join_test2 <- states_sf |>
  left_join(
    quake_count |> sf::st_drop_geometry(),
    by = c("state", "region")
  )
  
    
quake_count_plot <- join_test2 |> 
  #select(-state.y, -region.y) |>
  #rename(state = state.x, region = region.x) |>
  tidyr::drop_na(region) |>          # drop territories (no census region assigned)
  tidyr::replace_na(list(n = 0)) |>  # states with no quakes get count of 0
  dplyr::mutate(                     # 👈 dplyr::mutate, not plotly::mutate
    smallest_mag = as.character(smallest_quake) |> tidyr::replace_na("None"),
    largest_mag  = as.character(largest_quake)  |> tidyr::replace_na("None"),
    # Build hover text as a proper column
    hover_text = paste0(
      "<b>", state, "</b>",
      "<br>Region: ", region,
      "<br>Earthquakes: ", n,
      "<br>Smallest magnitude: ", smallest_mag,
      "<br>Largest magnitude: ", largest_mag
    )
  )

# Shift Alaska and Hawaii to sit below the continental US
quake_count_plot <- tigris::shift_geometry(quake_count_plot)

# --- Define colors ---
# 9 census regions = 9 colors
region_colors <- viridisLite::viridis(9, option = "D")

# --- Build the plotly map ---
fig <- plot_ly(
  data = quake_count_plot,
  split = ~state,       # one polygon per state
  color = ~region,      # color by census region
  colors = region_colors,
  text = ~hover_text,   # 👈 reference the column we built
  hoverinfo = "text",   # only show our custom text, nothing auto-generated
  stroke = I("white"),  # white state borders
  span = I(1)
) |>
  layout(
    title = list(
      text = "US Earthquakes by State (2025)",
      font = list(size = 16)
    ),
    showlegend = FALSE,
    legend = NULL,
    # Remove the default lat/lon axis labels plotly adds
    xaxis = list(visible = FALSE),
    yaxis = list(visible = FALSE)
  )

fig

# alternative with customdata hoover text
fig <- plot_ly(
  data = quake_count_plot,
  split = ~state,
  color = ~region,
  colors = region_colors,
  customdata = ~hover_text,      # 👈 pass hover text through customdata
  hovertemplate = "%{customdata}<extra></extra>",  # 👈 display it exactly as built
  stroke = I("white"),
  span = I(1)
) |>
  layout(
    title = list(
      text = "US Earthquakes by State (2025)",
      font = list(size = 16)
    ),
    showlegend = FALSE,
    #legend = list(title = list(text = "<b>Census Region</b>")),
    xaxis = list(visible = FALSE),
    yaxis = list(visible = FALSE)
  )


fig


### claude attempt 2 ----
# State name to abbreviation lookup
# needed because plotly's choropleth uses abbreviations, not full names
state_abbrev <- data.frame(
  state = state.name,        # built-in R vector of state names
  abbrev = state.abb         # built-in R vector of state abbreviations
)

# Join abbreviations onto your plotting data
quake_count_choropleth <- quake_count_plot |>
  sf::st_drop_geometry() |>        # plotly choropleth doesn't need sf geometry
  left_join(state_abbrev, by = "state")

glimpse(quake_count_choropleth)

# Build the choropleth
fig <- plot_ly(
  data = quake_count_choropleth,
  type = "choropleth",
  locations = ~abbrev,             # state abbreviations
  locationmode = "USA-states",     # tells plotly to use US state boundaries
  color = ~region,
  colors = region_colors,
  text = ~hover_text,
  hoverinfo = "text"
) |>
  layout(
    title = "US Earthquakes by State (2025)",
    geo = list(scope = "usa"),     # constrains map to USA including AK and HI
    showlegend = TRUE
  )

fig

### claude attempt 3 ---
fig <- plot_ly(
  data = quake_count_choropleth,
  type = "choropleth",
  locations = ~abbrev,
  locationmode = "USA-states",
  z = ~n,                        # 👈 continuous variable drives the color
  colorscale = "Viridis",        # consistent with your earlier maps
  reversescale = TRUE,           # darker = fewer quakes, brighter = more
  text = ~hover_text,
  hoverinfo = "text",
  colorbar = list(
    title = "# Earthquakes",
    x = 0.9,      # horizontal position (0 = far left, 1 = far right)
    y = 0.5,       # vertical position (0 = bottom, 1 = top)
    len = 0.5      # length of the colorbar as fraction of plot height
  )
) |>
  layout(
    title = "US Earthquakes by State (2025)",
    geo = list(scope = "usa"),
    showlegend = FALSE
  )

fig

# add quake number bins
quake_count_choropleth <- quake_count_choropleth |>
  dplyr::mutate(
    quake_bin = dplyr::case_when(
      n == 0          ~ "0",
      n >= 1  & n <= 10  ~ "1-10",
      n >= 11 & n <= 100 ~ "11-100",
      n > 100            ~ "100+"
    ),
    # Make it an ordered factor so the legend displays in logical order
    quake_bin = factor(quake_bin, 
                       levels = c("0", "1-10", "11-100", "100+"))
  )


# alternative
# add quake number bins
quake_count_choropleth <- quake_count_choropleth |>
  dplyr::mutate(
    quake_bin = dplyr::case_when(
      n == 0          ~ "0",
      n >= 1  & n <= 10  ~ "1-10",
      n >= 11 & n <= 100 ~ "11-100",
      n > 100            ~ "100+"
    ),
    # Make it an ordered factor so the legend displays in logical order
    quake_bin = factor(quake_bin, 
                       levels = c("0", "1-10", "11-100", "100+"))
  )

table(quake_count_choropleth$quake_bin)

quake_count_choropleth <- quake_count_choropleth |>
  dplyr::mutate(
    quake_bin = dplyr::case_when(
      n == 0           ~ "None",
      n >= 1  & n <= 5  ~ "1–5",
      n >= 6  & n <= 25 ~ "6–25",
      n >= 26 & n <= 100 ~ "26-100",
      n >= 101 ~ "101+"
    ),
    quake_bin = factor(quake_bin,
                       levels = c("None", "1–5", "6–25", "26-100", "101+"))
  )

table(quake_count_choropleth$quake_bin)


# assign numeric codes to color scale
quake_count_choropleth <- quake_count_choropleth |>
  dplyr::mutate(
    bin_code = as.integer(quake_bin)  # 1, 2, 3, or 4 — one per bin
  )

table(quake_count_choropleth$bin_code)

# build discrete color scale
# The repeated position values are intentional 
# each color starts and ends at the same boundary, 
# creating a hard edge rather than a gradient. 
# This is the standard plotly pattern for discrete color scales.
discrete_colors <- list(
  list(0,    "#d0e8f1"),   # bin 1: 0 quakes — light blue
  list(0.25, "#d0e8f1"),   # 
  list(0.25, "#79b4a9"),   # bin 2: 1-10 — teal
  list(0.5,  "#79b4a9"),   #
  list(0.5,  "#e07b39"),   # bin 3: 11-100 — orange
  list(0.75, "#e07b39"),   #
  list(0.75, "#8B0000"),   # bin 4: 100+ — dark red
  list(1.0,  "#8B0000")    #
)

# update color bar to show bin labels
colorbar = list(
  title = "No. Earthquakes",
  tickvals = c(1, 2, 3, 4),              # match bin_code values
  ticktext = c("0", "1-10", "11-100", "100+"),  # display bin labels
  x = 0.9,
  len = 0.4
)

discrete_colors5 <- list(
  list(0,    "#d0e8f1"),   # bin 1: 0 quakes — light blue
  list(0.2, "#d0e8f1"),   # 
  list(0.2, "#79b4a9"),   # bin 2: 1-5 — teal
  list(0.4,  "#79b4a9"),   #
  list(0.4,  "#e07b39"),   # bin 3: 6-25 — orange
  list(0.6, "#e07b39"),   #
  list(0.6, "#8B0000"),   # bin 4: 25-100 — dark red
  list(0.8,  "#8B0000"),    #
  list(0.8, "#4e1609"),     # bin 5: 101+ - dark brown "french puce"
  list(1.0, "#4e1609")
)

# update color bar to show bin labels
colorbar = list(
  title = "No. Earthquakes",
  tickvals = c(1, 2, 3, 4, 5),              # match bin_code values
  ticktext = c("0", "1-10", "11-100", "100+"),  # display bin labels
  x = 0.9,
  len = 0.4
)



# check after bin column creation
table(quake_count_choropleth$quake_bin, useNA = "always")


fig <- plot_ly(
  data = quake_count_choropleth,
  type = "choropleth",
  locations = ~abbrev,
  locationmode = "USA-states",
  z = ~bin_code,
  colorscale = discrete_colors5,
  zmin = 1, zmax = 5,        # 👈 anchor the scale to your bin codes
  text = ~hover_text,
  hoverinfo = "text",
  colorbar = list(
    title = "No. Quakes",
    tickvals = c(1, 2, 3, 4, 5),
    ticktext = c("None", "1-5", "6-25", "25-100", "101+"),
    x = 0.85,
    y = 0.45,
    len = 0.4
  )
) |>
  layout(
    title = list(
      text = "US Earthquakes by State (2025)",
      x = 0.5,       # horizontal position: 0 = left, 0.5 = center, 1 = right
      y = 0.9,      # vertical position: 0 = bottom, 1 = top
      xanchor = "center",   # anchor point of the title text itself
      yanchor = "top",
      font = list(size = 16)
    ),
    geo = list(scope = "usa"),
    showlegend = FALSE
  )

fig

## hexagon choropleth map ----

# a simple map that uses color to indicate number of quakes
# in each state in 2025. The darker the color, the more quakes.
# not a bad map, but not really want I want for this data set
# it's also inaccurate because the quake_count data set does not 
# include any information about alaska or hawaii

p_load(geogrid, choroplethr, choroplethrMaps)

state_choropleth(quake_count, 
                 geoid.name = "state",
                 value.name = "n",
                 title  = "US Quakes", 
                 legend = "Quakes Number",
                 legend_position = "bottom")

## usamaps package ----
library(usmap)

# usmap() won't work for some reason if the geometry col is present
quake_count2 <- sf::st_drop_geometry(quake_count)

quake_count2$hover <- with(quake_count2, paste(state, '<br>', region, "<br>",
                                               "Number of quakes", n, "<br>", 
                                               "Smallest magnitude", smallest_mag, "<br>",
                                               "Largest magnitude", largest_mag))

# i like this package because it's easy to focus on specific
# US census regions. It might be good to use if I can layer
# earthquake coordinates on top of it to show where the epicenter of the
# earthquakes are located. 

plot_usmap(data = quake_count2,
           regions = "states",
           values = "n",
           labels = TRUE,
           label_color = "white",
           include = .mountain)

# looking better!
plot_usmap(data = quake_count2,
           regions = "states",
           values = "region",
           labels = TRUE,
           label_color = "white",
           include = .mountain) +
  theme(legend.position = "right",
        panel.background = element_rect(color = "black", fill = "lightblue")) +
  labs(title = "Earthquakes detected within and near\nthe Mountain Region (2025)") +
  geom_sf(data = usa_quakes |> 
            dplyr::filter(state %in% c("Nevada", "Idaho", "Montana",
                                       "Wyoming", "Utah", "Colorado",
                                       "Arizona", "New Mexico")),
          aes(size = mag, color = mag),
          alpha = 0.4)


# USE THIS -- do mountain region vs northwest central
plot_usmap(data = quake_count_choropleth,
           regions = "states",
           values = "region",
           labels = TRUE,
           label_color = "white",
           include = c(.mountain, .west_north_central)) +
  scale_fill_manual(values = c("#A3b18a", "#d2a39a")) +
  theme(legend.position = "right",
        panel.background = element_rect(color = "black", fill = "#f5f0e8")) +
  labs(title = "Earthquakes detected within the Mountain and West North Central Regions (2025)") +
  geom_sf(data = usa_quakes |> 
            dplyr::filter(region %in% c("Mountain", "West North Central")),
          aes(size = mag, color = mag),
          alpha = 0.7) +
  scale_color_viridis_c(
    name = "Magnitude",
    option = "inferno",
    direction = -1
  ) +
  scale_size_continuous(guide = "none")

# mountain map
mountain_map <- # USE THIS -- do mountain region vs northwest central
  plot_usmap(data = quake_count_choropleth,
             regions = "states",
             values = "region",
             labels = TRUE,
             label_color = "white",
             include = .mountain) +
  scale_fill_manual(values = c("#A3b18a")) +
  theme(legend.position = "right",
        panel.background = element_rect(color = "black", fill = "#f5f0e8")) +
  labs(title = "Earthquakes detected within the Mountain region (2025)") +
  geom_sf(data = usa_quakes |> 
            dplyr::filter(region %in% "Mountain"),
          aes(size = mag, color = mag),
          alpha = 0.7) +
  scale_color_viridis_c(
    name = "Magnitude",
    option = "inferno",
    direction = -1
  ) +
  scale_size_continuous(guide = "none")

mountain_map

# south atlantic map where Virginia is
se_mid_atlantic_map <- plot_usmap(data = quake_count_choropleth,
           regions = "states",
           values = "region",
           labels = TRUE,
           label_color = "black",
           include = c(.south_atlantic, .east_south_central, .mid_atlantic)) +
  scale_fill_manual(values = c("#d2a39a", "#C4CDD6", "#927541")) +
  theme(legend.position = "right",
        panel.background = element_rect(color = "black", fill = "#f5f0e8")) +
  labs(title = "Earthquakes detected within the South Atlantic &\nEast South Central regions (2025)") +
  geom_sf(data = usa_quakes |> 
            dplyr::filter(region %in% c("South Atlantic", "East South Central", "Middle Atlantic")),
          aes(size = mag, color = mag),
          alpha = 0.7) +
  scale_color_viridis_c(
    name = "Magnitude",
    option = "inferno",
    direction = -1
  ) +
  scale_size_continuous(guide = "none")

se_mid_atlantic_map

# just the USA regions -- not for final blog
region_map <- plot_usmap(data = quake_count_choropleth,
           regions = "states",
           values = "region",
           labels = TRUE,
           label_color = "black") +
  scale_fill_manual(values = c("#964477", "#d2a39a", "#C4CDD6",
                               "#A3b18a", "#e88d5f", "#dbde85",
                               "#927541", "#bd4669", "#bfb1cb")) +
  theme(legend.position = "right",
        panel.background = element_rect(color = "black", fill = "white")) +
  labs(title = "USA Census Regions")

region_map


# I need to use quake_count_choropleth to make a table of quake count by state
# and region
library(gt)

quake_table <- quake_count_choropleth |> 
  select(region, state, n, smallest_mag, largest_mag) |> 
  dplyr::rename("Smallest" = smallest_mag, "Largest" = largest_mag) |> 
  gt(rowname_col = "state", groupname_col = "region") |> 
  tab_header(title = md("USA Quakes (2025)")) |> 
  tab_source_note(source_note = "From: USGS Earthquake Hazards Program") |> 
  tab_style(style = list(cell_text(size = "larger",
                                   weight = "bold")),
            locations = cells_row_groups())

quake_table


# Shift Alaska and Hawaii to sit below the continental US
#tigris::shift_geometry(quake_count)
# thinking about adding geompetry to usa_quakes?

tigris::shift_geometry(usa_quakes) |> 
  dplyr::filter(region %in% "Pacific") |> 
  View()


# plot all of USA regions by color with quakes
# kind of difficult to figure out what's wrong with Hawaii...
plot_usmap(data = quake_count_choropleth,
           regions = "states",
           values = "region",
           labels = TRUE,
           label_color = "black") +
  theme(legend.position = "right") +
  geom_sf(data = tigris::shift_geometry(usa_quakes),
          aes(size = mag, color = mag),
          alpha = 0.7) +
  scale_color_viridis_c(
    name = "Magnitude",
    option = "inferno",
    direction = -1
  ) +
  scale_size_continuous(guide = "none")

# focus on continental USA
continental <- usa_quakes |> dplyr::filter(!state %in% c("Alaska", "Hawaii"))


# map of quakes within US borders
plot_usmap(data = quake_count2,
           regions = "states",
           exclude = c("Hawaii", "Alaska"),
           values = "region") +
  theme(legend.position = "none") +
  labs(title = "Earthquakes detected in the Continental USA in 2025") +
  scale_fill_brewer(type = "qual", palette = "Pastel1") +
  geom_sf(data = usa_quakes |> tidyr::drop_na(state),
          aes(size = mag),
          color = "black",
          alpha = 0.2)

# of course - quakes don't need to be within state borders to be detected
plot_usmap(data = quake_count2,
           regions = "states",
           include = c("California", "Oregon", "Washington"),
           values = "region") +
  theme(legend.position = "none",
        panel.background = element_rect(color = "black", fill = "lightblue")) +
  labs(title = "Earthquakes detected within\nand near the West Coast") +
  scale_fill_brewer(type = "qual", palette = "Pastel1") +
  geom_sf(data = usa_quakes_complete |> 
            dplyr::filter(state %in% c("California", "Oregon", "Washington")),
          aes(size = mag),
          color = "black",
          alpha = 0.2)

# San Ramon Quake Maps ----------------------------------------------------

library(dplyr)
library(ggplot2)
library(sf)

# Read the data
sanramon <- readr::read_csv("blog/earthquakes/sanramon.csv")

# Convert to an sf object
# This adds a geometry column from your lat/lon columns
sanramon_sf <- st_as_sf(
  sanramon,
  coords = c("longitude", "latitude"),  # x first, then y — easy to mix up!
  crs = 4326,
  remove = FALSE  # keep the original lat/lon columns too
)

bayarea <- readr::read_csv("blog/earthquakes/bayarea_quakes.csv")

bayarea_sf <- st_as_sf(
  bayarea,
  coords = c("longitude", "latitude"),  # x first, then y — easy to mix up!
  crs = 4326,
  remove = FALSE  # keep the original lat/lon columns too
)

library(tigris)
options(tigris_use_cache = TRUE)  # cache downloads so you're not re-downloading every session

# Download California county boundaries
ca_counties <- counties(state = "CA", cb = TRUE) %>%
  st_transform(4326)  # make sure CRS matches our earthquake data

# Now build the map in layers
ggplot() +
  # Layer 1: county polygons as the base
  geom_sf(data = ca_counties, fill = "lightyellow", color = "gray60") +
  # Layer 2: earthquake points on top
  geom_sf(data = bayarea_sf, color = "red", alpha = 0.6) +
  # Zoom into the Bay Area bounding box
  coord_sf(
    xlim = c(-122.5, -121.5),  # longitude range (west to east)
    ylim = c(37.3, 38.1)       # latitude range (south to north)
  )

# slightly modified map
ggplot() +
  # this adds the county layer
  geom_sf(data = ca_counties, 
          fill = "#f5f0e8", 
          color = "gray50", 
          linewidth = 0.3) +
  # this overlays the earthquake points
  geom_sf(
    data = sanramon_sf,
    aes(color = mag),
    alpha = 0.7
  ) +
  # add county labels
  geom_sf_label(data = ca_counties,
                aes(label = NAME)) +
  # this changes the color of the quake points
  scale_color_viridis_c(
    name = "Magnitude",
    option = "viridis",
    direction = -1
  ) +
  # this zooms in on the map area
  coord_sf(xlim = c(-122.5, -121.7), ylim = c(37.5, 38)) +
  # add labels to plot
  labs(
    title = "Earthquakes Near San Ramon, CA",
    subtitle = "January 2025 – February 2026",
    caption = "Source: USGS Earthquake Hazards Program",
    x = NULL,  # suppress axis labels — lat/lon on axis is usually obvious
    y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right",
    axis.text.x = element_text(angle = 30, hjust = 1), 
    panel.background = element_rect(fill = "lightblue")
  )

## map with inset ####
library(ggplot2)
library(dplyr)
library(sf)
library(patchwork)  # install if needed: install.packages("patchwork")

# --- Define the inset bounding box ONCE ---
# Using a single object means the rectangle and the inset zoom
# are guaranteed to match each other — no copy-paste errors
inset_xlim <- c(-122.08, -121.82)
inset_ylim <- c(37.68, 37.835)

# smaller inset
inset_xlim <- c(-122.02, -121.85)
inset_ylim <- c(37.68, 37.8)

# Build a small data frame of landmarks
landmarks <- data.frame(
  name = c("San Ramon", "Danville", "Dublin"),
  lon  = c(-121.900, -121.996, -121.936),
  lat  = c(37.780,   37.822,   37.702)
  )

landmarks <- data.frame(
  name = c("San Ramon", "Dublin"),
  lon  = c(-121.900, -121.936),
  lat  = c(37.780,   37.702)
)

# --- Map 1: Bay Area overview ---
map_overview <- ggplot() +
  geom_sf(data = ca_counties,
          fill = "#f5f0e8",
          color = "gray50",
          linewidth = 0.3) +
  geom_sf(data = sanramon_sf,
          aes(color = mag),
          alpha = 0.85
          ) +
  geom_sf(data = bayarea_sf,
          aes(color = mag),
          alpha = 0.85
          ) +
  geom_point(data = landmarks,
             aes(x = lon, y = lat),
             shape = 17,      # filled triangle
             size = 2,
             color = "black"
             ) +
  scale_color_viridis_c(
    name = "Magnitude",
    option = "viridis",
    direction = -1
    ) +
  # Draw the rectangle using the same coords as the inset
  # This is what visually connects the two maps
  # put the rectangle UNDER geom_sf_labels, else it might cover a label
  geom_rect(
    aes(xmin = inset_xlim2[1], xmax = inset_xlim2[2],
        ymin = inset_ylim2[1], ymax = inset_ylim2[2]),
    fill = NA,         # transparent interior
    color = "red",     # red border so it stands out
    linewidth = 0.8
    ) +
  geom_sf_label(data = ca_counties,
                aes(label = NAME),
                size = 2.5,
                nudge_y = -0.012,
                label.padding = unit(0.1, "lines")) +
  coord_sf(xlim = c(-122.6, -121.6), ylim = c(37.3, 38.1)) +
  labs(
    title = "San Francisco Bay Earthquakes",
    subtitle = "January 2025 – February 2026",
    caption = "Source: USGS Earthquake Hazards Program",
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    axis.text.x = element_text(angle = 30, hjust = 1),
    panel.background = element_rect(fill = "lightblue"),
    # remove legend from overview — it lives in the inset
    legend.position = "right"  
  )

map_overview


# --- Map 2: San Ramon detail inset ---
map_inset <- ggplot() +
  geom_sf(data = ca_counties,
          fill = "#f5f0e8",
          color = "gray50",
          linewidth = 0.3) +
  geom_sf(data = sanramon_sf,
          aes(color = mag),  # size is readable here since we're zoomed in
          alpha = 0.85
  ) +
  scale_color_viridis_c(
    name = "Magnitude",
    option = "viridis",
    direction = -1
  ) +
  coord_sf(xlim = inset_xlim2, 
           ylim = inset_ylim2) +
  labs(
    title = title = "Earthquake swarm in San Ramon, CA",
    x = NULL, 
    y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 8, face = "bold"),
    plot.caption = element_text(size = 6),
    axis.text = element_text(size = 6),
    axis.text.x = element_text(angle = 30),
    legend.position = "none",
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 6),
    legend.key.size = unit(0.4, "cm"),
    plot.background = element_rect(fill = "#f5f0e8",
                                   color = "red", 
                                   linewidth = 0.8)
  )

map_inset

# Add to map_inset
map_inset2 <- map_inset + 
  geom_point(data = landmarks,
           aes(x = lon, y = lat),
           shape = 17,      # filled triangle
           size = 2,
           color = "black") +
  geom_text(data = landmarks,
            aes(x = lon, y = lat, label = name),
            size = 2.5,
            nudge_y = -0.006)  # shift label slightly below the point


map_inset2

# --- Embed the inset into the overview ---
# annotation_custom() places a grob at specific coordinates
# The x/y values here are in the DATA coordinates of map_overview
# so we use longitude/latitude values to position it
map_final <- map_overview +
  annotation_custom(
    grob = ggplotGrob(map_inset2),  # convert ggplot to embeddable grob
    xmin = -122.13, xmax = -121.55, # horizontal position in lon degrees
    ymin = 37.28,   ymax = 37.62   # vertical position in lat degrees
  ) +
  annotate("segment",
           x = -122.02,
           xend = -122.073,
           y = 37.68, 
           yend = 37.62,
           color = "red",
           linewidth = 0.8) +
  annotate("segment",
           x = -121.85,
           xend = -121.6,
           y = 37.68, 
           yend = 37.62,
           color = "red",
           linewidth = 0.8)

map_final



