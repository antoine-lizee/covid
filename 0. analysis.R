library(ggplot2)
library(dplyr)
library(stringr)
library(RColorBrewer)

nyt_us_states_url <- 'https://github.com/nytimes/covid-19-data/raw/master/us-states.csv'
nyt_us_raw <- readr::read_csv(nyt_us_states_url)

census_pop_filepath <- 'input/nst-est2019-01 - NST01.csv'
state_pop_raw <- readr::read_csv(census_pop_filepath, skip = 3) %>% rename(state = 'X1')

# Prep data ---------------------------------------------------------------

# Note: this excludes Puerto-Rico
state_pop <- state_pop_raw %>% 
  filter(str_sub(state, 1, 1) == '.') %>% 
  mutate(state = str_sub(state, 2)) %>%
  select(state, pop = `2019`)

add_analysis_columns <- function(df, include_d = TRUE) {
  if (include_d) {  # A bit ugly, because the data for the us is totals only vs europe per day.
    df <- df %>% mutate(d_cases = cases - lag(cases), d_deaths = deaths - lag(deaths))
  }
  df %>% mutate(
    current_death = max(deaths)
    , dt = date - lag(date)
    , prevalence = cases / pop * 100000  # Actually: Cumulative cases in pop. It's not strictly prevalence since it counts remitted people
    , deaths_in_pop = deaths / pop * 100000  # Cumulative deaths in pop.
    , d_cases_7d = (cases - lag(cases, 7))
    , d_deaths_7d = (deaths - lag(deaths, 7))
    , incidence_1d = d_cases / pop * 100000
    , incidence_7d = d_cases_7d / pop * 100000
    , mortality_1d = d_deaths / pop * 100000
    , mortality_7d = d_deaths_7d / pop * 100000
    , hundredth_case_date = date[cases > 100][1]
    , first_death_date = date[deaths > 0][1]
    , tenth_death_date = date[deaths > 9][1]
    , first_prevalence_date = date[prevalence >= 4][1]
    , first_deaths_in_pop_date = date[deaths_in_pop >= 0.5][1]
  )
}

nyt_us <- nyt_us_raw %>%
  merge(state_pop) %>%
  group_by(state) %>%
  arrange(date) %>% 
  add_analysis_columns() %>% 
  ungroup

## Checking that we have no gap in date
nyt_us %>% count(state, dt) %>% ungroup %>% count(dt)


# First graphs ------------------------------------------------------------

get_labels_from_max <- function(df, ref_variable, type = 'max') {
  q_ref_variable <- enquo(ref_variable)
  df %>% group_by(state) %>%
    arrange(desc(!! q_ref_variable), date) %>%
    filter( row_number() == 1 ) %>%
    ungroup
}

get_labels_from_last <- function(df) {
  df %>% group_by(state) %>%
    arrange(date) %>% 
    filter( row_number() == n() ) %>%
    ungroup 
}

### Zoomed death counts, averaged over last week
nyt_us %>% 
  filter(date > first_death_date, current_death > 100, pop > 5e6) %>% 
  ggplot(aes(x = date, y = d_deaths_7d, color = state)) +
  geom_line() +
  # coord_cartesian(ylim = c(0, 200)) +
  geom_label(data = . %>% get_labels_from_max(d_deaths_7d), aes(label = state), vjust = 0)

### One state
nyt_us %>% 
  filter(date > first_death_date, current_death > 100, state == "Pennsylvania") %>% 
  ggplot(aes(x = date, y = d_deaths, fill = state)) +
  geom_bar(stat = 'identity') +
  geom_line(aes(y = d_deaths_7d))


### Mortality rate graph
nyt_us %>% 
  filter(date > first_death_date, current_death > 100, pop > 5e6) %>% 
  ggplot(aes(x = date, y = mortality_7d, color = state)) +
  geom_line() +
  geom_label(data = . %>% get_labels_from_max(mortality_7d), aes(label = state), vjust = 0)

### US
nyt_us %>% 
  group_by(date) %>% summarize_at(vars(contains('d_deaths')), sum, na.rm = TRUE) %>%
  ggplot(aes(x = date, y = d_deaths)) +
  geom_bar(stat = 'identity') + 
  geom_line(aes(y = d_deaths_7d))


# Log graphs, adjusted --------------------------------------------------------------
selected_us_states <- c("California", "New York", "Oregon", "Washington")
make_log_graph <- function(df, first_date, y, selected_states = selected_us_states, pop_limit = 5e6, custom_palette_name = "Set1") {
  q_first_date <- enquo(first_date)
  q_y <- enquo(y)
  filtered_data <- df %>% filter(date > !! q_first_date, current_death > 100, !! q_y > 0, pop > pop_limit)
  plot <- filtered_data %>% 
    ggplot(aes(x = date - !! q_first_date, y = !! q_y, color = state)) +
    scale_y_log10() +
    geom_line(show.legend = FALSE) + ## aes(linetype = state)
    geom_text(data = . %>% get_labels_from_last() %>% filter(!state %in% selected_states), aes(label = state), hjust = -0.1, show.legend = FALSE) +
    geom_line(data = . %>% filter(state %in% selected_states), size = 2, show.legend = FALSE) +
    geom_label(data = . %>% get_labels_from_last() %>% filter(state %in% selected_states), aes(label = sprintf('%s (%.0f)', state, !! q_y)), hjust = -0.1, show.legend = FALSE) +
    scale_size_manual(values=c(0.7, 2), guide = FALSE) +
    theme_minimal() +
    theme(text = element_text(size = 11),
          plot.title = element_text(hjust = 0.5))
  if (!is.na(custom_palette_name)) {
    n_states <- filtered_data %>% distinct(state) %>% nrow
    # state_colors <- colorRampPalette(brewer.pal(8, custom_palette_name))(n_states)
    state_colors <- colorRampPalette(brewer.pal(9, "Set1")[-6])(n_states)
    plot + scale_color_manual(values = state_colors)
  } else {
    plot
  }
}

## Cases
### Cumulative Cases
nyt_us %>% make_log_graph(hundredth_case_date, cases)
### Differential
nyt_us %>% make_log_graph(hundredth_case_date, d_cases_7d)
### Prevalence
nyt_us %>% make_log_graph(first_prevalence_date, prevalence)
### Incidence
nyt_us %>% make_log_graph(first_prevalence_date, incidence_7d)

## Deaths
### Cumulative Cases
nyt_us %>% make_log_graph(tenth_death_date, deaths)
### Differential
nyt_us %>% make_log_graph(tenth_death_date, d_deaths_7d)
### Death in pop
nyt_us %>% make_log_graph(first_deaths_in_pop_date, deaths_in_pop)
### Mortality rate
nyt_us %>% make_log_graph(first_deaths_in_pop_date, mortality_7d)



# Let's look at Europe ----------------------------------------------------

ecdc_url <- 'https://opendata.ecdc.europa.eu/covid19/casedistribution/csv'
ecdc_raw <- readr::read_csv(ecdc_url, col_types = readr::cols(dateRep = readr::col_date("%d/%m/%Y"))) %>% 
  select(state = countriesAndTerritories, date = dateRep, d_deaths = deaths, d_cases = cases, pop = popData2018, continent = continentExp)

ecdc_raw %>% distinct(state, .keep_all = TRUE) %>% count(continent)

ecdc_raw %>% filter(pop > 5e6, continent == 'Europe') %>% group_by(state) %>% arrange(date) %>% filter(row_number() == n()) %>% arrange(desc(pop)) %>% data.frame

selected_countries <- c("France", "Germany", "Italy", "Spain", "United_Kingdom", "Netherlands", "Belgium", "Sweden", "Switzerland")

ecdc <- ecdc_raw %>%
  filter(state %in% selected_countries) %>% 
  group_by(state) %>% 
  arrange(date) %>% 
  mutate(cases = cumsum(d_cases), deaths = cumsum(d_deaths)) %>%
  add_analysis_columns(include_d = FALSE) %>%
  ungroup

## Double checking that Spain is annoying
ecdc %>% count(state, dt) %>% ungroup %>% count(dt)

## Cases
highlighted_countries <- c("France", "Italy")
### Cumulative Cases
ecdc %>% make_log_graph(hundredth_case_date, cases, selected_states = highlighted_countries)
### Differential
ecdc %>% make_log_graph(hundredth_case_date, d_cases_7d, selected_states = highlighted_countries)
### Prevalence
ecdc %>% make_log_graph(first_prevalence_date, prevalence, selected_states = highlighted_countries)
### Incidence
ecdc %>% make_log_graph(first_prevalence_date, incidence_7d, selected_states = highlighted_countries)

## Deaths
### Cumulative Cases
ecdc %>% make_log_graph(tenth_death_date, deaths, selected_states = highlighted_countries)
### Differential
ecdc %>% make_log_graph(tenth_death_date, d_deaths_7d / 7, selected_states = highlighted_countries)
### Death in pop
ecdc %>% make_log_graph(first_deaths_in_pop_date, deaths_in_pop, selected_states = highlighted_countries)
### Mortality rate
ecdc %>% make_log_graph(first_deaths_in_pop_date, mortality_7d, selected_states = highlighted_countries)


# Conclusion --------------------------------------------------------------

### Deaths per day in the US -- notice weekend dips
deaths_per_day <- nyt_us %>% 
  group_by(date) %>% summarize_at(vars(contains('d_deaths')), sum, na.rm = TRUE) %>%
  ggplot(aes(x = date, y = d_deaths)) +
  geom_bar(stat = 'identity') + 
  geom_line(aes(y = d_deaths_7d / 7)) +
  theme_minimal() + 
  scale_y_continuous(labels = scales::comma) + 
  labs(x = "Date", y = "Number of deaths", title = "Reported deaths per day in the US") + 
  theme(text = element_text(size = 11),
        plot.title = element_text(hjust = 0.5))

### Incidence by US state
us_incidence <- nyt_us %>% mutate(state = reorder(state, current_death)) %>%
  make_log_graph(first_prevalence_date, incidence_7d) +
  labs(x = "Number of days since first case", y = "Incidence (last 7 days average)", title = "New cases per day in US states") + 
  scale_x_continuous(limits = c(0, 65))
  
### Death in pop
us_deaths <- nyt_us %>% 
  # mutate(state = reorder(state, current_death)) %>%
  make_log_graph(first_deaths_in_pop_date, deaths_in_pop) +
  labs(x = "Number of days since first death", y = "Deaths per 100,000 inhabitants", title = "Total death count in US states adjusted for population size") + 
  scale_x_continuous(limits = c(0, 65)) 

### Incidence by country
ec_incidence <- ecdc %>% make_log_graph(first_prevalence_date, incidence_7d, selected_states = highlighted_countries, custom_palette_name = "Set1") + 
  labs(x = "Number of days since first case", y = "Incidence (last 7 days average)", title = "New cases per day in European countries") + 
  scale_x_continuous(limits = c(0, 65))

### Death in pop
ec_deaths <- ecdc %>% make_log_graph(first_deaths_in_pop_date, deaths_in_pop, selected_states = highlighted_countries, custom_palette_name = "Set1") + 
  labs(x = "Number of days since first death", y = "Deaths per 100,000 inhabitants", title = "Total death count in European countries adjusted for population size") + 
  scale_x_continuous(limits = c(0, 65)) + 
  scale_y_log10(limits = c(NA, 100))

### Combine US and EU 
us_eu <- bind_rows(nyt_us %>% filter(state %in% selected_us_states), ecdc %>% filter(state %in% highlighted_countries))

### Incidence of cases
us_eu_incidence <- us_eu %>% make_log_graph(first_prevalence_date, incidence_7d, selected_states = NULL) + 
  labs(x = "Number of days since first case", y = "Incidence (last 7 days average)", title = "New cases per day in selected US states and European countries") + 
  scale_x_continuous(limits = c(0, 65))

### Mortality per 100,000 population 
us_eu_mortality <- us_eu %>% make_log_graph(first_deaths_in_pop_date, mortality_7d, selected_states = NULL) + 
  labs(x = "Number of days since first death", y = "Deaths per week per 100,000 population", title = "Weekly mortality in selected US states and European countries") + 
  scale_x_continuous(limits = c(0, 65))

### Cumulative deaths per population 
us_eu_deaths <- us_eu %>% make_log_graph(first_deaths_in_pop_date, deaths_in_pop, selected_states = NULL) + 
  labs(x = "Number of days since first death", y = "Number of deaths per 100,000 population", title = "Total death count in selected US states and European countries") + 
  scale_x_continuous(limits = c(0, 65))



