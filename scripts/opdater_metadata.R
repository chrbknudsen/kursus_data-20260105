# dette er scriptet der køres af github actions, og henter metadata på
# kurser vi ikke tidligere har hentet data på - og opdaterer datasættet.

library(tidyverse)
# test at denne kan klare sig uden tidyverse.' det kan det ikke helt...
# opdater derefter den tilhørende yaml fil mhp hurtigere kørsel.
# overvej også at refaktorisere mhp at slippe af med i hvert fald lubridate

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(httr)
library(here)
cal_id <- 6416L
kalender_id <- cal_id

# har pt 1749 rækker i metadata. Efter dagens åbne værksted, skal der gerne være
# mindst en mere. som er fra 2. juli
# find dog også ud af hvorfor id 4216763 fører til 10 rækker?

meta_data <- read_csv2("data/kursus_metadata.csv")

if(here::here() == "C:/Users/cbk/Documents/R_projekter/kursus_data"){
  client_secret <- keyring::key_get("libcal")
}else{
  client_secret <- Sys.getenv("CLIENT_SECRET")
}

# Get token
# returnerer et access-token. Tager client_secret som input.
get_token <- function(client_secret){
  token_endpoint <- "https://kubkalender.kb.dk/1.1/oauth/token"
  client_id <- "110"
  token <- POST(token_endpoint,
                body = list(grant_type = "client_credentials",
                            client_id = client_id,
                            client_secret = client_secret)) %>% 
    content() 
  token[["access_token"]]
  
}

# get_token ----
token <- get_token(client_secret = client_secret)


# Henter de 500 første events i year for kalenderen med id'et calid, fra for
# en måned siden, og et år frem. 
# returnerer liste med events.
get_events <- function(calid, year){
  dato <- today() %m-% months(1)
  dato <- as.character(dato)
  
  url <- modify_url(
    url = "https://kubkalender.kb.dk",
    path = c("1.1", "events"),
    query = list(
      cal_id = calid,
      date = dato,
      days = 365,
      limit = 500
    )
  )
  GET(url, add_headers('Authorization' = paste("bearer", token))) %>% 
    content()
}


i <- year(today())
data <- get_events(kalender_id, i) %>% 
  as_tibble() 


nye_meta_data <- data %>% unnest_wider(events) %>% 
  select(-c(future_dates)) %>% 
  filter(as_datetime(end) < now()) %>% 
  unnest_wider(url) %>% 
  unnest_wider(location, names_sep = "_") %>% 
  # select(-c(id, title, allday, start, end, description, public, admin, location_id,
  #           location_type, location_name)) %>% 
  unnest_wider(campus, names_sep = "_") %>% 
  # select(-c(campus_id, campus_name, campus_1, presenter, registration, registration_form_id,
  #           registration_series_linked, physical_seats_taken, online_seats, physical_seats,
  #           online_seats_taken, wait_list, color, featured_image, seats_taken, 
  #           registration_cost, setup_time, teardown_time, more_info, zoom_email,
  #           online_user_id, online_meeting_id, online_host_url, online_join_url,
  #           online_join_password, online_provider)) %>% 
  unnest_wider(owner, names_sep = "_") %>% 
  #  select(-c(has_registration_opened, has_registration_closed))  %>% 
  unnest(geolocation, keep_empty = TRUE) %>% 
  unnest(geolocation, keep_empty = TRUE) %>% 
  unnest_wider(calendar, names_sep = "_") %>% 
  #select(-c(calendar_id, calendar_name, calendar_public, calendar_admin, geolocation, owner_id, owner_name)) %>% 
  unnest_wider(any_of("audience"), names_sep = "_")  %>% 
  unnest_wider(any_of("audience_1"), names_sep = "_") %>% 
  unnest_wider(any_of("audience_2"), names_sep = "_") %>% 
  unnest_wider(any_of("audience_3"), names_sep = "_") %>% 
  mutate(seats = map(seats, ~ as.numeric(.x))) %>% 
  unnest(seats) %>% 
  unnest_longer(category) %>% 
  unnest_wider(category, names_sep = "_") 

print("data hentet")


# konvertere til korrekt type, og filtrerer eksisterende kursus-id'er ud.
nye_meta_data <- nye_meta_data %>% 
  type_convert() %>% filter(!(id %in% meta_data$id))

if(nrow(nye_meta_data != 0)){
  nye_meta_data %>% 
    bind_rows(meta_data) %>% 
    write_csv2("data/kursus_metadata.csv")
  }


# Hent samlede metadata for alle kursuskalendere

# Henter de 500 første events i year for kalenderen med id'et calid, fra for
# en måned siden, og et år frem. 
# returnerer liste med events.
get_all_events <- function(calid, year){
  dato <- paste(year, "01", "01", sep = "-")
  
  url <- modify_url(
    url = "https://kubkalender.kb.dk",
    path = c("1.1", "events"),
    query = list(
      cal_id = calid,
      date = dato,
      days = 365,
      limit = 500
    )
  )
  GET(url, add_headers('Authorization' = paste("bearer", token))) %>% 
    content() |>
    as_tibble() 
  # |> 
  #   mutate(year = year,
  #          calid = calid)
}



prep_all_events <- function(df){df |> 
  unnest_wider(events) %>% 
  select(-c(future_dates)) %>% 
  filter(as_datetime(end) < now()) %>% 
  unnest_wider(url) %>% 
  unnest_wider(location, names_sep = "_") %>% 
  # select(-c(id, title, allday, start, end, description, public, admin, location_id,
  #           location_type, location_name)) %>% 
  unnest_wider(campus, names_sep = "_") %>% 
  # select(-c(campus_id, campus_name, campus_1, presenter, registration, registration_form_id,
  #           registration_series_linked, physical_seats_taken, online_seats, physical_seats,
  #           online_seats_taken, wait_list, color, featured_image, seats_taken, 
  #           registration_cost, setup_time, teardown_time, more_info, zoom_email,
  #           online_user_id, online_meeting_id, online_host_url, online_join_url,
  #           online_join_password, online_provider)) %>% 
  unnest_wider(owner, names_sep = "_") %>% 
  #  select(-c(has_registration_opened, has_registration_closed))  %>% 
  unnest(geolocation, keep_empty = TRUE) %>% 
  unnest(geolocation, keep_empty = TRUE) %>% 
  unnest_wider(calendar, names_sep = "_") %>% 
  #select(-c(calendar_id, calendar_name, calendar_public, calendar_admin, geolocation, owner_id, owner_name)) %>% 
  unnest_wider(any_of("audience"), names_sep = "_")  %>% 
  unnest_wider(any_of("audience_1"), names_sep = "_") %>% 
  unnest_wider(any_of("audience_2"), names_sep = "_") %>% 
  unnest_wider(any_of("audience_3"), names_sep = "_") %>% 
  mutate(seats = map(seats, ~ as.numeric(.x))) %>% 
  unnest(seats) %>% 
  unnest_longer(category) %>% 
  unnest_wider(category, names_sep = "_") }

get_all_events(55, 2024) |> 
  prep_all_events() |> view()

grab_all_events <- function(calid, year){
  get_all_events(calid, year) |> 
    prep_all_events()
}

calids <- tribble(~navn, ~calid,
"city", 55,
"datalab", 6416,
"forskerservice", 6646,
"The libraris formerly known as Nørre Alle and DVJB", 7422,
"SC", 54)

years <- 2022:year(today())

expand_grid(calid = calids$calid, year = years) |> 
  mutate(data = pmap(list(calid, year), grab_all_events)) |> 
  unnest(data) |> 
  filter(seats != 0) |>
  distinct(id, .keep_all = TRUE) |> 
  group_by(calid, year) |> 
  summarise(antal = n(),
            seats = sum(seats),
          seats_taken = sum(seats_taken)) |> 
  left_join(calids) |> 
  write_csv2("data/summeret_alle_kursuskalendere.csv")
  
  