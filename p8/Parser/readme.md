# R parser for public transport text data

Data is also availabile in the `data` folder.

## How to load the data?

`
text_file <- load_file("RA231125.TXT")
`

`
stops <- get_stops(text_file)
`

`
connections <- get_connections(text_file, stops)
`

## Output tables

### Stops

Table contains data about stops and columns such as:
- name <chr> - name of the stop
- id <int> - id of the stop
- y <dbl> - latitude
- x <dbl> - longitude
- stop_type <chr> - one of "A" (bus), "T" (tram), "K" (train)

### Connections

Table contains information about each connection between each consecutive stops
(one per course of the vehicle).
- stop_id <int> - id of the stop, may be used to merge with stops
- course_id <chr> - id of the course 
- day_type <chr> - type of the day (holiday etc.)
- time <dbl> - should be treated as string - hh.mm
- time_from_mn <dbl> - time from midnight in minutes
- next_stop_id <int> - id of the next stop
- travel_time <dbl> - time in minutes between mentioned stops 
- stop_type <chr> - one of "A" (bus), "T" (tram), "K" (train), same as in Stops

### Dependencies

- data.table
- utf8
- stringi
- purrr