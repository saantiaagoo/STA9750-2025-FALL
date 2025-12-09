if(!dir.exists(file.path("data", "finalproject"))){
  dir.create(file.path("data", "finalproject"), showWarnings=FALSE, recursive=TRUE)
}

library <- function(pkg){
  ## Mask base::library() to automatically install packages if needed
  ## Masking is important here so downlit picks up packages and links
  ## to documentation
  pkg <- as.character(substitute(pkg))
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  if(!require(pkg, character.only=TRUE, quietly=TRUE)) install.packages(pkg)
  stopifnot(require(pkg, character.only=TRUE, quietly=TRUE))
}

library(tidyverse)
library(glue)
library(readxl)
library(tidycensus)

get_acs_all_years <- function(variable, geography="zcta",
                              start_year=2013, end_year=2023){
  fname <- glue("{variable}_{geography}_{start_year}_{end_year}.csv")
  fname <- file.path("data", "finalproject", fname)
  
  if(!file.exists(fname)){
    YEARS <- seq(start_year, end_year)
    YEARS <- YEARS[YEARS != 2020] # Drop 2020 - No survey (covid)
    
    ALL_DATA <- map(YEARS, function(yy){
      tidycensus::get_acs(geography, variable, year=yy, survey="acs5") |>
        mutate(year=yy) |>
        select(-moe, -variable) |>
        rename(!!variable := estimate)
    }) |> bind_rows()
    
    write_csv(ALL_DATA, fname)
  }
  
  read_csv(fname, show_col_types=FALSE)
}

NYC_ZCTAS <- c(
  # Bronx (005)
  "10451", "10452", "10453", "10454", "10455", "10456", "10457", "10458", 
  "10459", "10460", "10461", "10462", "10463", "10464", "10465", "10466", 
  "10467", "10468", "10469", "10470", "10471", "10472", "10473", "10474", 
  "10475",
  # Brooklyn (047) - Kings County
  "11201", "11203", "11204", "11205", "11206", "11207", "11208", "11209", 
  "11210", "11211", "11212", "11213", "11214", "11215", "11216", "11217", 
  "11218", "11219", "11220", "11221", "11222", "11223", "11224", "11225", 
  "11226", "11228", "11229", "11230", "11231", "11232", "11233", "11234", 
  "11235", "11236", "11237", "11238", "11239",
  # Manhattan (061) - New York County
  "10001", "10002", "10003", "10004", "10005", "10006", "10007", "10009", 
  "10010", "10011", "10012", "10013", "10014", "10016", "10017", "10018", 
  "10019", "10020", "10021", "10022", "10023", "10024", "10025", "10026", 
  "10027", "10028", "10029", "10030", "10031", "10032", "10033", "10034", 
  "10035", "10036", "10037", "10038", "10039", "10040", "10044",
  # Queens (081)
  "11101", "11102", "11103", "11104", "11105", "11106", "11354", "11355", 
  "11356", "11357", "11358", "11360", "11361", "11362", "11363", "11364", 
  "11365", "11366", "11367", "11368", "11369", "11370", "11371", "11372", 
  "11373", "11374", "11375", "11377", "11378", "11379", "11385", "11411", 
  "11412", "11413", "11414", "11415", "11416", "11417", "11418", "11419", 
  "11420", "11421", "11422", "11423", "11426", "11427", "11428", "11429", 
  "11432", "11433", "11434", "11435", "11436", "11691", "11692", "11693", 
  "11694", "11695", "11697",
  # Staten Island (085) - Richmond County
  "10301", "10302", "10303", "10304", "10305", "10306", "10307", "10308", 
  "10309", "10310", "10311", "10312", "10314"
)
# Household income (12 month)
INCOME <- get_acs_all_years("B19013_001") |>
  rename(household_income = B19013_001)
INCOME <- INCOME |>
  filter(GEOID %in% NYC_ZCTAS)

# Monthly rent
RENT <- get_acs_all_years("B25064_001") |>
  rename(monthly_rent = B25064_001)
RENT  <- RENT  |>
  filter(GEOID %in% NYC_ZCTAS)

# Combine tables
JOIN_TABLE <- INCOME |>
  left_join(RENT, by = c("GEOID", "year","NAME" )) |>
  rename(zipcode = "GEOID") |>
  select(-NAME)

## optional function to format the column name
format_titles <- function(df){
  colnames(df) <- str_replace_all(colnames(df), "_", " ") |> str_to_title()
  df
}
JOIN_TABLE |> 
  format_titles()
