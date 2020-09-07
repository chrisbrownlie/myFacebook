#' Get account activity
#'
#' @param folder the name of the data folder (in the project root directory)
#'
#' @return data frame of account activity from the last 3 years
#'
#' @export
get_account_activity <- function(folder = "data") {

  account_activity <- fromJSON(file = file.path(folder, "security_and_login_information", "account_activity.json"))[[1]] %>%
    lapply(as.data.frame) %>%
    bind_rows() %>%
    transmute(date_time = as_datetime(timestamp),
              action = action,
              ip_address = ip_address,
              city = city,
              region = region,
              country = country,
              user_agent = user_agent,
              site_name = site_name)

  return(account_activity)
}


#' Get administrative records
#'
#' @param folder the name of the data folder (in the project root directory)
#'
#' @return data frame of admin records
#'
#' @export
get_admin_records <- function(folder = "data") {

  admin_records <- fromJSON(file = file.path(folder, "security_and_login_information", "administrative_records.json"))[[1]] %>%
    lapply(as.data.frame) %>%
    bind_rows() %>%
    transmute(date_time = as_datetime(session.created_timestamp),
              event = event,
              ip_address = session.ip_address,
              user_agent = session.user_agent,
              old_name = extra_info.old_name,
              new_name = extra_info.new_name,
              old_number = extra_info.old_number,
              new_number = extra_info.new_number,
              old_vanity = extra_info.old_vanity,
              new_vanity = extra_info.new_vanity,
              old_email = extra_info.old_email,
              new_email = extra_info.new_email)

  return(admin_records)
}


#' Get authorised logins
#'
#' @param folder the name of the data folder (in the project root directory)
#'
#' @return data frame of authorised logins
#'
#' @export
get_auth_logins <- function(folder = "data") {

  auth_logins <- fromJSON(file = file.path(folder, "security_and_login_information", "authorized_logins.json"))[[1]] %>%
    lapply(as.data.frame) %>%
    bind_rows() %>%
    transmute(created_date_time = as_datetime(created_timestamp),
              updated_date_time = as_datetime(updated_timestamp),
              name = name,
              ip_address = ip_address)
  return(auth_logins)
}


#' Get login protection data
#'
#' @param folder the name of the data folder (in the project root directory)
#'
#' @return data frame of login protection data
#'
#' @export
get_login_protection_data <- function(folder = "data") {

  login_protection_data <- fromJSON(file = file.path(folder, "security_and_login_information", "login_protection_data.json"))[[1]] %>%
    lapply(as.data.frame) %>%
    bind_rows() %>%
    transmute(created_date_time = as_datetime(session.created_timestamp),
              updated_date_time = as_datetime(session.updated_timestamp),
              name = name,
              ip_address = session.ip_address,
              inferred_location = ifelse(str_detect(name, pattern = "Estimated location inferred from IP"),
                                         str_replace(name, pattern = "Estimated location inferred from IP ", replacement = ""),
                                         NA))
  return(login_protection_data)
}


#' Get logins and logouts
#'
#' @param folder the name of the data folder (in the project root directory)
#'
#' @return data frame of logins and logouts
#'
#' @export
get_logins_and_outs <- function(folder = "data") {

  logins_and_outs <- fromJSON(file = file.path(folder, "security_and_login_information", "logins_and_logouts.json"))[[1]] %>%
    lapply(as.data.frame) %>%
    bind_rows() %>%
    transmute(date_time = as_datetime(timestamp),
              action = action,
              site = site,
              ip_address = ip_address)

  return(logins_and_outs)
}



