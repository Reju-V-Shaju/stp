
#' Stipend Calculation and sheet generation
#'
#' @param url input url of the the respective stipend sheet
#' @param month_year input month in Month_year format
#'
#' @return
#' @export
#'
#' @examples
stipend_old <- function(url, month_year) {
  data_old <- gsheet2tbl(url)
  data_old <- data_old %>% dplyr::select(1:22, contains(month_year))
  data_old <- data_old[1:1116,]
  data_old <- as.data.frame(data_old)
  data_old[is.na(data_old)] <- 0
  data_old[data_old == ""] <- 0
  data_bs <- data_old

  data_old$CM_Total <- as.numeric(data_old[[paste0(month_year, "_Committee_Meeting")]]) * 100
  data_old$CR_Total <- as.numeric(data_old[[paste0(month_year, "_Community_Registry")]]) * 3
  data_old$EF_Total <- data_old[[paste0(month_year, "_Entitlement_facilitation")]] * 50
  data_old$KG_Total <- data_old[[paste0(month_year, "_Kitchen_Garden")]] * 50
  data_old$HE_Total <- as.numeric(data_old[[paste0(month_year, "_HE_Sessions")]]) * 50

  # Check if the Health Camps columns exist, if not, set totals to zero
  if (paste0(month_year, "_Health_Camps_Organised_By_CHE") %in% colnames(data_old)) {
    data_old$HCO_Total <- as.numeric(data_old[[paste0(month_year, "_Health_Camps_Organised_By_CHE")]]) * 200
  } else {
    data_old$HCO_Total <- 0
  }

  if (paste0(month_year, "_Health_Camps_Supported_By_CHE") %in% colnames(data_old)) {
    data_old$HCS_Total <- as.numeric(data_old[[paste0(month_year, "_Health_Camps_Supported_By_CHE")]]) * 100
  } else {
    data_old$HCS_Total <- 0
  }

  data_old[[paste0("Total_Stp_", month_year)]] <- (
    data_old$HE_Total +
      data_old$KG_Total +
      data_old$CM_Total +
      data_old$EF_Total +
      data_old$CR_Total +
      data_old$HCO_Total +
      data_old$HCS_Total
  )

  data_old$On_Hold <- ifelse(data_old[[paste0(month_year, "_HE_Sessions")]] > 12, "Hold",
                             ifelse(data_old[[paste0(month_year, "_Kitchen_Garden")]] > 6, "Hold",
                                    ifelse(data_old[[paste0(month_year, "_Entitlement_facilitation")]] > 150, "Hold",
                                           ifelse(data_old[[paste0(month_year, "_Committee_Meeting")]] > 1, "Hold",
                                                  ifelse(data_old[[paste0(month_year, "_Community_Registry")]] > 300, "Hold",
                                                         ifelse(data_old[[paste0(month_year, "_Health_Camps_Organised_By_CHE")]] > 1, "Hold",
                                                                ifelse(data_old[[paste0(month_year, "_Health_Camps_Supported_By_CHE")]] > 1, "Hold", "Release")))))))

  data_old <- data_old %>% filter(`short id` != 0)
  x <- data_old
  x <- x %>% filter(`Active/ inactive` == "Active")

  x <- x %>%
    filter(Telehealth %in% c("YES", "Yes") | District %in% c("Deoria", "Jaunpur", "Mirzapur"))
  write.csv(x, "Full_data.csv")
  data_old <- data_old %>% filter(data_old[[paste0("Total_Stp_", month_year)]] != 0)

  data_old_1 <- data_old %>% filter(`Active/ inactive` == "Active")

  data_filtered_test <- data_old_1 %>%
    filter(Telehealth %in% c("YES", "Yes") | District %in% c("Deoria", "Jaunpur", "Mirzapur"))

  filename <- paste0(month_year, "_stipend_Old.xlsx")
  write.xlsx(data_filtered_test, file = filename)

  held <- data_filtered_test %>% filter(On_Hold =="Hold")
  filename <- paste0(month_year, "_stipend_Old_held.xlsx")
  write.xlsx(held, file = filename)
  release <- data_filtered_test %>% filter(On_Hold =="Release")
  filename <- paste0(month_year, "_stipend_Old_release.xlsx")
  write.xlsx(release, file = filename)

  # Show the outcomes
  print("Data before subsetting:")
  print(nrow(data_bs))

  print("data including zero:")
  print(nrow(x))

  print("data after subsetting:")
  print(nrow(data_filtered_test))


  print("Telehealth CHEs")
  print(table(data_old$Telehealth))

  print("District")
  print(table(data_old_1$District))

  print("Table of Stipend")
  print(table(data_filtered_test[[paste0("Total_Stp_", month_year)]]))

  print("Table of On_hold")
  print(table(data_filtered_test$On_Hold))
}


#' function to calculate new stipend and creat sheets
#'
#' @param new_url input url
#' @param month  input month in Month_Year format
#'
#' @return
#' @export
#'
#' @examples
stipend_new <- function(new_url, month) {
  data <- gsheet2tbl(new_url)

  data <- data %>%
    dplyr::select(1:21, contains(month))

  data <- data %>%
    filter(`CHE ID` != 0)

  data <- as.data.frame(data)
  data[is.na(data)] <- 0
  data[data == ""] <- 0

  data$CM_Total <- data[[paste0(month, "_Commitee_Meeting")]] * 100
  data$CR_Total <- data[[paste0(month, "_Community_Registry")]] * 3
  data$EF_Total <- data[[paste0(month, "_Entitlement_facilitation")]] * 50
  data$KG_Total <- data[[paste0(month, "_Kitchen_Garden")]] * 50
  data$HE_Total <- data[[paste0(month, "_HE_Sessions")]] * 50
  data$HCO_Total <- as.numeric(data[[paste0(month, "_Health_Camps_Organised_By_CHE")]]) * 200
  data$HCS_Total <- data[[paste0(month, "_Health_Camps_Supported_By_CHE")]] * 100

  total_stip_var <- paste("Total_Stip_", month, sep = "_")
  data[[total_stip_var]] <- (data$CM_Total +
                               data$CR_Total +
                               data$EF_Total +
                               data$KG_Total +
                               data$HE_Total +
                               data$HCO_Total +
                               data$HCS_Total)

  data$On_Hold <- ifelse(data[[paste0(month, "_HE_Sessions")]] > 12, "Hold",
                         ifelse(data[[paste0(month, "_Kitchen_Garden")]] > 6, "Hold",
                                ifelse(data[[paste0(month, "_Entitlement_facilitation")]] > 15, "Hold",
                                       ifelse(data[[paste0(month, "_Commitee_Meeting")]] > 1, "Hold",
                                              ifelse(data[[paste0(month, "_Community_Registry")]] > 300, "Hold",
                                                     ifelse(data[[paste0(month, "_Health_Camps_Organised_By_CHE")]] > 0, "Hold",
                                                            ifelse(data[[paste0(month, "_Health_Camps_Supported_By_CHE")]] > 0, "Hold", "Release")))))))


  data <- data %>%
    filter(`CHE ID` != 0)
  data <- data %>%
    filter(Dropout == 0)

  Block_list <- c("Imamganj", "Chapra", "Musahari", "Nawdiha-Bazar", "Chhatarpur")

  block_data <- data %>%
    filter(!Block %in% Block_list)

  final_data <- block_data %>%
    filter(District != "Buxar")

  write.csv(final_data, paste0(month, "New_CHE_Full", ".csv"))

  held <- final_data %>% dplyr::filter(On_Hold=="Hold")

  release <- final_data %>% dplyr::filter(On_Hold=="Release")

  write.csv(held, paste0( month, "New_CHE_Held", ".csv"))
  write.csv(release, paste0( month, "New_CHE_Release", ".csv"))

  return(list(
    total_stp_table = table(final_data$final_data[[total_stip_var]] ),
    district_table = table(final_data$District),
    block_table = table(final_data$Block)
  ))
}


#' datacheck the old sheet
#'
#' @param url
#' @param month_year
#'
#' @return
#' @export
#'
#' @examples
check_old <- function(url, month_year) {
  data_old <- gsheet2tbl(url)
  data_old <- data_old %>% dplyr::select(1:22, contains(month_year))
  data_old <- data_old[1:1116,]
  data_old <- as.data.frame(data_old)
  data_old[is.na(data_old)] <- 0
  data_old[data_old == ""] <- 0
  data_bs <- data_old

  print("CM")
  as.numeric(data_old[[paste0(month_year, "_Committee_Meeting")]])
  print(class(data_old[[paste0(month_year, "_Committee_Meeting")]]))
  print(table(data_old[[paste0(month_year, "_Committee_Meeting")]]))

  print("CR")
  data_old[[paste0(month_year, "_Community_Registry")]]
  print(class(data_old[[paste0(month_year, "_Community_Registry")]]))
  print(table(data_old[[paste0(month_year, "_Community_Registry")]]))

  print("EF")
  data_old[[paste0(month_year, "_Entitlement_facilitation")]]
  print(class(data_old[[paste0(month_year, "_Entitlement_facilitation")]]))
  print(table(data_old[[paste0(month_year, "_Entitlement_facilitation")]]))

  print("KG")
  data_old[[paste0(month_year, "_Kitchen_Garden")]]
  print(class(data_old[[paste0(month_year, "_Kitchen_Garden")]]))
  print(table(data_old[[paste0(month_year, "_Kitchen_Garden")]]))

  print("HE Session:")
  data_old[[paste0(month_year, "_HE_Sessions")]]
  print(class(data_old[[paste0(month_year, "_HE_Sessions")]]))
  print(table(data_old[[paste0(month_year, "_HE_Sessions")]]))
}

#' Data check the new sheet
#'
#' @param new_url
#' @param month
#'
#' @return
#' @export
#'
#' @examples
check_new <- function(new_url, month) {
  data <- gsheet2tbl(new_url)

  data <- data %>%
    dplyr::select(1:21, contains(month))

  data <- data %>%
    filter(`CHE ID` != 0)

  data <- as.data.frame(data)
  data[is.na(data)] <- 0
  data[data == ""] <- 0

  columns_to_print <- c(
    paste0(month, "_Committee_Meeting"),
    paste0(month, "_Community_Registry"),
    paste0(month, "_Entitlement_facilitation"),
    paste0(month, "_Kitchen_Garden"),
    paste0(month, "_HE_Sessions"),
    paste0(month, "_Health_Camps_Organised_By_CHE"),
    paste0(month, "_Health_Camps_Supported_By_CHE")
  )

  for (column_name in columns_to_print) {
    column <- data[[column_name]]

    print(column_name)
    print("Class:")
    print(class(column))
    print("Table:")
    print(table(column))
  }
}
