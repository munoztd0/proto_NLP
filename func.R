


#### everything below is 

generate_timeline  <- function(data, grouped=NULL,  period="day") {

    dates <- as_tibble(as.Date(as.Date(min(data$Date)):as.Date(max(data$Date)), origin="1970-01-01"))
      data %>%
        mutate(Date = lubridate::floor_date(Date, period)) %>%
        group_by(Date, !!! rlang::syms(grouped)) %>%
        summarise(count = n() ) %>% #do sums
        full_join(
          dates %>%
            mutate(Date = lubridate::floor_date(value, period)) %>%
        group_by(Date)%>%
        summarise(drop = n() ),
          by = c("Date"), keep = F
        ) %>% # doing this to get missing dates
        mutate(across(where(is.numeric), ~ ifelse(is.na(.x), 0, .x))) %>%
        arrange(Date) %>%
        group_by_at(grouped)

}



generate_timeline_gravite  <- function(data, grouped=NULL,  period="day") {

    dates <- as_tibble(as.Date(as.Date(min(data$Date)):as.Date(max(data$Date)), origin="1970-01-01"))
      data %>%
        mutate(Date = lubridate::floor_date(Date, period)) %>%
        group_by(Date, !!! rlang::syms(grouped)) %>%
        summarise(count = n() ) %>% #do sums
        full_join(
          dates %>%
            mutate(Date = lubridate::floor_date(value, period)) %>% 
        tidyr::expand(Date, c("Préoccupant", "Sérieux", "Grave") ) %>%
        group_by(Date, `c(\"Préoccupant\", \"Sérieux\", \"Grave\")`) %>%
        select(Date, Gravite=`c(\"Préoccupant\", \"Sérieux\", \"Grave\")`) %>%
        summarise(drop = n()) ,
          by = c("Date", "Gravite"), keep = F
        ) %>% # doing this to get missing dates
        mutate(across(where(is.numeric), ~ ifelse(is.na(.x), 0, .x))) %>%
        arrange(Date) %>%
        group_by_at(grouped)

}


#' @import ggplot2

generate_histogram <- function(data,
                               col,
                               title = "histogram",
                               color = "#3CBA7E",
                               show_legend = FALSE,
                               show_labels = FALSE,
                               position_labels = "top",
                               y_type = NaN # 'log'
) {
    histogram <- renderEcharts4r({
        data |>
            e_charts_() |>
            e_histogram_(
                col
            ) |>
            e_labels(
                position = position_labels
            ) |>
            e_tooltip() |>
            e_color(
                color
            ) |>
            e_title(
                title
            ) |>
            e_legend(
                show = show_legend,
            ) |>
            e_x_axis(
                axisLabel = list(
                    rotate = 45
                )
            ) |>
            e_y_axis(
                type = y_type
            ) |>
            e_labels(
                show = show_labels
            )
    })
    return(histogram)
}

generate_bar <- function(data,
                         x_col,
                         y_col,
                         title = "histogram",
                         subtext = "",
                         space_under_tittle = "0%",
                         color = "#3CBA7E",
                         show_legend = FALSE,
                         show_labels = FALSE,
                         position_labels = "top",
                         y_type = "", # 'log',
                         connect = NaN) {
    bar <- renderEcharts4r({
        data |>
            e_charts_(
                x_col
            ) |>
            e_bar_(
                y_col
            ) |>
            e_labels(
                position = position_labels
            ) |>
            e_tooltip() |>
            e_color(
                color
            ) |>
            e_title(
                title,
                subtext = subtext,
                top = space_under_tittle
            ) |>
            e_legend(
                show = show_legend,
            ) |>
            e_x_axis(
                axisLabel = list(
                    rotate = 45
                )
            ) |>
            e_y_axis(
                type = y_type
            ) |>
            e_labels(
                show = show_labels
            ) |>
            e_tooltip(
                trigger = "axis"
            ) |>
            e_group(
                connect
            ) |>
            e_connect_group(
                connect
            )
    })
    return(bar)
}

generate_line <- function(data,
                         x_col,
                         y_col,
                         title = "",
                         subtext = "",
                         space_under_tittle = "0%",
                         color = "#3CBA7E",
                         show_legend = FALSE,
                         show_labels = FALSE,
                         position_labels = "top",
                         y_type = "", # 'log',
                         connect = NaN) {
    bar <- renderEcharts4r({
        data |>
            e_charts_(
                x_col
            ) |>
            e_line_(
                y_col
            ) |>
            e_labels(
                position = position_labels
            ) |>
            e_tooltip() |>
            e_color(
                color
            ) |>
            e_title(
                title,
                subtext = subtext,
                top = space_under_tittle
            ) |>
            e_legend(
                show = show_legend,
            ) |>
            e_x_axis(
                axisLabel = list(
                    rotate = 45
                )
            ) |>
            e_y_axis(
                type = y_type
            ) |>
            e_labels(
                show = show_labels
            ) |>
            e_tooltip(
                trigger = "axis"
            ) |>
            e_group(
                connect
            ) |>
            e_connect_group(
                connect
            )
    })
    return(bar)
}

generate_bar_multiple <- function(data,
                                  x_col,
                                  y_col,
                                  groupby,
                                  title = "histogram",
                                  subtext = "",
                                  space_under_tittle = "0%",
                                  show_legend = FALSE,
                                  show_labels = FALSE,
                                  position_labels = "top",
                                  y_type = "", # 'log',
                                  connect = NaN) {
    plot <- renderEcharts4r({
        data |>
            rename(
                !!"name" := groupby
            ) |>
            group_by(
                name
            ) |>
            e_charts_(
                x_col
            ) |>
            e_bar_(
                y_col,
                stack = "grp"
            ) |>
            e_labels(
                position = position_labels
            ) |>
            e_tooltip() |>
            e_title(
                title,
                subtext = subtext,
                top = space_under_tittle
            ) |>
            e_legend(
                show = show_legend,
            ) |>
            e_x_axis(
                axisLabel = list(
                    rotate = 45
                )
            ) |>
            e_y_axis(
                type = y_type
            ) |>
            e_labels(
                show = show_labels
            ) |>
            e_tooltip(
                trigger = "axis"
            ) |>
            e_group(
                connect
            ) |>
            e_connect_group(
                connect
            )
    })
    return(plot)
}


generate_scatter_xy <- function(data,
                                x_col,
                                y_col,
                                title = "histogram",
                                color = "#3CBA7E",
                                show_legend = FALSE,
                                show_labels = FALSE,
                                position_labels = "top",
                                x_type = "",
                                y_type = "", # 'log',
                                connect = NaN) {
    plot <- renderEcharts4r({
        data |>
            e_charts_(
                x_col
            ) |>
            e_scatter_(
                y_col
            ) |>
            e_datazoom() |>
            e_labels(
                position = position_labels
            ) |>
            e_tooltip() |>
            e_color(
                color
            ) |>
            e_title(
                title
            ) |>
            e_legend(
                show = show_legend
            ) |>
            e_x_axis(
                axisLabel = list(
                    rotate = 45
                ),
                type = x_type
            ) |>
            e_y_axis(
                type = y_type
            ) |>
            e_labels(
                show = show_labels
            ) |>
            e_group(
                connect
            ) |>
            e_connect_group(
                connect
            )
    })
    return(plot)
}

generate_scatter_xy_with_index_z <- function(data,
                                             x_col,
                                             y_col,
                                             z_col,
                                             title = "histogram",
                                             show_legend = FALSE,
                                             show_labels = FALSE,
                                             position_labels = "top",
                                             x_type = "",
                                             y_type = "", # 'log',
                                             connect = NaN,
                                             col_nb_to_infos = NULL) {
    colnames <- colnames(data)

    data <- data |>
        mutate_if(
            is.numeric,
            ~ round(., 1)
        )

    labels <- c()
    for (i in 1:nrow(data[, 1])) {
        labels_tmp <- paste0(
            paste0(
                "<strong>",
                colnames[col_nb_to_infos],
                "</strong> : ",
                data[i, col_nb_to_infos]
            ),
            collapse = "<br />"
        )
        labels <- append(labels, labels_tmp)
    }
    data$infos <- labels

    plot <- renderEcharts4r({
        data |>
            e_charts_(
                x_col
            ) |>
            e_scatter_(
                y_col,
                symbol_size = 5,
                bind = colnames(data)
            ) |>
            e_visual_map_(
                z_col
            ) |>
            e_datazoom() |>
            e_labels(
                position = position_labels
            ) |>
            e_tooltip(
                formatter = htmlwidgets::JS("
                                function(params){
                                    return(params.name)
                                            }
                            ")
            ) |>
            e_title(
                title
            ) |>
            e_legend(
                show = show_legend
            ) |>
            e_x_axis(
                axisLabel = list(
                    rotate = 45
                ),
                type = x_type
            ) |>
            e_y_axis(
                type = y_type
            ) |>
            e_labels(
                show = show_labels
            ) |>
            e_group(
                connect
            ) |>
            e_connect_group(
                connect
            )
    })
    return(plot)
}


generate_graph_xy <- function(data,
                              x_col,
                              y_col,
                              title = "histogram",
                              color = "#3CBA7E",
                              show_legend = FALSE,
                              show_labels = FALSE,
                              position_labels = "top",
                              y_type = "", # 'log',
                              connect = NaN) {
    plot <- renderEcharts4r({
        data |>
            e_charts_(
                x_col
            ) |>
            e_line_(
                y_col
            ) |>
            e_labels(
                position = position_labels
            ) |>
            e_tooltip() |>
            e_color(
                color
            ) |>
            e_title(
                title
            ) |>
            e_legend(
                show = show_legend
            ) |>
            e_x_axis(
                axisLabel = list(
                    rotate = 45
                )
            ) |>
            e_y_axis(
                type = y_type
            ) |>
            e_labels(
                show = show_labels
            ) |>
            e_group(
                connect
            ) |>
            e_connect_group(
                connect
            )
    })
    return(plot)
}

#' @importFrom rlang :=
generate_graph_xy_multiple <- function(data,
                                       x_col,
                                       y_col,
                                       groupby,
                                       title = "grouped graph",
                                       show_legend = FALSE,
                                       show_labels = FALSE,
                                       position_labels = "top",
                                       x_type = "", # 'log',
                                       y_type = "", # 'log',
                                       connect = NaN) {
    plot <- renderEcharts4r({
        data |>
            rename(
                !!"name" := groupby
            ) |>
            group_by(
                name
            ) |>
            e_charts_(
                x_col
            ) |>
            e_line_(
                y_col,
            ) |>
            e_labels(
                position = position_labels
            ) |>
            e_tooltip() |>
            e_title(
                title
            ) |>
            e_legend(
                show = show_legend,
                type = c("plain", "scroll"),
            ) |>
            e_x_axis(
                axisLabel = list(
                    rotate = 45
                ),
                type = x_type
            ) |>
            e_y_axis(
                type = y_type
            ) |>
            e_labels(
                show = show_labels
            ) |>
            e_group(
                connect
            ) |>
            e_connect_group(
                connect
            )
    })
    return(plot)
}

generate_pie_status <- function(data,
                                x_col,
                                y_col,
                                selection,
                                min_radius = "50%",
                                max_radius = "75%",
                                connect = NaN) {
    data <- data |>
        filter(
            # Status != "Total"
            department %in% c("eBK", "CS Contact", "Central File Approval", "Compliance Approval", "KYC", "Funded", "Traded")
        ) |>
        mutate(
            name = paste(
                department,
                n,
                sep = ","
            )
        )

    pie <- renderEcharts4r({
        data |>
            e_charts_(
                x_col,
            ) |>
            e_pie_(
                y_col,
                radius = c(min_radius, max_radius),
                label = list(
                    formatter = htmlwidgets::JS(
                        "function(params){
                                                    var vals = params.name.split(',')
                                                    return(vals[0])}"
                    )
                )
            ) |>
            e_legend(show = FALSE, ) |>
            e_title(
                isolate(selection)
            ) |>
            e_tooltip(formatter = htmlwidgets::JS("
                                                        function(params){
                                                        var vals = params.name.split(',')
                                                        return('<strong>' + vals[0] +
                                                        '</strong><br />' +  vals[1] + ' #' +
                                                        '</strong><br />' + params.value + ' %'
                                                        )   }  ")) |>
            e_group(
                connect
            ) |>
            e_connect_group(
                connect
            )
    })

    return(pie)
}


generate_pie <- function(data,
                         x_col,
                         y_col,
                         selection = "",
                         type = "", # "radius"
                         min_radius = "50%",
                         max_radius = "75%",
                         connect = NaN) {
    pie <- renderEcharts4r({
        req(data)
        data |>
            e_charts_(
                x_col,
            ) |>
            e_pie_(
                y_col,
                radius = c(min_radius, max_radius),
                roseType = type
            ) |>
            e_legend(show = FALSE, ) |>
            e_title(
                isolate(selection)
            ) |>
            e_tooltip() |>
            e_group(
                connect
            ) |>
            e_connect_group(
                connect
            )
    })

    return(pie)
}




build_cases_summary <- function(x, freq, lst_group_sel, lst_group_remove) {
  if (freq == "Daily") {
    x %>%
      select(-all_of(lst_group_remove)) %>%
      group_by_at(lst_group_sel) %>%
      summarise_all(~ sum(.x, na.rm = TRUE)) %>%
      ungroup()
  } else if (freq == "Weekly") {
    x %>%
      select(-all_of(lst_group_remove)) %>%
      mutate(Date = lubridate::ceiling_date(Date, "week") - 1) %>%
      group_by_at(lst_group_sel) %>%
      summarise_all(~ sum(.x, na.rm = TRUE)) %>%
      ungroup()
  } else if (freq == "Monthly") {

    x %>%
      select(-all_of(lst_group_remove)) %>%
      mutate(Date = lubridate::ceiling_date(Date, "month") - 1) %>%
      group_by_at(lst_group_sel) %>%
      summarise_all(~ sum(.x, na.rm = TRUE)) %>%
      ungroup()
  } else if (freq == "Quarterly") {
    x %>%
      select(-all_of(lst_group_remove)) %>%
      mutate(Date = lubridate::ceiling_date(Date, "quarter") - 1) %>%
      group_by_at(lst_group_sel) %>%
      summarise_all(~ sum(.x, na.rm = TRUE)) %>%
      ungroup()
  }
}



colorPalette = c(
  '#2ec7c9',
  '#b6a2de',
  '#5ab1ef',
  '#ffb980',
  '#d87a80',
  '#8d98b3',
  '#e5cf0d',
  '#97b552',
  '#95706d',
  '#dc69aa',
  '#07a2a4',
  '#9a7fd1',
  '#588dd5',
  '#f5994e',
  '#c05050',
  '#59678c',
  '#c9ab00',
  '#7eb00a',
  '#6f5553',
  '#c14089'
);


yearMonthDate <- htmlwidgets::JS('function (value) {
  var monthShortNames = ["Janvier", "Février", "Mars", "Avril", "Mai", "Juin",
    "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre"
  ];
  
  var d = new Date(value);
  var datestring =  monthShortNames[d.getMonth()] + "  "
  if (d.getMonth()===0) {
    var datestring =  d.getFullYear() + "  " + monthShortNames[d.getMonth()]  + "  "
    
  }
  
  
  return datestring
}')


yearOnly <- htmlwidgets::JS('function (value) {
  var d = new Date(value);
  var datestring = d.getFullYear()  + "  "
  return datestring
}')













#' Style My Workbook
#'
#'
#' @description
#' Apply a style to a given workbook
#'
#' @param wb workbook to apply the style
#' @param dt dataframe to put in the wb
#' @param SheetName name of the spreadsheet
#' @param alt_rows activate or not the color alternation
#' 
#'
#' @examples
#' \dontrun{
#' 
#' options(openxlsx.datetimeFormat = "yyyy-mm-dd")
#' wb <- openxlsx::createWorkbook()
#' style_my_workbook(wb,dt, "TestStyle", TRUE)
#' openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
#' }
#'
#' @return a workbook
#'
#' @importFrom openxlsx addWorksheet createStyle writeData addStyle setColWidths setRowHeights
#'

style_my_workbook <-function (wb, dt, SheetName, alt_rows = TRUE) 
{
    if (alt_rows == TRUE) {
        c_alt_row = "#F0F0F0"
    }
    else {
        c_alt_row = "white"
    }
    addWorksheet(wb, SheetName)
    cellStyle_even <- createStyle(numFmt = "##,##0.00", fontSize = 12, 
        fontColour = "black", halign = "left", valign = "center", 
        borderColour = "white", fgFill = c_alt_row)
    cellStyle_even_DATE <- createStyle(numFmt = "yyyy/mm/dd", fontSize = 12, 
        fontColour = "black", halign = "left", valign = "center", 
        borderColour = "white", fgFill = c_alt_row)
    cellStyle_odd <- createStyle(numFmt = "#,##0.00", fontSize = 12, 
        fontColour = "black", halign = "left", valign = "center", 
        borderColour = "white", fgFill = "white")
    cellStyle_odd_DATE <- createStyle(numFmt = "yyyy/mm/dd", fontSize = 12, 
        fontColour = "black", halign = "left", valign = "center", 
        borderColour = "white", fgFill = "white")
    headerStyle <- createStyle(fontSize = 14, fontColour = "white", 
        halign = "center", valign = "center", borderColour = "white", 
        fgFill = "#1D1861", textDecoration = "bold")
    width_vec <- apply(dt, 2, function(x) max(nchar(as.character(x)) + 
        9, na.rm = TRUE))

    width_vec_header <- nchar(colnames(dt)) + 15
    max_vec_header <- pmax(width_vec, width_vec_header)
    nr <- nrow(dt) + 1
    nc <- ncol(dt)
    rowsn <- 2:nr
    even <- list()
    odd <- list()

    for (i in rowsn) {
        if (i%%2 == 1) {
            odd <- append(odd, i)
        }
        else {
            even <- append(even, i)
        }
    }

    odd <- unlist(odd)
    even <- unlist(even)
   
    writeData(wb, sheet = SheetName, x = dt, withFilter = TRUE)
    addStyle(wb, sheet = SheetName, headerStyle, rows = 1:2, 
        cols = 1:ncol(dt), gridExpand = TRUE)

    x <- sapply(dt, lubridate::is.timepoint)

    if (sum(x) == 0) {

        addStyle(wb, sheet = SheetName, cellStyle_odd, rows = odd, 
            cols = 1:nc, gridExpand = TRUE)
        addStyle(wb, sheet = SheetName, cellStyle_even, rows = even, 
            cols = 1:nc, gridExpand = TRUE)
    }
    else {

        addStyle(wb, sheet = SheetName, cellStyle_odd, rows = odd, 
            cols = 1:nc, gridExpand = TRUE)
        addStyle(wb, sheet = SheetName, cellStyle_even, rows = even, 
            cols = 1:nc, gridExpand = TRUE)


        for (i in colnames(dt[x]) ) {
            idx <- grep(i, colnames(dt)) 
            addStyle(wb, sheet = SheetName, cellStyle_even_DATE, 
                rows = even, cols = idx, gridExpand = TRUE)
            addStyle(wb, sheet = SheetName, cellStyle_odd_DATE, rows = odd, 
            cols = idx, gridExpand = TRUE)
        }
    }
    setColWidths(wb, sheet = SheetName, cols = 1:nc, widths = max_vec_header)
    setRowHeights(wb, sheet = SheetName, 1, 40)
    return(wb)
}
