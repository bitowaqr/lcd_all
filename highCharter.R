# hc default theme -------------------------
hc_theme_default <- function(){
    hc_theme(
        chart = list(
            backgroundColor = "transparent"
        ),
        stock = list(
            backgroundColor = "transparent"
        )
    )
}


# high liner -------------------------

highLiner <- function(
    incidence,
    dates,
    line_col = "#53858a",
    y_lab = "7-Tages-Inzidenz",
    title_text = "<b>7-Tages-Inzidenz</b>",
    show_nav = T,
    show_dots = T,
    show_labels = F,
    show_rangeSel = T,
    start_view = 1,
    exporting = F,
    height = 350,
    nav_height = 40,
    theme = hc_theme_google(), #hc_theme_superheroes(), #hc_theme_alone(),
    nav_margin = 10) {

require(xts)
require(highcharter)

df_xts <- xts(round(incidence, 1), order.by = dates)
max_inc = max(incidence)

highchart(type = "stock") %>%
    hc_add_theme(theme) %>%
    # hc_chart(borderRadius = "10px") %>%
    hc_add_series(df_xts, type = "line", name = "", color = line_col) %>%
    hc_title(text = title_text, align = "left", margin = 0) %>%
    hc_xAxis(
        type = "datetime",
        gridLineWidth = 1
        # dateTimeLabelFormats = list(day = '%d of %b'),
        #title = list(text = "x Axis at top"),
        # alternateGridColor = "#FDFFD5",
        # tickInterval = 24 * 3600 * 1000 * 1
        # minorTickInterval = 50
    ) %>%
    hc_add_yAxis(
            title = list(text = y_lab),
            opposite = T,
            min = 0,
            max = max_inc,
            # tickInterval = 25,
            # minorTickInterval = NULL,
            plotLines = list(
                list(
                    dashStyle = "ShortDash",
                    color = "darkorange",
                    width = 1.5,
                    value = 50
                ),
                list(
                    dashStyle = "ShortDash",
                    color = "darkred",
                    width = 1.5,
                    value = 200
                )
            )
        ) %>%
        hc_plotOptions(
            series = list(
                dataGrouping = list(
                    enabled = F
                ),
                dataLabels = list(
                    enabled = show_labels
                ),
                marker = list(
                    enabled = ifelse(show_dots, "undefined", F),
                    symbol = "circle",
                    radius = 4,
                    lineWidth = 0,
                    enabledThreshold = 2
                )
            )
        ) %>%
        hc_size(height = height) %>%
        hc_tooltip(
            pointFormat = "{point.y: .2f}"
        ) %>%
        hc_navigator(
            outlineColor = "gray",
            enabled = show_nav,
            outlineWidth = 1,
            height = nav_height,
            margin = nav_margin,
            series = list(
                # color = line_col,
                lineWidth = 2,
                type = "line"#,   # you can change the type
                # fillColor = "white"
            ) # ,
            #  handles = list(
            #      backgroundColor = "yellow",
            #      borderColor = "red"
            #  )
        ) %>%
        hc_rangeSelector(
            labelStyle = list(display = "none"),
            enabled = show_rangeSel,
            inputEnabled = F,
            # verticalAlign = "bottom",
            buttons = list(
                list(type = "all", text = "Seit März 2020"),
                list(type = "month", count = 1, text = "Dieser Monat"),
                list(type = "week", count = 1, text = "Diese Woche")
            ),
            buttonTheme = list(
                r = 4,
                width = "120px"
            ),
            buttonSpacing = 10,
            inputDateFormat = "%e.%m.%Y",
            selected = start_view
        ) %>%
        hc_exporting(enabled = exporting) %>%
        hc_scrollbar(enabled = F)
 }



# inc_df = inci7day(rki$cases,rki$date,rki_now$EWZ)
# highLiner(
#     inc_df$inc7,
#     inc_df$date,
#     show_dots = F,title_text = "NULL",
#     show_nav = T,
#     show_labels = F,
#     show_rangeSel = T,
#     height = 300, nav_height = 40, nav_margin = 5
# )






# HIGH STOCKER 1 ----------------------

highStocker <- function(
                        cases,
                        dates,
                        col_bar = "#3575ae",
                        y_lab = "Neue Fälle",
                        title_text = "<b>Neue Covid Fälle</b>",
                        height = 350,
                        theme = hc_theme_google(), 
                        start_view = 1,
                        nav_height = 40,
                        nav_margin = 10) {
    df_xts <- xts(cases, order.by = dates)

    highchart(type = "stock") %>%
        hc_add_theme(theme) %>%
        hc_add_series(df_xts, type = "column", name = "", color = col_bar) %>%
        hc_title(text = title_text, align = "left", margin = 10) %>%
        hc_add_yAxis(
            title = list(text = y_lab),
            opposite = TRUE# ,
            # tickInterval = 10,
            # minorTickInterval = 5 # ,
        ) %>%
        hc_plotOptions(
            column = list(
                dataGrouping = list(
                    # "enabled" = F
                    dateTimeLabelFormats = list(
                        week = list("Woche vom %d.%m.%Y", "%A, %b %e", "-%A, %b %e, %Y")
                    )
                ),
                groupPadding = .1,
                pointPadding = 0
            )
        ) %>%
        hc_size(height = height) %>%
        hc_tooltip(
            pointFormat = "{point.y: .0f}" # ,
            # dateTimeLabelFormats = list(week = "wf")
        ) %>%
        hc_rangeSelector(
            labelStyle = list(display = "none"),
            enabled = T,
            inputEnabled = F,
            buttons = list(
                list(type = "all", text = "Seit März 2020"),
                list(type = "month", count = 1, text = "Dieser Monat"),
                list(type = "week", count = 1, text = "Diese Woche")
            ),
            buttonTheme = list(
                r = 4,
                width = "120px"
            ),
            buttonSpacing = 10,
            inputDateFormat = "%e.%m.%Y",
            selected = start_view
        ) %>%
        hc_navigator(
            outlineColor = "gray",
            outlineWidth = 1,
            height = nav_height,
            margin = nav_margin,
            series = list(
                color = col_bar,
                lineWidth = 1,
                type = "column", # you can change the type
                fillColor = "white"
            ) # ,
            #  handles = list(
            #      backgroundColor = "yellow",
            #      borderColor = "red"
            #  )
        ) %>%
        hc_scrollbar(enabled = F)
}





# HIGH CLIUMNER ----------------------


# highStocker(rki_data$ref_df$AnzahlFall, rki_data$ref_df$Refdatum, height = 200, nav_height = 40, nav_margin = 5)

highColumner <- function(
                        cases,
                        categories,
                        y_lab = "Fallzahl",
                        x_lab = "Altersgruppe",
                        title_text = "<b>Fallzahlen nach Alter</b>",
                        theme = hc_theme_google(), 
                        height = 350) {

    df <- data.frame(categories,cases)
    df$categories <- gsub("A","",df$categories)
    df$categories <- gsub("00","0",df$categories)
    df$categories <- gsub("04","4",df$categories)
    df$categories <- gsub("05","5",df$categories)

    hchart(df, 'column', hcaes(x = categories, y = cases)) %>%
        hc_add_theme(theme) %>%
        # hc_title(text = title_text, align = "left", margin = 10) %>%
        hc_yAxis(
            opposite = T,
            title = list(text = y_lab)
        ) %>%
        hc_xAxis(
            title = list(text = x_lab)
        ) %>%
        hc_plotOptions(
            column = list(
                groupPadding = 0.1,
                pointPadding = 0
            )
        ) %>%
        # hc_size(height = height) %>%
        hc_tooltip(
            pointFormat = "{point.y: ,.0f}" # ,
            # dateTimeLabelFormats = list(week = "wf")
        )
        
}

