# Designs

# v3NewCaseHc
    # Freitag, 4.12.2020
    # neue fälle
    # 204
    # highchart neue fälle
    # 58 mehr als Donnerstag
 v3NewCaseHc = function(rki_data,hc_height = 150, color = "primary",info = NULL,line_col = "white") {

        # new cases
        rki_2d = ttrim(rki_data, 2)
        cases_latest = rki_2d$cases[2]
        date_latest = rki_2d$date[2]
        german_weekdays <- c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag","Sonntag")
        date_latest_weekday <- format(date_latest, format = "%u")
        date_latest_weekday <- german_weekdays[as.numeric(date_latest_weekday)]
        date_latest = format(date_latest, format = "%e.%m.%Y")
        date_latest = paste0(date_latest_weekday,", ",date_latest)
        # diff to last reporting
        diff_str = diffEvalTxter(rki_2d$cases[2], rki_2d$cases[1], rki_2d$date[1])
        my_icon = iconEval(rki_2d$cases[2],rki_2d$cases[1])

        # highchart 7 days
        rki_week = ttrim(rki_data, 14)
        hc <- hchart(
            rki_week, "line",
            hcaes(date, cases),
            color = line_col,
            name = "Neue Fälle"
        ) %>%
            hc_yAxis(min = 0, title =list(text = "")) %>%
            hc_size(height = hc_height) %>%
            hc_add_theme(hc_theme_sparkline_vb()) %>%
            hc_credits(enabled = FALSE)

        # putting all into a claue box
        v3 = valueBox3(
            value = toupper(nicer(cases_latest)),
            title = HTML(paste0("<b>", date_latest, "</b><br>", toupper("Neue Fälle "))),
            sparkobj = hc,
            subtitle = HTML(diff_str),
            icon = my_icon,
              info = info,

            color = color,
            href = NULL
        )

        return(v3)
}


# v3NewCase
    # Freitag, 4.12.2020
    # neue fälle
    # 204
    # 58 mehr als Donnerstag
 v3NewCase = function(rki_data, color = "primary",info = NULL) {

        # new cases
        rki_2d = ttrim(rki_data, 2)
        cases_latest = rki_2d$cases[2]
        date_latest = rki_2d$date[2]
        date_latest = format(date_latest, format = "%A, %e.%m.%Y")
        # diff to last reporting
        diff_str = diffEvalTxter(rki_2d$cases[2], rki_2d$cases[1], rki_2d$date[1])
        my_icon = iconEval(rki_2d$cases[2],rki_2d$cases[1])

        # putting all into a claue box
        v3 = valueBox3(
            value = HTML(paste0(toupper(nicer(cases_latest)),"", tags$small("  Neue Fälle "))),
            title = HTML(paste0("<b>", date_latest, "</b>")),
            subtitle = HTML(diff_str),
            icon = my_icon,
            info = info,
            color = color,
            href = NULL
        )

        return(v3)
}




# v3Inc7Hc
    # Freitag, 4.12.2020
    # 7tage inzidenz
    # 13.4
    # highchart inzidenz
    # +5% zum vortag
 v3Inc7Hc = function(rki,rki_now,hc_height = 150, color = "primary",info = NULL,line_col = "white") {

    incidence2 = inci7day(
        cases = rki$cases,
        dates = rki$date,
        ewz = rki_now$EWZ,
        horizon = 8
    )
        diff = (1 - (incidence2$inc7[1] / incidence2$inc7[2])) * 100
        # diff_day = weekdays.Date(incidence2$date[1])

        my_icon = iconEval(incidence2$inc7[2],incidence2$inc7[1])
        
        in7_latest = incidence2$inc7[2]
        
        date_latest = incidence2$date[2]
        date_latest = format(date_latest, format = "%A, %e.%m.%Y")
        
        incidence14 = inci7day(
        cases = rki$cases,
        dates = rki$date,
        ewz = rki_now$EWZ,
        horizon = 20
    )
        
        incidence14$inc7 = round(incidence14$inc7,2)
        # highchart 7incidence over 7 days
        hc <- hchart(
            incidence14, "line",
            hcaes(date, inc7),
            color = line_col,
            name = "7-Tage-Inzidenz"
        ) %>%
            hc_yAxis(min = 0, title =list(text = "")) %>%
            hc_size(height = hc_height) %>%
            hc_add_theme(hc_theme_sparkline_vb()) %>%
            hc_credits(enabled = FALSE)

        # putting all into a claue box
        v3 = valueBox3(
            value = toupper(nicer(in7_latest,1)),
            title = HTML(paste0("<b>", date_latest, "</b><br>", toupper("7-Tage-Inzidenz "))),
            sparkobj = hc,
            subtitle = paste0(nicer(diff, 1,sign = T), "% zum Vortag"),
            icon = my_icon,
            info = info,
            color = color,
            href = NULL
        )

        return(v3)
}



# v2Inc7
    # 7 Tages Inzident
    # 134,0
    # 1,8% seit Dienstag

v2Inc7 <- function(rki,rki_now, col = "primary", info = NULL){
    incidence2 = inci7day(
        cases = rki$cases,
        dates = rki$date,
        ewz = rki_now$EWZ,
        horizon = 8
    )
        diff = (1 - (incidence2$inc7[1] / incidence2$inc7[2])) * 100
        # diff_day = weekdays.Date(incidence2$date[1])

        my_icon = iconEval(incidence2$inc7[2],incidence2$inc7[1])

        v3 = valueBox3(
            value = nicer(incidence2$inc7[2], 1),
            title = tags$b("7-Tage-Inzidenz"),
            subtitle = paste0(nicer(diff, 1,sign = T), "% zum Vortag"),
            icon = my_icon,
            color = col,
            info = info
        )
        
        return(v3)
}

# fälle insgesamt
# 9.129
# seit März 2020

v2Ever = function(cases, tag = "Fälle insgesamt", sub = HTML("&nbsp"), col = "primary", info = NULL) {
    cases_ever = sum(cases)
    v2 = valueBox3(
        value = nicer(cases_ever, 0),
        title = tags$b(paste0(tag)),
        subtitle = sub,
        color = col,
        info = info
    )
    return(v2)
}


v2Infected = function(cases, t_recover = 14,col = "primary", info = NULL){
        # assuming recovery after 14 days,
        # i.e. recovered at day 15
        len = length(cases)
        infected = sum(cases) - sum(cases[-c(len:(len-t_recover))])
        
        v2 = valueBox3(
            value = nicer(infected, 0),
            title = HTML(paste0(tags$b("Aktuell infiziert"),tags$sup("*"))),
            subtitle = HTML(paste0(tags$small(tags$sup("*"), "geschätzt"))),
            color = col,
            info = info
        )
        return(v2)
    }