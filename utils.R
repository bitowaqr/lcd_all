# utils

# load local data to object
loadLocal <- function(file){
    env <- new.env()
    var <- load(file = file, envir = env)
    res <- get(var, env)
    return(res)
}

# change highchart labels to german
germanLabels = function(hcoptslang){
    hcoptslang$months <- c("Januar", "Februar","März","April","Mai","Juni","Juli" ,"August", "September", "Oktober","November", "Dezember" )
    hcoptslang$shortMonths <- c("Jan.", "Feb.", "Mär.", "Apr.", "Mai.", "Jun.", "Jul.", "Aug.", "Sep.", "Okt.", "Nov.", "Dez.")
    hcoptslang$weekdays <- c("Sonntag", "Montag", "Dienstag","Mittwoch","Donnerstag","Freitag","Samstag")
    hcoptslang$decimalPoint <- ","
    return(hcoptslang)
}

# trim data to the last x days
ttrim <- function(tsdf,horizon){
    if(!("date" %in% names(tsdf))){
        stop("tsdf has no var date")
    }
    tsdf_trimmed = tsdf[tsdf$date > max(tsdf$date) - horizon, ]
    return(tsdf_trimmed)
}
# ttrim(tdsd,7)

# utility fun to calculate 7 day incidence
    inci7day = function(cases, dates, ewz, horizon = NULL){
        len = length(cases)
        if (is.null(horizon)) {
            horizon <- 1:len
        } else {
            horizon <- (len - horizon + 1):len
        }
        cases <- cases[horizon]
        dates <- dates[horizon]

        inc = c()
        for (i in 1:(length(dates) - 6)) {
            inc = c(inc, (sum(cases[0:6 + i]) / ewz) * 100000)
        }
        inc_df = data.frame(date = dates[-c(1:6)],inc7 = inc)
        return(inc_df)
    }





########## ------------------------------------------
# SHINY UI HELPER
########## ------------------------------------------

# make numbers pretty
nicer = function(x,dec = 0, sign = F){
 str = formatC(x, digits = dec, big.mark = ".", decimal.mark = ",", format = "f")
 if(sign){
   str = ifelse(x > 0,paste0("+",str),str)
 }
 return(str)
}

# string for better/worse/same changes
diffEvalTxter <- function(now,was,was_date, icon = F){
    was_weekday = weekdays(as.Date(was_date))
    diff = now - was
    
    str_change = ifelse(
        diff > 0,
        paste0(if(icon){"&uarr; "},"+", diff, " zum Vortag"),
        ifelse(diff == 0,
            paste0(if(icon){"&rarr; "}," Unverändert zum Vortag"),
            paste0(if (icon) {"&darr; "}, diff, " zum Vortag")
        )
    )
    return(str_change)
}

iconEval = function(val1, val2){
    if (val1 > val2) {
        return(icon("arrow-up"))
    }
    if (val1 < val2) {
        return(icon("arrow-down"))
    }
    if (val1 > val2) {
        return("\2205")
    }
}

# fix the icon in sideboard 1
ic <-function(str){
  div(str, style = "
    width: 22px;
    line-height: 18px;
    border-radius: 50%;
    text-align: center;
    font-size: 18px;
    border: 2px solid var(--primary);
    display: inline-block;
    ")
}

# fix the icon in sideboard 2
bs4SidebarMenuItem <- function (text, ..., tabName = NULL, icon = NULL, startExpanded = FALSE) 
{
    subitems <- list(...)
    if (length(subitems) == 0) {
        return(shiny::tags$li(class = "nav-item", shiny::tags$a(class = "nav-link", 
            id = paste0("tab-", tabName), href = paste0("#shiny-tab-", 
                tabName), `data-toggle` = "tab", `data-value` = tabName, 
                icon, #shiny::tags$i(class = paste0("nav-icon fa fa-", icon)), 
            shiny::tags$p(text))))
    }
    else {
        menuItemCl <- "nav-item has-treeview"
        if (startExpanded) 
            menuItemCl <- paste0(menuItemCl, " menu-open")
        shiny::tags$li(class = menuItemCl, shiny::tags$a(href = "#", 
            class = "nav-link", shiny::tags$i(class = paste0("nav-icon fas fa-", 
                icon)), shiny::tags$p(text, shiny::tags$i(class = "right fas fa-angle-left"))), 
            shiny::tags$ul(class = "nav nav-treeview", ...))
    }
}



# BOX 3 -----------------------------
valueBox3 <- function(value, title, sparkobj = NULL, subtitle, info = NULL, 
                          icon = NULL, color = "aqua", width = 4, href = NULL){
  
  # shinydashboard:::validateColor(color)
  
  if (!is.null(icon))
    shinydashboard:::tagAssert(icon, type = "i")
  
  info_icon <- tags$small(
    tags$i(
      class = "fa fa-info-circle fa-lg",
      title = info,
      `data-toggle` = "tooltip",
      style = "color: rgba(255, 255, 255, 0.75);"
      ),
    # bs3 pull-right 
    # bs4 float-right
    class = "pull-right float-right"
    )
  
  boxContent <- div(
    class = paste0("small-box bg-", color),
    div(
      class = "inner",
      tags$small(title),
      if (!is.null(info)) info_icon,
      h3(value),
      if (!is.null(sparkobj)) sparkobj,
      tags$small(subtitle)
      ),
    # bs3 icon-large
    # bs4 icon
    if (!is.null(icon)) div(class = "icon-large icon", icon, style = "z-index; 0")
    )
  
  if (!is.null(href)) 
    boxContent <- a(href = href, boxContent)
    
  div(
    class = if (!is.null(width)) paste0("col-sm-", width), 
    boxContent
    )
}






# HC SPARKLINE FOR BOX 3 -----------------------------
hc_theme_sparkline_vb <- function(...) {
  
  theme <- list(
    chart = list(
      backgroundColor = NULL,
      margins = c(0, 0, 0, 0),
      spacingTop = 0,
      spacingRight = 0,
      spacingBottom = 10,
      spacingLeft = 0,
      plotBorderWidth = 0,
      borderWidth = 0,
      style = list(overflow = "visible")
    ),
    xAxis = list(
      visible = F, 
      endOnTick = FALSE, 
      startOnTick = FALSE
    ),
    yAxis = list(
      visible = T,
      endOnTick = F, 
      startOnTick = T,
      labels = list(
        style = list(
            color = "white"
        )          
      )
    ),
    tooltip = list(
      outside = FALSE,
      shadow = F,
      borderColor = "transparent",
      botderWidth = 0,
      bodyFontColor = "white",
      backgroundColor = "transparent",
      style = list( color = "white", fontshadow = "red")
    ),
    plotOptions = list(
      series = list(
        marker = list(enabled = FALSE),
        lineWidth = 2,
        shadow = FALSE,
        fillOpacity = 0.25,
        color = "#FFFFFFBF",
        fillColor = list(
          linearGradient = list(x1 = 0, y1 = 1, x2 = 0, y2 = 0),
          stops = list(
            list(0.00, "#FFFFFF00"),
            list(0.50, "#FFFFFF7F"),
            list(1.00, "#FFFFFFFF")
          )
        )
      )
    ),
    credits = list(
      enabled = FALSE,
      text = ""
      )
    )
  
  theme <- structure(theme, class = "hc_theme")
  
  if (length(list(...)) > 0) {
    theme <- hc_theme_merge(
      theme,
      hc_theme(...)
    )
  }
  
  theme
}