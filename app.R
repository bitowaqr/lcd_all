library(shiny)
library(jsonlite)
library(shinyWidgets)
library(shinyjs)
library(highcharter)
library(xts)
library(bs4Dash)
library(shinycssloaders)
library(waiter)

source("./utils.R",local = T)
source("./covidApi.R",local = T)
source("./elements.R",local = T)
source("./highCharter.R", local = T)

rki_ref <<- read.csv("rki_ref.csv")
rki_ref <<- rki_ref[order(rki_ref$GEN),]
lkList <<- as.list(rki_ref$county)
lkList2 <<- rki_ref$GEN
names(lkList) <<- rki_ref$GEN


# GLOBAL OPTIONS
options(scipen = 99)
# Sys.setlocale("LC_TIME", "de_DE.UTF-8")
options(highcharter.lang = germanLabels(getOption("highcharter.lang")))



# dashboard 1: medium, no map
db_compact = function(session){

    list(
        covBox(id = "test",
                fluidRow(
                    valueBoxOutput("b_newcases", width = 3),
                    valueBoxOutput("b_7inc_nchange", width = 3),
                    valueBoxOutput("b_current_infected", width = 2),
                    valueBoxOutput("b_cases_total", width = 2),
                    valueBoxOutput("b_deaths_total", width = 2)),
                    fluidRow(

                     tabBox(
                        id  = "tabBox1", headerBorder = T,# collapsible = F,closable = F,
                        width = 12,solidHeader = T,status = "primary",
                        bs4TabPanel(
                            tabName = "Fallzahlen", active = T,
                                highchartOutput("hc_cases", height = 290, width = "100%")
                        ),
                        bs4TabPanel(
                            tabName = "7-Tages-Inzidenz", active = F,
                                highchartOutput("hc_inc", height = 290, width = "100%")
                        ),
                        bs4TabPanel(
                            tabName = "Fälle nach Altersgrupen", active = F,
                                highchartOutput("hc_cases_by_age", height = 290, width = "100%")
                        ),
                        bs4TabPanel(
                            tabName = "Todasfälle nach Altersgrupen", active = F,
                                highchartOutput("hc_deaths_by_age", height = 290, width = "100%")
                        )
                    )
                )
            )


        )
    }



covBox <- function(...){
   bs4Dash::box(
        title = HTML(paste0("COVID-19 in <b>", textOutput("meine_stadt_out", inline=T),"</b>: Aktuelle Lage" )),
        status = "primary",
        width = 12,
        solidHeader = F,
        closable = F,
        footer =
            div(style = "display: flex;justify-content: space-between; padding: 5px;",
                div("Datenstand: ",textOutput("last_update",inline = T),"- Robert Koch-Institut.", style = "margin-left: 10px; display: inline-block;"),
                div(class= "contact",icon("copyright"),HTML('<a href="https://github.com/bitowaqr/" target= "_blank">Paul Schneider</a>'), style = "font-weight:350; font-size: 75%; display: inline-block; margin-top:5px;")
        ),
        maximizable = T,
        labelText = "Vollbild",
        collapsible = F,
        ...
        )
}


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



# UI --------------

ui <-  bs4DashPage(

    sidebar_mini = F,
    sidebar_collapsed = FALSE,
    controlbar_collapsed = T,
    controlbar_overlay = T,

     navbar =  bs4DashNavbar(
            skin = "dark",
            border = F,
            sidebarIcon = NULL,
            controlbarIcon = "palette"
            ),

    # SIDEBAR  -------------------------
    sidebar = bs4DashSidebar(
        title = "COVID-19 Dashboard",
        src = "dpa2.png",
        br(),

       bs4SidebarMenu(
           id = "sidebar",compact = F,

        
            br(),
           div(
               "LANDKREIS AUSWÄHLEN",
               style = "text-align:center;margin-bottom:-10px; font-size:120%; color: white;"
               ),
               selectizeInput(
                   "meine_stadt",
                    "",
                    c(  "Suche nach Landkreis" = "",
                        lkList)
               ),

           br(),
           div(
               style = "margin-left: 5px;",
                materialSwitch("theme_selector",label="Blue theme",status = "primary",inline =T)
           ),
           br(),
        
           div(
               "Kontakt",
               style = "margin-left:10px;margin-bottom:5px; font-size:120%; color: white;"
            ),
            div(class = "contact",
                style = "margin-left: 20px; margin-bottom: 70px; font-color:cyan; font-weight:450;",
                HTML('<a href="mailto:p.schneider@darkpeakanalytics.com">Paul Schneider</a>'),
                div(
                    HTML('<a href="http://darkpeakanalytics.com/" target= "_blank">Dark Peak Analytics</a>'),

                )
            )

       )
    ),
    bs4DashBody(

    # PREAMBLE + CSS STLYING  ----------------
        useShinyjs(),
        if(file.exists("google-analytics.html")) tags$head(includeHTML(("google-analytics.html"))),
        includeCSS("lcd2020.css"),
        use_waiter(), # loader
        waiter_show_on_load(html = spin_solar(), logo = "dpa2.png"),
        tags$script(HTML("setInterval(function(){ $('[title]').tooltip(); }, 1000)")),
        tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);"),
        tags$style(
            type = "text/css",
            "
        #hc_inc {
            border-radius: 20px !important;
        }
        .fa-eye{
            color:black !important;
        }
        .card-comment{
            color:black;
            overflow:hidden;
            max-height: 500px !important;
        }
        .card-footer.card-comments{
            max-height: 300px !important;
        }
        .content {
            margin-bottom:100px;
        }
        #comment_box {
            height: 0 !important;
            display: none;
        }
        .loader{
            color: var(--primary) !important;
        }
        .brand-text.font-weight-light {
            color: white;
            font-weight: 350;
        }
        .fa-palette{
            font-size:130%;
        }
        .card-footer{
            padding: 1px;
            vertical-align:center;
        }
        .btn.radiobtn.btn-primary {
            text-align:left;
            padding-left:12px;
            border-radius:7px;
            background-color:#333A40;
            border: 0;
        }
        .btn.radiobtn.btn-primary.focus.active, .btn.radiobtn.btn-primary.focus, .btn.radiobtn.btn-primary.active {
            background-color: var(--primary) !important;
        }
        #help {
            width: 70%;
            background-color: var(--primary);
        }
        .contact a {
            color: var(--primary) !important;
        }
        .contact a:hover {
            text-decoration: underline;
            color: orange !important;
        }

        .modal-content {
            border-radius: 30px;
            margin-top:200px;
        }
        .modal-body {
            padding:20px;
        }
        #close_modal {
            background-color: var(--primary);
        }
        .h1,.h2,h3,h4,h5 {
            color: var(--text_light) !important;
        }
        .bg-primary {
            color: var(--text_light) !important;
        }
        .selectize-input.input-active, .selectize-input.input-active:hover, .selectize-control.multi .selectize-input.focus {border-color: #2196f3 !important;}
        .selectize-dropdown .active {background: var(--primary) !important;}
        "),

        br(), br(),
        column(id = "main_ui",
            offset = 0, width = 12,
            withSpinner(
                type = 6, # color = "var(--primary)",
                uiOutput("dashboard_panel")
            )
        )
    )
)


# SERVER ------------------------------------
server <- function(input, output, session){

lk <- reactive({
    input$meine_stadt
})

# Waiter --------
waiter_hide()

# LOAD DATA ------------------------------------------------------------
check_interval_ms <- 1000 * 60 * 60 # every 60 mins
query_interval_s <- 1 * 60 * 60 # every 60 mins



rki_data <- eventReactive(input$meine_stadt,{

    rki = covidApi(lk())
    rki_now = covidNow(lk())
    rki_age = covidApi(lk() , return_cases_by_age = T)

    res = list(
        "rki" = rki,
        "rki_now" = rki_now,
        "rki_age" = rki_age
    )

    return(res)
} )




# # Modal ------------------------------------------------------------
    observeEvent(input$help,ignoreNULL = F,{
        showModal(
            modalDialog(
                title = NULL,

                div(
                    style = "text-align:left; margin-top:10px; margin-left:20px;",
                    h2("COVID-19 Dashboard")
                ),

                column(offset = 1,width =10,align="center",
                        br(),
                        br(),
                    span(
                        style = "color: var(--primary) !important; font-size:130%; font-weight:400;",
                        "Wähle Deine Stadt bzw. Deinen Landkreis aus um ein lokales COVID-19 Datendashboard zu erzeugen."
                        ),
                                selectizeInput(
                                    "meine_stadt_alt",
                                    "",
                                    c(
                                        "Suche nach Landkreis oder wähle aus der Liste" = "",
                                        lkList
                                    ),
                                    # options = list(maxItems = 10, closeAfterSelect = TRUE),
                                    width = "75%"
                                )
                    ),
                    div(
                    style = "font-size:90%; width: 90%;margin-left:20px;",
                    HTML("<blockquote> <i>„A dashboard is a visual display of the most important
                    information needed to achieve one or more objectives;
                    consolidated and arranged on a single screen so the
                    information can be monitored at a glance.</i>“<br>
                    - Stephen Few
                    </blockquote>")
                ),
                div(class = "contact",
                hr(),
                    style = "margin-left: 20px; margin-bottom: 20px; font-color:cyan; font-weight:450;",
                    HTML('Kontakt: <a href="mailto:p.schneider@darkpeakanalytics.com">Paul Schneider</a>')
                    ),
                  #  div(actionBttn("close_modal", "Fenster schließen", style = "jelly", color = "primary"), style = "text-align:center"),
                footer = NULL,
                size = "l",
                easyClose = F
            )
        )
    })

# meine stadt text input
output$meine_stadt_out <- renderText({
    lk()
})
output$meine_stadt_out2 <- renderText({
    input$meine_stadt
})


# last update text input
output$last_update <- renderText({attributes(rki_data()$rki)$Datenstand})



observeEvent(input$meine_stadt_alt,{
    removeModal()
    print(input$meine_stadt_alt)
    updateSelectizeInput(session,inputId = "meine_stadt",selected = input$meine_stadt_alt)
})


observe({
    

    if(input$theme_selector){
        col_theme = list(col_primary = "#188BBC",col_text = "#ECF0F1",col_bg = "#FFFFFF",col_boxbg = "#ECF0F1")
    } else {
        col_theme = list(col_primary = "#EB6864",col_text = "#ECF0F1",col_bg = "#FFFFFF",col_boxbg = "#D5E4EB")
    }
        

    runjs(paste0('document.documentElement.style.setProperty("--primary", "', col_theme[[1]], '");'))
    runjs(paste0('document.documentElement.style.setProperty("--text_light", "', col_theme[[2]], '");'))
    runjs(paste0('document.documentElement.style.setProperty("--bg", "', col_theme[[3]], '");'))
    runjs(paste0('document.documentElement.style.setProperty("--white", "', col_theme[[4]], '");'))


})





# DB SWITCH----------------
output$dashboard_panel <- renderUI({

    if(lk() == ""){
        NULL
    } else {
        db_compact(session)
    }


})

## VALUE BOXES ------------------------------------

# neue fälle + highcart box
output$b_newcases_hc <- renderbs4ValueBox({
    v3NewCaseHc(rki_data()$rki, hc_height = 137, line_col = input$col_text)
})

# inc7 + highchart
output$b_inc7_hc <- renderbs4ValueBox({
    v3Inc7Hc(rki_data()$rki,rki_data()$rki_now, hc_height = 137, line_col = input$col_text)
})


# neue fälle box
output$b_newcases <- renderbs4ValueBox({
    v3NewCase(rki_data()$rki)
})

# infizierte
output$b_current_infected <- renderbs4ValueBox({
    v2Infected(rki_data()$rki$cases, info = "Geschätzt unter der Annahme das alle Infizierten nach 14 Tagen genesen sind.")
})

# Fälle insgesamt
output$b_cases_total <- renderbs4ValueBox({
    v2Ever(rki_data()$rki$cases,tag = "Covid-19 Fälle insgesamt",info =  "Gesamtzahl aller Covid-19 Fälle seit März 2020")
})

# Fälle insgesamt
output$b_deaths_total <- renderbs4ValueBox({
    v2Ever(rki_data()$rki$deaths, "Todesfälle insgesamt", info = "Verstorben mit oder an COVID-19", sub = HTML("&nbsp"))
})

# 7 day incidence today
output$b_7inc_nchange <- renderbs4ValueBox({
    v2Inc7(rki_data()$rki, rki_data()$rki_now)
})


# Highchart 7 DAY INCIDENCE ------------------
output$hc_inc <- renderHighchart({

    inc_df = inci7day(rki_data()$rki$cases,rki_data()$rki$date,rki_data()$rki_now$EWZ)
    highLiner(
        inc_df$inc7,
        inc_df$date,
        title = NULL,#"Verlauf 7-Tages-Inzidenz",
        height = 280,
        start_view = 1,
        show_dots = F,
        line_col = NULL,
        theme = hc_theme_default()
    )
})

# Highchart CASE NUMBERS ------------------
output$hc_cases <- renderHighchart({

    highStocker(
        rki_data()$rki$cases,
        rki_data()$rki$date,
        title = NULL,#"Covid-19 Fälle",
        height = 280,
        start_view = 1,
        theme = hc_theme_default()
        )
})


# Highchart cases by age ------------------
output$hc_cases_by_age <- renderHighchart({

    highColumner(
        rki_data()$rki_age$cases,
        rki_data()$rki_age$age,
        title = NULL,#"Covid-19 Fälle",
        height = 280,
        theme = hc_theme_default()
        )
})


# Highchart deaths by age ------------------
output$hc_deaths_by_age <- renderHighchart({

    highColumner(
        rki_data()$rki_age$deaths,
        rki_data()$rki_age$age,
        title = NULL,#"Covid-19 Fälle",
        y_lab = "Todesfälle",
        height = 280,
        theme = hc_theme_default()
        )
})





}

# -------------------------
shinyApp(ui,server)

