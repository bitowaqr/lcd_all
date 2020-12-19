# # query rki data set - api via arcgis
    # # 
    # # data reference
    # https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0
    # #
    # # api
    # https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0/geoservice
    # # 
    # # non-helpful api docu
    # https://developers.arcgis.com/rest/services-reference/query-feature-service-layer-.htm
    # # 
    # # note: 'Bei der Darstellung der Neuinfektionen pro Tag wird das Meldedatum verwendet'
    # #  ref: https://svn.vsp.tu-berlin.de/repos/public-svn/matsim/scenarios/countries/de/episim/original-data/Fallzahlen/RKI/raw-data/RKI_COVID19_IMPORTANT_documentation.txt

covidNow <- function(lk){
    
    require(jsonlite)

    lk = toupper(lk) # format lk string
    lk = gsub(" ", "%20", lk)
    query_str = paste0("https://services7.arcgis.com/mOBPykOjAyBO2ZKk/arcgis/rest/services/RKI_Landkreisdaten/FeatureServer/0/query?where=county%20%3D%20'",lk,"'&outFields=OBJECTID,death_rate,cases,deaths,cases_per_100k,cases_per_population,county,last_update,cases7_per_100k,recovered,EWZ,cases7_bl_per_100k,GEN&outSR=4326&f=json")
    res = fromJSON(query_str)$features$attributes
    if (is.null(res)) {
        return(NULL)
    }
    return(res)
}
# rki_now <- covidNow("sk dortmund")

covidApi <-  function(
    lk = "sk bochum", 
    date_first = "2020-01-01", 
    date_last = Sys.Date()-1, 
    check_uptodate = F,
    return_cases_by_age = F){

        # ---------------------------
        # NOTE:                     |
        #   USES MELDEDATUM !       |
        #   REF DATUM NOT IN USE !! |
        # ---------------------------

    require(jsonlite)
    # Sys.setenv(TZ='CET')  # needed???

    # utility function to convert weird date format from API to R date
    getRDate = function(api_date){
        api_date = api_date/1000
        class(api_date) <- c("POSIXt", "POSIXct")
        api_date <- as.Date(api_date,tz = "CET")
        return(api_date)
        }
    
    lk = toupper(lk) # format lk string
    lk = gsub(" ", "%20", lk)
    
    fields_out = paste(
        "IdBundesland", "Bundesland", "Landkreis", "Geschlecht", "AnzahlFall",
        "AnzahlTodesfall", "ObjectId", "Meldedatum", "IdLandkreis", "Datenstand",
        "NeuerFall", "NeuerTodesfall", "Refdatum", "NeuGenesen", "AnzahlGenesen",
        "IstErkrankungsbeginn", "Altersgruppe",
        sep = ","
    )

    query_str = paste0("https://services7.arcgis.com/mOBPykOjAyBO2ZKk/arcgis/rest/services/RKI_COVID19/FeatureServer/0/query?where=Refdatum%20%3E%3D%20TIMESTAMP%20'", date_first, "%2000%3A00%3A00'%20AND%20Refdatum%20%3C%3D%20TIMESTAMP%20'", date_last, "%2000%3A00%3A00'%20AND%20Landkreis%20%3D%20'", lk, "'&outFields=", fields_out, "&outSR=4326&f=json")
    res_raw = fromJSON(query_str)

    # stop if no results
    if (is.null(res_raw$features$attributes)) {
        return(NULL)
    }

    # write to res
    res = res_raw$features$attributes
    res$Meldedatum <- getRDate(res$Meldedatum)

    while ("exceededTransferLimit" %in% names(res_raw)) {
        print("exceededTransferLimit - sending another query")
        last_data_return = max(res$Meldedatum)
        res = res[res$Meldedatum < last_data_return, ]
        new_start_date = last_data_return
        repeated_query_str = paste0("https://services7.arcgis.com/mOBPykOjAyBO2ZKk/arcgis/rest/services/RKI_COVID19/FeatureServer/0/query?where=Refdatum%20%3E%3D%20TIMESTAMP%20'", new_start_date, "%2000%3A00%3A00'%20AND%20Refdatum%20%3C%3D%20TIMESTAMP%20'", date_last, "%2000%3A00%3A00'%20AND%20Landkreis%20%3D%20'", lk, "'&outFields=", fields_out, "&outSR=4326&f=json")
        res_raw = fromJSON(repeated_query_str)

        # stop if no results
        if (is.null(res_raw)) {
            stop("Repeated request not resolved")
        }

        temp = res_raw$features$attributes
        temp$Meldedatum <- getRDate(temp$Meldedatum)

        res <- rbind(res, temp)
    }
    
    if(check_uptodate){
        print("Datenstand")
        print(unique(res$Datenstand))
        updated = as.POSIXct(unique(res$Datenstand), format = "%d.%m.%Y, %H:%M")
        if(updated < Sys.Date()){
            stop("Outdated data")
        }
    }
    # Referenzdatum: Erkrankungsdatum, wenn bekannt, sonst Meldedatum
    # res$Refdatum <- getRDate(res$Refdatum) 
    # res$Meldedatum <- getRDate(res$Meldedatum)
    
    # total cases and deaths by age
    cases_by_age = aggregate(AnzahlFall ~ Altersgruppe, res, sum)
    deaths_by_age = aggregate(AnzahlTodesfall ~ Altersgruppe, res, sum)
    events_by_age = merge(cases_by_age, deaths_by_age, by = "Altersgruppe", all = T)
    events_by_age[is.na(events_by_age)] = 0
    names(events_by_age) = c("age","cases","deaths")
    if(return_cases_by_age){
        return(events_by_age)
    }
    
    # total cases by sex
    # cases_by_sex = aggregate(AnzahlFall ~  Geschlecht, res, sum)

    # correct -1 data entries ???
    res$AnzahlTodesfall[res$AnzahlTodesfall < 0] <- 0 ### ?????
    res$AnzahlGenesen[res$AnzahlGenesen < 0] <- 0 ### ?????
    res$AnzahlFall[res$AnzahlFall < 0] <- 0 ### ?????
    
    # aggregate data across ages + sex by MELDEDATUM 
    cases  = aggregate(AnzahlFall ~ Meldedatum,res,sum)
    names(cases) = c("date","cases") 
    cured = aggregate(AnzahlGenesen ~ Meldedatum, res, sum)
    names(cured) = c("date","cured") 
    deaths = aggregate(AnzahlTodesfall ~ Meldedatum, res, sum)
    names(deaths) = c("date","deaths") 
    
    tsd = data.frame(date = as.Date(as.Date(date_first):max(res$Meldedatum), origin = "1970-01-01"))
    tsd = Reduce(function(x, y) merge(x, y, all=TRUE), list(tsd, cases,cured, deaths))
    tsd[is.na(tsd)] = 0 # replace missing with 0

    
    attributes(tsd)$Datenstand <- unique(res$Datenstand)

    return(tsd)
    
}
# rki <- covidApi(lk = "sk dortmund")
# rki_age <- covidApi("sk dortmund",return_cases_by_age = T)

# rki_now = covidNow("sk dortmund")

rkiAllNow = function(){
    require(jsonlite)
    query_str = paste0("https://services7.arcgis.com/mOBPykOjAyBO2ZKk/arcgis/rest/services/RKI_Landkreisdaten/FeatureServer/0/query?where=1%3D1&outFields=OBJECTID,GEN,EWZ,Shape__Area,Shape__Length,death_rate,cases,deaths,cases_per_100k,cases_per_population,BL_ID,county,last_update,cases7_per_100k,recovered,EWZ_BL,cases7_bl_per_100k&outSR=4326&f=json")
    res = fromJSON(query_str)
    res_df = res$features$attributes
    return(res_df)
}

# test = rkiAllNow()
# grep("Stuttgart", test$GEN)
# test[179,]

# rki_ref = test
# write.csv(rki_ref,"rki_ref.csv",row.names = F)


