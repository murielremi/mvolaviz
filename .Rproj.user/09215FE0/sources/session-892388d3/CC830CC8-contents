valueUnite <- function(val){
  if(!length(val)){
    return(0)
  }
  if(is.na(val)){
    return("")
  }
  if(is.nan(val)){
    return("")
  }
  if(val<= 1000){
    return(paste0(val))
  }
  else if(val<= 1000000){
    return(paste0(round(val/1000,2)," K"))
  }
  else if(val<= 1000000000){
    return(paste0(round(val/1000000,2)," M"))
  }
  else{
    return(paste0(round(val/1000000000,2)," MDS"))
  }
}

#function
time_diff <- function(hours){
  minutes <- round(60 * (hours - floor(hours)), 0)
  minutes <- ifelse(minutes < 10, paste0('0',minutes),minutes)
  paste(floor(hours), minutes, sep=":")
}

#separatuer des millier
sepMillier<-function(x){
  if(is.na(as.numeric(x))) return(x)
  else return(format(as.numeric(x), format = "f", digits = 0, big.mark = " ", scientific=FALSE, trim=TRUE))
}

#get number of date in the month
numberOfDays <- function(date) {
  m <- format(date, format="%m")

  while (format(date, format="%m") == m) {
    date <- date + 1
  }

  return(as.integer(format(date - 1, format="%d")))
}

create_survival_data <- function(data,date_origine,grps){

  survival<- data[data$Mois_souscription == date_origine,]

  selected <- c(grps,c('souscrit','X01.01.2019','X01.02.2019','X01.03.2019','X01.04.2019','X01.05.2019',
                       'X01.06.2019','X01.07.2019','X01.08.2019','X01.09.2019','X01.10.2019','X01.11.2019','X01.12.2019'))

  survival <- survival %>% select(selected)

  survival <- survival %>%  group_by(grps = .[,c(1)])%>%
    summarise(souscrit = sum(souscrit,na.rm = T),X01.01.2019 = sum(X01.01.2019,na.rm = T), X01.02.2019 = sum(X01.02.2019,na.rm = T),
              X01.03.2019 = sum(X01.03.2019,na.rm = T),X01.04.2019 = sum(X01.04.2019,na.rm = T),X01.05.2019 = sum(X01.05.2019,na.rm = T),
              X01.06.2019 = sum(X01.06.2019,na.rm = T),X01.07.2019 = sum(X01.07.2019,na.rm = T),X01.08.2019 = sum(X01.08.2019,na.rm = T),
              X01.09.2019 = sum(X01.09.2019,na.rm = T),X01.10.2019 = sum(X01.10.2019,na.rm = T),X01.11.2019 = sum(X01.11.2019,na.rm = T),
              X01.12.2019 = sum(X01.12.2019,na.rm = T))

  survival <- survival%>%mutate(X01.01.2019 =   souscrit - X01.01.2019,X01.02.2019 = souscrit - X01.02.2019,
                                X01.03.2019 = souscrit - X01.03.2019,X01.04.2019 = souscrit - X01.04.2019,
                                X01.05.2019 = souscrit - X01.05.2019,X01.06.2019 = souscrit - X01.06.2019,
                                X01.07.2019 = souscrit - X01.07.2019,X01.08.2019 = souscrit - X01.08.2019,
                                X01.09.2019 = souscrit - X01.09.2019,X01.10.2019 = souscrit - X01.10.2019,
                                X01.11.2019 = souscrit - X01.11.2019,X01.12.2019 = souscrit - X01.12.2019)

  survival <- survival%>%mutate(
    X01.01.2019 = (100/souscrit)*X01.01.2019,X01.02.2019 = (100/souscrit)*X01.02.2019,
    X01.03.2019 = (100/souscrit)*X01.03.2019,X01.04.2019 = (100/souscrit)*X01.04.2019,
    X01.05.2019 = (100/souscrit)*X01.05.2019,X01.06.2019 = (100/souscrit)*X01.06.2019,
    X01.07.2019 = (100/souscrit)*X01.07.2019,X01.08.2019 = (100/souscrit)*X01.08.2019,
    X01.09.2019 = (100/souscrit)*X01.09.2019,X01.10.2019 = (100/souscrit)*X01.10.2019,
    X01.11.2019 = (100/souscrit)*X01.11.2019,X01.12.2019 = (100/souscrit)*X01.12.2019)
  result <- survival[!survival$grps == 'NULL',]
  return(result[,-c(2)])
}

transformDataBeforePlot <- function(data){
  grps <- data$grps
  data_f <- as.data.frame(t(data[-1]))
  names(data_f) <- grps
  date <- colnames(data[-1])
  print(date)
  #data_f <- cbind(date,data_f)
  return(data_f)
}

createList <- function(data){
  ds <- lapply(1:(length(data)-1), function(x){
    list(data = data[,x],name = colnames(data[x]))
  })
  return(ds)
}

createHighchart <- function(title = "",data,date_origine,grps){
  survival_created <- create_survival_data(data,date_origine,grps)
  data_transformed <- transformDataBeforePlot(survival_created)
  # createHighchart('Survival Analysis group by region',test)
  serie_list <- createList(data_transformed)

  date <- dmy(str_replace_all(str_replace_all(rownames(data_transformed), "[X]", ""), "[.]", "-"))

  highchart() %>%
    highchart() %>%
    hc_title(text = title,style = list(color = "#1395CD",fontFamily = "Helvetica", useHTML = TRUE)) %>%
    hc_xAxis(categories = dmy(str_replace_all(str_replace_all(rownames(data_transformed), "[X]", ""), "[.]", "-"))) %>%
    hc_add_series_list(serie_list)
}

# createHighchart("title",survival_data,'2019-01','REGION.COMMERCIALE')
# survival_created <- create_survival_data(survival_data,'2019-01','REGION.COMMERCIALE')
# transformDataBeforePlot(survival_created)
# #
# a <- transformDataBeforePlot(survival_created)
# rownames(a)

#calcul cumul
# cumul_ceritf_percent <- function(dt){
#   tmp <- dt[-c(1:2)]
#
#   tmp_df <- dt[c(1:3)]
#
#   for(i in 2:ncol(tmp)){
#     r <- rowSums(tmp[c(1:i)], na.rm = T)
#     tmp_df <- cbind(tmp_df,r)
#   }
#
#   for(i in 3:ncol(tmp_df)){
#     tmp_df[i] <- round(tmp_df[i]/tmp_df[2]*100,0)
#   }
#
#   tmp_df <- replace(tmp_df, is.na(tmp_df), 0)
#   return(tmp_df)
# }

#transformer les données sous forme écoulement
transf_data_to_ecoul <- function(dt,col){
  dt$DATE.REGISTER <- as.Date(dt$DATE.REGISTER)
  # t <- dt %>%
  #   # select(-c(REGION.COMMERCIALE,cat_souscr)) %>%
  #   group_by(DATE.REGISTER,d) %>%
  #   summarise(certifie = sum(certifie, na.rm = T), souscrit = sum(souscrit, na.rm = T)) %>% ungroup() %>%
  #   pivot_wider(names_from=d, values_from = certifie)

  t <- dt %>%
    # select(-c(REGION.COMMERCIALE,cat_souscr)) %>%
    group_by(DATE.REGISTER,d) %>%
    summarise(certifie = sum(!!as.name(col),  na.rm = T), souscrit = sum(souscrit, na.rm = T)) %>% ungroup() %>%
    pivot_wider(names_from=d, values_from = certifie)

  names(t)[-c(1,2)] <- paste0('j',names(t)[-c(1,2)])
  return(t)
}


cumul_ceritf_percent <- function(tmp_df){
  for(i in 3:ncol(tmp_df)){
    tmp_df[i] <- tmp_df[i]/tmp_df[2]
  }

  # tmp_df[is.na(tmp_df)]=''

  for(i in 3:ncol(tmp_df)){
    tmp_df[i] <- as.character(tmp_df[[i]])
  }

  tmp_df[-c(1:2)][is.na(tmp_df[-c(1:2)])]=''


  return(tmp_df)
}


cumul_ceritf_percent_only_global <- function(tmp_df){
  for(i in 3:ncol(tmp_df)){
    tmp_df[i] <- round(tmp_df[i]/tmp_df[2],2)*100
  }

  # tmp_df[is.na(tmp_df)]=''

  for(i in 3:ncol(tmp_df)){
    tmp_df[i] <- as.character(tmp_df[[i]])
  }

  tmp_df[-c(1:2)][is.na(tmp_df[-c(1:2)])]=''


  return(tmp_df)
}


# cumul_ceritf_percent_ <- function(tmp_df){
#   for(i in 3:ncol(tmp_df)){
#     tmp_df[i] <- tmp_df[i]/tmp_df[2]
#   }
#
#   # tmp_df[is.na(tmp_df)]=''
#
#   for(i in 3:ncol(tmp_df)){
#     tmp_df[i] <- as.character(tmp_df[[i]])
#   }
#
#   tmp_df[-c(1:2)][is.na(tmp_df[-c(1:2)])]=''
#
#
#   return(tmp_df)
# }


# recuprer la matrice mirroir
mirror.matrix <- function (x)
{
  xx <- as.data.frame(x)
  xx <- rev(xx)
  xx <- as.matrix(xx)
  return(xx)
}

# transformer donnes pour le dash
# transform_data <- function(tmp_df,dt){
#   mir_mat <- mirror.matrix(tmp_df[-c(1:2)])
#   mir_mat[t(upper.tri(mir_mat))] <- NA
#   mat <- mirror.matrix(mir_mat)
#
#   mat <- as.data.frame(mat)
#
#   dt_f <- cbind(dt[c(1:2)],mat)
#   names(dt_f) <- names(dt)
#
#   dt_f[is.na(dt_f)]=''
#
#   return(dt_f)
# }

#creation list series
create_list_series <- function(y,type,color=NULL, en = F){
  serielist <- list()
  if(!is.null(color)){
    for(i in 1:ncol(y)){
      serielist[[i]]<-list(
        name = names(y)[i],
        type = type,
        data = as.list(y[[i]]),
        color = color[[i]],
        dataLabels = list(enabled = en,format = '<div style="font-size: 8px;">{point.y:.0f}%</div>')
      )
    }
  }else{
    for(i in 1:ncol(y)){
      serielist[[i]]<-list(
        name = names(y)[i],
        type = type,
        data = as.list(y[[i]]),
        dataLabels = list(enabled = en,format = '<div style="font-size: 8px;">{point.y:.0f}%</div>')
      )
    }
  }
  return(serielist)
}

# filter par RC ou par Categorie les donnees de l'ecoulement
filter_data_ecoul <- function(dt,rc,cat,c){
  if(rc == "GLOBAL"){
    tmp <- dt
  }else{
    tmp <- dt %>% filter(REGION.COMMERCIALE == rc)
  }

  if(cat == "ALL"){
    tmp2 <- tmp
  }else{
    tmp2 <- tmp %>% filter(cat_souscr == cat)
  }
  res <- tmp2 %>% group_by(DATE.REGISTER,d) %>% summarise(souscrit = sum(souscrit, na.rm = T),
                                                          kpi = sum(!!as.name(c), na.rm = T))
  return(res)
}

filter_data_daily <- function(dt,rc,cat){
  if(rc == "GLOBAL"){
    tmp <- dt
  }else{
    tmp <- dt %>% filter(REGION.COMMERCIALE == rc)
  }

  if(cat == "ALL"){
    tmp2 <- tmp
  }else{
    tmp2 <- tmp %>% filter(sous_categorie  == cat)
  }
  res <- tmp2 %>% group_by(DATE.REGISTER) %>% summarise(souscrit = sum(souscrit, na.rm = T),
                                                        certifie = sum(certifie, na.rm = T))
  return(res)
}

# code couleur dans l'ecoulement
stoplighttile <- function(cut1 = .1, cut2 = .2, fun = "comma", digits = 0) {
  fun <- match.fun(fun)
  formatter("span", x ~ fun(x, digits = digits),
            style = function(y) style(
              display = "block",
              # padding = "0 4px",
              #"border-radius" = "4px",
              "color" = ifelse( y < cut1, csscolor("#FFFDF9"), csscolor("#645a43")),
              "border-left" = "solid white 1px",
              "background-color" = ifelse( y < cut1, csscolor("#FF8C00"),
                                           ifelse( y > cut2, csscolor("#9fd063"),
                                                   csscolor("#d0ba63")))
            )
  )
}

colorText <- function(val){
  if(val<= 0){
    return(paste0("<strong style='font-size:15px;color:red'>",round(val,0),"% </strong>"))
  }else{
    return(paste0(" <strong style='font-size:15px;color:green'>",round(val,0),"% </strong>"))
  }
}


color_code <- function(evolution){
  color <- ifelse(evolution <0,"red",'green' )
  return(color)
}

#calcul objectif
calcul_objectif <- function(cum_d,objectif){
  for(i in 1:nrow(cum_d)){
    cum_d$obj[i] <- (cum_d$nb_actif_cumul_m_1[i+1]/cum_d$nb_actif_cumul_m_1[i])-1
    # print()
  }
  cum_d$obj_cum[nrow(cum_d)] <- objectif

  for(j in nrow(cum_d):2){
    cum_d$obj_cum[j-1] <- cum_d$obj_cum[j]*(1-cum_d$obj[j-1])
  }

  return(cum_d)
}


evolution_mtd <- function(data,col){
  # data <- animateur_mtd
  res <- list()

  val_min <- data%>%
    filter(Date == min(Date))%>%select(col)

  val_max <- data%>%
    filter(Date == max(Date))%>%select(col)

  ev <- (val_max/val_min - 1)*100
  res[['val_curr']] <- val_max
  res[['val_prev']] <- val_min
  res[['ev']] <- round(ev,2)
  return(res)
}

improvement_formatter <- function(cut1 = .1, cut2 = .2, fun = "comma", digits = 0) {
  fun <- match.fun(fun)
  formatter("span", x ~ fun(x, digits = digits),
            style = function(y) style(
              display = "block",
              # padding = "0 4px",
              # "border-radius" = "4px",
              "color" = ifelse( y < 0.745, csscolor("#FFFDF9"), csscolor("#645a43")),
              "border-left" = "solid white 1px",
              "background-color" =  ifelse(y < 0.745, "#ff4747",
                                           ifelse(y >= 0.745 & y< 0.795 ,"#E18E52",
                                                  ifelse(y >= 0.795 & y < 0.845,"#d0ba63",
                                                         ifelse(y >= 0.845 & y < 0.895,"#9fd063","#69db50")
                                                  )
                                           )
              )
            )
  )
}
improvement_formatter <- function(cut1 = .1, cut2 = .2, fun = "comma", digits = 0) {
  fun <- match.fun(fun)
  formatter("span", x ~ fun(x, digits = digits),
            style = function(y) formattable::style(
              display = "block",
              # padding = "0 4px",
              # "border-radius" = "4px",
              "color" = ifelse( y < 0.745, csscolor("#FFFDF9"), csscolor("#645a43")),
              "border-left" = "solid white 1px",
              "background-color" =  ifelse(y < 0.745, "#ff4747",
                                           ifelse(y >= 0.745 & y< 0.795 ,"#E18E52",
                                                  ifelse(y >= 0.795 & y < 0.845,"#d0ba63",
                                                         ifelse(y >= 0.845 & y < 0.895,"#9fd063","#69db50")
                                                  )
                                           )
              )
            )
  )
}

my_checkboxGroupInput <- function(variable, label,choices, selected, colors){
  my_names <- choices
  if(length(names(choices)) > 0)
    my_names <- names(choices)
  div(id=variable,class="form-group shiny-input-checkboxgroup shiny-input-container shiny-bound-input",
      HTML("<label class='control-label' id='",variable,"-label'>",label,"</label>"),
      div( class="form-group",
           HTML(paste0('
                      <div class="form-check">
                      <label class="form-check-label">
                        <input type="checkbox" value="',my_names,'" for="', variable,'"',' name="', variable,'"', ifelse(choices %in% selected, 'checked="checked"', ''),'>
                        <span class="checkmark"></span>
                        <span>',str_replace_all(my_names,"_"," ")%>%str_to_title(),'</span>
                      </label>
                      </div>'
                       , collapse = " "))
      )
  )
}

get_data <- function(nom_query, dest = "data/data.json"){
  api <- paste0("http://tapp577lv:3939/dashviz/content/939db627-0747-48eb-a89a-154aa6e048ba/query?id=",nom_query)
  data_json <- curl_download(api, destfile = dest)
  data_json <- jsonlite::fromJSON(dest)
  return(data_json)
}

force_croissant <- function(x){
  if(length(x) < 2){
    return(x)
  }else{
    # delta <- ifelse(x - lag(x)<0, 0, x - lag(x))
    # delta <- ifelse(is.na(delta),0,delta)
    ans <- c(x[1])
    for(i in 2:length(x)){
      if(x[i]==0){
        ans <- c(ans,ans[i-1])
      }else{
        ans <- c(ans,x[i])
      }

    }
    return(ans)
  }
}

# source('module/suivi_souscrit_animateur/function_mpisera.R',local = T)$value
# source('module/sousc_actif/function_sousc_actif.R',local = T)$value
