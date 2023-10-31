# script for Ashley B. Dann
# author: Wes SLaughter, Ashley B. Dann 
# date: 10/30/23

# water quality on Scott's Level Branch, Paint Branch, Hickey Run, and Rock Creek
# Scott's Level Branch: 01589290
# Paint Branch: 01649190
# Hickey Run: 01651770
# Rock Creek: 01648010

# libraries
if(!require("dataRetrieval")) install.packages("dataRetrival")
if(!require("ggplot2")) install.packages("ggplot2")
if(!require("plotly")) install.packages("plotly")

## objectives:
  # - explore wether organic matter is a vector or "middleman" in aiding in the movement of elements downstream
  # - lead, zinc, copper, arsenic, cadmium, and other toxic metals interact with organic matter when salt is added to the system
  # - E. Coli
  # - Q vs lead/zinc/copper/NONO/OC
  # - OC vs. zinc/lead/copper

## Step 1: retrieve USGS data for each site
#   a) set gauge IDs
site_info <- c('slb' = '01589290', 'pb' = '01649190', 'hr' = '01651770', 'rc' = '01648010')

target_variables <- c('Cu' = '01040', 'Cl' = '00940', 'EColi' = '50468', 'P' = '00666', 
                      'TN' = '62854', 'Total Coliforms' = '50569', 'Nitrite' = '00613', 'Nitrate' = '00618',
                      'NH3 and NH4' = '00608', 'SPC at 25C' = '00095', 'Particulate N' = '49570',
                      'ON' = '00607', 'Cd' = '01025', 'Pb' = '01049', 'Zn' = '01090', 'OC' = '00681',
                      'TW' = '00010')

#   All variables are filtered, except for SPC at 25C.
#   Units for P, TN, Cl, Particulate N, ON, and OC are in mg/L.
#   Units for Cd, Cu, Pb, and Zn are in micrograms/L.
#   Units for Nitrite, Nitrate, and NH3 and NH4 (Ammonia) are in mg/L as nitrogen.
#   Units for EColi and Total Coliforms is per 100 ml.
#   TW = water temperature


#   b) load in some data
#     this read in USGS parameter codes
pcode <- readNWISpCode("all")

#     this indexes available parameter codes by their
#     normal name
qCds <- pcode[grep("discharge|DOC",
                      pcode$parameter_nm,
                      ignore.case=TRUE),]

# let's check out what we got (we're lookng for discharge in CFS)
head(qCds, 3)

# now let's retrieve this data for one of our sites
siteNo <- site_info[['hr']]
pCode <- c(qCds[[1,1]], qCds[qCds$srsname == "Organic carbon",]$parameter_cd)
start.date <- "2022-01-01"
end.date <- "2023-01-01"

site_data <- readNWISdata(siteNumbers = siteNo,
                       parameterCd = pCode,
                       startDate = start.date,
                       endDate = end.date)

site_data <- renameNWISColumns(site_data)
names(site_data)
parameterInfo <- attr(site_data, "variableInfo")
siteInfo <- attr(site_data, "siteInfo")

ts <- ggplot(data = site_data,
             aes(dateTime, Flow_Inst)) +
  geom_line() +
  xlab("") +
  ylab(parameterInfo$variableDescription) +
  ggtitle(siteInfo$station_nm)
ts

plotly::ggplotly(ts)

# now that we have seen an example of using this workflow, let's start making the plots
# from our objectives

# Q vs OC
siteNo <- as.character(site_info)
start.date <- "2012-01-01"
end.date <- "2023-01-01"

site_q <- readNWISdata(
                       siteNumbers = siteNo,
                       service = "dv",
                       parameterCd = "00060",
                       startDate = start.date,
                       endDate = end.date) %>%
  mutate(sitecode = paste0(agency_cd, '-', site_no), 
         var = 'discharge_dv_cfs') %>%
  select(sitecode, date = dateTime, var, val = 'X_00060_00003')

site_c <- readWQPqw(
      siteNumbers = paste0("USGS-", siteNo),
      parameterCd = as.character(target_variables),
      startDate = start.date,
      endDate = end.date) %>%
  select(sitecode = MonitoringLocationIdentifier, 
         date = ActivityStartDate, 
         # time = ActivityStartTime.Time, 
         var = CharacteristicName, 
         val = ResultMeasureValue)

site_cq_all <- rbind(site_q, site_c) %>%
  tidyr::pivot_wider(id_cols = c('sitecode', 'date'), values_from = val, names_from = var, values_fn = {mean})

## change objects below to alter plot
site <- 'slb'
usgs_variables <- colnames(site_cq_all)

site_cq <- site_cq_all %>%
  filter(sitecode == paste0("USGS-", site_info[[site]]))

cq_plot <- ggplot(data = site_cq,
             aes(discharge_dv_cfs, `Total Coliform`)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE, formula = 'y ~ x', color = 'grey20', alpha = 0.4) +
  scale_x_log10() +
  scale_y_log10() +
  ggtitle(paste("Paint Branch   USGS", site_info[[site]]))
  # facet_wrap(~sitecode)

cq_plot

# organic carbon against solutes
oc_plot <- ggplot(data = site_cq,
                  aes(`Organic carbon`, `Total Coliform`)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE, formula = 'y ~ x', color = 'grey20', alpha = 0.4) +
  scale_x_log10() +
  scale_y_log10() +
  ggtitle(paste("Paint Branch   USGS", site_info[[site]]))

oc_plot


# correlation plot
cq_cor <- site_cq %>%
  select('Copper', 
         EColi = "Escherichia coli", 
         "Lead", 
         "Cadmium", 
         "Organic carbon",
         Temp = "Temperature, water",
         SpCond = "Specific conductance")


corrplot.mixed(cor(cq_cor, use="pairwise.complete.obs"),
               lower = "number",
               title = paste("Paint Branch   USGS", site_info[[site]]), 
               mar = c(2, 1, 3, 1))






# interactive
cq_plotly <- plotly::ggplotly(cq_plot, mode = 'scatter', type = 'lines') %>% layout(
  annotations = list(
    list(
      text = "<b>X-Axis Variable:</b>", x=0.05, y=1.13, 
      xref='paper', yref='paper',xanchor = "left", showarrow=FALSE
    )
  ),
  updatemenus = list(
    list(
      type = "list",
      x = 0.25,
      xanchor = "left",
      y = 1.15,
      buttons = list(
        list(
          method = "update",
          args = list(list(y = list(site_cq$Copper)),
                      list(yaxis = list(title = "Copper"))),
          label = "Copper"
        ),
        list(
          method = "update",
          args = list(list(y =list(site_cq$Nitrate)),
                      list(yaxis = list(title = "Nitrate"))),
          label = "Nitrate"
        )
      )
    )
  )
)  

cq_plotly
