
library(nhdplusTools)
library(cdssr)
library(dplyr)
library(sf)
library(mapview)
library(dataRetrieval)

# ***********************
# ---- NHD mainstems ----
# ***********************

# Test AOI (Larimer county)
# aoi <- AOI::aoi_get(county = "Larimer", state = "CO")
aoi <- AOI::aoi_get(county = "Boulder", state = "CO")

# HUC8's within AOI
hucs <- nhdplusTools::get_huc8(AOI = aoi) %>%
  dplyr::filter(states == "CO")

mapview::mapview(hucs) + aoi

# # subset single huc8
# hucs_sub <-
#   hucs %>%
#   dplyr::filter(huc8 %in% c("14010001", "10180001"))

huc_flines <- lapply(1:nrow(hucs), function(i) {

  message(paste0("HUC8: ", hucs$huc8[i], " - ", i, "/", nrow(hucs)))

  flines <- nhdplusTools::get_nhdplus(
    AOI         = hucs[i,],
    realization = "flowline"
    # realization = c("outlet", "flowline"),
    # streamorder = 4
  ) %>%
    dplyr::mutate(dplyr::across(c(-geometry), as.character)) %>%
    dplyr::filter(streamcalc != 0) %>%
    dplyr::mutate(huc8 = hucs$huc8[i])

}) %>%
  dplyr::bind_rows()

mapview::mapview(huc_flines$geometry) + hucs$geometry

# # split stream segments into larger chunks
# min_flines <-
#   huc_flines %>%
#   dplyr::group_by(gnis_id) %>%
#   dplyr::filter(hydroseq == min(hydroseq))
#
# mapview::mapview(huc_flines$geometry, color = "red") + hucs$geometry + min_flines$geometry
# mapview::mapview(min_flines$geometry, color = "red") + hucs$geometry + huc_flines$geometry

sub_flines <-
  huc_flines %>%
  dplyr::filter(streamorde >= 4)
huc_flines %>%
  dplyr::filter(streamorde >= 4) %>%
  dplyr::group_by(streamorde, gnis_id)
# start and ending flow lines for every main GNIS ID
end_flines <-
  huc_flines %>%
  dplyr::filter(streamorde >= 4) %>%
  # dplyr::group_by(gnis_id) %>%
  dplyr::group_by(streamorde, gnis_id) %>%
  dplyr::filter(hydroseq == min(hydroseq) |hydroseq == max(hydroseq)) %>%
  dplyr::ungroup()
end_flines
# end points for each start/end of a GNIS ID
end_pts <-
  end_flines %>%
  nhdplusTools::get_node(position = "end") %>%
  dplyr::bind_cols(dplyr::select(sf::st_drop_geometry(end_flines),
                                 huc8, comid, hydroseq, gnis_id)) %>%
  dplyr::relocate(huc8, comid, hydroseq, gnis_id, geometry)
  # dplyr::filter(huc8 %in%  c("10190005", "10190006"))
  # dplyr::filter(huc8 == "14010001")
  # dplyr::filter(huc8 == "10190006")
# start_pts <-
#   end_flines %>%
#   nhdplusTools::get_node(position = "start") %>%
#   dplyr::bind_cols(dplyr::select(sf::st_drop_geometry(end_flines),
#                                  huc8, comid, hydroseq, gnis_id)) %>%
#   dplyr::relocate(huc8, comid, hydroseq, gnis_id, geometry)
end_pts

# mapview
mapview::mapview(end_flines$geometry, color = "red") +
  hucs +
  sub_flines$geometry

# # buffer around HUC centroid
huc_buff <-
  hucs %>%
  sf::st_centroid() %>%
  sf::st_transform(5070) %>%
  sf::st_buffer(1608*25) %>%
  sf::st_transform(4326)

# # buffer around HUC centroid
end_buff <-
  end_pts %>%
  sf::st_centroid() %>%
  sf::st_transform(5070) %>%
  sf::st_buffer(1608*13) %>%
  sf::st_transform(4326)

# # mapview
mapview::mapview(end_flines$geometry, color = "red") +
  hucs +
  sub_flines$geometry +
  # huc_buff$geometry +
  mapview::mapview(end_buff$geometry, col.regions = "red")

# # huc centroids
# huc_cntr <-  sf::st_centroid(hucs)

# water rights in each HUC
# huc_wr <- lapply(1:nrow(huc_cntr), function(i) {
#   # structs
#   tryCatch({
#     message(paste0(i, "/", nrow(huc_cntr)))
#
#     wr_net <- cdssr::get_water_rights_netamount(
#       aoi    = huc_cntr[i, ],
#       radius = 60
#     ) %>%
#       dplyr::mutate(
#         huc8     =   huc_cntr$huc8[i]
#       )
#     wr_net
#   }, error = function(e)
#     NULL
#   )}
# ) %>%
#   dplyr::bind_rows()

# mapview::mapview(end_pts) + end_flines

end_wr <- lapply(1:nrow(end_pts), function(i) {
  # structs <- cdssr::get_structures(
  #   aoi    = end_pts[i, ],
  #   radius = 20
  #   ) %>%
  #   dplyr::mutate(
  #     comid    =   as.character(sf::st_drop_geometry(end_pts[i, ])$comid),
  #     hydroseq =   as.character(sf::st_drop_geometry(end_pts[i, ])$hydroseq),
  #     gnis_id  =   as.character(sf::st_drop_geometry(end_pts[i, ])$gnis_id)
  #   )
  #
  # structs
  tryCatch({

    message(paste0(i, "/", nrow(end_pts)))

    wr_net <- cdssr::get_water_rights_netamount(
      aoi    = end_pts[i, ],
      radius = 13
    ) %>%
      dplyr::mutate(
        huc8     =   as.character(sf::st_drop_geometry(end_pts[i, ])$huc8),
        comid    =   as.character(sf::st_drop_geometry(end_pts[i, ])$comid),
        hydroseq =   as.character(sf::st_drop_geometry(end_pts[i, ])$hydroseq),
        gnis_id  =   as.character(sf::st_drop_geometry(end_pts[i, ])$gnis_id)
      )

    wr_net

    }, error = function(e)

    NULL

    )
  }) %>%
  dplyr::bind_rows()

# Water rights points
wr_pts <-
  end_wr %>%
  # huc_wr %>%
  dplyr::tibble() %>%
  dplyr::select(huc8, comid, hydroseq, wdid, structure_name, structure_type,
                gnis_id, stream_mile, water_district, latitude, longitude,
                admin_number, appropriation_date) %>%
  dplyr::filter(!is.na(stream_mile)) %>%
  sf::st_as_sf(
    coords = c("longitude", "latitude"),
    crs    = 4326
    ) %>%
  sf::st_transform(5070)

# start and ending flow lines for every main GNIS ID
# flines_sub <-
gnis_flines <-
  huc_flines %>%
  dplyr::filter(streamorde >= 4) %>%
  # dplyr::group_by(gnis_id) %>%
  dplyr::group_by(streamorde, gnis_id) %>%
  dplyr::filter(hydroseq == min(hydroseq) |hydroseq == max(hydroseq)) %>%
  dplyr::ungroup() %>%
  nhdplusTools::get_node(position = "end") %>%
  dplyr::bind_cols(dplyr::select(sf::st_drop_geometry(end_flines),
                                 huc8, comid, hydroseq, gnis_id)) %>%
  dplyr::relocate(huc8, comid, hydroseq, gnis_id, geometry) %>%
  # dplyr::filter(huc8 %in%  c("10190005", "10190006")) %>%
  # dplyr::filter(gnis_id == "178354") %>%
  sf::st_transform(5070)
# dplyr::slice(1)
# wr_sub <-
#   wr_pts %>%
#   # dplyr::filter(gnis_id == "177345") %>%
#   dplyr::filter(gnis_id == "178354")
#   # dplyr::slice(1:10)

end_pts
# 10190005000077
# "Boulder Creek"

# ***************************************
# ---- Subset to single GNIS ID area ----
# ***************************************

# # # gnis points
# gnis_pts <-
#   wr_pts %>%
#   # dplyr::filter(gnis_id == "178354") %>%
#   sf::st_transform(5070)

# nearest point index
near_idx <- sf::st_nearest_feature(gnis_flines, wr_pts)

# nearest WDIDs to most downstream points of river segments
ds_wdid <- wr_pts[near_idx, ] %>%
  sf::st_transform(4326)

mapview::mapview(wr_pts, col.regions = "blue") +
  mapview::mapview(ds_wdid, col.regions = "red") +
  mapview::mapview(gnis_flines, col.regions = "green")

# ********************************
# ---- get call analysis data ----
# ********************************


call_lst <- lapply(1:nrow(ds_wdid), function(i) {

  # ds_wdid$wdid[i]
  # ds_wdid$comid[i]

  message(
    paste0("WDID: ", ds_wdid$wdid[i], " - ",
           i, "/", nrow(ds_wdid))
    )
  tryCatch({

    call_df <- cdssr::get_call_analysis_wdid(
                        wdid       = ds_wdid$wdid[i],
                        admin_no   = "99999.00000",
                        start_date = "2010-01-01",
                        # start_date = "2022-12-01",
                        end_date   = "2023-01-01"
                        ) %>%
      dplyr::mutate(
        huc8     = ds_wdid$huc8[i],
        comid    = ds_wdid$comid[i],
        hydroseq = ds_wdid$hydroseq[i],
        gnis_id  = ds_wdid$gnis_id[i]
      )

  }, error = function(e)

    NULL

  )

}) %>%
  dplyr::bind_rows()



call_outs <-
  call_lst %>%
  dplyr::tibble() %>%
  dplyr::select(
    huc8,
    comid,
    hydroseq,
    gnis_id,
    datetime,
    date     = analysis_date,
    wdid     = analysis_wdid,
    admin_no = analysis_wr_admin_no,
    priority_wdid,
    priority_admin_no,
    priority_date,
    out_pct  = analysis_out_of_priority_percent_of_day,
    ) %>%
  dplyr::group_by(wdid) %>%
  dplyr::mutate(
    year             = lubridate::year(datetime),
    priority_date    = as.Date(priority_date),
    priority_date    = dplyr::case_when(
                          is.na(priority_date) ~ max(priority_date, na.rm = TRUE),
                          TRUE                 ~ priority_date
                          )
    ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(wdid, datetime) %>%
  dplyr::slice(which.min(priority_date)) %>%
  dplyr::ungroup()

call_outs %>%
  dplyr::group_by(wdid)

call_outs %>%
  dplyr::group_by(huc8)

call_outs %>%
  dplyr::group_by(comid)
# call_outs$gnis_id %>% unique()
  # call_outs$priority_date %>% min(na.rm = T)
call_outs %>%
  dplyr::filter(year == 2015) %>%
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = datetime, y = priority_date, color = wdid)) +
  # ggplot2::geom_line(ggplot2::aes(x = datetime, y = priority_date)) +
  ggplot2::facet_wrap(~huc8)
  # ggplot2::facet_wrap(~wdid)
  # ggplot2::facet_wrap(~comid)
  # ggplot2::facet_grid(comid~huc8)
  # ggplot2::facet_grid(wdid~comid)
  # ggplot2::facet_grid(hydroseq~comid)


# ********************************
# ---- appropriation date map ----
# ********************************
wr_bins <-
  wr_pts %>%
  dplyr::mutate(date_bin = dplyr::case_when(
    appropriation_date <= "1880-01-01"                                     ~ "1",
    appropriation_date > "1880-01-01" & appropriation_date <= "1900-01-01" ~ "2",
    appropriation_date > "1900-01-01" & appropriation_date <= "1950-01-01" ~ "3",
    appropriation_date > "1950-01-01"                                      ~ "4",
    )
    )
tmp <-
  wr_bins %>%
  dplyr::filter(huc8 == "10190003")

tmp %>%
  ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(color = date_bin))

# ***************************************
# ---- Subset to single GNIS ID area ----
# ***************************************
# dplyr::

# gnis_pts

# group split by comid
comid_lst <-
  wr_pts %>%
  # dplyr::filter(gnis_id == "178354") %>%
  sf::st_transform(5070) %>%
  dplyr::group_by(comid, gnis_id) %>%
  dplyr::group_split()
comid_lst

# start and ending flow lines for every main GNIS ID
# flines_sub <-
gnis_flines <-
  huc_flines %>%
  dplyr::filter(streamorde >= 4) %>%
  dplyr::group_by(gnis_id) %>%
  # dplyr::group_by(streamorde, gnis_id) %>%
  dplyr::filter(hydroseq == min(hydroseq) |hydroseq == max(hydroseq)) %>%
  dplyr::ungroup() %>%
  # dplyr::filter(huc8 %in%  c("10190005", "10190006")) %>%
  # dplyr::filter(gnis_id == "178354")
  nhdplusTools::get_node(position = "end") %>%
  dplyr::bind_cols(dplyr::select(sf::st_drop_geometry(end_flines),
                                 huc8, comid, hydroseq, gnis_id)) %>%
  dplyr::relocate(huc8, comid, hydroseq, gnis_id, geometry) %>%
  dplyr::filter(huc8 %in%  c("10190005", "10190006")) %>%
  # dplyr::filter(gnis_id == "178354") %>%
  sf::st_transform(5070)
  # dplyr::slice(1)

tmp <- gnis_pts  %>% dplyr::slice(1:20)
data.frame(dist = sf::st_distance(tmp, gnis_flines) )
rm(tmp)
i <- 1
unique(comid_lst[[i]]$comid)

for (i in 1:length(comid_lst)) {

  message(
    paste0("COMID: ", comid_lst[[i]]$comid[1], " - ",
           i, "/", length(comid_lst))
    )
  fline <-
    gnis_flines %>%
    # dplyr::filter(comid == comid_lst[[i]]$comid[1])
  dplyr::filter(gnis_id ==   comid_lst[[i]]$gnis_id[1])
  comid_lst[[i]]$gnis_id[1]
  structs <- comid_lst[[i]]

  nn <- sf::st_nearest_feature(fline, structs)
# structs[701, ]
#   tmp2 <- structs[nn, ]
#   tmp2
    # mapview::mapview(structs, col.regions = "blue") +
    #   mapview::mapview(tmp2, col.regions = "red") +
    # mapview::mapview(fline, col.regions = "green")
  }


tmp <-
  gnis_pts %>%
  # dplyr::slice(1:10) %>%
  dplyr::bind_cols(
    data.frame(dist = sf::st_distance(., gnis_flines))
                   ) %>%
  dplyr::arrange(dist) %>%
  dplyr::filter(dist == min(dist))

mapview::mapview(tmp, col.regions = "red") + gnis_flines
gnis_flines
# gnis_flines <-
#   flines_sub %>%
#   dplyr::filter(gnis_id == "178354")
huc_sub <-
  huc_flines %>%
  dplyr::filter(huc8 %in%  c("10190005", "10190006"))


mapview::mapview(huc_sub, color = "blue") +
  mapview::mapview(gnis_flines, color = "red") +
  mapview::mapview(gnis_pts, col.regions = "green")
wr_pts %>%
  dplyr::group_by(gnis_id)
# mapview
mapview::mapview(wr_pts) + huc_flines$geometry
# mapview::mapview(wr_pts) + end_flines$geometry

wr_pts %>%
  dplyr::filter(gnis_id %in% c("205012", "178190", "178354")) %>%
  ggplot2::ggplot() +
  ggplot2::geom_sf() +
  ggplot2::facet_wrap(~gnis_id)
wr_pts %>%
  dplyr::group_by(gnis_id)

wr_pts$gnis_id %>% unique()
names(end_wr)

cdssr::get_water_rights_netamount()
# cdssr::get_structures(aoi = )

# max_flines <-
#   sub_flines %>%
#   dplyr::group_by(gnis_id) %>%
#   # dplyr::group_by(streamorde, gnis_id) %>%
#   dplyr::filter(hydroseq == max(hydroseq))

# NHDPlus features for HUC8
huc_nhd <- nhdplusTools::get_nhdplus(
  AOI         = hucs_sub,
  realization = "all"
  )
tmp <- nhdplusTools::get_nhdplus(
  AOI         = hucs_sub,
  realization = c("outlet", "flowline"),
  streamorder = 4
)
plot(tmp$outlet$geometry)
plot(tmp$flowline$geometry, add = T)
tmp %>%
  dplyr::filter(streamcalc >= 1) %>%
  .$geometry %>%
  plot()
# flowlines
flines <- huc_nhd$flowline

# catchments
catch <- huc_nhd$catchment

# outlets
outs <- huc_nhd$outlet

# # plot flines, outlets, catchments
# plot(catch$geometry)
# plot(outs$geometry, add = T)
# plot(flines$geometry, add = T)

# subset flowline columns
flines_sub <-
  flines %>%
  dplyr::select(comid, hydroseq, streamleve, streamorde, streamcalc, geometry) %>%
  # dplyr::filter(streamcalc >= 1, streamleve  <= min(streamleve)+1)
  # dplyr::filter(streamcalc >= 1, streamleve  == min(streamleve))
  dplyr::filter(streamcalc >= 1)


flines_sub %>%
  # dplyr::filter(streamorde >= 4) %>%
  # dplyr::filter(streamcalc >= 1) %>%
  # dplyr::filter(streamcalc >= 1, streamleve < 6) %>%
  # dplyr::filter(streamcalc >= 1, streamleve  <= min(streamleve)+1) %>%
  # dplyr::filter(streamcalc >= 1, streamleve  == min(streamleve)) %>%
  # dplyr::filter(streamcalc >= 1) %>%
  dplyr::mutate(
    streamleve = as.factor(streamleve),
    streamorde = as.factor(streamorde),
    streamcalc = as.factor(streamcalc)
  ) %>%
  ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(color = streamorde))

mapview::mapview(flines_sub) + hucs_sub
plot(flines_sub$geometry)
# navigate network (upstream mainstem)
net_um <- nhdplusTools::navigate_network(
  start = huc8_um$geometry,
  mode  = "UM"
  )

# ************************
# ---- NHD catchments ----
# ************************

# Paths
wr_net_path    <- "data/water_right_netamounts.rds"
wr_trans_path  <- "data/water_right_transactions.rds"
districts_path <- "data/water_districts.rds"
# ca_path        <- "D:/cpo/data/call_analysis/aggregate/call_analysis.csv"
# ca_path        <- "D:/cpo/data/call_analysis/aggregate/call_analysis2.csv"

# water rights net amounts
wr_net <- readRDS(wr_net_path)

# call analysis dataframe
# ca_df  <- readr::read_csv(ca_path) %>%
#   janitor::clean_names()


# subset of water rights for district 2
sub_wr <-
  wr_net %>%
  dplyr::tibble() %>%
  # dplyr::filter(!is.na(stream_mile), county == "LARIMER")
  dplyr::filter(!is.na(stream_mile), water_district %in% c(1, 3))

sub_wr %>%
  dplyr::group_by(wdid)
# gnisid <- unique(sub_wr$gnis_id)

sub_wr %>%
  dplyr::group_by(wdid) %>%
  dplyr::tally() %>%
  dplyr::arrange(-n)

# gnisid <-
#   sub_wr %>%
#   dplyr::filter(wdid == "0200810") %>%
#   .$gnis_id %>%
#   unique()
buff_pt <- sf::st_transform(
              sf::st_buffer(
                sf::st_transform(
                  dplyr::filter(
                    sf::st_as_sf(
                      sub_wr, coords = c("longitude", "latitude"), crs = 4326),
                    wdid == "0302426"
                    ),
                  5070),
                3000),
              4326
            )

# filter down to a few points
pts <-
  sub_wr %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  sf::st_filter(
    sf::st_transform(
      sf::st_buffer(
        sf::st_transform(
          dplyr::filter(., wdid == "0302426"),
          5070),
        3000),
      4326
      )
    )
pts$wdid %>%
  unique()
pts$gnis_id %>%
  unique()

tele_stations <- cdssr::get_telemetry_stations(
  aoi = buff_pt
) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

tele_df <- lapply(1:nrow(tele_stations), function(i) {

  message(paste0(tele_stations$abbrev[i], " - ", i, "/", nrow(tele_stations)))

  # dschrg <- cdssr::get_telemetry_ts(
  #   abbrev     = tele_stations$abbrev[i],
  #   parameter  = "DISCHRG",
  #   start_date = "2015-01-01",
  #   end_date   = "2023-01-01",
  #   timescale  = "day"
  # )
  # dschrg

  tryCatch({

    dschrg <- cdssr::get_telemetry_ts(
      abbrev     = tele_stations$abbrev[i],
      parameter  = "DISCHRG",
      start_date = "2015-01-01",
      end_date   = "2023-01-01",
      timescale  = "day"
    )

    dschrg

  },
  error = function(e) {
    NULL
  })

})

tele_df <- dplyr::bind_rows(tele_df)

pts %>%
  sf::st_drop_geometry() %>%
  dplyr::group_by(wdid)

# NHD flowlines
flines <- nhdplusTools::get_nhdplus(AOI = sf::st_as_sfc(sf::st_bbox(pts)))



mapview::mapview(pts, color = "red") + buff_pt + flines$geometry + tele_stations
0303400
0300910 
# subset not geospatial rights data
sub_calls <-
  sub_wr %>%
  dplyr::filter(wdid %in% unique(pts$wdid))

length(unique(sub_calls$wdid))

call_df <- lapply(1:length(unique(sub_calls$wdid)), function(i) {

  message(paste0(unique(sub_calls$wdid)[i], " - ", i, "/", length(unique(sub_calls$wdid))))

  ca <-
    cdssr::get_call_analysis_wdid(
    wdid       = unique(sub_calls$wdid)[i],
    admin_no   = "99999.00000",
    start_date = "2015-01-01",
    end_date   = "2023-01-01",
    api_key    = "2fx+0sUzKbpOWeqkWzbU4BIIOtpwoVyE"
    )

  ca

})

# call_df <- dplyr::bind_rows(call_df)
#
call_df2 <- readr::read_csv("tmp_calls.csv")
# readr::write_csv(call_df, "tmp_calls.csv")
out_calls3 <-
  call_df2 %>%
  # dplyr::select(analysis_date, analysis_wdid, analysis_wr_admin_no,
  #               priority_wdid, priority_admin_no, priority_date,
  #               out_pct = analysis_out_of_priority_percent_of_day,
  #               call_type
  # ) %>%
  dplyr::mutate(
    out_pct        = analysis_out_of_priority_percent_of_day,
    year           = lubridate::year(analysis_date),
    water_district = substr(analysis_wdid, 0, 2),
    analysis_date  = as.Date(analysis_date),
    priority_date  = as.Date(priority_date)
  ) %>%
  dplyr::filter(analysis_wdid %in% unique(pts$wdid)) %>%
  # dplyr::group_by(water_district, analysis_date) %>%
  dplyr::group_by(analysis_wdid, analysis_date) %>%
  dplyr::slice(which.min(priority_date)) %>%
  dplyr::ungroup()
library(patchwork) # To display 2 charts together

out_plot <-
  out_calls3 %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    year       = lubridate::year(analysis_date)
  ) %>%
  dplyr::filter(analysis_wdid %in% c("0300907")) %>%
  dplyr::filter(year %in% c(2020)) %>%
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = analysis_date, y = priority_date)) +
  ggplot2::facet_wrap(~analysis_wdid)
min_date <- out_calls3 %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    year       = lubridate::year(analysis_date)
  ) %>%
  dplyr::filter(analysis_wdid %in% c("0300907")) %>%
  dplyr::filter(year %in% c(2020)) %>%
  .$analysis_date %>%
  min()
max_date <- out_calls3 %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    year       = lubridate::year(analysis_date)
  ) %>%
  dplyr::filter(analysis_wdid %in% c("0300907")) %>%
  dplyr::filter(year %in% c(2020)) %>%
  .$analysis_date %>%
  max()
discharge_plot <-
  tele_df %>%
  dplyr::mutate(
    year       = lubridate::year(datetime)
  ) %>%
  dplyr::filter(datetime >= min_date, datetime <= max_date) %>%
  dplyr::filter(abbrev == "CLAFTCCO") %>%
  dplyr::filter(year %in% c(2020)) %>%
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = datetime, y = meas_value))
  # ggplot2::facet_wrap(~abbrev)

out_plot + discharge_plot

tele_df %>%
  dplyr::tibble() %>%
  dplyr::mutate(
    year       = lubridate::year(datetime)
  ) %>%
  dplyr::filter(abbrev == "CLAFTCCO") %>%
  dplyr::filter(year %in% c(2019))

out_calls %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    year       = lubridate::year(analysis_date)
  ) %>%
  dplyr::filter(analysis_wdid %in% c("0300527")) %>%
  dplyr::filter(year %in% c(2019))
call_df %>%
  dplyr::mutate(
    out_pct        = analysis_out_of_priority_percent_of_day,
    year           = lubridate::year(analysis_date),
    water_district = substr(analysis_wdid, 0, 2),
    analysis_date  = as.Date(analysis_date),
    priority_date  = as.Date(priority_date)
  ) %>%
  dplyr::filter(year %in% c(2019)) %>%
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = analysis_date, y = out_pct)) +
  # ggplot2::facet_wrap(~analysis_wdid)
  ggplot2::facet_wrap(~priority_date)
# ggplot2::facet_grid(priority_date~priority_date)
rm(senior_calls)
unique(sub_calls$wdid)








































