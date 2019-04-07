#' Create a List of Flextable Objects
#'
#' Create a list of flextable objects to display Seasonal Kendall results in the NERRS reserve level template
#'
#' @param sk_result a \code{data.frame} of reformatted results from \code{\link{sk_seasonal}}
#' @param stations chr, vector of stations to be displayed
#' @param param chr, vector of parameters to be displayed
#' @param trend_col chr, a four element vector that specifies colors for increasing, decreasing, no change, and insufficient data trends
#' @param font_sz_stn int, specify the font size of displayed station names
#' @param font_sz_result int, specify the font size of the displayed results
#' @param font_sz_head int, specify the font size of the table header row
#' @param ht_head num, specify the cell height of the table body rows. Units for this parameter are in inches.
#' @param ht_body num, specify the cell height of the table header row. Units for this parameter are in inches.
#' @param is_swmp logical, are the parameter names consistent with SWMP parameter names? default is \code{TRUE}.
#' @param par_names chr, a list of parameter names to be used if the names to not match standard CDMO parameters.
#'
#' @importFrom flextable align border flextable height style width
#' @importFrom stats formula
#' @importFrom officer fp_border fp_cell fp_par fp_text
#'
#' @export
#'
#' @details This function is intended for internal use with the NERRS reserve level reporting scripts. Using the results from the reserve level trend analysis, \code{create_sk_flextable_list} creates a list of two \code{flextable} objects to be displayed in the NERRS reserve level template. The first \code{flextable} in the list contains the two-letter station IDs for each station and the full location name of each station. The second table lists the seasonal kendall results and the names of the parameters of interest.
#'
#' @author Julie Padilla
#'
#' @concept reporting
#'
#' @return Returns a list of \code{\link[flextable]{flextable}} objects
#'

create_sk_flextable_list <- function(sk_result, stations, param, trend_col = c('#247BA0', '#A3DFFF', '#D9D9D9', 'white')
                                     , font_sz_stn = 6, font_sz_result = 12, font_sz_head = 6
                                     , ht_head = 0.28, ht_body = 0.202, is_swmp = T, par_names = NULL) {

  tbl_station <- generate_station_table(sk_result, stations)
  tbl_result <- generate_results_table(sk_result, stations, param)

  # STATIONS TBL ----
  # make flextable object
  ft_header <- flextable::flextable(data = tbl_station)

  # change column names to words
  header_col_names <- ft_header$header$dataset
  header_col_names[1, ] <- c('Location ID', 'Location Name')
  ft_header$header$dataset <- header_col_names

  # format
  def_txt_hd <- fp_text(color = '#404040', bold = T, font.size = font_sz_head)
  def_txt_bdy <- fp_text(color = '#404040', bold = T, font.size = font_sz_stn)
  def_par <- fp_par(text.align = 'center')
  def_cell <- fp_cell(background.color = "white", border = fp_border(color = '#444E65'))

  ft_header <- style(ft_header, pr_c = def_cell, pr_t = def_txt_hd, pr_p = def_par, part = 'head')
  ft_header <- style(ft_header, pr_c = def_cell, pr_t = def_txt_bdy, pr_p = def_par, part = 'body')

  # RESULTS TBL ----
  # make flextable object for SK results
  ft_result <- flextable(data = tbl_result)

  # generate a data.frame with preferred names for head
  if(is_swmp) {
    nms <- ft_col_names(param = param)
  } else {
    if(is.null(par_names))
      stop('par_names is NULL. Please provide a vector of parameter names')

    nms <- par_names
  }

  ft_result$header$dataset <- data.frame(as.list(nms))

  # set styling elements
  def_par <- fp_par(text.align = 'center')
  def_txt_hd <- fp_text(color = '#404040', bold = T, font.size = font_sz_head)
  def_cell_hd <- fp_cell(background.color = 'white', border = fp_border(color = '#444E65')
                         , margin.top = 2, margin.bottom = 2)
  def_txt_bdy <- fp_text(color = '#444E65', font.size = font_sz_result)
  def_cell_bdy <- fp_cell(background.color = trend_col[3], border = fp_border(color = '#444E65'))

  # set alignment and border for all parts of the table
  ft <- align(ft_result, align = 'center', part = 'all')
  ft <- border(ft, border = fp_border(color = '#444E65'), part = 'all')

  ft <- style(ft, pr_c = def_cell_hd, pr_t = def_txt_hd, part = 'head')

  # set default styling for the body
  ft <- style(ft, pr_c = def_cell_bdy, pr_t = def_txt_bdy, part = 'body')

  # set custom styling based on SK results
  ## will need to loop 3 times, one for each potential value
  col_names <- names(tbl_result)

  # formatting for increasing trends
  for(i in 1:length(names(tbl_result))) {
    condition <- formula(paste('~ ', col_names[[i]], ' == "h"', sep = ''))
    result <- formula(paste('~ ', col_names[[i]], sep = ''))

    ft <- style(ft, condition, result,
                pr_t = fp_text(color = "white", font.family = 'Wingdings 3', font.size = font_sz_result, bold = T),
                pr_c = fp_cell(background.color = trend_col[1], border = fp_border(color = '#444E65')))
  }

  # formatting for decreasing trends
  for(i in 1:length(names(tbl_result))) {
    condition <- formula(paste('~ ', col_names[[i]], ' == "i"', sep = ''))
    result <- formula(paste('~ ', col_names[[i]], sep = ''))

    ft <- style(ft, condition, result,
                pr_t = fp_text(color="white", font.family = 'Wingdings 3', font.size = font_sz_result),
                pr_c = fp_cell(background.color = trend_col[2], border = fp_border(color = '#444E65')))

  }

  # formatting for insufficient data for trends
  for(i in 1:length(names(tbl_result))) {
    condition <- formula(paste('~ ', col_names[[i]], ' == "x"', sep = ''))
    result <- formula(paste('~ ', col_names[[i]], sep = ''))

    ft <- style(ft, condition, result,
                pr_t = fp_text(color='#A5A5A5', font.size = font_sz_result),
                pr_c = fp_cell(background.color = trend_col[4], border = fp_border(color = '#444E65')))

  }

  # final formatting
  ft <- width(ft, width = 0.6)
  ft <- height(ft, height = ht_head, part = 'head')
  ft <- height(ft, height = ht_body, part = 'body')


  ft_header <- width(ft_header, j = ~ loc_id, width = 0.55)
  ft_header <- width(ft_header, j = ~ loc_name, width = 1.1)
  ft_header <- height(ft_header, height = ht_head, part = 'head')
  ft_header <- height(ft_header, height = ht_body, part = 'body')

  ls_ft <- list(ft_header, ft)

  return(ls_ft)

}
