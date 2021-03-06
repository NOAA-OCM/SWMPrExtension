#' Create a Flextable Object of Seasonal Kendall Results
#'
#' Create a flextable object to display Seasonal Kendall results for each reserve in the NERRS national level template
#'
#' @param sk_result a \code{data.frame} of reformatted results generated by national Level template scripts from reserve level handoff files
#' @param param chr, the name of the parameter that corresponds to the seasonal kendall results in \code{sk_result}
#' @param font_sz_result int, specify the font size of the displayed results
#' @param font_sz_head int, specify the font size of the table header row
#' @param ht_head num, specify the cell height of the table body rows. Units for this parameter are in inches.
#' @param ht_body num, specify the cell height of the table header row. Units for this parameter are in inches.
#'
#' @importFrom flextable flextable style align border width height
#' @importFrom stats formula
#' @importFrom officer fp_border fp_cell fp_par fp_text
#'
#' @export
#'
#' @details This function is intended for internal use with the NERRS national level reporting scripts. Using results from the reserve level trend analyses, \code{create_sk_national_ft_results} creates  a \code{flextable} object of seasonal kendall results to be displayed in the NERRS national level template.
#'
#' @author Julie Padilla
#'
#' @concept Reporting
#'
#' @return Returns a \code{\link[flextable]{flextable}} object
#'

create_sk_national_ft_results <- function(sk_result, param
                                          , font_sz_result = 12, font_sz_head = 8
                                          , ht_head = 0.375, ht_body = 0.2) {

  tbl_result <- sk_result[ , c(2:length(names(sk_result)))]

  # RESULTS TBL ----
  # make flextable object for SK results
  ft_result <- flextable(data = tbl_result)

  # change column names to words
  loc_nms <- paste('LOC', 1:length(ft_result$header$dataset))
  result_col_names <- ft_result$header$dataset
  result_col_names[1, ] <- loc_nms
  ft_result$header$dataset <- result_col_names

  # generate a data.frame with preferred names for head
  nms <- ft_col_names(param = param) #%>% gsub('-', '', .)

  ft_result <- add_header(ft_result, LOC.1 = nms, LOC.2 = nms, LOC.3 = nms, LOC.4 = nms)
  ft_result <- merge_h(ft_result, part = 'header')

  # set styling elements
  def_par <- fp_par(text.align = 'center')
  def_txt_hd <- fp_text(color = '#404040', bold = TRUE, font.size = font_sz_head)
  def_cell_hd <- fp_cell(background.color = 'white', border = fp_border(color = '#444E65')
                         , margin.top = 2, margin.bottom = 2)
  def_txt_bdy <- fp_text(color = '#444E65', font.size = font_sz_result)
  def_cell_bdy <- fp_cell(background.color = '#D9D9D9', border = fp_border(color = '#444E65'))

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
                pr_t = fp_text(color = "white", font.family = 'Wingdings 3', font.size = font_sz_result, bold = TRUE),
                pr_c = fp_cell(background.color = '#247BA0', border = fp_border(color = '#444E65')))
  }

  # formatting for decreasing trends
  for(i in 1:length(names(tbl_result))) {
    condition <- formula(paste('~ ', col_names[[i]], ' == "i"', sep = ''))
    result <- formula(paste('~ ', col_names[[i]], sep = ''))

    ft <- style(ft, condition, result,
                pr_t = fp_text(color="white", font.family = 'Wingdings 3', font.size = font_sz_result),
                pr_c = fp_cell(background.color = '#A3DFFF', border = fp_border(color = '#444E65')))
  }

  # formatting for insufficient data for trends
  for(i in 1:length(names(tbl_result))) {
    condition <- formula(paste('~ ', col_names[[i]], ' == "x"', sep = ''))
    result <- formula(paste('~ ', col_names[[i]], sep = ''))

    ft <- style(ft, condition, result,
                pr_t = fp_text(color='#A5A5A5', font.size = font_sz_result),
                pr_c = fp_cell(background.color = 'white', border = fp_border(color = '#444E65')))
  }

  # final formatting
  ft <- width(ft, width = 0.344) #11/32"
  ft <- height(ft, height = ht_head, part = 'head')
  ft <- height(ft, height = ht_body, part = 'body')

  return(ft)

}
