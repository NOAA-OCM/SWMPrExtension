#' Create a Flextable Object of Reserve Names
#'
#' Create a \code{\link[flextable]{flextable}} of reserve names for use with the NERRS national level template.
#'
#' @param sk_result a \code{data.frame} of reformatted results from \code{\link{sk_seasonal}}
#' @param font_sz_stn int, specify the font size of displayed station names
#' @param font_sz_head int, specify the font size of the table header row
#' @param ht_head num, specify the cell height of the table body rows. Units for this parameter are in inches.
#' @param ht_body num, specify the cell height of the table header row. Units for this parameter are in inches.
#'
#' @importFrom flextable add_header align border flextable height merge_h padding style width
#' @importFrom stats formula
#' @importFrom officer fp_border fp_cell fp_par fp_text
#'
#' @export
#'
#' @details This function is intended for internal use with the NERRS national level reporting scripts. Using results from the reserve level trend analyses, \code{create_sk_national_ft_reserves} creates a \code{flextable} object of reserve names for display in the NERRS national level template.
#'
#' @author Julie Padilla
#'
#' @concept reporting
#'
#' @return Returns a \code{\link[flextable]{flextable}} object
#'

create_sk_national_ft_reserves <- function(sk_result
                                           , font_sz_stn = 8, font_sz_head = 8
                                           , ht_head = 0.75, ht_body = 0.2) {

  tbl_station <- sk_result[ , 1]
  # tbl_result <- tbl[ , c(2:length(names(tbl)))]

  # STATIONS TBL ----
  # make flextable object
  ft_header <- flextable::flextable(data = tbl_station)

  # change column names to words
  header_col_names <- ft_header$header$dataset
  header_col_names[1, ] <- c('Reserve')
  ft_header$header$dataset <- header_col_names

  # format
  def_txt_hd <- fp_text(color = '#404040', bold = TRUE, font.size = font_sz_head)
  def_txt_bdy <- fp_text(color = '#404040', bold = TRUE, font.size = font_sz_stn)
  def_par_hd <- fp_par(text.align = 'center')
  def_par_bdy <- fp_par(text.align = 'left')
  def_cell <- fp_cell(background.color = "white", border = fp_border(color = '#444E65'))

  ft_header <- style(ft_header, pr_c = def_cell, pr_t = def_txt_hd, pr_p = def_par_hd, part = 'head')
  ft_header <- style(ft_header, pr_c = def_cell, pr_t = def_txt_bdy, pr_p = def_par_bdy, part = 'body')

  ft_header <- width(ft_header, j = ~ Reserve.Name, width = 1.875)
  ft_header <- height(ft_header, height = ht_head, part = 'head')
  ft_header <- height(ft_header, height = ht_body, part = 'body')
  ft_header <- padding(ft_header, padding.left = 2)

  return(ft_header)

}
