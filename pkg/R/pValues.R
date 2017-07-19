#
# ==================================================================================================
#
# --------------- pFromT                         p-value from t-value                             .. # {{{
# RR 20150109     ---------------------------------------------------
# Manual          ------------------------------------------------------------------------------- --
#' @title       Calculate p-values from t-statistics.
#' @description Calculate p-values from t-statistics.
#' @details utility function for conversion of p-values
#' @param t_value The t-statistics to be converted into a p-Value.
#' @param df Degree of freedom in t-statistics.
#' @param two.sided Indicator of the statistics be calculatede as two-sided (TURE) or one-sided (FALSE).
#' @return corresponding p-Value from the t-, z- or F-statistics
#' @author Roland Rapold
#' @keywords p-value
#' @seealso \code{Link{pFromZ}} and \code{Link{pFromF}}
#' @examples
#'         pFromT(t_value=1.96, df=10)
#'         pFromT(t_value=1.96, df=100)
#'         pFromT(t_value=1.96, df=1000)
#'         pFromT(t_value=1.96, df=1000, two.sided = TRUE)
#' 
#'         x <- c(seq(0.0001, 0.6, 0.01), NA, seq(0.6001, 1.0, 0.01))
#'         p_value <- pFromT(t_value=x, df=100)
#'         p_value
#'         formatPValue(p_value, digits=3)
#' @export
pFromT <- function(t_value=1.96, df=10, two.sided=FALSE)
{
    #
    # P-Wert aus t-Wert:
    #
    ifelse(two.sided, factor <- 2, factor <- 1)
    t_value <- abs(t_value)
    #
    p_value <- factor * stats::pt(t_value, df=df, lower.tail=FALSE)
    return(p_value)
    #
}
# --------------- pFromT --------------------------------------------
# ENDE DER FUNKTION -------------------------------------------------
# # }}}
# ==================================================================================================
#
# --------------- pFromZ                         p-value from z-value (normal distribution)       .. # {{{
# RR 20150109     ---------------------------------------------------
# Manual          ------------------------------------------------------------------------------- --
#' @title       Calculate p-values from z-statistics.
#' @description Calculate p-values from z-statistics.
#' @details utility function for conversion of p-values
#' @param z_value The z-statistics to be converted into a p-Value.
#' @param two.sided Indicator of the statistics be calculatede as two-sided (TURE) or one-sided (FALSE).
#' @return corresponding p-Value from the t-, z- or F-statistics
#' @author Roland Rapold
#' @keywords p-value
#' @seealso \code{Link{pFromT}} and \code{Link{pFromF}}
#' @examples
#'         pFromF(f_value=35.68, df1=2,  df2=5)
#'         pFromF(f_value=35.68, df1=2,  df2=5, two.sided=TRUE)
#'         pFromF(f_value= 1.96, df1=10, df2=10)
#'         pFromF(f_value= 1.96, df1=10, df2=100)
#'         pFromF(f_value= 1.96, df1=10, df2=1000)
#'         pFromF(f_value= 1.96, df1=10, df2=1000, two.sided=TRUE)
#'         pFromF(f_value= 1.96, df1=10, df2=1000, two.sided=TRUE)
#' @export
pFromZ <- function(z_value=1.96, two.sided=FALSE)
{
    #
    # P-Wert aus z-Wert:
    #
    ifelse(two.sided, factor <- 2, factor <- 1)
    z_value <- abs(z_value)
    #
    p_value <- factor * stats::pnorm(z_value, lower.tail=FALSE)
    return(p_value)
    #
}
# --------------- pFromT --------------------------------------------
# ENDE DER FUNKTION -------------------------------------------------
# # }}}
# ==================================================================================================
#
# --------------- pFromF                         p-value from F-value                             .. # {{{
# RR 20150109     ---------------------------------------------------
# Manual          ------------------------------------------------------------------------------- --
#' @title       Calculate p-values from F-statistics.
#' @description Calculate p-values from F-statistics.
#' @details utility function for conversion of p-values
#' @param f_value The f-statistics to be converted into a p-Value.
#' @param df1,df2 Degrees of freedom in F-statistics.
#' @param two.sided Indicator of the statistics be calculatede as two-sided (TURE) or one-sided (FALSE).
#' @return corresponding p-Value from the t-, z- or F-statistics
#' @author Roland Rapold
#' @keywords p-value
#' @seealso \code{Link{pFromT}} and \code{Link{pFromZ}}
#' @examples
#'         pFromF(f_value=35.68, df1=2,  df2=5)
#'         pFromF(f_value=35.68, df1=2,  df2=5, two.sided=TRUE)
#'         pFromF(f_value= 1.96, df1=10, df2=10)
#'         pFromF(f_value= 1.96, df1=10, df2=100)
#'         pFromF(f_value= 1.96, df1=10, df2=1000)
#'         pFromF(f_value= 1.96, df1=10, df2=1000, two.sided=TRUE)
#'         pFromF(f_value= 1.96, df1=10, df2=1000, two.sided=TRUE)
#' @export
pFromF <- function(f_value=1.96, df1=10, df2=10, two.sided=FALSE)
{
    #
    # P-Wert aus F-Wert:
    #
    ifelse(two.sided, factor <- 2, factor <- 1)
    f_value <- abs(f_value)
    #
    p_value <- factor * stats::pf(f_value, df1=df1, df2=df2, lower.tail=FALSE)
    return(p_value)
    #
    # in analysis of variance table:
    # df_1      = df of variable
    # df_2      = df_res = df_tot - df1             ...
    # df_tot    = # measurements - 1                ...
    #
}
# --------------- pFromF --------------------------------------------
# ENDE DER FUNKTION -------------------------------------------------
# # }}}
# ==================================================================================================
