# --------------- pValues                           convert statistics to p-Vaues                 --
# RR 20160320     ------------------------------------------------------------------------------- --
#
# --------------- pFromT                         p-value from t-value                             .. # {{{
# RR 20150109     ---------------------------------------------------
pFromT <- function(t_value=1.96, df=10, two.sided=FALSE)
{
    #
    # Beispiel: 8% bei t-Verteilung mit 10 Freiheitsgraden
    if (0==1) {
        pFromT(t_value=1.96, df=10)
        pFromT(t_value=1.96, df=100)
        pFromT(t_value=1.96, df=1000)
        pFromT(t_value=1.96, df=1000, two.sided=TRUE)
    }
    #
    # P-Wert aus t-Wert:
    #
    ifelse(two.sided, factor <- 2, factor <- 1)
    t_value <- abs(t_value)
    #
    p_value <- factor * pt(t_value, df=df, lower.tail=FALSE)
    return(p_value)
    #
}
# --------------- pFromT --------------------------------------------
# ENDE DER FUNKTION -------------------------------------------------
# # }}}
# --------------- pFromZ                         p-value from z-value (normal distribution)       .. # {{{
# RR 20150109     ---------------------------------------------------
pFromZ <- function(z_value=1.96, two.sided=FALSE)
{
    #
    # Beispiel: 8% bei T-Verteilung mit 10 Freiheitsgraden
    if (0==1) {
        pFromZ(z_value=1.96)
        pFromZ(z_value=1.96, two.sided=TRUE)
    }
    #
    # P-Wert aus z-Wert:
    #
    ifelse(two.sided, factor <- 2, factor <- 1)
    z_value <- abs(z_value)
    #
    p_value <- factor * pnorm(z_value, lower.tail=FALSE)
    return(p_value)
    #
}
# --------------- pFromT --------------------------------------------
# ENDE DER FUNKTION -------------------------------------------------
# # }}}
# --------------- pFromF                         p-value from F-value                             .. # {{{
# RR 20150109     ---------------------------------------------------
pFromF <- function(f_value=1.96, df1=10, df2=10, two.sided=FALSE)
{
    #
    # Beispiel: 8% bei T-Verteilung mit 10 Freiheitsgraden
    if (0==1) {
        pFromF(f_value=35.68, df1=2, df2=5)
        pFromF(f_value=35.68, df1=2, df2=5, two.sided=TRUE)
        pFromF(f_value=1.96, df1=10, df2=10)
        pFromF(f_value=1.96, df1=10, df2=100)
        pFromF(f_value=1.96, df1=10, df2=1000)
        pFromF(f_value=1.96, df1=10, df2=1000, two.sided=TRUE)
    }
    #
    # P-Wert aus t-Wert:
    #
    ifelse(two.sided, factor <- 2, factor <- 1)
    f_value <- abs(f_value)
    #
    p_value <- factor * pf(f_value, df1=df1, df2=df2, lower.tail=FALSE)
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
#
# END OF FUNCTION  ----------------------------------------------------------------------------- --
# --------------- pValues ----------------------------------------------------------------------- --
#

