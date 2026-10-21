# Functions defined in this file:
#   fmt_money0


# Number formatting ─────────────────────────────────────────────────────────

# Used in: figure_main, figure_appendix
fmt_money0 <- function(x) format(round(x, 0), big.mark = ",")
