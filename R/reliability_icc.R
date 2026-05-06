# @param ratings A subject x rater matrix or data.frame
# @param model One of "oneway" or "twoway"
# @param type One of "consistency" or "agreement"
# @param unit One of "single" or "average"
# @param r0 Null hypothesis value of the ICC
# @param conf.level Confidence level for the interval
# @return A list with elements: subjects, raters, model, type, unit, icc.name,
#   value, r0, Fvalue, df1, df2, p.value, conf.level, lbound, ubound
# @keywords internal
# @noRd
reliability_icc <- function(ratings,
                            model = c("oneway", "twoway"),
                            type  = c("consistency", "agreement"),
                            unit  = c("single", "average"),
                            r0 = 0, conf.level = 0.95) {
  model <- match.arg(model)
  type  <- match.arg(type)
  unit  <- match.arg(unit)
  irr::icc(ratings, model = model, type = type, unit = unit,
           r0 = r0, conf.level = conf.level)
}
