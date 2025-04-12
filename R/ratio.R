calc_ratio = function(data) {
    data |>
    pivot_wider(
      names_from = Level,
      values_from = n
    ) |>
    mutate(Total_Ratio = Total/sum(Total, na.rm = TRUE) *100) |>
    select(State,Year,undergrad_ratio,professional_ratio,grad_ratio,Total_Ratio)
}
