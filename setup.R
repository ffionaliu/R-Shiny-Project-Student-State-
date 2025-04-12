library(tidyverse)

url = "https://raw.githubusercontent.com/wadefagen/datasets/master/students-by-state/uiuc-students-by-state.csv"
students_state = read_csv(url)
view(students_state)

students_state = students_state |>
  mutate(n = Undergrad +Professional+Grad) |>
  mutate(undergrad_ratio = Undergrad/n *100) |>
  mutate(professional_ratio = Professional/n *100) |>
  mutate(grad_ratio = Grad/n * 100) |>
  pivot_longer(
    cols = Undergrad : Total,
    names_to = "Level",
    values_to = "Number"
  ) |>
  select(State, Year, Level,undergrad_ratio,professional_ratio,grad_ratio,n)
view(students_state)
write_csv(x = students_state, file ="data/students_state.csv")

ratio_order = c(
  "undergrad_ratio",
  "professional_ratio",
  "grad_ratio"
)


students_state |>
  filter(State == "Alaska") |>
  filter(Year == 1976) |>
  pivot_longer(
    cols = undergrad_ratio : grad_ratio,
    names_to = "Ratio",
    values_to = "Percent"
  )|>
  mutate(Ratio = factor(Ratio, levels = ratio_order)) |>
  filter(Level == "Undergrad") |>
  ggplot() +
  aes(x = Ratio, y = Percent,fill = Ratio) |>
  geom_col()

