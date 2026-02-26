# Fly Egg-Lay Assay Analysis Tool

This repository contains a Shiny app (`app.R`) for exploratory analysis and publication-ready plotting of fly egg-lay assay datasets.

## Features

- Upload `.xlsx` files (with sheet selection) or paste tabular data.
- Interactive mapping of x/y/fill/shape/facet variables.
- Dynamic filtering and optional ID-based row selection.
- Boxplot + jitter plotting with configurable styling.
- Optional x-axis background shading by specific levels or numeric ranges.
- Assumption checks (Shapiro-Wilk, Fligner-Killeen, Bartlett, outlier count).
- Configurable ANOVA with optional blocks and interactions.
- Optional Tukey HSD post-hoc tests and compact letter display overlays.
- Plot splitting by panel variable and PNG export.

## Run locally

```r
install.packages(c("shiny", "readxl", "dplyr", "ggplot2", "janitor", "multcompView"))
shiny::runApp("app.R")
```

