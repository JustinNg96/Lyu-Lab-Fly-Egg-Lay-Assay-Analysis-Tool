# Fly Egg-Lay Assay Analysis Tool

This repository contains a Shiny app (`app.R`) for exploratory analysis and publication-ready plotting of fly egg-lay assay datasets.

## Features

- Upload `.xlsx` files (with sheet selection) or paste tabular data.
- Interactive mapping of x/y/fill/shape/facet variables.
- Dynamic filtering and optional ID-based row selection.
- Boxplot + jitter plotting with configurable styling.
- Optional x-axis background shading by specific levels or numeric ranges.
- Assumption checks (Fligner-Killeen, Bartlett, Shapiro-Wilk, outlier count).
- Configurable regular multi-way ANOVA or multi-way Welch ANOVA (Welch for 2+ factors requires `welchADF`), with optional interactions and blocks for regular ANOVA.
- Optional Tukey HSD post-hoc tests and compact letter display overlays.
- Plot splitting by panel variable and PNG export.

## Run locally

```r
install.packages(c("shiny", "readxl", "dplyr", "ggplot2", "janitor", "multcompView", "welchADF"))
shiny::runApp("app.R")
```

