# Browser-Based Survival Analysis Studio

This project turns a Shiny survival-analysis workflow into a static `shinylive` website, which means uploaded data is processed entirely in the browser through `webR` and is not sent to a server. That privacy-first deployment model is especially useful for medical or course datasets where local-only analysis is a real advantage.

The app opens with a complete worked example based on `survival::lung`, so reviewers can immediately see a Kaplan-Meier curve, a stratified Kaplan-Meier curve with risk table, a log-rank test, a multivariable Cox model, PH diagnostics, and downloadable outputs before uploading any file. Users can then switch to their own `.csv` or `.xlsx` data and remap the analysis fields from the sidebar.

## Features

- Overall Kaplan-Meier curve plus stratified and faceted Kaplan-Meier curves.
- Risk table / number at risk using `survminer::ggsurvplot(risk.table = TRUE)`.
- Median survival summary table with 95% confidence intervals.
- Log-rank test for the selected KM grouping variable.
- Multivariable Cox proportional hazards model with selectable predictors, categorical reference levels, `strata()` terms, and ties handling.
- Hazard-ratio forest plot via `survminer::ggforest()`.
- PH diagnostics from `cox.zph()`, including p-value table and Schoenfeld residual plot grid.
- Missing-data summary, event-value frequency table, and `time <= 0` filtering diagnostics.
- Downloadable filtered data, Excel workbook, plot images, a reproducible `.R` script, and a Quarto `.qmd` report prepared for Typst PDF rendering.

## First Load

Because the app runs fully in the browser, the first visit may take 10-20 seconds on slower networks while `webR` and its package assets download. Later visits are usually faster thanks to browser caching.

## Local Development

Install the required R packages, then run the app locally:

```r
shiny::runApp(".")
```

To export the static site locally:

```r
source("scripts/export_shinylive.R")
```

This creates a `site/` directory containing the GitHub Pages-ready static build.

## GitHub Pages Deployment

This repository includes `.github/workflows/deploy-pages.yml`, which:

1. installs the app dependencies,
2. exports the app with `shinylive::export()`,
3. uploads the generated `site/` folder as a GitHub Pages artifact,
4. deploys it with `actions/deploy-pages`.

In the repository settings, set **Pages** to use **GitHub Actions** as the source.

## Example Datasets

The app includes three built-in datasets from the `survival` package:

- `lung`
- `veteran`
- `ovarian`

`lung` is loaded by default with a prefilled working variable mapping so the app renders complete output immediately.
