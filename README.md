# AQS AI Usage Baseline Survey

A bilingual (Chinese/English) Shiny survey app for the AQS department's 2026 AI tool rollout planning.

## Features

- One question per screen with smooth fade transitions
- Section A / Section B clearly labelled
- Drag-and-drop ranking for Q3, Q7, Q9
- "Other — please specify" text fields on all relevant questions
- Skip logic: selecting **G** on Q2 automatically skips Q3 and Q4
- 100% anonymous — responses saved as timestamped `.rds` files (gitignored)
- Progress bar showing completion percentage
- Back navigation respecting skip logic

## Setup

```r
install.packages(c("shiny", "bslib", "shinyjs", "sortable",
                   "dplyr", "tidyr", "readr", "stringr", "purrr"))
```

## Run the survey

```r
shiny::runApp("aqs-survey")
```

## Analyse results

After collecting responses, run from the project root:

```r
source("aqs-survey/analyze_responses.R")
```

This prints a summary report and exports `responses_combined.csv`.

## File structure

```
aqs-survey/
├── app.R                  # Main Shiny app
├── analyze_responses.R    # Analysis helper script
├── www/
│   └── style.css          # Custom UI styles
├── responses/             # Auto-created; gitignored
└── .gitignore
```

## Question map

| Page | Question | Type |
|------|----------|------|
| 1 | Q0 — Role | Single choice |
| 2 | Q1 — Tools used | Multiple choice |
| 3 | Q2 — Most frequent tool | Single choice (skip logic) |
| 4 | Q3 — Task ranking | Drag-and-drop ranking |
| 5 | Q4 — Usage frequency | Single choice |
| 6 | Q5 — Helpful moment | Single choice + optional text |
| 7 | Q6 — Failed tasks | Multiple choice (max 3) |
| 8 | Q7 — Challenge ranking | Drag-and-drop ranking |
| 9 | Q8 — Agreement statement | Single choice |
| 10 | Q9 — Desired tools ranking | Drag-and-drop ranking |
| 11 | Q10 — Open question | Free text (optional) |
