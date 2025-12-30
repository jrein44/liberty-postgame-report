# WNBA Post-Game Report Generator

An automated post-game analytics report built with R, using Second Spectrum tracking data. This project demonstrates sports analytics skills including data processing, visualization, and player/lineup analysis.

## 📊 Live Report
**[View Sample Report: NY Liberty vs Washington Mystics (9/9/2025)](https://jrein44.github.io/liberty-postgame-report/)**

## Features

### Game Summary
- Quarter-by-quarter team stats with +/- differential
- Shooting efficiency by location (Rim, Paint, Mid-Range, 3PT)
- Pace analysis (Transition vs Halfcourt)

### Player Analysis
- Individual shooting stats (FG%, 3P%, PPS, AST)
- Shot charts using official WNBA court dimensions via `sportyR`

### Advanced Analytics
- **Zone Analysis**: FG% breakdown by court zone with visual comparison
- **Play Type Breakdown**: Halfcourt, Transition, and Putback efficiency
- **Substitution Patterns**: Rotation timeline with stint +/- (Gantt-style visualization)
- **Lineup Stats**: 5-man unit analysis with offensive/defensive ratings and net rating

### Four Factors
- eFG%, TOV%, OREB%, PPP comparison

## Sample Visualizations

### Shot Charts
Side-by-side shot charts with made/missed shots on WNBA court

### Substitution Maps
Player rotation timelines showing when each player was on court, color-coded by +/- during their stints

### Zone Efficiency
Bar chart comparing FG% by zone (Rim, Paint, Mid-Range, Above Break 3, Corner 3)

## Tech Stack

- **R** with RMarkdown for report generation
- **tidyverse** (dplyr, tidyr, ggplot2) for data manipulation and visualization
- **gt** for styled tables
- **sportyR** for accurate WNBA court rendering
- **jsonlite** for Second Spectrum data parsing
- **patchwork** for combining plots

## Data Source

This report uses [Second Spectrum](https://www.secondspectrum.com/) tracking data, which provides:
- Player tracking coordinates
- Possession-level statistics
- Shot locations and outcomes
- Event-by-event game logs

## Project Structure

```
wnba-postgame-report/
├── data/
│   ├── metadata_json.json          # Game metadata, rosters, scores
│   ├── basketball-possessions.json # Possession-level data
│   ├── basketball-events.jsonl     # Event stream (touches, passes, etc.)
│   └── basketball-markings__3_.json # Shot attempts with locations
├── process_second_spectrum.R        # Data processing functions
├── postgame_report.Rmd             # RMarkdown report template
├── output/
│   └── index.html                  # Generated report
└── README.md
```

## Usage

1. Clone the repo
2. Install required packages:
```r
install.packages(c("rmarkdown", "dplyr", "tidyr", "ggplot2", "gt", "jsonlite", "sportyR", "patchwork"))
```
3. Place Second Spectrum data files in `data/` folder
4. Render the report:
```r
rmarkdown::render("postgame_report.Rmd", output_dir = "output")
```

## Customization

The report is parameterized and can be adapted for any game by:
1. Updating the data files in `data/`
2. Modifying team colors in the setup chunk
3. Adjusting the `data_dir` parameter

## About

- **LinkedIn:** www.linkedin.com/in/jessica-reinhardt
- **Email:** jess.reinhardt44@gmail.com
- **GitHub:** https://github.com/jrein44

---
