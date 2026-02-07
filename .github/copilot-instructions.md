# AI Coding Agent Instructions: Survivor Pool Dashboard

## Project Overview
This is an **R Shiny dashboard** for a family Survivor fantasy league. Players pick 5 castaways (1 MVP + 4 others) and score points as their picks survive each week. The app reads data from Google Sheets and provides interactive scorekeeping with visualizations.

**Live app:** https://erichdenk.shinyapps.io/survivor-pool/

## Architecture & Data Flow

### Single-File Shiny App
- Primary file: `app.R` (~1300 lines)
- Structure: UI definition → Server logic → `shinyApp()` call
- No separate modules - all functionality in one file

### Data Source: Google Sheets (read-only, public)
```r
gs4_deauth()  # No authentication required
sheet_id <- "1WFD5SAMUbJO40HbMj3TtjDCWgVbdxw0ftDgmVbhHIb4"
```

**Required sheets with exact column names:**
- `Tribes`: `cast`, `tribe`, `color` (hex codes for tribe colors)
- `Picks`: `Contestant`, `MVP`, `Pick2`, `Pick3`, `Pick4`, `Pick5`
- `Eliminated`: `cast`, `week`, `placement` (1=winner, 2=runner-up, 3=third)

**Critical:** Column names are case-sensitive. Use `rename_with(tolower)` on sheets that need it.

## Scoring Algorithm

### Weekly Scoring Logic (`weekly_score` function)
- **Pre-merge (weeks 1-5):** 1 point per surviving pick
- **Post-merge (week 6+):** 3 points per surviving pick
- Global: `mergeweek <- 6`, `currentweek <- max(eliminated$week)`

### Bonus Points (calculated in `scoreboard_data` reactive)
```r
mvpbonus    # 30 points if MVP wins the season
winneradd   # 30 points if winner is on team (any position)
secondadd   # 20 points if runner-up is on team
thirdadd    # 10 points if 3rd place is on team
```

### Winner Detection (dynamic)
Depends on `placement` column in `Eliminated` sheet. If missing, falls back to `NA` (season still in progress).

## Key Shiny Patterns

### Reactive Data Flow
```r
scoreboard_data() <- reactive({...}) %>% bindCache(weekInput())
```
- Central reactive builds full scoreboard by calling `weekly_score()` for each week 1→N
- Uses `bindCache()` for performance - only recalculates when week slider changes
- All value boxes and tables derive from this single reactive

### Interactive Visualizations (ggiraph)
All plots use `geom_*_interactive()` + `girafe()` for hover tooltips:
- Score trajectory over time
- Risk/Reward scatter (popularity vs performance)
- Head-to-head comparison trajectories

Example pattern:
```r
geom_line_interactive(aes(tooltip = paste0(...), data_id = Contestant))
girafe(ggobj = p, options = list(opts_hover(...), opts_sizing(...)))
```

## Custom Styling

### CSS Theme
- Dark mode: charcoal background `#2a2e3d`, panel `#323745`
- Accent color: blue `#4a90a4`, highlight `#6db8d4`
- All styles inline in `tags$head(tags$style(HTML(...)))`

### Tribe Badge Formatter
```r
tribe_badge_formatter(pick_col)  # Returns formattable() formatter
```
- Applies tribe background colors from Google Sheet `color` column
- Strikes through eliminated castaways (gray text)
- Used on MVP, Pick2-5 columns in tables

### Plot Theme (`mytheme()`)
- Based on `ggthemes::theme_tufte()`
- Consistent dark backgrounds for all plots
- Dotted y-gridlines, solid blue x-axis

## Dashboard Tabs

1. **Scoreboard** - Main leaderboard with week slider, value boxes for top 3
2. **Visuals** - Popular picks bar chart, picks by tribe
3. **Pick Performance** - Aggregate stats per castaway (best/worst picks)
4. **Head-to-Head** - Compare 2-3 contestants' teams and trajectories
5. **Rules** - Static markdown explanation (in UI)

## Development Workflow

### Running Locally
```r
# In RStudio or R console:
shiny::runApp("app.R")

# Or use RStudio "Run App" button (top-right of editor)
```

### Deployment to shinyapps.io
```r
rsconnect::deployApp(appName = "survivor-pool", account = "erichdenk")
```
Config stored in `rsconnect/shinyapps.io/erichdenk/survivor-pool.dcf`

### Testing Changes
1. Modify `app.R`
2. Reload app (RStudio auto-detects changes)
3. Interact with dashboard - check browser console for JS errors
4. Verify reactive updates (e.g., week slider triggers table refresh)

## Common Modification Patterns

### Adding a New Tab
1. Add `menuItem()` in `dashboardSidebar(sidebarMenu(...))`
2. Add `tabItem()` in `dashboardBody(tabItems(...))`
3. Add corresponding `output$*` in server function

### Modifying Scoring
- Adjust `mergeweek` or point multipliers in `weekly_score()`
- Update bonus calculations in `scoreboard_data()` reactive
- Change affects all downstream reactives automatically

### Adding New Google Sheet Column
1. Update `stopifnot()` defensive checks at top
2. Access via `picks$NewColumn` or `eliminated$newcolumn` (mind case!)
3. Join to other data as needed

### Styling Updates
- Modify CSS in `tags$head(tags$style(HTML(...)))`
- Adjust `mytheme()` for plot consistency
- Update `tribe_badge_formatter()` for table badges

## Dependencies & Package Management
Install all packages from top of `app.R`:
```r
install.packages(c("shinydashboard", "bslib", "fontawesome", "formattable",
                   "tidyverse", "markdown", "forcats", "googlesheets4", "ggiraph"))
```

## New Season Preparation Checklist

Before each new Survivor season, update the following:

1. **Google Sheet Setup**
   - Create/update `Tribes` sheet with new cast members, tribes, and colors
   - Clear `Eliminated` sheet (or archive previous season)
   - Update `Picks` sheet with new contestant picks
   
2. **Code Updates in `app.R`**
   - Update `mergeweek` global (usually week 6, but verify with season format)
   - Reset `sheet_id` if using a new Google Sheet
   - Clear winner/finalist globals: set `winner`, `second`, `third` to `NA` at season start
   
3. **Testing**
   - Run app locally: `shiny::runApp("app.R")`
   - Verify all tabs load without errors
   - Test week slider (should go from 1 to current week)
   - Confirm tribe colors display correctly in badges
   - Check that eliminated castaways appear struck-through
   
4. **Deploy**
   - Deploy to shinyapps.io: `rsconnect::deployApp()`
   - Test live URL to ensure data loads correctly

## Project Quirks
- Two `app.R` files exist: root and `survivor-pool/app.R` (latter appears older/backup)
- **Main development file is root `app.R`** - the `survivor-pool/` version should be deleted or kept in sync
- Season-specific globals: `mergeweek`, `winner`, `second`, `third` - update between seasons
- `currentweek` auto-calculated from `eliminated` sheet max week
- No automated tests - manual verification via dashboard interaction
