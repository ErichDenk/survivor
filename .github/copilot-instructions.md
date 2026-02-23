# AI Coding Agent Instructions: Survivor Pool Dashboard

## Project Overview
This is an **R Shiny dashboard** for a family Survivor fantasy league. Players pick 5 castaways (1 MVP + 4 others) and score points as their picks survive each week. The app reads data from Google Sheets and provides interactive scorekeeping with visualizations.

**Live app:** https://erichdenk.shinyapps.io/survivor-pool/

## Architecture & Data Flow

### Single-File Shiny App
- Primary file: `app.R` (~1900 lines)
- Structure: globals/helpers → UI definition → Server logic → `shinyApp()` call
- No separate modules - all functionality in one file

### Data Source: Google Sheets (read-only, public)
```r
gs4_deauth()  # No authentication required
sheet_id <- "1WFD5SAMUbJO40HbMj3TtjDCWgVbdxw0ftDgmVbhHIb4"
```

**Required sheets with exact column names:**
- `Tribes`: `cast`, `tribe`, `color` (hex codes for tribe colors)
- `Picks`: `Contestant`, `MVP`, `Pick2`, `Pick3`, `Pick4`, `Pick5`
- `Eliminated`: `cast`, `week`

**Optional sheets:**
- `Finale`: `cast`, `finish` (values: `"winner"`, `"second"`, `"third"`) — add after finale airs
- `Idols`: `cast`, `week_found` — one row per idol find event

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
idol_bonus  # 2 points per team member who found an idol (from Idols sheet)
```

### Winner/Finalist Detection
Driven by the optional `Finale` sheet. If the sheet doesn't exist or is empty, all finale bonuses are `NA` (season still in progress).

## Key Shiny Patterns

### Reactive Data Flow
```r
scoreboard_data <- reactive({...}) %>% bindCache(weekInput())
all_trajectory_data <- reactive({ compute_trajectory(currentweek) })
```
- `scoreboard_data()` is the central reactive — builds the full scoreboard for the selected week
- `all_trajectory_data()` is a shared reactive used by both trajectory charts (avoids duplicate computation)
- Uses `bindCache()` so the scoreboard only recalculates when the week slider changes
- All value boxes, tables, and charts derive from these reactives

### Interactive Visualizations (ggiraph)
All plots use `geom_*_interactive()` + `girafe()` for hover tooltips:
- Score trajectory over time (`ScoreTrajectory`)
- Risk/Reward scatter (popularity vs performance, `RiskReward`)
- Head-to-head comparison trajectories (`h2h_trajectory`)

Example pattern:
```r
geom_line_interactive(aes(tooltip = paste0(...), data_id = Contestant))
girafe(ggobj = p, options = list(opts_hover(...), opts_sizing(...)))
```

## Custom Styling

### CSS Theme
- Light mode: white/light-gray background (`#f8f9fa`, `#ffffff`)
- Accent color: blue `#2563eb`
- All styles inline in `tags$head(tags$style(HTML(...)))`
- Google Fonts: Inter (body), Bebas Neue (headers/titles)

### Tribe Badge Formatter
Two badge formatters exist:
- `tribe_badge_formatter(pick_col)` — used in the H2H team comparison table
- `badge_fmt` (local inside `renderFormattable`) — used in the main scoreboard table

Both apply tribe background colors from the Google Sheet `color` column and strike through eliminated castaways (gray text). The local `badge_fmt` is more performant (pre-computes lookups as vectors).

### Plot Theme (`mytheme()`)
- Based on `ggthemes::theme_tufte()`
- Light backgrounds for all plots
- Dotted y-gridlines, solid x-axis

### Status Column Colors
The `Status` column (scoreboard + pick performance table) uses plain text labels with color formatting — no emoji, to avoid `formattable`'s unicode escape rendering bug:
- `"Full Team"` → green `#059669`
- `"Strong"` → green `#16a34a`
- `"Vulnerable"` → amber `#d97706`
- `"Critical"` → red `#dc2626`
- `"Eliminated"` → gray `#6b7280`

## Dashboard Tabs

1. **Scoreboard** - Main leaderboard with week slider, value boxes for top 3, full pick table
2. **Visuals** - Score trajectory over time + Risk/Reward scatter matrix
3. **Pick Performance** - Aggregate stats per castaway (best/worst picks, points breakdown)
4. **Head-to-Head** - Compare 2-3 pool contestants' teams, trajectories, and pick overlap
5. **Rules** - Static scoring rules explanation

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
- Mirror any changes in `compute_trajectory()` so the charts stay in sync

### Adding New Google Sheet Column
1. Update `stopifnot()` defensive checks at top
2. Access via `picks$NewColumn` or `eliminated$newcolumn` (mind case!)
3. Join to other data as needed

### Styling Updates
- Modify CSS in `tags$head(tags$style(HTML(...)))`
- Adjust `mytheme()` for plot consistency
- Update `tribe_badge_formatter()` / `badge_fmt` for table badges

## Dependencies & Package Management
Install all packages from top of `app.R`:
```r
install.packages(c("shinydashboard", "bslib", "fontawesome", "formattable",
                   "tidyverse", "markdown", "forcats", "googlesheets4", "ggiraph",
                   "ggthemes", "RColorBrewer"))
```

## New Season Preparation Checklist

Before each new Survivor season, update the following:

1. **Google Sheet Setup**
   - Create/update `Tribes` sheet with new cast members, tribes, and colors
   - Clear `Eliminated` sheet (or archive previous season data)
   - Update `Picks` sheet with new pool contestant picks
   - Clear `Finale` and `Idols` sheets (or leave empty until needed)

2. **Code Updates in `app.R`**
   - Update `mergeweek` global (usually week 6, but verify with season format)
   - Reset `sheet_id` if using a new Google Sheet

3. **Testing**
   - Run app locally: `shiny::runApp("app.R")`
   - Verify all tabs load without errors
   - Test week slider (should go from 1 to current week)
   - Confirm tribe colors display correctly in badges
   - Check that eliminated castaways appear struck-through
   - Verify trajectory chart final scores match scoreboard

4. **Deploy**
   - Deploy to shinyapps.io: `rsconnect::deployApp()`
   - Test live URL to ensure data loads correctly

## Project Quirks
- Season-specific globals: `mergeweek` — update between seasons
- `currentweek` auto-calculated from `eliminated` sheet max week
- `compute_trajectory()` is O(n²) in weeks — fine for a ~14-week season but worth knowing
- No automated tests — manual verification via dashboard interaction
- `formattable` escapes multi-byte emoji as `<U+XXXX>` — use plain text labels with color formatters instead
