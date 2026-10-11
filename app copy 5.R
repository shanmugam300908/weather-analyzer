
# ============================================================
# WEATHER PATTERN ANALYZER & CLIMATE DASHBOARD
# R ANALYSIS + AUTOMATIC WEBPAGE
# ============================================================

# 1. PROJECT FOLDER

project_folder <- getwd()

output_folder <- file.path(
  project_folder,
  "Weather_Analysis_Results"
)

if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

# 2. GENERATE SAMPLE WEATHER DATA

set.seed(789)

days <- 1:30

weather_data <- data.frame(
  Day = days,
  Date = seq(
    as.Date("2026-09-01"),
    by = "day",
    length.out = 30
  ),
  Temperature_C = round(
    runif(30, 22, 38), 1
  ),
  Humidity_Percent = sample(
    40:95, 30, replace = TRUE
  ),
  Rainfall_mm = round(
    ifelse(
      runif(30) < 0.35,
      runif(30, 0.5, 25),
      0
    ), 1
  )
)

weather_data$Temperature_Status <- ifelse(
  weather_data$Temperature_C >= 35,
  "Hot",
  ifelse(
    weather_data$Temperature_C >= 28,
    "Warm",
    "Mild"
  )
)

weather_data$Rain_Status <- ifelse(
  weather_data$Rainfall_mm > 0,
  "Rainy",
  "No Rain"
)

# 3. CALCULATE WEATHER STATISTICS

average_temp <- round(
  mean(weather_data$Temperature_C), 1
)

maximum_temp <- max(
  weather_data$Temperature_C
)

minimum_temp <- min(
  weather_data$Temperature_C
)

average_humidity <- round(
  mean(weather_data$Humidity_Percent), 1
)

total_rainfall <- round(
  sum(weather_data$Rainfall_mm), 1
)

rainy_days <- sum(
  weather_data$Rainfall_mm > 0
)

hottest_day <- weather_data[
  which.max(weather_data$Temperature_C), ]

rainiest_day <- weather_data[
  which.max(weather_data$Rainfall_mm), ]

# 4. SAVE WEATHER DATA

write.csv(
  weather_data,
  file.path(output_folder, "Weather_Dataset.csv"),
  row.names = FALSE
)

# 5. SAVE SUMMARY DATA

weather_summary <- data.frame(
  Metric = c(
    "Average Temperature (C)",
    "Maximum Temperature (C)",
    "Minimum Temperature (C)",
    "Average Humidity (%)",
    "Total Rainfall (mm)",
    "Rainy Days"
  ),
  Value = c(
    average_temp,
    maximum_temp,
    minimum_temp,
    average_humidity,
    total_rainfall,
    rainy_days
  )
)

write.csv(
  weather_summary,
  file.path(output_folder, "Weather_Summary.csv"),
  row.names = FALSE
)

# 6. GRAPH 1: TEMPERATURE TREND

png(
  file.path(output_folder, "01_Temperature_Trend.png"),
  width = 1000,
  height = 650
)

plot(
  weather_data$Day,
  weather_data$Temperature_C,
  type = "o",
  pch = 16,
  col = "tomato",
  main = "Daily Temperature Trend",
  xlab = "Day of Month",
  ylab = "Temperature (C)"
)

abline(
  h = average_temp,
  col = "blue",
  lty = 2,
  lwd = 2
)

legend(
  "topleft",
  legend = c("Daily Temperature", "Average Temperature"),
  col = c("tomato", "blue"),
  lty = c(1, 2),
  bty = "n"
)

dev.off()

# 7. GRAPH 2: HUMIDITY TREND

png(
  file.path(output_folder, "02_Humidity_Trend.png"),
  width = 1000,
  height = 650
)

plot(
  weather_data$Day,
  weather_data$Humidity_Percent,
  type = "o",
  pch = 16,
  col = "steelblue",
  main = "Daily Humidity Trend",
  xlab = "Day of Month",
  ylab = "Humidity (%)",
  ylim = c(0, 100)
)

dev.off()

# 8. GRAPH 3: DAILY RAINFALL

png(
  file.path(output_folder, "03_Rainfall_Trend.png"),
  width = 1000,
  height = 650
)

barplot(
  weather_data$Rainfall_mm,
  names.arg = weather_data$Day,
  col = "deepskyblue3",
  border = NA,
  main = "Daily Rainfall",
  xlab = "Day of Month",
  ylab = "Rainfall (mm)",
  cex.names = 0.7
)

dev.off()

# 9. GRAPH 4: TEMPERATURE DISTRIBUTION

png(
  file.path(output_folder, "04_Temperature_Distribution.png"),
  width = 900,
  height = 650
)

hist(
  weather_data$Temperature_C,
  breaks = 8,
  col = "orange",
  border = "white",
  main = "Temperature Distribution",
  xlab = "Temperature (C)",
  ylab = "Number of Days"
)

dev.off()

# 10. CREATE HTML CHART CARDS

chart_files <- c(
  "01_Temperature_Trend.png",
  "02_Humidity_Trend.png",
  "03_Rainfall_Trend.png",
  "04_Temperature_Distribution.png"
)

chart_titles <- c(
  "Temperature Trend",
  "Humidity Trend",
  "Daily Rainfall",
  "Temperature Distribution"
)

chart_descriptions <- c(
  "Daily temperatures and the monthly average.",
  "Daily changes in relative humidity.",
  "Rainfall recorded in the sample dataset.",
  "Distribution of temperatures across the month."
)

chart_html <- paste(
  vapply(seq_along(chart_files), function(i) {
    paste0(
      '<article class="chart"><h3>',
      chart_titles[i],
      '</h3><p>',
      chart_descriptions[i],
      '</p><img src="Weather_Analysis_Results/',
      chart_files[i],
      '" alt="',
      chart_titles[i],
      '"></article>'
    )
  }, character(1)),
  collapse = "\n"
)

# 11. CREATE WEATHER DATA TABLE

table_rows <- paste(
  vapply(seq_len(nrow(weather_data)), function(i) {
    paste0(
      "<tr><td>", weather_data$Date[i],
      "</td><td>", weather_data$Temperature_C[i],
      "</td><td>", weather_data$Humidity_Percent[i],
      "%</td><td>", weather_data$Rainfall_mm[i],
      " mm</td><td>", weather_data$Temperature_Status[i],
      "</td></tr>"
    )
  }, character(1)),
  collapse = "\n"
)

# 12. BUILD THE WEBPAGE

dashboard_file <- file.path(
  project_folder,
  "index.html"
)

html <- paste0(
  '<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Weather Pattern Analyzer</title>
<style>
*{box-sizing:border-box}
body{margin:0;background:#eef5fb;color:#183047;font-family:Arial,sans-serif;line-height:1.6}
header{padding:45px 7%;color:white;background:linear-gradient(120deg,#12365a,#087e8b)}
header p{color:#d6f5fa}
h1{font-size:clamp(2rem,5vw,3.4rem);margin:8px 0}
main{max-width:1150px;margin:auto;padding:28px 20px}
.stats{display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:15px}
.card,.chart,.tablebox{background:white;padding:20px;border-radius:14px;box-shadow:0 4px 16px #18304712;margin-bottom:16px}
.card p,.chart p{color:#62778b}
.card strong{font-size:1.8rem;color:#087e8b}
.charts{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:16px}
.chart img{display:block;width:100%;height:auto}
.tablebox{overflow-x:auto}
table{width:100%;border-collapse:collapse;min-width:600px}
th,td{text-align:left;padding:12px;border-bottom:1px solid #e1eaf1}
th{background:#e5f5f7;color:#075e68}
.note{padding:15px;background:#dff4f5;border-radius:10px;color:#18525a}
footer{padding:24px 0;color:#62778b}
@media(max-width:700px){.stats,.charts{grid-template-columns:1fr}header{padding:32px 20px}}
</style>
</head>
<body>
<header>
<p>WEATHER DATA ANALYTICS</p>
<h1>Weather Pattern Analyzer</h1>
<p>Explore temperature, humidity and rainfall patterns through R data analysis.</p>
</header>
<main>
<p class="note">Demonstration only: all weather readings are simulated. They are not real observations or a weather forecast.</p>
<h2>Weather Overview</h2>
<section class="stats">
<article class="card"><p>Average Temperature</p><strong>',
  average_temp,
  ' &deg;C</strong></article>
<article class="card"><p>Highest Temperature</p><strong>',
  maximum_temp,
  ' &deg;C</strong></article>
<article class="card"><p>Lowest Temperature</p><strong>',
  minimum_temp,
  ' &deg;C</strong></article>
<article class="card"><p>Average Humidity</p><strong>',
  average_humidity,
  '%</strong></article>
<article class="card"><p>Total Rainfall</p><strong>',
  total_rainfall,
  ' mm</strong></article>
<article class="card"><p>Rainy Days</p><strong>',
  rainy_days,
  ' / 30</strong></article>
</section>
<h2>Weather Charts</h2>
<section class="charts">',
  chart_html,
  '</section>
<h2>Key Findings</h2>
<div class="card">
<p><strong>Hottest day:</strong> ',
  hottest_day$Date,
  ' (', hottest_day$Temperature_C, ' &deg;C)</p>
<p><strong>Rainiest day:</strong> ',
  rainiest_day$Date,
  ' (', rainiest_day$Rainfall_mm, ' mm)</p>
<p><strong>Average humidity:</strong> ',
  average_humidity,
  '%</p>
</div>
<h2>Daily Weather Records</h2>
<div class="tablebox">
<table>
<thead><tr><th>Date</th><th>Temperature</th><th>Humidity</th><th>Rainfall</th><th>Temperature Status</th></tr></thead>
<tbody>',
  table_rows,
  '</tbody></table>
</div>
<footer>Weather Pattern Analyzer | Built using R, HTML and CSS</footer>
</main>
</body>
</html>'
)

writeLines(html, dashboard_file)

# 13. SAVE A TEXT REPORT

report <- c(
  "WEATHER PATTERN ANALYZER",
  "------------------------",
  paste("Average temperature:", average_temp, "C"),
  paste("Maximum temperature:", maximum_temp, "C"),
  paste("Minimum temperature:", minimum_temp, "C"),
  paste("Average humidity:", average_humidity, "%"),
  paste("Total rainfall:", total_rainfall, "mm"),
  paste("Rainy days:", rainy_days),
  paste("Hottest day:", hottest_day$Date),
  paste("Rainiest day:", rainiest_day$Date),
  "",
  "Note: All weather readings are simulated."
)

writeLines(
  report,
  file.path(output_folder, "Weather_Analysis_Report.txt")
)

# 14. OPEN WEBPAGE AUTOMATICALLY

cat("\nPROJECT COMPLETED SUCCESSFULLY!\n")
cat("Dashboard:", dashboard_file, "\n")
cat("Results folder:", output_folder, "\n")

browseURL(normalizePath(dashboard_file))
