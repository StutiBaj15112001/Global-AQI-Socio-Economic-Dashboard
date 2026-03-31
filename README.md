Pollution vs Progress: Air Quality & Emissions Dashboard

This interactive R Shiny application explores the relationship between global industrial activity and air quality. It provides a unified platform to analyze how economic progress intersects with environmental health through three integrated datasets: city-level AQI readings, global socio-economic indicators, and historical UK industrial emissions.

Work Performed

Integrated Multi-Source Data: Cleaned and joined city-level air quality data with country-level socio-economic metrics (e.g., GDP, population density, electricity access) to identify regional pollution drivers.

Interactive Dashboard Architecture: Developed a dual-track narrative interface in R, enabling users to switch between global geographic analysis and UK-specific industrial trends.

Custom UI/UX Design: Built a responsive interface featuring a persistent dark-mode toggle and interactive sidebar controls to manage complex data views without cognitive overload.

Key Features

Global Air Quality Analysis

Interactive World Map: A Leaflet-based map visualizing city-level AQI categories with real-time filtering by country, gas type (PM2.5, NO2, CO, Ozone), and air quality band.

Socio-Economic Correlations: Dynamic Plotly scatterplots and histograms that illustrate the relationship between AQI values and selected development indicators.

UK Industrial Emissions Analysis

Packed Bubble Charts: Visualizes emission volumes by industry, where bubble area encodes the magnitude of pollutants.

Temporal Trend Lines: Tracks total emissions over several decades to highlight the long-term impact of policy and industrial changes.

User Experience & Tools

Dark Mode Toggle: A built-in switch to toggle between light and dark themes for better accessibility.

Details-on-Demand: Comprehensive hover tooltips across all maps and charts providing precise numerical values.

Smart Filtering: Multi-select options for countries and industries with built-in limiters to maintain visual clarity.

Author: Stuti Baj

Project: Data Exploration and Visualization
