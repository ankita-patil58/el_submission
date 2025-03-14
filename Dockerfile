# Use the official R image with Shiny Server pre-installed
FROM rocker/shiny:4.0.5

# Install system dependencies required for the packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install the required R packages
RUN R -e "install.packages(c('shinydashboard', 'mapGCT', 'DT', 'ggplot2', 'shinyjs', 'shinyhelper', 'edgeR', 'reactable', 'plotly', 'shinyalert'))"

# Create a directory for the Shiny app (Shiny Server will look for apps here)
RUN mkdir -p /srv/shiny-server/myapp

# Copy your ui.R and server.R into the Shiny app directory
COPY ui.R /srv/shiny-server/myapp/ui.R
COPY server.R /srv/shiny-server/myapp/server.R

# Expose port 3838 for Shiny Server
EXPOSE 3838

# Command to run Shiny Server and serve the app
CMD ["/usr/bin/shiny-server"]
