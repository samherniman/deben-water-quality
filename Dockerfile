FROM rocker/r-ver:4.4.0

# Install system dependencies
RUN apt-get update && apt-get install -y \
    curl \
    cmake \
    build-essential \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libgdal-dev \
    libproj-dev \
    libgeos-dev \
    libudunits2-dev \
    libabsl-dev \
    pandoc \
    && rm -rf /var/lib/apt/lists/*

# Install Quarto
RUN curl -LO https://quarto.org/download/latest/quarto-linux-amd64.deb && \
    dpkg -i quarto-linux-amd64.deb && \
    rm quarto-linux-amd64.deb

# Set working directory
WORKDIR /app

# Copy renv files first for better caching
COPY renv.lock renv.lock
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json

# Install renv and restore packages
RUN R -e "install.packages('renv', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "renv::restore()"

# Copy application files
COPY . .

# Expose port 3000 (Quarto's default port)
EXPOSE 3000

# Run Quarto serve
CMD ["quarto", "serve", "app.qmd", "--host", "0.0.0.0", "--port", "3000"]
