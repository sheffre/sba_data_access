# Используем базовый образ с установленной версией R и Shiny Server
FROM rocker/shiny:latest

# Устанавливаем PostgreSQL и необходимые зависимости
RUN apt-get update && \
    apt-get install -y postgresql postgresql-contrib libpq-dev
	
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

# Устанавливаем необходимые дополнительные пакеты R, если это необходимо
RUN R -e "install.packages('shiny')"
RUN R -e "install.packages('bslib')"
RUN R -e "install.packages('DBI')"
RUN R -e "install.packages('RPostgres')"
RUN R -e "install.packages('lubridate')"
RUN R -e "install.packages('shinyWidgets')"
RUN R -e "install.packages('daterangepicker')"
RUN R -e "install.packages('plotly')"
RUN R -e "install.packages('utils')"

# Копируем содержимое вашего приложения внутрь контейнера
COPY . /srv/shiny-server/dasba

# Указываем рабочую директорию
WORKDIR /srv/shiny-server/dasba

# Определяем порт, на котором будет работать приложение Shiny
EXPOSE 3838


