FROM oydeu/oyd-shiny_armhf:latest
MAINTAINER "Christoph Fabianek" christoph@ownyourdata.eu

RUN cd /srv/shiny-server \
        && rm -rf * \
        && git clone https://github.com/OwnYourData/app-scheduler.git \
        && mv app-scheduler/* . \
        && rm -r app-scheduler \
        && R -e "source('init.r')"

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]