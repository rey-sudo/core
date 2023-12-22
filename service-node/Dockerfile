FROM ubuntu:22.04 as node-stage

WORKDIR /app

COPY . /app

EXPOSE 30

RUN ls .

RUN chmod +x /start.sh

CMD ["/start.sh"]