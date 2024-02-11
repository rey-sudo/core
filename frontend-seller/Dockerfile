FROM node:lts as build-stage

WORKDIR /app

COPY /dist /app/dist

FROM nginx:alpine  as production-stage

COPY --from=build-stage /app/dist /usr/share/nginx/html

COPY nginx.conf /etc/nginx/nginx.conf 

EXPOSE 80

CMD ["nginx", "-g", "daemon off;"]