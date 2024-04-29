FROM node:lts as build-stage

WORKDIR /app

COPY package.json ./

RUN npm config set fetch-timeout 2900000

RUN npm config set fetch-retry-maxtimeout 2900000

RUN npm config set fetch-retry-mintimeout 2900000

RUN npm install --verbose

COPY . .

RUN npm run build

FROM nginx:alpine  as production-stage

COPY --from=build-stage /app/dist /usr/share/nginx/html

COPY nginx.conf /etc/nginx/nginx.conf

EXPOSE 80

CMD ["nginx", "-g", "daemon off;"]