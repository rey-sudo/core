FROM node:19 as build-stage

WORKDIR /interface-main

COPY package.json ./

RUN npm config set fetch-timeout 2900000

RUN npm config set fetch-retry-maxtimeout 2900000

RUN npm config set fetch-retry-mintimeout 2900000

RUN npm install --verbose

COPY . .

RUN npm run build

FROM nginx:alpine  as production-stage

COPY --from=build-stage /interface-main/dist /usr/share/nginx/html

COPY nginx.conf /etc/nginx/nginx.conf

EXPOSE 80

CMD ["nginx", "-g", "daemon off;"]