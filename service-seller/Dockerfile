FROM node:alpine

WORKDIR /app

COPY package.json .

RUN npm config set fetch-timeout 3600000

RUN npm config set fetch-retry-maxtimeout 3600000

RUN npm config set fetch-retry-mintimeout 3600000

RUN npm install --verbose

COPY . .

RUN npm run build 

RUN rm -rf node_modules

RUN npm install --omit=dev

CMD ["npm", "start"]