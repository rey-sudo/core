FROM node:alpine

WORKDIR /app

COPY package.json .

RUN npm config set fetch-timeout 3600000

RUN npm config set fetch-retry-maxtimeout 3600000

RUN npm config set fetch-retry-mintimeout 3600000

RUN npm install --verbose --omit=dev

COPY . .

RUN npm run build 

COPY cert.pem /app/dist/cert.pem

CMD ["npm", "start"]