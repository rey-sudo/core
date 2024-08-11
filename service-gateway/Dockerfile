# Use an official Node.js runtime as a parent image
FROM node:18-alpine

# Set the working directory in the container
WORKDIR /usr/src/app

# Copy package.json and package-lock.json to the working directory
COPY package*.json ./

# Install dependencies
RUN npm install

# Copy the rest of the application code, including the src directory
COPY . .

# Compile TypeScript to JavaScript
RUN npx tsc

# Command to run the app
CMD ["node", "dist/index.js"]
