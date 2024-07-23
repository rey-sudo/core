const express = require("express");
const http = require("http");
const { Server } = require("socket.io");
const dotenv = require("dotenv");
dotenv.config();
const app = express();
const server = http.createServer(app);
const socketServer = new Server(server);

// Middleware
app.use(express.static("public"));

app.use(express.json());

//////////////////////////////////////////////////////////////

const sockets = {};

const roomMessages = {};

app.get("/", (req, res) => {
  res.sendFile(join(__dirname, "index.html"));
});

//EVENT
app.get("/create-session/:orderId", (req, res) => {
  const orderId = req.params["orderId"];

  socketServer.to(orderId).emit("message", "Hello, Room!");

  res.json({ success: true, message: "session created " + orderId });
});

socketServer.on("connection", (socket) => {
  const userId = generateRandomString(4);

  console.log(userId + " USER CONNECTED");

  sockets[userId] = socket;

  sockets[userId].on("join", (orderId) => {
    sockets[userId].join(orderId);
    console.log("USER JOINED TO ROOM " + orderId);
  });

  sockets[userId].on("message", (payload) => {
    const data = JSON.parse(payload);
    socketServer.to(payload.room).emit("message", data.content);
  });

  sockets[userId].on("disconnect", () => {
    console.log("User disconnected");
  });
});

const PORT = process.env.PORT || 3000;

server.listen(PORT, () => {
  console.log(`Server is running on port ${PORT}`);
});

function generateRandomString(length) {
  const characters =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
  let result = "";
  const charactersLength = characters.length;
  for (let i = 0; i < length; i++) {
    result += characters.charAt(Math.floor(Math.random() * charactersLength));
  }
  return result;
}

// Example usage:
console.log(generateRandomString(10)); // e.g., 'A1b2C3
