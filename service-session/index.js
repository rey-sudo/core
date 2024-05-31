const express = require("express");
const http = require("http");
const { Server } = require("socket.io");
const dotenv = require("dotenv");
dotenv.config();
const app = express();
const server = http.createServer(app);
const io = new Server(server);

// Middleware
app.use(express.static("public"));

app.use(express.json());

//////////////////////////////////////////////////////////////
const socketList = {};

const roomMessages = {};

app.get("/", (req, res) => {
  res.sendFile(join(__dirname, "index.html"));
});

//EVENT
app.get("/create-session/:slotId", (req, res) => {
  const slotId = req.params["slotId"];

  io.to(slotId).emit("message", "Hello, Room!");

  res.json({ success: true, message: "session created " + slotId });
});

//////////////////////////////////////////////////////////////

io.on("connection", (socket) => {
  socketList["socket1"] = socket;
  socketList["socket1"].join("1");

  const socketId = generateRandomString(3);

  console.log(socketId + "A user joined");

  //////////////////////////////////////////////////////////////

  socketList["socket1"].on("clientMessage", (msg) => {
    console.log("message: " + msg);
    
    io.to("1").emit("clientMessage", msg);
  });

  //////////////////////////////////////////////////////////////
  socketList["socket1"].on("disconnect", () => {
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
