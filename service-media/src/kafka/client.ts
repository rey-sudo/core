const { Kafka } = require("kafkajs");

const kafka = new Kafka({
  clientId: "service-media",
  ssl: false,
  enforceRequestTimeout: false,
  brokers: [
    "streaming-kafka-bootstrap:9092",
    "streaming-kafka-bootstrap:9092",
    "streaming-kafka-bootstrap:9092",
  ],
});

export default kafka