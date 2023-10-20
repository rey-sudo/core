
import mongoose, { ClientSession, Schema } from "mongoose";
import { EVENT } from "../stream/types/types";

//

type EventBaseType = [EVENT, string, object];

interface PubAttrs {
  event: EventBaseType;
}

interface PubModel extends mongoose.Model<PubDocument> {
  build(attrs: PubAttrs, session: ClientSession): any;
}

interface PubDocument extends mongoose.Document {
  event: EventBaseType;
}

const pubSchema = new mongoose.Schema({
  event: {
    type: Schema.Types.Mixed,
    required: true,
  },
});

pubSchema.statics.build = (attrs: PubAttrs, session: ClientSession) => {
  return Pub.create([attrs], { session: session });
};

const Pub = mongoose.model<PubDocument, PubModel>("Pub", pubSchema);

export { Pub, PubDocument };