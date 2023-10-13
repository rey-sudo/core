//WRITE MODEL V1.0.2
import mongoose, { ClientSession } from "mongoose";
import { updateIfCurrentPlugin } from "mongoose-update-if-current";
import { nanoid } from "nanoid";

interface DaovoteAttrs {
  round_pid: string;
  project_pid: string;
}

interface DaovoteModel extends mongoose.Model<DaovoteDocument> {
  build(
    attrs: DaovoteAttrs,
    session: ClientSession
  ): Promise<DaovoteDocument | any>;
}

interface DaovoteDocument extends mongoose.Document {
  pid: string;
  round_pid: string;
  project_pid: string;
  createdAt: Date;
  updatedAt: Date;
}

const daovoteSchema = new mongoose.Schema(
  {
    pid: {
      type: String,
      unique: true,
      index: true,
      default: () => nanoid(15),
    },

    round_pid: {
      type: String,
      ref: "Round",
      required: true,
    },

    project_pid: {
      type: String,
      ref: "Project",
      required: true,
    },
  },
  {
    timestamps: true,
    toJSON: {
      virtuals: true,
      transform(doc, ret) {
        delete ret.updatedAt;
        delete ret.id;
        delete ret._id;
        delete ret.__v;
      },
    },
    toObject: { virtuals: true },
  }
);

daovoteSchema.plugin(updateIfCurrentPlugin);

daovoteSchema.statics.build = (attrs: DaovoteAttrs, session: ClientSession) => {
  return Daovote.create([attrs], { session });
};

const Daovote = mongoose.model<DaovoteDocument, DaovoteModel>(
  "Daovote",
  daovoteSchema,
  "Daovote"
);

export { Daovote, DaovoteDocument, DaovoteAttrs };
