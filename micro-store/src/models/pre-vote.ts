//WRITE MODEL V1.0.2
import mongoose, { ClientSession } from "mongoose";
import { updateIfCurrentPlugin } from "mongoose-update-if-current";
import { nanoid } from "nanoid";

interface PrevoteAttrs {
  user_pid: string;
  round_pid: string;
  project_pid: string;
}

interface PrevoteModel extends mongoose.Model<PrevoteDocument> {
  build(
    attrs: PrevoteAttrs,
    session: ClientSession
  ): Promise<PrevoteDocument | any>;
}

interface PrevoteDocument extends mongoose.Document {
  pid: string;
  user_pid: string;
  round_pid: string;
  project_pid: string;
  createdAt: Date;
  updatedAt: Date;
}

const prevoteSchema = new mongoose.Schema(
  {
    pid: {
      type: String,
      unique: true,
      index: true,
      default: () => nanoid(20),
    },

    user_pid: {
      type: String,
      required: true
    },

    round_pid: {
      type: String,
      ref: "Round",
      required: true,
    },

    project_pid: {
      ref: "Project",
      type: String,
      required: true,
    },
  },
  {
    timestamps: true,
    toJSON: {
      virtuals: true,
      transform(doc, ret) {
        delete ret.updatedAt;
        delete ret._id;
        delete ret.id;
        delete ret.user_pid;
        delete ret.__v;
      },
    },
    toObject: { virtuals: true },
  }
);

prevoteSchema.index({ round_pid: 1, user_pid: 1 }, { unique: true });

prevoteSchema.plugin(updateIfCurrentPlugin);

prevoteSchema.statics.build = (attrs: PrevoteAttrs, session: ClientSession) => {
  return Prevote.create([attrs], { session: session });
};

const Prevote = mongoose.model<PrevoteDocument, PrevoteModel>(
  "Prevote",
  prevoteSchema,
  "Prevote"
);

export { Prevote, PrevoteDocument, PrevoteAttrs };
