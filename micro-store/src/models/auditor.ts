//USER WRITE MODEL v1.0.1

import { Password } from "@alphaicterus/global";
import mongoose, { ClientSession } from "mongoose";
import { updateIfCurrentPlugin } from "mongoose-update-if-current";
import { customPid } from "../utils/nano";
import { randAvatar } from "../utils/randAvatar";

interface AuditorAttrs {
  email: string;
  password: string;
  username: string;
  region: string;
  wallet: string;
  contact: string;
}

interface AuditorModel extends mongoose.Model<AuditorDocument> {
  build(
    attrs: AuditorAttrs,
    session: ClientSession
  ): Promise<AuditorDocument | any>;
  findByEvent(event: {
    email: string;
    __v: number;
  }): Promise<AuditorDocument | null>;
}

interface AuditorDocument extends mongoose.Document {
  pid: string;
  type: string;
  avatar: string;
  email: string;
  password: string;
  username: string;
  status: string;
  region: string;
  wallet: string;
  contact: string;
}

const auditorSchema = new mongoose.Schema(
  {
    pid: {
      type: String,
      unique: true,
      index: true,
      default: () => customPid(),
    },

    avatar: {
      type: String,
      default: () => randAvatar(),
    },

    email: {
      type: String,
      required: true,
      lowercase: true,
      trim: true
    },

    password: {
      type: String,
      required: true,
    },

    username: {
      type: String,
      required: true,
    },

    type: {
      type: String,
      ref: "Round",
      enum: ["public", "private"],
      default: "public",
    },

    status: {
      type: String,
      required: true,
      default: "VERIFIED",
    },

    wallet: {
      type: String,
      required: true,
    },

    contact: {
      type: String,
      required: true,
    },

    region: {
      type: String,
      required: true,
      default: "",
    },
  },
  {
    timestamps: true,
    toJSON: {
      virtuals: true,
      transform(doc, ret) {
        delete ret.id;
        delete ret._id;
        delete ret.region;
        delete ret.status;
        delete ret.password;
        delete ret.email;
        delete ret.wallet;
        delete ret.contact;
        delete ret.createdAt;
        delete ret.updatedAt;
        delete ret.__v;
      },
    },
    toObject: { virtuals: true },
  }
);

auditorSchema.plugin(updateIfCurrentPlugin);

auditorSchema.pre("save", async function (done) {
  if (this.isModified("password")) {
    const hashed = await Password.toHash(this.get("password"));
    this.set("password", hashed);
  }
  done();
});

auditorSchema.statics.findByEvent = (event: { email: string; __v: number }) => {
  return Auditor.findOne({
    email: event.email,
    __v: event.__v - 1,
  });
};

auditorSchema.statics.build = (attrs: AuditorAttrs, session: ClientSession) => {
  return Auditor.create([attrs], { session: session });
};

const Auditor = mongoose.model<AuditorDocument, AuditorModel>(
  "Auditor",
  auditorSchema,
  "Auditor"
);

export { Auditor, AuditorAttrs, AuditorDocument };
