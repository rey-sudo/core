//AUDIT WRITE MODEL  V1.0.0
import mongoose, { ClientSession } from "mongoose";
import { nanoid } from "nanoid";
import { formatDate } from "../utils/format-date";

interface RoundAttrs {
  round: string;
  budget: number;
  reward: number;
}

interface RoundModel extends mongoose.Model<RoundDocument> {
  build(
    attrs: RoundAttrs,
    session: ClientSession
  ): Promise<RoundDocument | any>;
}

interface RoundDocument extends mongoose.Document {
  pid: string;
  round: string;
  epoch: string;
  budget: number;
  reward: number;
  type: string;
  status: string;
  active_auditors: any;
  groups: any;
  selection_deadline: Date;
  governance_deadline: Date;
  auditing_deadline: Date;
  draft_deadline: Date;
  revision_deadline: Date;
  createdAt: Date;
  updatedAt: Date;
}

const roundSchema = new mongoose.Schema(
  {
    pid: {
      type: String,
      unique: true,
      index: true,
      default: () => nanoid(12),
    },

    round: {
      type: String,
      required: true,
      unique: true,
      index: true,
    },

    epoch: {
      type: String,
      default: () => new Date().getFullYear(),
    },

    budget: {
      type: Number,
      default: 0,
    },

    reward: {
      type: Number,
      default: 0,
    },

    type: {
      type: String,
      enum: ["public", "private"],
      default: "public",
    },

    status: {
      type: String,
      enum: [
        "pending",
        "selection",
        "selection-ended",
        "governance",
        "governance-ended",
        "auditing",
        "auditing-ended",
      ],
      default: "pending",
    },

    selection_deadline: {
      type: Date,
      default: null,
    },

    governance_deadline: {
      type: Date,
      default: null,
    },

    auditing_deadline: {
      type: Date,
      default: null,
    },

    draft_deadline: {
      type: Date,
      default: null,
    },

    revision_deadline: {
      type: Date,
      default: null,
    },
  },
  {
    timestamps: true,
    toJSON: {
      virtuals: true,
      transform(doc, ret: any) {
        ret.id = ret._id;
        ret.createdAt = formatDate(ret.createdAt);
        ret.selection_deadline = formatDate(ret.selection_deadline);
        ret.governance_deadline = formatDate(ret.governance_deadline);
        ret.auditing_deadline = formatDate(ret.auditing_deadline);
        ret.draft_deadline = formatDate(ret.draft_deadline);
        ret.revision_deadline = formatDate(ret.revision_deadline);
        delete ret._id;
        delete ret.__v;
        delete ret.updatedAt;
      },
    },
    toObject: {
      virtuals: true,
    },
  }
);

roundSchema.statics.build = (attrs: RoundAttrs, session: ClientSession) => {
  return Round.create([attrs], { session: session });
};

roundSchema.virtual("prevotes", {
  ref: "Prevote",
  localField: "pid",
  foreignField: "round_pid",
  count: true,
});

roundSchema.virtual("daovotes", {
  ref: "Daovote",
  localField: "pid",
  foreignField: "round_pid",
  count: true,
});

roundSchema.virtual("subscriptions", {
  ref: "Subscription",
  localField: "pid",
  foreignField: "round_pid",
  count: true,
});

roundSchema.virtual("active_auditors", {
  ref: "Subscription",
  localField: "pid",
  foreignField: "round_pid",
});

roundSchema.virtual("reports", {
  ref: "Report",
  localField: "pid",
  foreignField: "round_pid",
  match: { finished: true },
});

roundSchema.virtual("groups", {
  ref: "Group",
  localField: "pid",
  foreignField: "round_pid",
});

const Round = mongoose.model<RoundDocument, RoundModel>(
  "Round",
  roundSchema,
  "Round"
);

export { Round, RoundAttrs, RoundDocument };
