//WRITE MODEL V1.0.0
import mongoose, { ClientSession } from "mongoose";
import { updateIfCurrentPlugin } from "mongoose-update-if-current";
import { nanoid } from "nanoid";

 interface SubscriptionAttrs {
  auditor_pid: string;
  round_pid: string;
}

interface SubscriptionModel extends mongoose.Model<SubscriptionDocument> {
  build(
    attrs: SubscriptionAttrs,
    session: ClientSession
  ): Promise<SubscriptionDocument | any>;
}

interface SubscriptionDocument extends mongoose.Document {
  pid: string;
  auditor_pid: string;
  round_pid: string;
  createdAt: Date;
  updatedAt: Date;
}

const subscriptionSchema = new mongoose.Schema(
  {
    pid: {
      type: String,
      unique: true,
      index: true,
      default: () => nanoid(16),
    },

    auditor_pid: {
      type: String,
      required: true
    },

    round_pid: {
      type: String,
      ref: "Round",
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
        delete ret.__v;
        delete ret.createdAt;
      },
    },
    toObject: { virtuals: true },
  }
);

subscriptionSchema.index({ round_pid: 1, auditor_pid: 1 }, { unique: true });

subscriptionSchema.plugin(updateIfCurrentPlugin);

subscriptionSchema.statics.build = (attrs: SubscriptionAttrs, session: ClientSession) => {
  return Subscription.create([attrs], { session: session });
};

const Subscription = mongoose.model<SubscriptionDocument, SubscriptionModel>(
  "Subscription",
  subscriptionSchema,
  "Subscription"
);

export { Subscription, SubscriptionDocument, SubscriptionAttrs };
