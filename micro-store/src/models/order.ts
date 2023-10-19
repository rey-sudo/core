import mongoose, { ClientSession } from "mongoose";
import { updateIfCurrentPlugin } from "mongoose-update-if-current";
import { generatePid } from "../utils/nano";

interface OrderAttrs {
  name: string;
  last_name: string;
  phone: string;
  address: string;
  department: string;
  city: string;
  product_pid: string;
}

interface OrderModel extends mongoose.Model<OrderDocument> {
  build(
    attrs: OrderAttrs,
    session: ClientSession
  ): Promise<OrderDocument | any>;
}

interface Timeline {
  purchase: {
    status: boolean;
    date: Date;
    text: string;
  };

  dispatch: {
    status: boolean;
    date: Date;
    text: string;
  };

  shipping: {
    status: boolean;
    date: Date;
    text: string;
  };

  delivery: {
    status: boolean;
    date: Date;
    text: string;
  };
}

interface OrderDocument extends mongoose.Document {
  pid: string;
  name: string;
  last_name: string;
  phone: string;
  address: string;
  department: string;
  city: string;
  product_pid: string;
  timeline: Timeline;
  createdAt: Date;
  updatedAt: Date;
}

const orderSchema = new mongoose.Schema(
  {
    pid: {
      type: String,
      unique: true,
      index: true,
      default: () => generatePid("UAN", 15),
    },

    name: {
      type: String,
      required: true,
    },

    last_name: {
      type: String,
      required: true,
    },

    phone: {
      type: String,
      required: true,
    },

    address: {
      type: String,
      required: true,
    },

    department: {
      type: String,
      required: true,
    },

    city: {
      type: String,
      required: true,
    },

    product_pid: {
      type: String,
      required: true,
    },

    timeline: {
      purchase: {
        status: { type: Boolean, default: true },
        date: { type: Date, default: new Date() },
        text: { type: String, default: "Confirmación" },
      },

      dispatch: {
        status: { type: Boolean, default: false },
        date: { type: Date, default: new Date() },
        text: { type: String, default: "..." },
      },

      shipping: {
        status: { type: Boolean, default: false },
        date: { type: Date, default: new Date() },
        text: { type: String, default: "..." },
      },

      delivery: {
        status: { type: Boolean, default: false },
        date: { type: Date, default: new Date() },
        text: { type: String, default: "..." },
      },
    },
  },
  {
    timestamps: true,
    toJSON: {
      virtuals: true,
      transform(doc, ret) {
        delete ret._id;
        delete ret.id;
        delete ret.__v;
      },
    },
    toObject: { virtuals: true },
  }
);

orderSchema.virtual('product', {
  ref: 'Product',
  localField: 'product_pid',
  foreignField: 'pid'
});

orderSchema.plugin(updateIfCurrentPlugin);

orderSchema.statics.build = (attrs: OrderAttrs, session: ClientSession) => {
  return Order.create([attrs], { session: session });
};

const Order = mongoose.model<OrderDocument, OrderModel>(
  "Order",
  orderSchema,
  "Order"
);

export { Order, OrderDocument, OrderAttrs };
