import mongoose, { ClientSession } from "mongoose";
import { updateIfCurrentPlugin } from "mongoose-update-if-current";
import { numberPid } from "../utils/nano";

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

interface OrderDocument extends mongoose.Document {
  pid: string;
  name: string;
  last_name: string;
  phone: string;
  address: string;
  department: string;
  city: string;
  product_pid: string;
  createdAt: Date;
  updatedAt: Date;
}

const orderSchema = new mongoose.Schema(
  {
    pid: {
      type: String,
      unique: true,
      index: true,
      default: () => numberPid(),
    },

    name: {
      type: String,
      required: true,
    },

    space_url: {
      type: String,
      required: true,
    },

    image: {
      small: { type: String, required: true },
      medium: { type: String, required: true },
      large: { type: String, required: true },
    },

    payment_type: {
      type: String,
      required: true,
    },

    shipping_tax: {
      type: Boolean,
      required: true,
    },

    shipping_label: {
      type: String,
      required: true,
    },

    stock_supply: {
      type: Number,
      required: true,
    },

    price: {
      type: Number,
      required: true,
    },

    price_diff: {
      type: Number,
      required: true,
    },

    discount_label: {
      type: String,
      required: true,
    },

    discount_color: {
      type: String,
      required: true,
    },
    
    theme: {
      title: { type: String, required: true },
      subtitle: { type: String, required: true },
      price: { type: String, required: true },
      background: {
        pageOne: {
          color: { type: String, required: true },
          mask: { type: String, required: true },
        },
        pageTwo: {
          color: { type: String, required: true },
          mask: { type: String, required: true },
        },
        pageThree: {
          color: { type: String, required: true },
          mask: { type: String, required: true },
        },
      },
      slider_images: [{ url: { type: String, required: true } }],
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
