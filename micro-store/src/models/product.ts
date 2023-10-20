import mongoose, { ClientSession } from "mongoose";
import { updateIfCurrentPlugin } from "mongoose-update-if-current";
import { generatePid } from "../utils/nano";

interface Image {
  small: string;
  medium: string;
  large: string;
}

interface Theme {
  title: string;
  subtitle: string;
  price: string;
  background: {
    pageOne: {
      color: string;
      mask: string;
    };
    pageTwo: {
      color: string;
      mask: string;
    };
    pageThree: {
      color: string;
      mask: string;
    };
  };
  slider_images: [{ url: string }];
}

interface ProductAttrs {
  name: string;
  space_url: string;
  image: Image;
  payment_type: string;
  shipping_tax: boolean;
  shipping_label: string;
  shipping_icon: string;
  stock_supply: number;
  price: number;
  price_diff: number;
  discount: number;
  discount_label: string;
  discount_color: string;
  seller_pid: string;
  theme: Theme;
}

interface ProductModel extends mongoose.Model<ProductDocument> {
  build(
    attrs: ProductAttrs,
    session: ClientSession
  ): Promise<ProductDocument | any>;
}

interface ProductDocument extends mongoose.Document {
  pid: string;
  name: string;
  space_url: string;
  image: Image;
  payment_type: string;
  shipping_tax: boolean;
  shipping_label: string;
  shipping_icon: string;
  stock_supply: number;
  price: number;
  price_diff: number;
  discount: number;
  discount_label: string;
  discount_color: string;
  seller_pid: string;
  theme: Theme;
  createdAt: Date;
  updatedAt: Date;
}

const productSchema = new mongoose.Schema(
  {
    pid: {
      type: String,
      unique: true,
      index: true,
      default: () => generatePid('N', 15),
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

    shipping_icon: {
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

    discount: {
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

    seller_pid: {
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

productSchema.plugin(updateIfCurrentPlugin);

productSchema.statics.build = (attrs: ProductAttrs, session: ClientSession) => {
  return Product.create([attrs], { session: session });
};

const Product = mongoose.model<ProductDocument, ProductModel>(
  "Product",
  productSchema,
  "Product"
);

export { Product, ProductDocument, ProductAttrs };
