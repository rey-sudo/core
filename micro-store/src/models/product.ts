import mongoose, { ClientSession } from "mongoose";
import { updateIfCurrentPlugin } from "mongoose-update-if-current";
import { numberPid } from "../utils/nano";

interface Image {
  small: string;
  medium: string;
  large: string;
}

interface ProductAttrs {
  name: string;
  space_url: string;
  image: Image;
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
  createdAt: Date;
  updatedAt: Date;
}

const productSchema = new mongoose.Schema(
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
      medium: { type: String, required: false },
      large: { type: String, required: false },
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
