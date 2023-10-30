import mongoose, { ClientSession } from "mongoose";
import { updateIfCurrentPlugin } from "mongoose-update-if-current";
import { generatePid } from "../utils/nano";

interface FooterComponent {
  emoji: string;
  title: string;
  subtitle: string;
}

interface SliderComponent {
  background_color: string;
  mask_url: string;
  images: [{ url: string }];
}

interface TwoBoxComponent {
  background_color: string;
  mask_url: string;
  section: {
    image_url: string;
    content: {
      title: string;
      subtitle: string;
    };
  };
}

interface ThreeBoxComponent {
  emoji: string;
  title: string;
  subtitle: string;
  section: {
    left: {
      image_url: string;
      title: string;
      subtitle: string;
    };
    center: {
      image_url: string;
      title: string;
      subtitle: string;
    };
    right: {
      image_url: string;
      title: string;
      subtitle: string;
    };
  };
}

interface Image {
  small: string;
  medium: string;
  large: string;
}

interface Theme {
  title: string;
  subtitle: string;
  config: {
    page_1: SliderComponent;
    page_2: TwoBoxComponent;
    page_3: TwoBoxComponent;
    page_4: ThreeBoxComponent;
    page_5: FooterComponent;
  };
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
  seller_whatsapp: string;
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
  seller_whatsapp: string;
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
      default: () => generatePid("N", 15),
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

    seller_whatsapp: {
      type: String,
      required: true,
    },

    theme: {
      title: { type: String, required: true },
      subtitle: { type: String, required: true },
      config: {
        page_1: {
          background_color: { type: String, required: true },
          mask_url: { type: String, required: true },
          images: [{ url: { type: String, required: true } }],
        },

        page_2: {
          background_color: { type: String, required: true },
          mask_url: { type: String, required: true },
          section: {
            image_url: { type: String, required: true },
            content: {
              title: { type: String, required: true },
              subtitle: { type: String, required: true },
            },
          },
        },

        page_3: {
          background_color: { type: String, required: true },
          mask_url: { type: String, required: true },
          section: {
            image_url: { type: String, required: true },
            content: {
              title: { type: String, required: true },
              subtitle: { type: String, required: true },
            },
          },
        },

        page_4: {
          emoji: { type: String, required: true },
          title: { type: String, required: true },
          subtitle: { type: String, required: true },
          section: {
            left: {
              image_url: { type: String, required: true },
              title: { type: String, required: true },
              subtitle: { type: String, required: true },
            },
            center: {
              image_url: { type: String, required: true },
              title: { type: String, required: true },
              subtitle: { type: String, required: true },
            },
            right: {
              image_url: { type: String, required: true },
              title: { type: String, required: true },
              subtitle: { type: String, required: true },
            },
          },
        },

        page_5: {
          emoji: { type: String, required: true },
          title: { type: String, required: true },
          subtitle: { type: String, required: true },
        },
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
