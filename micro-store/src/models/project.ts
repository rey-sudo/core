//PROJECT MODEL  V1.0.1
import mongoose, { ClientSession } from "mongoose";
import { nanoid } from "nanoid";

interface ProjectAttrs {
  name: string;
  category: string;
  symbol: string;
  description: string;
  funded: string;
  listed: string;
  audited: string;
  logo_url: {
    base: string;
    path: string;
    base64: string;
  };
  repository: {
    name: string;
    link: string;
  };
  whitepaper: string;
  website: string;
  community: {
    reddit: string;
    telegram: string;
    instagram: string;
    youtube: string;
    discord: string;
    medium: string;
    facebook: string;
    twitter: string;
  };
  tags: [string];
}

interface ProjectModel extends mongoose.Model<ProjectDocument> {
  build(
    attrs: ProjectAttrs,
    session: ClientSession
  ): Promise<ProjectDocument | any>;
}

interface ProjectDocument extends mongoose.Document {
  pid: string;
  name: string;
  category: string;
  symbol: string;
  description: string;
  funded: string;
  listed: string;
  audited: string;
  logo_url: {
    base: string;
    path: string;
    base64: string;
  };
  repository: {
    name: string;
    link: string;
  };
  whitepaper: string;
  website: string;
  community: {
    reddit: string;
    telegram: string;
    instagram: string;
    youtube: string;
    discord: string;
    medium: string;
    facebook: string;
    twitter: string;
  };
  tags: [string];
  createdAt: Date;
  updatedAt: Date;
}

const projectSchema = new mongoose.Schema(
  {
    pid: {
      type: String,
      unique: true,
      index: true,
      default: () => nanoid(12),
      ref: 'Group'
    },
    name: {
      type: String,
      required: true,
      unique: true,
      index: true,
    },
    category: {
      type: String,
      required: true,
      index: true,
    },
    funded: {
      type: String,
      required: true,
      enum: ["private", "catalyst"],
    },
    listed: {
      type: String,
      required: true,
      enum: ["community", "dao"],
    },
    audited: {
      type: String,
      required: true,
      enum: ["yes", "not"],
    },
    symbol: {
      type: String,
      index: true,
      default: "",
    },
    description: {
      type: String,
      required: true,
    },
    logo_url: {
      base: {
        type: String,
        default: "https://space.auditocean.com",
        required: true
      },
      path: {
        type: String,
        required: true,
      },

      base64: {
        type: String,
        required: true,
      },
    },
    repository: {
      name: {
        type: String,
        default: "",
      },
      link: {
        type: String,
        default: "",
      },
    },
    whitepaper: {
      type: String,
      default: "",
    },
    website: {
      type: String,
      required: true,
    },
    community: {
      reddit: { type: String, default: "" },
      telegram: { type: String, default: "" },
      instagram: { type: String, default: "" },
      youtube: { type: String, default: "" },
      discord: { type: String, default: "" },
      medium: { type: String, default: "" },
      facebook: { type: String, default: "" },
      twitter: { type: String, default: "" },
    },
    tags: {
      type: Array,
      required: true,
    },
  },
  {
    timestamps: true,
    toJSON: {
      virtuals: true,
      transform(doc, ret) {
        delete ret.id;
        delete ret._id;
        delete ret.__v;
      },
    },
    toObject: { virtuals: true }
  }
);

projectSchema.statics.build = (attrs: ProjectAttrs, session: ClientSession) => {
  return Project.create([attrs], { session: session });
};

projectSchema.virtual("prevotes", {
  ref: "Prevote",
  localField: "pid",
  foreignField: "project_pid",
  count: true
});

projectSchema.virtual("daovotes", {
  ref: "Daovote",
  localField: "pid",
  foreignField: "project_pid",
  count: true
});

projectSchema.virtual("groups", {
  ref: "Group",
  localField: "pid",
  foreignField: "project_pid"
});

const Project = mongoose.model<ProjectDocument, ProjectModel>(
  "Project",
  projectSchema,
  "Project"
);

export { Project, ProjectDocument, projectSchema };
