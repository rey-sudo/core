//WRITE MODEL V1.0.0
import mongoose, { ClientSession } from "mongoose";
import { updateIfCurrentPlugin } from "mongoose-update-if-current";
import { nanoid } from "nanoid";

interface GroupAttrs {
  group_name: string;
  round_pid: string;
  project_pid: string;
  reviewer_pid: string;
  auditor_pid: string;
}

interface GroupModel extends mongoose.Model<GroupDocument> {
  build(
    attrs: GroupAttrs,
    session: ClientSession
  ): Promise<GroupDocument | any>;
}

interface GroupDocument extends mongoose.Document {
  pid: string;
  group_name: string;
  round_pid: string;
  project_pid: string;
  reviewer_pid: string;
  auditor_pid: string;
  report: any[];
  review: any[];
  createdAt: Date;
  updatedAt: Date;
}

const groupSchema = new mongoose.Schema(
  {
    pid: {
      type: String,
      unique: true,
      index: true,
      default: () => nanoid(20),
    },

    group_name: {
      type: String,
      required: true,
    },

    round_pid: {
      type: String,
      ref: "Round",
      required: true,
    },

    project_pid: {
      ref: 'Project',
      type: String,
      required: true,
    },

    reviewer_pid: {
      type: String,
      required: true,
    },

    auditor_pid: {
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
        delete ret.__v;
      },
    },
    toObject: { virtuals: true },
  }
);

groupSchema.index({ round_pid: 1, project_pid: 1 }, { unique: true });

groupSchema.plugin(updateIfCurrentPlugin);

groupSchema.statics.build = (attrs: GroupAttrs, session: ClientSession) => {
  return Group.create([attrs], { session });
};

groupSchema.virtual("report", {
  ref: "Report",
  localField: "pid",
  foreignField: "group_pid",
});

groupSchema.virtual("review", {
  ref: "Review",
  localField: "pid",
  foreignField: "group_pid",
});

groupSchema.virtual("project", {
  ref: "Project",
  localField: "project_pid",
  foreignField: "pid",
});

const Group = mongoose.model<GroupDocument, GroupModel>(
  "Group",
  groupSchema,
  "Group"
);

export { Group, GroupDocument, GroupAttrs };
