import mongoose, { ClientSession, Schema } from "mongoose";
import { largePid } from "../utils/nano";
import { ProjectDocument } from "./project";

interface QuestionScheme {
  id: number;

  answer: number;

  textarea: string;

  input: any;
}

interface ReportAttrs {

  round: string;

  round_pid: string;

  group_pid: string;

  group_name: string;

  auditor_pid: string;

  project: ProjectDocument;

  category_score: object;

  category_max_score: object;

  total_score: number;

  total_max_score: number;

  total_percentage: string;

  category_scheme: object;

  notes: {
    answer: string;
    textarea: string;
  };

  data: [QuestionScheme];

  hash: string;
}

interface ReportModel extends mongoose.Model<ReportDocument> {
  build(attrs: ReportAttrs, session: ClientSession): Promise<ReportDocument | any>;
}

interface ReportDocument extends mongoose.Document {
  pid: string;

  version: string;

  finished: boolean;

  round: string;

  round_pid: string;

  group_pid: string;

  group_name: string;

  auditor_pid: string;

  project: ProjectDocument;

  category_score: object;

  category_max_score: object;

  total_score: number;

  total_max_score: number;

  total_percentage: string;

  category_scheme: object;

  notes: {
    answer: string;
    textarea: string;
  };

  data: [QuestionScheme];

  hash: string;
}

const questionSchema = new mongoose.Schema({
  id: {
    type: Number,
    required: true,
  },

  answer: {
    type: Number,
    required: true,
  },

  textarea: {
    type: String,
  },

  input: {
    type: [Schema.Types.Mixed],
  },
});

const reportSchema = new mongoose.Schema(
  {
    pid: {
      type: String,
      required: true,
      default: () => largePid(),
    },

    version: {
      type: String,
      default: "1.0",
    },

    finished: {
      type: Boolean,
      required: true,
      default: false,
    },

    round: {
      type: String,
      required: true,
    },

    round_pid: {
      type: String,
      required: true,
      ref: 'Round'
    },

    group_pid: {
      type: String,
      required: true,
      ref: "Group",
      index: true,
    },

    group_name: {
      type: String,
      required: true,
    },

    auditor_pid: {
      type: String,
      required: true,
    },

    project: {
      type: Object,
      required: true,
    },

    category_score: {
      type: Object,
      required: true,
    },

    category_max_score: {
      type: Object,
      required: true,
    },

    total_score: {
      type: Number,
      required: true,
    },

    total_max_score: {
      type: Number,
      required: true,
    },

    total_percentage: {
      type: String,
      required: true,
    },

    category_scheme: {
      type: Object,
      required: true,
    },

    notes: {
      answer: {
        type: String,
        required: true,
        enum: ["Positive", "Negative", "Neutral"],
      },

      textarea: { type: String, required: true, default: "" },
    },

    data: [questionSchema],

    hash: {
      type: String,
      required: true,
    }
    
  },
  {
    timestamps: true,
    toJSON: {
      virtuals: true,
      transform(doc, ret) {
        delete ret._id;
        delete ret.__v;
      },
    },
    toObject: { virtuals: true },
  }
);

reportSchema.statics.build = (attrs: ReportAttrs, session: ClientSession) => {
  return Report.create([attrs], { session: session } );
};

reportSchema.virtual("review", {
  ref: "Review",
  localField: "pid",
  foreignField: "report_pid",
});

const Report = mongoose.model<ReportDocument, ReportModel>(
  "Report",
  reportSchema,
  "Report"
);

export { Report, ReportDocument, ReportAttrs };
