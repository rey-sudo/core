import mongoose from "mongoose";
import { EVENT, Listener } from "@alphaicterus/global";
import { Project } from "../../models/project";
import { _ } from "../../utils/logger";

export interface ProjectActionEvent {
  streamName: string;
  data: any;
}

export class ProjectActionListener extends Listener<ProjectActionEvent> {
  streamName = EVENT.service_projects;
  consumerName = "consumer-1";
  consumerGroup = "audit-group";

  async onMessage(data: ProjectActionEvent["data"]) {
    const {
      id,
      message: { event_action, event_payload },
    } = data;

    const payload = JSON.parse(event_payload);

    delete payload._id;

    const { pid, __v } = payload;

    const version = `${event_action}-${pid}-${__v}`;

    if (this.history.has(version)) return await this.ack(id);

    //////////////////////////////////////////////////////////////////
    if (event_action === "project-created") {
      let session: any = null;
      await mongoose
        .startSession()
        .then((_session) => {
          session = _session;
          return session.withTransaction(async () => {
            const response = await Project.findOneAndReplace(
              { pid: pid, __v: __v },
              payload,
              {
                session: session,
                upsert: true,
                new: true,
              }
            );

            if (!response) {
              throw new Error("project-created error");
            }

            return await this.ack(id, version);
          });
        })
        .catch((e) => _.error(e))
        .finally(() => session.endSession());
    }
    //////////////////////////////////////////////////////////////////
    if (event_action === "project-updated") {
      let session: any = null;
      await mongoose
        .startSession()
        .then((_session) => {
          session = _session;
          return session.withTransaction(async () => {
            const response = await Project.findOneAndReplace(
              { pid: pid, __v: __v - 1 },
              payload,
              {
                session: session,
                new: true,
              }
            );

            if (!response) {
              throw new Error("project-updated error");
            }

            return await this.ack(id, version);
          });
        })
        .catch((e) => _.error(e))
        .finally(() => session.endSession());
    }
    //////////////////////////////////////////////////////////////////
    if (event_action === "project-deleted") {
      let session: any = null;
      await mongoose
        .startSession()
        .then((_session) => {
          session = _session;
          return session.withTransaction(async () => {
            const response = await Project.findOneAndDelete(
              {
                pid: pid,
                __v: __v,
              },
              { session: session }
            );

            if (!response) {
              throw new Error("project-deleted error");
            }

            return await this.ack(id, version);
          });
        })
        .catch((e) => _.error(e))
        .finally(() => session.endSession());
    }
    //////////////////////////////////////////////////////////////////
  }
}
