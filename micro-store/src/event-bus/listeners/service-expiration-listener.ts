import assert from "node:assert";
import { EVENT, Listener } from "@alphaicterus/global";
import { Round, Report } from "../../models";
import { updateAudit } from "../../routes/transactions/transactions";
import { _ } from "../../utils/logger";

interface ExpirationActionEvent {
  streamName: string;
  data: any;
}

export class ExpirationActionListener extends Listener<ExpirationActionEvent> {
  streamName = EVENT.service_expiration;
  consumerName = "consumer-1";
  consumerGroup = "audit-group";

  async onMessage(data: ExpirationActionEvent["data"]) {
    const {
      id,
      message: { event_action, event_payload },
    } = data;

    _.info(data);

    const { pid, status } = JSON.parse(event_payload);

    const version = event_action + "-" + pid + "-" + id;

    if (this.history.has(version)) {
      return await this.ack(id);
    }

    if (event_action === "selection-ended") {
      return await Round.findOne({ pid: pid })
        .then((round) => assert.equal(round?.status, "selection"))
        .then(() => updateAudit(pid, { status: status }))
        .then((res) => assert.equal(res.success, true))
        .catch((error) => _.error(error))
        .finally(() => this.ack(id, version));
    }

    if (event_action === "governance-ended") {
      return await Round.findOne({ pid: pid })
        .then((round) => assert.equal(round?.status, "governance"))
        .then(() => updateAudit(pid, { status: status }))
        .then((res) => assert.equal(res.success, true))
        .catch((error) => _.error(error))
        .finally(() => this.ack(id, version));
    }

    if (event_action === "auditing-ended") {
      return await Round.findOne({ pid: pid })
      .then((round) => assert.equal(round?.status, "auditing"))
        .then(() =>
          Report.updateMany(
            { round_pid: pid },
            { finished: true },
            { multi: true }
          )
        )
        .then(() => updateAudit(pid, { status: status }))
        .then((res) => assert.equal(res.success, true))
        .catch((error) => _.error(error))
        .finally(() => this.ack(id, version));
    }
  }
}
