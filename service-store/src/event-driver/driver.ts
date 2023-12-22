import { BaseDriver } from "@alphaicterus/global";

export class EventDriver extends BaseDriver {
    onMessage(data: any): void {
        throw new Error("Method not implemented.");
    } 
}

export const eventDriver = new EventDriver();
