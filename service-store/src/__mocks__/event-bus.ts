export const eventBus = {
  client: {
    publish: jest
      .fn()
      .mockImplementation(
        (channel: string, message: string, callback: () => void) => {
          callback();
        }
      ),
  },
};
