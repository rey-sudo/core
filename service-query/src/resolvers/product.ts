export const product = {
  Query: {
    product: (_: any, args: { id: string }) => {
      console.log(args.id);

      const product = [
        {
          title: "The Awakening",
          author: "Kate Chopin",
        },
      ];

      return product;
    },
  },
};
