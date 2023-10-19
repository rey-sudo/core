import { Request, Response } from "express";
import { Product } from "../models";

const getAllProductsHandler = async (req: Request, res: Response) => {
  const findProducts = await Product.find({});

  const scheme = [
    {
      category: "trending",
      title: "Mira las ultimas tendencias",
      page: 1,
      items: findProducts,
    },
  ];

  res.status(200).send(scheme);
};

export { getAllProductsHandler };
