import DB from "../db";
import { Request, Response } from "express";
import { sellerMiddleware } from "../utils/seller";
import { requireAuth } from "../utils/required";

const getOrdersMiddlewares: any = [sellerMiddleware, requireAuth];

const getOrdersHandler = async (req: Request, res: Response) => {
  const params = req.body;

  const SELLER = req.sellerData;

  let connection = null;

  try {
    connection = await DB.client.getConnection();

    const [response] = await connection.execute(
      `
      SELECT 
        p.*,
        JSON_ARRAYAGG(
            JSON_OBJECT(
                'id', s.id,
                'mode', s.mode,
                'status', s.status,
                'contract_units', s.contract_units,
                'contract_price', s.contract_price,
                'contract_collateral', s.contract_collateral,
                'contract_state', s.contract_state,
                'contract_0_utx', s.contract_0_utx,
                'contract_0_tx', s.contract_0_tx,
                'product_discount', s.product_discount,
                'created_at', s.created_at              
            )
        ) AS orders,
        COUNT(s.id) AS slots_count
      FROM 
        products p
      LEFT JOIN 
        orders s ON p.id = s.product_id
      WHERE
        p.seller_id = ?
      GROUP BY 
        p.id;      
      `,
      [SELLER.id],
    );

    await connection.commit();

    res.status(200).send({ success: true, payload: response });
  } catch (err: any) {
    await connection.rollback();

    res.status(404).send({ success: true });
  } finally {
    connection.release();
  }
};

export { getOrdersHandler, getOrdersMiddlewares };
