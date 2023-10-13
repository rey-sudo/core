export interface TokenPayload {
  scope: string;
  entity: string;
  id: string;
  email: string;
  iat: number;
  exp: number;
}
