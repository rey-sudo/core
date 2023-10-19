import { customAlphabet } from "nanoid";


const generatePid = (type: string, length: number) => {
    if(type === 'UAN'){
        return customAlphabet("0123456789ABCDEFGHJKLMNOPQRTUVWXYZ", length)
    }

    if(type === 'N'){
        return customAlphabet("0123456789", length)
    }
}
export { generatePid };
