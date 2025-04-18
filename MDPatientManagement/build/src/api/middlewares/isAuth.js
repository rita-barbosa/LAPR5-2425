"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
// remove by JRT : import jwt from 'express-jwt';
var { expressjwt: jwt } = require("express-jwt");
const config_1 = __importDefault(require("../../../config"));
/**
 * We are assuming that the JWT will come in a header with the form
 *
 * Authorization: Bearer ${JWT}
 *
 * But it could come in a query parameter with the name that you want like
 * GET https://my-bulletproof-api.com/stats?apiKey=${JWT}
 * Luckily this API follow _common sense_ ergo a _good design_ and don't allow that ugly stuff
 */
const getTokenFromHeader = req => {
    /**
     * @TODO Edge and Internet Explorer do some weird things with the headers
     * So I believe that this should handle more 'edge' cases ;)
     */
    if ((req.headers.authorization && req.headers.authorization.split(' ')[0] === 'Token') ||
        (req.headers.authorization && req.headers.authorization.split(' ')[0] === 'Bearer')) {
        return req.headers.authorization.split(' ')[1];
    }
    return null;
};
const isAuth = jwt({
    secret: config_1.default.jwtSecret,
    userProperty: 'token',
    getToken: getTokenFromHeader,
    algorithms: ["HS256"],
    issuer: 'HealthcareSystemFAE',
    audience: 'HealthcareSystemUser'
});
exports.default = isAuth;
//# sourceMappingURL=isAuth.js.map