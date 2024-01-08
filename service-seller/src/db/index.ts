import postgres from "postgres";

const DB = postgres("postgres://service_seller:test1@host:5433/citus", {
    
});

export default DB;
