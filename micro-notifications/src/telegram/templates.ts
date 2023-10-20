const getTemplate = (payload: any) => {
  return `
Nueva compra 🎉🥳
    
Nombre: ${payload.name}
Apellido: ${payload.last_name}
Telefono: ${payload.phone}
Dirección: ${payload.address}
Departamento: ${payload.department}
Ciudad: ${payload.city}
Producto: ${payload.product_pid}
`;

};


export { getTemplate }