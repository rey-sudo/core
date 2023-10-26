const commit__viewPaymentModal = (state, data) => {
  state.viewPaymentModal = data;
};

const commit__getProductData = (state, data) => {
  console.log(data);
  const datum = {
    image: {
      small: "string",
      medium: "empty",
      large: "empty",
    },
    theme: {
      title: "Cortadora manual de verduras y frutas",
      subtitle:
        "¡Utiliza los 7 tipos de corte y el recipiente para preparar lo que quieras!",
      config: {
        page_1: {
          background_color: "initial",
          mask: "/backgrounds/background-wave-darkgreen.svg",
          images: [
            {
              url: "/products/123456789/images/p123456789-a1-w1200-h1200.webp",
            },
            {
              url: "/products/123456789/images/p123456789-a2-w1200-h1200.webp",
            },
            {
              url: "/products/123456789/images/p123456789-a3-w1200-h1200.webp",
            },
            {
              url: "/products/123456789/images/p123456789-a4-w1200-h1200.webp",
            },
            {
              url: "/products/123456789/images/p123456789-a5-w1200-h1200.webp",
            },
          ],
        },
        page_2: {
          background_color: "initial",
          mask: "/backgrounds/empty.png",
          section: {
            image: "/products/123456789/images/web/1080x1080-3.webp",
            content: {
              title: "No pierdas tiempo",
              text: "Ahorra tiempo en la cocina cortando los alimentos de diversas formas en un solo recipiente.",
            },
          },
        },
        page_3: {
          background_color: "initial",
          mask: "/backgrounds/empty.png",
          section: {
            image: "/products/123456789/images/web/1080x1080-1.webp",
            content: {
              title: "Sin límites",
              text: "Transforma tus ideas en deliciosos platos y diviértete con las miles de posibilidades de combinación.",
            },
          },
        },
      },
    },
    name: "cortadora manual de verduras y frutas para cocina con 7 cuchillas diferentes y recipiente",
    space_url: "https://arkastore.nyc3.digitaloceanspaces.com",
    payment_type: "contraentrega",
    shipping_tax: false,
    shipping_label: "Envio gratis",
    shipping_icon: "pi pi-bolt",
    stock_supply: 99,
    price: 69000,
    price_diff: 71000,
    discount: 15,
    discount_label: "% OFF",
    discount_color: "#8c99b3",
    seller_pid: "adriana",
    pid: "980760079186818",
  };
  state.productData = datum;
};

const commit__createOrder = (state, data) => {
  state.orderData = data;
};

export {
  commit__viewPaymentModal,
  commit__getProductData,
  commit__createOrder,
};
