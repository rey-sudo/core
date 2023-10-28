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
      title: "Cortadora multifuncional de frutas y verduras",
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
          background_color: "#27b46a",
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
        page_4: {
          emoji: "🤯",
          title: "¡Más beneficios!",
          subtitle:
            " ¿Por qué perder más tiempo cortando manualmente cuando puedes hacerlo de manera rápida y precisa con nuestro picador?",
          section: {
            left: {
              image: "/test/1200x1200-1.webp",
              title: "Seguridad",
              subtitle:
                "Superficie de corte con rieles y sujetador de mano para más seguridad.",
            },
            center: {
              image: "/test/1200x1200-10.webp",
              title: "Eficiencia",
              subtitle: "Los cortes se realizan en menos de un segundo.",
            },
            right: {
              image: "/test/1200x1200-14.webp",
              title: "Versatilidad",
              subtitle:
                "Las 7 cuchillas de corte permiten gran variedad de resultados.",
            },
          },
        },
        page_5: {
          emoji: "🇨🇴",
          title: "Envíos gratis a toda Colombia",
          subtitle:
            "Enviamos a todas las ciudades de Colombia: Realizamos entregas rápidas y seguras a cada rincón del país. ¡Haz tu pedido ahora y paga en casa!",
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
