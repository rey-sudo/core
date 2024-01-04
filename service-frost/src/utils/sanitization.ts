export const sanitizeContent = (element: any) => {
  const formated = JSON.stringify(element);

  const result = formated.replace(
    /.js|.css|.onion|<|>|href|src=|script>|<HTML>|onclick|onload|.cookie|default=|style=|html|<%|onmouse|data:|javascript:|javascript&|<alert|.html|.scss|.svg|.swf|.asp|.txt|@import/gi,
    ""
  );

  return JSON.parse(result);
};
