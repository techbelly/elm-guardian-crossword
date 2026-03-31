export async function onRequest(context) {
  const url = new URL(context.request.url);
  const guardianUrl =
    "https://www.theguardian.com" +
    url.pathname.replace(/^\/guardian/, "") +
    url.search;

  const response = await fetch(guardianUrl, {
    headers: { "User-Agent": "Mozilla/5.0" },
  });

  return new Response(response.body, {
    status: response.status,
    headers: {
      "Content-Type": response.headers.get("Content-Type") ?? "text/html",
    },
  });
}
