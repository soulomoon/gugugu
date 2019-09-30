export const guguguExampleHost: string = (
  process.env.GUGUGU_EXAMPLE_HOST == undefined
    ? "127.0.0.1"
    : process.env.GUGUGU_EXAMPLE_HOST
);

export const guguguExamplePort: number = (
  process.env.GUGUGU_EXAMPLE_PORT == undefined
    ? 8080
    : parseInt(process.env.GUGUGU_EXAMPLE_PORT)
);
