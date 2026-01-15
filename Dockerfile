FROM rust:slim AS builder

WORKDIR /app
COPY . .
WORKDIR /app/compiler

RUN cargo build --release

CMD ["cargo", "run", "--release"]


FROM debian:12-slim AS runner

WORKDIR /app
COPY --from=builder /app/compiler/target/release/compiler .

EXPOSE 3000

CMD ["./compiler"]
