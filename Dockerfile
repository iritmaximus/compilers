FROM alpine:3.23 AS builder

WORKDIR /app
COPY . .
WORKDIR /app/compiler

RUN cargo build --release

CMD ["cargo", "run", "--release"]


FROM alpine:3.23 AS runner
# RUN apt-get update && apt-get install -y extra-runtime-dependencies && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY --from=builder /app/compiler/target/release/compiler .

CMD ["./compiler"]

