FROM alpine:3.23 AS builder

RUN apk update
RUN apk add --no-cache build-base git
RUN apk add --no-cache cmake cmake-doc cmake-extras pkgconf

WORKDIR /usr/src/compiler

COPY compiler .

RUN rm -rf build

RUN mkdir build/ && cd build && cmake .. && make -j8


FROM python:3.12-alpine3.21 AS runner

RUN pip install --no-cache-dir poetry==2.0.0
RUN apk add --no-cache bash binutils

WORKDIR /usr/src/app
COPY pyproject.toml poetry.lock ./
RUN poetry install --no-root --no-cache

COPY --from=builder /usr/src/compiler/build/compilers_compiler ./compilers_compiler
COPY . .

RUN poetry install --no-cache && ./check.sh

EXPOSE 3000

# Setting PYTHONUNBUFFERED forces print() to flush even if stdout is not a TTY.
ENV PYTHONUNBUFFERED=1
CMD ["./compiler.sh", "serve", "--host=0.0.0.0"]
