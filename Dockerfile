FROM alpine:3.23 AS builder

RUN apk update
RUN apk add --no-cache build-base git
RUN apk add --no-cache cmake cmake-doc cmake-extras pkgconf
RUN apk add --no-cache python poetry

WORKDIR /usr/src/app

COPY . .

RUN mkdir build/ && cd build && cmake .. && make


FROM python:slim AS runner


ENV PYTHONFAULTHANDLER=1 \
    PYTHONUNBUFFERED=1 \
    PYTHONHASHSEED=random \
    PIP_NO_CACHE_DIR=off \
    PIP_DISABLE_PIP_VERSION_CHECK=on \
    PIP_DEFAULT_TIMEOUT=100 \
    POETRY_NO_INTERACTION=1 \
    POETRY_VIRTUALENVS_CREATE=false \
    POETRY_CACHE_DIR='/var/cache/pypoetry' \
    POETRY_HOME='/usr/local' \

# System deps:
RUN curl -sSL https://install.python-poetry.org | python3 -

WORKDIR /usr/src/app
COPY --from=builder /usr/src/app/build/compilers_compiler /usr/src/app/compilers_compiler

COPY --from=builder src/server server

WORKDIR /usr/src/app/server

RUN poetry install --no-interaction --no-ansi

# CMD ["/usr/src/app/compilers_compiler", "serve", "3000"]
CMD [ "./compilers_compiler"]
