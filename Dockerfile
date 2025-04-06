# Use the official Haskell image as the base image
FROM haskell:9.8.4-slim-bullseye as os-deps

# Install necessary system libraries and tools
RUN cat /etc/debian_version
RUN apt-get update && apt-get install -y curl ca-certificates gnupg
RUN curl https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add -
RUN echo "deb http://apt.postgresql.org/pub/repos/apt bullseye-pgdg main" > /etc/apt/sources.list.d/pgdg.list
RUN apt-get update
RUN apt-get install -y \
    libgmp-dev \
    build-essential \
    libpq-dev \
    curl \
    git \
    && rm -rf /var/lib/apt/lists/*

# Set environment variables
ENV STACK_VERSION=3.5.1
ENV GHC_VERSION=9.8.4

RUN curl -sSL https://get.haskellstack.org/ | sh -s - -f

FROM os-deps AS code-deps

WORKDIR /app

# Copy the project files into the container
COPY package.yaml stack.yaml stack.yaml.lock ./ 

# Install dependencies
RUN stack setup --install-ghc --compiler ghc-9.8.4
RUN stack build --only-dependencies 


FROM code-deps as build
WORKDIR /app
COPY . .
RUN stack build


FROM debian:bullseye-slim

RUN apt-get update && \
  apt-get install -y --no-install-recommends \
  ca-certificates \
  libpq-dev \
  libgmp10 && \
  rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY --from=build /app/.stack-work/dist/*/ghc-9.8.4/build/campaigns-exe/* /app/

ENTRYPOINT ["/app/campaigns-exe"]
