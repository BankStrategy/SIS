# ────────────────────────────────────────────────────────────────────────────
# Darwin Gödel Machine — Haskell MVP
# Multi-stage build: builder + minimal runtime image
# ────────────────────────────────────────────────────────────────────────────

# Stage 1: build
FROM haskell:9.6 AS builder

WORKDIR /build

# Cache dependencies separately from source code.
# Copy only package description files first.
COPY dgm.cabal cabal.project ./
RUN cabal update && cabal build --only-dependencies -j4 2>&1 | tail -20

# Now copy source and build.
COPY src/ src/
COPY app/ app/
COPY test/ test/
RUN cabal build all -j4

# Run tests during build (fail fast).
RUN cabal test --test-show-details=streaming

# Extract the binary.
RUN cp "$(cabal list-bin dgm)" /dgm-binary

# ────────────────────────────────────────────────────────────────────────────
# Stage 2: minimal runtime
# ────────────────────────────────────────────────────────────────────────────
FROM debian:bookworm-slim AS runtime

RUN apt-get update && apt-get install -y --no-install-recommends \
      libgmp10 \
    && rm -rf /var/lib/apt/lists/*

COPY --from=builder /dgm-binary /usr/local/bin/dgm

ENTRYPOINT ["dgm"]
CMD ["5"]
