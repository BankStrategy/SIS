# ────────────────────────────────────────────────────────────────────────────
# Darwin Gödel Machine — convenience targets
# ────────────────────────────────────────────────────────────────────────────

.PHONY: build test run clean docker-build docker-run lint

# ── Local Cabal targets ───────────────────────────────────────────────────

build:
	cabal build all

test:
	cabal test --test-show-details=streaming

run:
	cabal run dgm -- 5

run-cycles:
	cabal run dgm -- $(CYCLES)

lint:
	cabal build all 2>&1 | grep -E "warning|error" | head -40

clean:
	cabal clean

# ── Docker targets (no local GHC required) ────────────────────────────────

docker-build:
	docker build -t dgm-haskell:latest .

docker-run: docker-build
	docker run --rm dgm-haskell:latest 5

docker-test: docker-build
	@echo "Tests run as part of Docker build (see Dockerfile Stage 1)."

# ── Quick demo (Docker) ───────────────────────────────────────────────────

demo: docker-run

# ── Help ─────────────────────────────────────────────────────────────────

help:
	@echo "Targets:"
	@echo "  build        - Build with local cabal"
	@echo "  test         - Run test suite with local cabal"
	@echo "  run          - Run 5 Zero Data Cycles (local cabal)"
	@echo "  run-cycles   - Run CYCLES=n Zero Data Cycles (local cabal)"
	@echo "  docker-build - Build Docker image (no local GHC needed)"
	@echo "  docker-run   - Run via Docker"
	@echo "  demo         - docker-build + docker-run"
	@echo "  clean        - Clean build artefacts"
