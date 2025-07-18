FROM benz0li/ghc-musl:9.10.2

# Cache compiler
RUN stack install stylish-haskell hlint weeder --resolver=lts-24.0 --system-ghc --allow-newer && \
    wget --progress=dot:giga https://github.com/hadolint/hadolint/releases/download/v2.12.0/hadolint-Linux-x86_64 -O /bin/hadolint && chmod +x /bin/hadolint

# Cache dependencies
WORKDIR /app
COPY stack.yaml package.yaml stack.yaml.lock /app/
RUN stack build --test --dependencies-only --system-ghc

# Do the actual work
COPY . .
RUN stack test --system-ghc --ghc-options '-Werror'

# Validation
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
RUN diff <(paste -d - <(stack --system-ghc ls dependencies --filter \$locals --separator -) <(stack --system-ghc ls dependencies --filter \$locals --license | cut -d" " -f2)) stack_sbom.txt
RUN diff <(stack --system-ghc ls dependencies --filter \$locals --license | cut -d" " -f2 | LC_ALL=C sort | uniq) stack_licenses.txt

# Check that all code is formatted and used
RUN ~/.local/bin/stylish-haskell -i -r . && git diff --exit-code
RUN ~/.local/bin/hlint . && \
    hadolint Dockerfile && \
    ~/.local/bin/weeder --hie-directory=.stack-work
