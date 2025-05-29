FROM fpco/stack-build-small:lts-23.24

# Cache compiler
RUN stack setup 9.8.4 && \
    stack install stylish-haskell hlint weeder && \
    wget --progress=dot:giga https://github.com/hadolint/hadolint/releases/download/v2.12.0/hadolint-Linux-x86_64 -O /bin/hadolint && chmod +x /bin/hadolint

# Cache dependencies
WORKDIR /app
COPY stack.yaml package.yaml stack.yaml.lock /app/
RUN stack build --test --dependencies-only

# Do the actual work
COPY . .
RUN stack test --ghc-options '-Werror'

# Validation
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
RUN diff <(paste -d - <(stack ls dependencies --filter \$locals --separator -) <(stack ls dependencies --filter \$locals --license | cut -d" " -f2)) stack_sbom.txt
RUN diff <(stack ls dependencies --filter \$locals --license | cut -d" " -f2 | LC_ALL=C sort | uniq) stack_licenses.txt

# Check that all code is formatted and used
RUN stylish-haskell -i -r . && git diff --exit-code
RUN hlint . && \
    hadolint Dockerfile && \
    weeder --hie-directory=.stack-work
