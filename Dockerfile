FROM fpco/stack-build-small:lts-23.1

# Cache compiler
RUN stack setup 9.8.4 && \
    stack install stylish-haskell && \
    stack install hlint

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

# Check that all code is formatted
RUN stylish-haskell -i -r . && git diff --exit-code
RUN hlint .
