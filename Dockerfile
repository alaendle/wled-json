FROM fpco/stack-build-small:lts-22.40

# Cache compiler
RUN stack setup 9.6.6

# Cache dependencies
WORKDIR /app
COPY stack.yaml package.yaml stack.yaml.lock /app/
RUN stack build --dependencies-only

# Do the actual work
COPY . .
RUN stack test

# Validation
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
RUN diff <(paste -d - <(stack ls dependencies --filter \$locals --separator -) <(stack ls dependencies --filter \$locals --license | cut -d" " -f2)) stack_sbom.txt
RUN diff <(stack ls dependencies --filter \$locals --license | cut -d" " -f2 | LC_ALL=C sort | uniq) stack_licenses.txt
