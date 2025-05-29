with (import <nixpkgs> {});
mkShell {
  buildInputs = [
    gmp
    graphviz
    libz
  ];
}
