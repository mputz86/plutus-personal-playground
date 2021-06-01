{}:
let
  # Import shell from plutus repo
  # Makes tools like haskell-language-server available
  shell = import ./../plutus/shell.nix { };
in
shell
