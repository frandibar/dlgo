{
  description = "Flake for dlgo";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
  };

  outputs = { self, nixpkgs }:
    let pkgs = nixpkgs.legacyPackages.x86_64-linux;
        libs = with pkgs; [
        ];
    in {
      devShells."x86_64-linux".default = pkgs.mkShell {
        buildInputs = with pkgs; [
          sbcl                  # SBCL Common Lisp implementation
          gcc                   # C Compiler
          stdenv.cc.cc.lib      # C standard libraries, for basic linking
        ];

      # Set up the environment variable for the dev shell
      shellHook = ''
        export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath(libs)}:./lib
      '';
    };
  };
}
