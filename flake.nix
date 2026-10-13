{
  inputs = {
    nixpkgs.url = "https://channels.nixos.org/nixpkgs-unstable/nixexprs.tar.zst";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (
    system: let
      pkgs = inputs.nixpkgs.legacyPackages.${system};
      lib = pkgs.lib;
    in {
      devShells.default = pkgs.mkShell {
        LD_LIBRARY_PATH = with pkgs; lib.makeLibraryPath [
          postgresql
          libpq
          zlib
        ];

        buildInputs = with pkgs; [
          zlib
        ];

        nativeBuildInputs = with pkgs; [
          zlib
        ];

        packages = with pkgs; [
          postgresql
          pkg-config
          zlib
        ];
      };
    }
  );
}
