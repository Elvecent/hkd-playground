{ nixpkgs ? import ./nix/nixpkgs-unstable.nix
, hls ? true
, hoogle ? true
}:

let
  packageName = "hkd-playground";

  overlay = self: super: {
    myHaskellPackages =
      super.haskellPackages.override (old: {
        overrides = self.lib.composeExtensions (old.overrides or (_: _: {}))
          (hself: hsuper: {
            ${packageName} = hself.callCabal2nix packageName ./. {};
          });
      });
  };

  pkgs = import nixpkgs {
    overlays = [ overlay ];
  };

  haskellPackages = pkgs.myHaskellPackages;

in
haskellPackages.shellFor {
  packages = ps: [ ps.${packageName} ];
  withHoogle = hoogle;
  buildInputs = with haskellPackages;
    [ ghcid
      hlint
      stylish-haskell
    ] ++ (if hls
          then [ haskell-language-server ]
          else []) ;
}
