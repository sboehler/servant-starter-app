with import <nixpkgs> {};

let
  hpkgs = haskellPackages.override {
    overrides = self: super: {
    };
  };
  ghc = hpkgs.ghcWithPackages (p: with p; [
    aeson
    postgresql-simple
    postgresql-simple-migration
    containers
    formatting
    servant-server
    servant-auth
    servant-auth-server
    servant-client
    http-client
    resource-pool
  ]);
in runCommand "servant-starter-app" { buildInputs = [ ghc hpkgs.ghcid ]; } ''
  cp -vr ${./.} ./app
  mkdir -pv $out/bin
  chmod -R +w .
  find ./
  cd app/src
  ghc ../app/Main.hs -o $out/bin/servant-starter-app
  ${binutils-unwrapped}/bin/strip $out/bin/servant-starter-app
  patchelf --shrink-rpath $out/bin/servant-starter-app
''
