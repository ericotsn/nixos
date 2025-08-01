{
  lib,
  stdenvNoCC,
  buildGoModule,
  bun,
  fetchFromGitHub,
  models-dev,
  nix-update-script,
  testers,
  writableTmpDirAsHomeHook,
}:

let
  opencode-node-modules-hash = {
    "aarch64-linux" = "sha256-LOy/k8yeUEDe3FFDAgXGJ2BVIxMFy/Js0cgFbtThiDo=";
  };
  bun-target = {
    "aarch64-linux" = "bun-linux-arm64";
  };
in
stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "opencode";
  version = "0.3.110";
  src = fetchFromGitHub {
    owner = "sst";
    repo = "opencode";
    tag = "v${finalAttrs.version}";
    hash = "sha256-2fvUSbQWBxjXLRfVFwJ6VNO2tx+qGa+IDRCSwFPqw+o=";
  };

  tui = buildGoModule {
    pname = "opencode-tui";
    inherit (finalAttrs) version;
    # Use the full repository so that we can resolve replace
    # directives in `packages/tui/go.mod`.
    src = finalAttrs.src;

    modRoot = "packages/tui";

    vendorHash = "sha256-nBwYVaBau1iTnPY3d5F/5/ENyjMCikpQYNI5whEJwBk=";

    subPackages = [ "cmd/opencode" ];

    env.CGO_ENABLED = 0;

    ldflags = [
      "-s"
      "-X=main.Version=${finalAttrs.version}"
    ];

    installPhase = ''
      runHook preInstall

      install -Dm755 $GOPATH/bin/opencode $out/bin/tui

      runHook postInstall
    '';
  };

  node_modules = stdenvNoCC.mkDerivation {
    pname = "opencode-node_modules";
    inherit (finalAttrs) version src;

    impureEnvVars = lib.fetchers.proxyImpureEnvVars ++ [
      "GIT_PROXY_COMMAND"
      "SOCKS_SERVER"
    ];

    nativeBuildInputs = [
      bun
      writableTmpDirAsHomeHook
    ];

    dontConfigure = true;

    buildPhase = ''
      runHook preBuild

       export BUN_INSTALL_CACHE_DIR=$(mktemp -d)

       bun install \
         --filter=opencode \
         --force \
         --frozen-lockfile \
         --no-progress

      runHook postBuild
    '';

    installPhase = ''
      runHook preInstall

      mkdir -p $out/node_modules
      cp -R ./node_modules $out

      runHook postInstall
    '';

    # Required else we get errors that our fixed-output derivation
    # references store paths.
    dontFixup = true;

    outputHash = opencode-node-modules-hash.${stdenvNoCC.hostPlatform.system};
    outputHashAlgo = "sha256";
    outputHashMode = "recursive";
  };

  nativeBuildInputs = [
    bun
    models-dev
  ];

  patches = [
    # Patch `packages/opencode/src/provider/models-macro.ts` to get
    # contents of `api.json` from the file bundled with `bun build`.
    ./local-models-dev.patch
  ];

  configurePhase = ''
    runHook preConfigure

    cp -R ${finalAttrs.node_modules}/node_modules .

    runHook postConfigure
  '';

  env.MODELS_DEV_API_JSON = "${models-dev}/dist/api.json";

  buildPhase = ''
    runHook preBuild

    bun build \
      --define OPENCODE_VERSION="'${finalAttrs.version}'" \
      --compile \
      --minify \
      --target=${bun-target.${stdenvNoCC.hostPlatform.system}} \
      --outfile=opencode \
      ./packages/opencode/src/index.ts \
      ${finalAttrs.tui}/bin/tui

    runHook postBuild
  '';

  dontStrip = true;

  installPhase = ''
    runHook preInstall

    install -Dm755 opencode $out/bin/opencode

    runHook postInstall
  '';
})
