{
  pkgs ? import <nixpkgs> { },
}:

let
  overrides = final: prev: {
    lsp-mode = (
      prev.lsp-mode.overrideAttrs (
        f: p: {
          buildPhase =
            ''
              export LSP_USE_PLISTS=true
            ''
            + p.buildPhase;
        }
      )
    );
  };

  pragmatapro-mode = pkgs.emacsPackages.trivialBuild rec {
    pname = "pragmatapro-mode";
    version = "fadcc2e5cff3d8e69d011b815efc7808533ec09b";
    src = pkgs.fetchFromSourcehut {
      owner = "~eotsn";
      repo = "pragmatapro-mode";
      rev = version;
      sha256 = "sha256-V0oUAzs4uf0VAItk/MRMQIuEUFxedCEywwJ1O5z68C8=";
    };
  };

  modus-themes = pkgs.emacsPackages.trivialBuild rec {
    pname = "modus-themes";
    version = "5f9dc668511243d3cd26469d0574bba4a3aaf1d7";
    src = pkgs.fetchFromGitHub {
      owner = "protesilaos";
      repo = "modus-themes";
      rev = version;
      sha256 = "sha256-ts1EZl09T8V+FliNERkf1Om8HzRoQtM/hqGPfzAt2Ww=";
    };
  };

  emacsPackages = (pkgs.emacsPackagesFor pkgs.emacs-git).overrideScope overrides;

  emacsWithPackages = emacsPackages.withPackages;

  basePackages =
    epkgs: with epkgs; [
      avy
      cape
      consult
      corfu
      eat
      embark
      embark-consult
      envrc
      expand-region
      forge
      gptel
      helpful
      jinx
      magit
      marginalia
      markdown-mode
      modus-themes
      move-text
      nix-mode
      orderless
      perspective
      pragmatapro-mode
      vertico
      wgrep
      yasnippet

      # https://wiki.nixos.org/wiki/Emacs#Tree-sitter
      treesit-grammars.with-all-grammars
      tree-sitter-langs
    ];

  finalPackage = emacsWithPackages basePackages;
in
finalPackage.overrideAttrs (old: {
  passthru = (old.passthru or { }) // {
    pkgs = {
      withPackages =
        extraPackagesFn: emacsWithPackages (epkgs: basePackages epkgs ++ extraPackagesFn epkgs);
    };
  };
})
