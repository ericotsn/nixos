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
      expand-region
      flycheck
      forge
      gptel
      helpful
      jinx
      lsp-mode
      lsp-ui
      magit
      marginalia
      markdown-mode
      modus-themes
      move-text
      nix-mode
      orderless
      perspective
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
