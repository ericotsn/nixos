{ config, pkgs, ... }:

{
  imports = [
    ../modules/fzf.nix
  ];

  # Add ~/.local/bin to PATH.
  environment.localBinInPath = true;

  programs.fish.enable = true;

  users.users.eotsn = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    shell = pkgs.fish;
    hashedPassword = "$y$j9T$nsVaUSOUNIrbKtne7xTVQ/$EciO.2vyatQ7gAFv3NdCu5JazMgAzQ1rp4FMu/PhLpC";
  };

  environment.systemPackages = with pkgs; [
    alacritty
    emacs-lsp-booster
    emacs-with-pkgs
    fd
    git
    hunspell # Spell checker
    hunspellDicts.en_US
    hunspellDicts.sv_SE
    jq
    nodejs_22 # LTS
    pnpm
    ripgrep
    unzip
    vim
  ];

  environment.sessionVariables = {
    # https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
    LSP_USE_PLISTS = "true";
  };

  # When not using EXWM we can run Emacs as a systemd service.
  services.emacs = {
    enable = !(config.specialisation ? exwm);
    package = pkgs.emacs-with-pkgs;
    defaultEditor = true;
  };

  programs.fzf.enable = true;
  programs.zoxide.enable = true;

  programs._1password.enable = true;
  programs._1password-gui = {
    enable = true;
    package = pkgs._1password-gui-beta;
    # Certain features, including CLI integration and system authentication
    # support, require enabling PolKit integration on some desktop environments
    # (e.g. Plasma).
    polkitPolicyOwners = [ "eotsn" ];
  };
}
