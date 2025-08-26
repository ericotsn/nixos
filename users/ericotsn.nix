{ config, pkgs, ... }:

{
  imports = [
    ../modules/emacs.nix
    ../modules/eza.nix
    ../modules/fzf.nix
  ];

  # Add ~/.local/bin to PATH.
  environment.localBinInPath = true;

  programs.fish.enable = true;

  users.users.ericotsn = {
    isNormalUser = true;
    extraGroups = [
      "docker"
      "wheel"
    ];
    shell = pkgs.fish;
    hashedPassword = "$y$j9T$nsVaUSOUNIrbKtne7xTVQ/$EciO.2vyatQ7gAFv3NdCu5JazMgAzQ1rp4FMu/PhLpC";
  };

  environment.systemPackages = with pkgs; [
    alacritty
    docker-compose
    fd
    git
    hunspell # Spell checker
    hunspellDicts.en_US
    hunspellDicts.sv_SE
    jq
    lazydocker
    nodejs_22 # LTS
    opencode # AI coding agent, built for the terminal
    ripgrep
    unzip
    vim
  ];

  environment.sessionVariables = { };

  programs.emacs = {
    enable = true;
    package = pkgs.emacs-git;
  };

  programs.direnv.enable = true;
  programs.eza.enable = true;
  programs.fzf.enable = true;
  programs.zoxide.enable = true;

  programs._1password.enable = true;
  programs._1password-gui = {
    enable = true;
    package = pkgs._1password-gui-beta;
    # Certain features, including CLI integration and system authentication
    # support, require enabling PolKit integration on some desktop environments
    # (e.g. Plasma).
    polkitPolicyOwners = [ "ericotsn" ];
  };
}
