{
  pkgs,
  lib,
  config,
  inputs,
  ...
}:
let
  pkgs-30-1 = import inputs.nixpkgs-30-1 { system = pkgs.stdenv.system; };
  pkgs-29-4 = import inputs.nixpkgs-29-4 {
    system = pkgs.stdenv.system;
    config = {
      allowInsecurePredicate = pkg: builtins.elem (lib.getName pkg) [ "emacs" ];
    };
  };
  pkgs-29-3 = import inputs.nixpkgs-29-3 { system = pkgs.stdenv.system; };
  pkgs-29-2 = import inputs.nixpkgs-29-2 { system = pkgs.stdenv.system; };
  pkgs-29-1 = import inputs.nixpkgs-29-1 { system = pkgs.stdenv.system; };

  renameEmacs =
    pkgs: newName:
    pkgs.symlinkJoin {
      name = newName;
      paths = [ pkgs.emacs ];
      postBuild = ''
        mkdir -p $out/bin
        ln -s ${pkgs.emacs}/bin/emacs $out/bin/${newName}
      '';
    };

  # TODO(shackra): add scripts for running emacs in -nw and as a daemon
  prepareAndRunTest = (
    pkgs.writeShellScriptBin "prepare-and-run" (builtins.readFile ./etc/scripts/prepare-and-run.sh)
  );

  run-for-each-emacs = (
    pkgs.writeShellScriptBin "for-each-emacs" (builtins.readFile ./etc/scripts/for-each-emacs.sh)
  );
in
{
  env = {
    EMACS_VERSIONS_TO_TEST = "emacs-rolling emacs-30-1 emacs-29-4 emacs-29-3 emacs-29-2 emacs-29-1";
  };

  packages = [
    pkgs.git
    pkgs.rsync
    prepareAndRunTest
    run-for-each-emacs
    (renameEmacs pkgs "emacs-rolling")
    (renameEmacs pkgs-30-1 "emacs-30-1")
    (renameEmacs pkgs-29-4 "emacs-29-4")
    (renameEmacs pkgs-29-3 "emacs-29-3")
    (renameEmacs pkgs-29-2 "emacs-29-2")
    (renameEmacs pkgs-29-1 "emacs-29-1")

    # development files
    pkgs.enchant.dev # for jinx
  ];
  enterTest = ''
    for-each-emacs $DEVENV_ROOT/.
    exit $?
  '';
}
