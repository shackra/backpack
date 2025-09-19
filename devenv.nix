{
  pkgs,
  lib,
  config,
  inputs,
  ...
}:
let
  pkgs-30-1 = import inputs.nixpkgs-30-1 { system = pkgs.stdenv.system; };
  pkgs-29-4 = import inputs.nixpkgs-29-4 { system = pkgs.stdenv.system; };
  pkgs-29-3 = import inputs.nixpkgs-29-3 { system = pkgs.stdenv.system; };
  pkgs-29-2 = import inputs.nixpkgs-29-2 { system = pkgs.stdenv.system; };
  pkgs-29-1 = import inputs.nixpkgs-29-1 { system = pkgs.stdenv.system; };
  pkgs-28-2 = import inputs.nixpkgs-28-2 { system = pkgs.stdenv.system; };
  pkgs-28-1 = import inputs.nixpkgs-28-1 { system = pkgs.stdenv.system; };
  pkgs-27-2 = import inputs.nixpkgs-27-2 { system = pkgs.stdenv.system; };
  pkgs-27-1 = import inputs.nixpkgs-27-1 { system = pkgs.stdenv.system; };

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
  prepareAndRunTest = pkgs.writeShellScriptBin "prepare-and-run" ''
    TEST_HOME=$(mktemp -d)
    git clone --recurse-submodules $2 $TEST_HOME/.emacs.d

    HOME=$TEST_HOME $1 -batch -l ert -l $TEST_HOME/.emacs.d/test/all-tests.el -f ert-run-tests-batch-and-exit

    rm -rf $TEST_HOME
  '';
in
{
  packages = [
    pkgs.git
    prepareAndRunTest
    # (renameEmacs pkgs "emacsRolling")
    # (renameEmacs pkgs-30-1 "emacs-30-1")
    # (renameEmacs pkgs-29-4 "emacs-29-4")
    # (renameEmacs pkgs-29-3 "emacs-29-3")
    # (renameEmacs pkgs-29-2 "emacs-29-2")
    # (renameEmacs pkgs-29-1 "emacs-29-1")
    # (renameEmacs pkgs-28-2 "emacs-28-2")
    # (renameEmacs pkgs-28-1 "emacs-28-1")
    # (renameEmacs pkgs-27-2 "emacs-27-2")
    # (renameEmacs pkgs-27-1 "emacs-27-1")
  ];
  enterShell = ''
    git --version
  '';

  enterTest = ''
    echo "Running tests"
    prepare-and-run `which emacs` $DEVENV_ROOT
  '';
}
