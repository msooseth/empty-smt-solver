{
  description = "empty-smt-solver";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = (import nixpkgs {
          inherit system;
        });

        hspkgs = ps :
          ps.haskellPackages.override {
            overrides = hfinal: hprev: {
              with-utf8 =
                if (with ps.stdenv; hostPlatform.isDarwin && hostPlatform.isx86)
                then ps.haskell.lib.compose.overrideCabal (_ : { extraLibraries = [ps.libiconv]; }) hprev.with-utf8
                else hprev.with-utf8;
              # TODO: temporary fix for static build which is still on 9.4
              witch = ps.haskell.lib.doJailbreak hprev.witch;
            };
          };
        hlib = pkgs.haskell.lib;

        # base empty-smt-solver derivation.
        # parameterized on the pkgs definition to allow use of `pkgsStatic` or `pkgs` as needed.
        solverBase = ps :
          ps.lib.pipe
            (((hspkgs ps).callCabal2nix "empty-smt-solver" ./. { }).overrideAttrs(final: prev: { }))
            [
              (hlib.compose.overrideCabal (old: { testTarget = "test"; }))
              hlib.compose.dontHaddock
              hlib.compose.doCheck
            ];

        # "static" binary for distribution
        # on linux this is actually a real fully static binary
        # on macos this has everything except libcxx, libsystem and libiconv
        # statically linked. we can be confident that these three will always
        # be provided in a well known location by macos itself.
        empty-smt-solverRedistributable = let
          grep = "${pkgs.gnugrep}/bin/grep";
          otool = "${pkgs.darwin.binutils.bintools}/bin/otool";
          install_name_tool = "${pkgs.darwin.binutils.bintools}/bin/install_name_tool";
          codesign_allocate = "${pkgs.darwin.binutils.bintools}/bin/codesign_allocate";
          codesign = "${pkgs.darwin.sigtool}/bin/codesign";
        in if pkgs.stdenv.isLinux
        then hlib.dontCheck (solverBase pkgs.pkgsStatic)
        else pkgs.runCommand "stripNixRefs" {} ''
          mkdir -p $out/bin
          cp ${hlib.dontCheck (forceStaticDepsMacos (solverBase pkgs))}/bin/empty-smt-solver $out/bin/

          # get the list of dynamic libs from otool and tidy the output
          libs=$(${otool} -L $out/bin/empty-smt-solver | tail -n +2 | sed 's/^[[:space:]]*//' | cut -d' ' -f1)

          # rewrite /nix/... library paths to point to /usr/lib
          chmod 777 $out/bin/empty-smt-solver

          # check that no nix deps remain
          nixdeps=$(${otool} -L $out/bin/empty-smt-solver | tail -n +2 | { ${grep} /nix/store -c || test $? = 1; })
          if [ ! "$nixdeps" = "0" ]; then
            echo "Nix deps remain in redistributable binary!"
            exit 255
          fi
          # re-sign binary
          CODESIGN_ALLOCATE=${codesign_allocate} ${codesign} -f -s - $out/bin/empty-smt-solver
          chmod 555 $out/bin/empty-smt-solver
        '';


        # wrapped binary for use on systems with nix available.
        # does not statically link.
        # ensures all required runtime deps are available and on path.
        empty-smt-solverWrapped =
          pkgs.symlinkJoin {
            name = "empty-smt-solver";
            paths = [ (hlib.dontCheck (solverBase pkgs)) ];
            buildInputs = [ pkgs.makeWrapper ];
            postBuild = ''
              wrapProgram $out/bin/empty-smt-solver \
                --prefix PATH : "${pkgs.lib.makeBinPath [
                  pkgs.bash
                  pkgs.coreutils
                ]}"
            '';
          };

        # if we pass a library folder to ghc via --extra-lib-dirs that contains
        # only .a files, then ghc will link that library statically instead of
        # dynamically (even if --enable-executable-static is not passed to cabal).
        # we use this trick to force static linking of some libraries on macos.
        stripDylib = drv : pkgs.runCommand "${drv.name}-strip-dylibs" {} ''
          mkdir -p $out
          mkdir -p $out/lib
          cp -r ${drv}/* $out/
          rm -rf $out/**/*.dylib
        '';

        # ensures that all required deps will be linked statically on macos builds
        forceStaticDepsMacos = p : hlib.appendConfigureFlags p [ ];


      in rec {

        # --- packages ----

        packages.ci = pkgs.lib.pipe (solverBase pkgs) (with hlib.compose; [doBenchmark dontHaddock disableLibraryProfiling]);
        packages.unwrapped = hlib.dontCheck (solverBase pkgs);
        packages.empty-smt-solver = empty-smt-solverWrapped;
        packages.redistributable = empty-smt-solverRedistributable;
        packages.default = packages.empty-smt-solver;

        # --- apps ----

        apps.empty-smt-solver = flake-utils.lib.mkApp { drv = packages.empty-smt-solver; };
        apps.default = apps.empty-smt-solver;

        # --- shell ---

        devShells.default = let
          libraryPath = "${pkgs.lib.makeLibraryPath [ ]}";
        in (hspkgs pkgs).shellFor {
          packages = _: [ (solverBase pkgs) ];
          buildInputs = [
            (hspkgs pkgs).cabal-install
            (hspkgs pkgs).haskell-language-server
          ] ++ testDeps;
          withHoogle = true;

          # point cabal repl to system deps
          LD_LIBRARY_PATH = libraryPath;
          shellHook = pkgs.lib.optionalString pkgs.stdenv.isDarwin ''
            export DYLD_LIBRARY_PATH="${libraryPath}";
          '';
        };
      }
    );
}
