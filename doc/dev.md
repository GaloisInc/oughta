# Developer documentation

## Downstream projects

When evaluating breaking changes to the API, it can be helpful to consider the needs of downstream projects (and even provide patches to migrate them).
Here is a list:

- [GREASE](https://github.com/GaloisInc/grease)

## GHC versions

See the README for the GHC support policy.

### Adding a new version

The following checklist enumerates the steps needed to support a new version of GHC.
When performing such an upgrade, it can be helpful to copy/paste this list into the MR description and check off what has been done, so as to not forget anything.

- [ ] Allow the [new version of `base`][base] in the Cabal `build-depends`
- [ ] Run `cabal {build,test,haddock}`, bumping dependency bounds as needed
- [ ] Fix any new warnings from [`-Wdefault`][wdefault]
- [ ] Add the new GHC version to the matrix in the GitHub Actions configuration
- [ ] Change the `doc` job to use the new GHC version
- [ ] Optionally follow the below steps to remove any old GHC versions

[base]: https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/libraries/version-history
[wdefault]: https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag-Wdefault

### Removing an old version

- [ ] Remove the old version from the matrix in the GitHub Actions configuration
- [ ] Remove outdated CPP `ifdef`s that refer to the dropped version
- [ ] Remove outdated `if` stanzas in the Cabal file
- [ ] Bump the [lower bound on `base`][base] in the Cabal `build-depends`

