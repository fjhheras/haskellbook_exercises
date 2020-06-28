Here I am back to using Arch repositories for haskell, instead of doing a stack install

The trick is to remember that libraries are only dynamic and thus need to be called with
`ghc -dynamic`
