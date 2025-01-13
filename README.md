# F1 Pools

To develop run the following in one terminal:

```
nix develop
ghcid --setup=F1Pools.DevelMain.update --command "stack repl f1pools" --run=F1Pools.DevelMain.update --warnings --restart ./f1pools.cabal
```

If you'd like to develop the UI run the following in another terminal:

```
nix develop
bun install
bun run tailwindcss -i src/input.css -o dist/output.css --watch
```

## TODO

- Setup `just` or some other command runner to ease development
- Add Teams rather than using free-form text
- Add endpoint to edit drivers
- Add endpoint to edit races
- Consolidate styles and lean into DaisyUI
- Start working on submitting picks for a race
- Add Servant Auth so people can log in
- Add `dbmate` or some other system to setup/maintain the db schema
- Figure out deploy to Fly, as well as a Postgres instance in Fly
- Add much better errors (probably need to do some `htmx` stuff)
