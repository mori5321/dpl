# dpl
## Usage/Interface (WIP)
### Simple Mode
```
dpl こんにちは --to "EN"
dpl Hello World --to "JA"
```

- `--to/-t`: specify language which you want to translate into.
- `--from/-f`: specify language which you want to translate from.

### Editor Mode
```
dpl --editor --to "EN"
```

- `--editor/-e`: With this option, dpl opens your editor (seeing env $EDITOR). When you edit and save the editor buffer, dpl make a request to deep.l
