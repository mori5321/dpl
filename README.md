# dpl
## Installation
### With homebrew
WIP...

## Set up envs
``` sh
export DPL_API_KEY=<Your DeepL API Key>
export DPL_EDITOR=<Your Editor> // ex: vim, nvim, code(VSCode)
```

## Usage/Interface (WIP)
### Simple Mode
```
dpl こんにちは --to "EN"
dpl Hello World --to "JA"
```

`--to/-t`: Specifies language which you want to translate into. 
<br />
`--from/-f`: Specifies language which you want to translate from.

### Editor Mode
```
dpl --editor --to "EN"
```

`--editor/-e`: With this option, dpl opens your editor (seeing env $DPL_EDITOR or $EDITOR). When you edit and save the editor buffer, dpl runs a translation request to DeepL API. \n
