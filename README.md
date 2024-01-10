> Oh vim!  Stack those errors of hs, put them in a pile for all to see. Thus your pain will become less!
Anonymous 1702

# vim-stackThoseErrorsOfHs
A few bits and pieces to scrape errors from stack logs into vim.

This tool was built from the frustration of looking at errors from stack in one tmux pane and then navigating to them mechanically in vim by typing in the filename
and then pressing `[linenumber]G` and thinking 'there's got to be some way to automate this'!

The tool is composed of 3 parts
* a stack error parser written in haskell
* a bash script which triggers the parser expecting a file called errorLog and puts its output in a file called .errors
* a vimscript plugin which triggers the bash script, and then this just calls :cfile on .errors

In order for the plugin to work you need to modify the way you run stack to dump the build output to a file called errorLog
  * run stack like this `stack build ... --colour always 2>&1 | tee errorLog`

# installation

## dependencies
This tool depends on ansifilter which you can install using `sudo apt-get install ansifilter` on ubuntu
It also depends on stack to build the parser... although there's not much point installing the tool if you don't already have that!

## Installing some bash & the haskell parser
This assumes that you have a `~/.local/bin` folder that is on your path.  As long as the read-errors script & parser ends up on your path it doesn't matter where it goes.

if you use vim plug you can get the `read-errors` script installed automatically by adding the following to your .vimrc:

```
Plug 'JonnyRa/vim-stackThoseErrorsOfHs', { 'do': './install' }
let g:stackThoseErrorsCreateMappings = 1 
```

You then need to save and trigger installation using `:PlugInstall`

The second line there sets up the plugin's default bindings (see below)

this will make a link to `read-errors` in `~/.local/bin`
it will also run `stack install scraper` which makes the executable `stack-error-scraper`

# mappings/usage

This plugin makes one make command `StackReadErrors`.  This will trigger the reading of the `errorLog` file and the production of the `.errors` file (done by the read-errors script).
It will also get vim to read the .errors file and then jump to the last error in the file, most of the time this will be the one you can see at the bottom of your build window.  You can retrigger this process repeatedly.

If you want to setup the plugin's default mapping you can do the following:

```
let g:stackThoseErrorsCreateMappings = 1
```

this will setup a call to the command when you press `<leader>re` the mnenomic for this is r[ead]e[rrors].  Leader defaults to `\` and basically represents vim's big open space for user defined key-mappings.  See `:help mapleader` for more details
Alternately you could just run the command directly or set up your own keybinding.

## vim stuff
The plugin just piggybacks off an already existent vim feature known as the quickfix window (see `:h quickfix`).   You shouldn't need to edit any quickfix related settings (like errorformat) for this to work.

There are a load of quickfix related commands which are all prefixed with `c` for some reason (possibly this is for compiler?)!  A useful on is `:copen` which shows the quickfix window.  You can jump to errors
by selecting them in there and pressing enter and you can also do the same thing remotely (without being in or seeing the window) by using commands like `:cn[ext] :cN[ext] (moves backwards) :cla[st]` etc.
Looking at help for any of those will drop you in the right place to see the rest of the commands.

# configuration
The different parts of the plugin just communicate through the file system so you can have multiple different vim's/projects open and things should _just work_, however all the scripts just operate locally so you'll need to set your `:pwd` appropriately using `:cd` or `:lcd`.

# debugging
If anything doesn't work please raise issues on here!
