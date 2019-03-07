> Oh vim!  Stack those errors of hs, put them in a pile for all to see. Thus your pain will become less!
Anonymous 1702

# vim-stackThoseErrorsOfHs
A few bits and pieces to scrape errors from stack logs into vim.

This tool was built from the frustration of looking at errors from stack in one tmux pane and then navigating to them mechanically in vim by typing in the filename
and then pressing `[linenumber]G` and thinking 'there's got to be some way to automate this'!

The approach this tool takes is to read the logs produced by stack when it builds.  This makes it pretty flexible as there is a seperation between the command you use to build and the error reading. 
*The only important thing to know is that you need `--dump-logs` switched on in your builds*.  The documentation isn't really very clear but if you don't have this flag you don't get logs.

# known-issues
Stack doesn't output logs for the test part of a build in the same way as the rest, this currently means this tool doesn't work for tests.
I've logged an issue for this at https://github.com/commercialhaskell/stack/issues/3834 but so far haven't done anything about fixing it.

An alternative to improving stack would be to possibly use tee to get hold of the output more directly, I haven't fiddled around with this though and am not sure how it interacts with file-watch (which is my preferred way of running).  
I also don't know how you would know a particular build had ended (eg at what point to discard old output - otherwise there would be the danger of reporting errors/warnings for ever!).

# installation
There are quite a few moving parts to the tool:

## dependencies
This now just depends on:

### inotify-tools
To see what things are getting built it watches stack's log files.  This uses a utility called inotifywait.

to install run the following (assuming ubuntu or similar):
```
sudo apt install inotify-tools
```
That's it!

## Installing some bash
This assumes that you have a `~/.local/bin` folder that is on your path.  As long as the read-errors script ends up on your path it doesn't matter where it goes.

if you use vim plug you can get the `read-errors` script installed automatically by adding the following to your .vimrc:

```
Plug 'JonnyRa/vim-stackThoseErrorsOfHs', { 'do': './install' }
let g:stackThoseErrorsCreateMappings = 1 
```

You then need to save and trigger installation using `:PlugInstall`

The second line there sets up the plugin's default bindings (see below)

this will make a link to `read-errors` in `~/.local/bin`

## sourcing some bash
aside from all of the above you also need to source `watchStuff.sh`.  This adds a couple of bash functions:

* `setup-watches` which watches logs in .stack-work folders in the current directory and another called
* `clear-logschanged` which truncates the file.  This is useful as using `rm logschanged` will break the watch mechanism and you'll have to start it again.

you should add a line like this to your .profile/.bashrc/whatever you use:

```
source [path]/bashScripts/watchStuff.sh
```

where path should be substituted with the path to this repo.

if you installed via *vim-plug* you will want to do the following:

```
source ~/.vim/plugged/vim-stackThoseErrorsOfHs/bashScripts/watchStuff.sh
```

you should now be able to access both `clear-logschanged` and `setup-watches` after resourcing your profile.

# mappings/usage

This plugin makes one make command `StackReadErrors`.  This will trigger the reading of the `logschanged` file and the production of the `.errors` file (done by the read-errors script).
It will also get vim to read the .errors file and then jump to the last error in the file, most of the time this will be the one you can see at the bottom of your build window.  You can retrigger this process repeatedly.

If you want to setup the plugin's default mapping you can do the following:

```
let g:stackThoseErrorsCreateMappings = 1
```

this will setup a call to the command when you press `<leader>re` the mnenomic for this is r[ead]e[rrors].  Leader defaults to `\` and basically represents vim's big open space for user defined key-mappings.  See `:help mapleader` for more details
Alternately you could just run the command directly or set up your own keybinding.

From time to time you might also want to manually trigger clearing the logschanged file - for instance if you are seeing warnings from some other package that got built as a dependency or similar.
to do this you can run the bash function `clear-logschanged` from the commandline.  This doesn't actually do anything to the log files and is purely used by this plugin to track what log files it has seen change which it then greps through to find errors.

## vim stuff
The plugin just piggybacks off an already existent vim feature known as the quickfix window (see `:h quickfix`).   You shouldn't need to edit any quickfix related settings (like errorformat) for this to work.

There are a load of quickfix related commands which are all prefixed with `c` for some reason (possibly this is for compiler?)!  A useful on is `:copen` which shows the quickfix window.  You can jump to errors
by selecting them in there and pressing enter and you can also do the same thing remotely (without being in or seeing the window) by using commands like `:cn[ext] :cN[ext] (moves backwards) :cla[st]` etc.
Looking at help for any of those will drop you in the right place to see the rest of the commands.

# configuration
In order for the plugin to work you'll need to run the bash function `setup-watches` in the working directory for your project.  Generally this will be the repository root/wherever you run stack from.
The different parts of the plugin just communicate through the file system so you can have multiple different vim's/projects open and things should _just work_, however all the scripts just operate locally so you'll need to set your `:pwd` appropriately using `:cd` or `:lcd`.

setup watches expects all your stack work directories to have names beginning `.stack-work`, if they don't fit this pattern it won't watch them.  If you don't specify `--work-dir` in your builds then you'll be fine - the default is `.stack-work`

If you use tmux you can just add setup-watches to your startup script (in the appropriate folder(s)) and then forget about it!

# debugging
If anything doesn't work please raise issues on here!  Debugging the output mostly involves looking at the `.errors` and `logschanged` files and then poking around in the logs that are pointed to in the file.
