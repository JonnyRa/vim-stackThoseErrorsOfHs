# vim-stackThoseErrorsOfHs
A few bits and pieces to scrape errors from stack logs into vim

# dependencies
this plugin needs a few things installed to work properly:

## inotify-tools
To see what things are getting built it watches stack's log files.  This uses a utility called inotifywait.

to install run the following (assuming ubuntu or similar):
```
sudo apt install inotify-tools
```
That's it!

## ansifilter
Stack logs get nice colours put into them to make easier to read.  This is achieved
by putting ansi colour codes into the text.  This is great when you're viewing them
in the terminal but when you want to view the raw text you'll see loads of garbage escape sequences.

To remove these from the output you see in vim you need to install ansifilter...

Here's how:

this is going to pull some source so make run it in a directory where you don't mind adding some files.
The repos have actually moved over onto gitlab now but for simplicity I'm going to refer to the stale ones on github.

This assumes you have git installed.

```
git pull https://github.com/andre-simon/ansifilter
cd ansifilter
sudo make install
```

You can verify this has worked by running `which ansifilter`.  This should now be available on your commandline

# Installing some bash
This assumes that you have a ~/.local/bin folder that is on your path.  As long as the read-errors script ends up on your path it doesn't matter where it goes.

if you use vim plug you can get the `read-errors` script installed automatically by adding the following to your .vimrc:

```
Plug 'JonnyRa/vimstackThoseErrorsOfHs', { 'do': './install' }
```

this will make a link to `read-errors` in `~/.local/bin`

# sourcing some bash
aside from all of the above you also need to source `watchStuff.sh` this adds a couple of bash functions
one called `setup-watches` which watches logs in .stack-work folders in the current directory and another called
clear-logschanged which truncates the file.  This is useful as using rm will break the watch mechanism and you'll have to start it again.

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
