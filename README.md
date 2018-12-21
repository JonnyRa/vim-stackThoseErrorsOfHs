# vim-stackThoseErrorsOfHs
A few bits and pieces to scrape errors from stack logs into vim

# dependencies
this plugin needs a few things installed to work properly:

# inotify-tools
To see what things are getting built it watches stack's log files.  This uses a utility called inotifywait.

to install run the following (assuming ubuntu or similar):
```
sudo apt install inotify-tools
```
That's it!
