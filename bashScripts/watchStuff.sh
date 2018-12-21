#source this file in your .profile/.bashrc/whatever

setup-watches(){
    clear-logschanged
    ls -a | grep 'stack-work' | sed "s@\$@/logs@" | xargs --verbose inotifywait -m -o logschanged -e modify --format %w%f
}

clear-logschanged(){
    truncate logschanged --size 0     
}
