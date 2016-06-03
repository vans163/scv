<img src="http://i.imgur.com/AJLbnwu.jpg" width="400" height="400" />
# scv
Erlang /w Elixir, automated distributed build system.

Why: The only other library I found doing this in a simple way was Rotor, 
but it keeps crashing for me every few hours requiring hard restart.
Also it does not support remote deployment.

SCV uses inotify so it will only work on linux for now.
You are free to push another backend such as fsevents for OSX.


To install do:

> rebar get-deps
ignore the error

> cd deps/inoteefy 
> aclocal ; autoconf ; automake --add-missing ; ./configure ; make
> cd ../..