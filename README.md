<img src="http://i.imgur.com/AJLbnwu.jpg" width="400" height="400" />
# scv
Erlang /w Elixir, automated distributed build system.

### Point
SCV was made to give you a true REPL experience.  
Anytime you save a file all the changes you made get hotloaded into your local and all remote nodes.  
Now you have a space construction vehicle to go along with your space language :)

### Installation
run the Makefile to build the inotify NIF.

### Usage
```
scv:start().  %Default arguments

scv:start(#{
  source_paths => ["./src", "./lib"],
  beam_path => "./ebin",
  remote_compile => true,
  remote_nodes => [],
}).
```

### Known bugs
Makefile - assumes your erlang include path is /usr/lib/erlang/usr/include/.  
inotify.c add_watch - does not accept a utf-8 binary yet because I am lazy. (it returns a utf-8 binary)  
inotify.c read - the cookie is returned as an int, should be uint. I am not sure how to return a uint/int64.  
scv.erl - not all start options are working yet.

### Note

SCV uses inotify so it will only work on linux for now.  
You are free to push another backend such as fsevents for OSX.  
  
SCV is not fully tested yet, this will be removed when it has.  
  
SCV is not meant to be 99.9999% stable as it is a development tool.  
SCV is not meant to be used in production.
