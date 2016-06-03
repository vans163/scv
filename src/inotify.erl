-module(inotify).
-on_load(load_nif/0).

-export([init/0, add_watch/3, rm_watch/2, read/1]).

load_nif() -> erlang:load_nif("./c_src/inotify", 0).

init() -> "NIF library not loaded".
add_watch(_,_,_) -> "NIF library not loaded".
rm_watch(_,_) -> "NIF library not loaded".
read(_) -> "NIF library not loaded".