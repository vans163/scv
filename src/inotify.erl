-module(inotify).
-on_load(load_nif/0).

-export([init/0, add_watch/3, rm_watch/2, read/1]).

load_nif() -> 
    Path = [_ | _] = code:priv_dir(scv),
    FullPath = filename:join([Path, "scv_drv"]),
    erlang:load_nif(FullPath, 0).
    
init() -> "NIF library not loaded".
add_watch(_,_,_) -> "NIF library not loaded".
rm_watch(_,_) -> "NIF library not loaded".
read(_) -> "NIF library not loaded".