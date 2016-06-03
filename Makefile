all: c_src/inotify.so

c_src/inotify.so: c_src/inotify.c
  cc -fPIC -I$(ERL_INCLUDE_PATH) -dynamiclib -undefined dynamic_lookup -o c_src/inotify.so inotify.c