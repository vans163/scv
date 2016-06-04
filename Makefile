all: c_src/inotify.so

c_src/inotify.so: c_src/inotify.c
	gcc -fPIC -shared -I/usr/lib/erlang/usr/include/ -o priv/inotify.so c_src/inotify.c
