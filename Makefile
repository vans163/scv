all: c_src/inotify.so

c_src/inotify.so: c_src/inotify.c
	gcc -fPIC -shared -I/usr/lib/erlang/usr/include/ -o c_src/inotify.so c_src/inotify.c
