gcc -o skyfly database.o fly.o gm_main.o image.o perfdraw.o skyfly.o -L$HOME/OpenGL/lib -lglut -lGLU -lGL -L/usr/X11/lib -lXm -lXt -lX11 -lXext -lXmu -lm
