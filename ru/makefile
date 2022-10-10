#OBJS specifies which files to compile as part of the project 
MD = ch01.md 
 
%.md : ../work/%.po
	po2txt -i ../work/$*.po -o $*.md


#CC specifies which compiler we're using 
CC = g++ 

#COMPILER_FLAGS specifies the additional compilation options we're using 
# -w suppresses all warnings 
COMPILER_FLAGS = -w `pkg-config --cflags sdl2`

#LINKER_FLAGS specifies the libraries we're linking against 
LINKER_FLAGS = -lSDL2 

#OBJ_NAME specifies the name of our exectuable 
OBJ_NAME = 01_hello_SDL 

#This is the target that compiles our executable 

#all : $(OBJS) 
#	$(CC) $(OBJS) $(COMPILER_FLAGS) $(LINKER_FLAGS) -o $(OBJ_NAME)

#CC=gcc
#LDLIBS=`gnome-config --libs gnomeui`
#CFLAGS=-Wall -g `gnome-config --cflags gnomeui`
#LDLIBS=`gnome-config --libs gnomeui`
#CFLAGS=-Wall -g -I /usr/include/glib-2.0
#	bash -c "echo 1"

#make 1.o
#bash -c "echo 1.c 1.c 1.o 1"
#1.c 1.c 1.o 1
#1.o: 1.c
#	bash -c "echo $< $? $@ $*"


