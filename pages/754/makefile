# makefile for local CHAPTER
SHELL=/bin/sh

SRC= code-02.r code-03.r code-04.r code-05.r code-06.r homework1.r
OBJ= code-02.o code-03.o code-04.o code-05.o code-06.o homework1.o

# Use LOCAL_CFLAGS to add arguments for the C compiler
LOCAL_CFLAGS=
# Use LOCAL_CXXFLAGS to add arguments for the C++ compiler
LOCAL_CXXFLAGS=
# Use LOCAL_FFLAGS to add arguments for the FORTRAN compiler
LOCAL_FFLAGS=

# Use LOCAL_LIBS to add arguments or additional libraries to the linker
# LOCAL_LIBS="-lM77 -lF77 -lsunmath_mt"
LOCAL_LIBS=


include $(SHOME)/library/S_FLAGS

S.so:  $(OBJ) 
	$(SHOME)/cmd/Splus5 LIBRARY S.so  $(OBJ) $(LOCAL_LIBS)

dump:
	@if test -d ./.Data; then S dumpChapter $(SRC); fi

boot:
	@if test -s all.Sdata; 	  then \\
		(BOOTING_S="TRUE" export BOOTING_S;  \\
		echo "terminate\(should have been booting S\)"| $(SHOME)/S) ; \\
	fi

clean:
	-rm $(OBJ)

