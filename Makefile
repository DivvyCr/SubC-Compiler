# See: https://www.cs.colby.edu/maxwell/courses/tutorials/maketutor/
# See: https://makefiletutorial.com/

CXX=clang++ -std=c++17

CFLAGS= -g `llvm-config-17 --cppflags --ldflags --system-libs --libs all`

# NOTE: Use -O3 preferably:
OFLAGS= -g -O0 `llvm-config-17 --cppflags`

DEPS = abstract-syntax.h lexer.h parser.h

# NOTE: compile into (unlinked) object code first, in order to 
# only recompile changed files (existing links are preserved).
%.o: %.cpp $(DEPS)
	$(CXX) -c $< $(OFLAGS) -o $@
#  % acts as a wildcard to capture NAME.cpp files and create NAME.o
# $@ refers to the NAME.o file
# $< refers to the NAME.cpp file (specifically, the first dependency)

# TARGET: DEP1 DEP2 ...
#     COMMAND1
#     COMMAND2
#     ...
minic: minic.o parser.o lexer.o printer.o code-generator.o
	$(CXX) minic.o parser.o lexer.o printer.o code-generator.o $(CFLAGS) -o out

clean:
	rm -rf minic.o parser.o lexer.o abstract-syntax.o code-generator.o printer.o out
