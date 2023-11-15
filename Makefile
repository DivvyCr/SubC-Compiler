# See: https://www.cs.colby.edu/maxwell/courses/tutorials/maketutor/
# See: https://makefiletutorial.com/

CXX=clang++ -std=c++17

CFLAGS= -g -O3 `llvm-config-17 --cppflags --ldflags --system-libs --libs all` \
				-Wno-unused-function -Wno-unknown-warning-option -fno-rtti

OFLAGS= -g -O3 `llvm-config-17 --cppflags`

DEPS = abstract-syntax.h lexer.h parser.h visitor.h

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

mccomp: mccomp.cpp lexer.cpp parser.cpp printer.cpp code-generator.cpp $(DEPS)
	$(CXX) mccomp.cpp lexer.cpp parser.cpp printer.cpp code-generator.cpp $(CFLAGS) -o mccomp

minic: mccomp.o parser.o lexer.o printer.o code-generator.o
	$(CXX) mccomp.o parser.o lexer.o printer.o code-generator.o $(CFLAGS) -o out

clean:
	rm -rf mccomp out
	rm -rf mccomp.o parser.o lexer.o abstract-syntax.o code-generator.o printer.o
