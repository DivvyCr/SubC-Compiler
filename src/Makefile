CXX=clang++ -std=c++17

CFLAGS= -g -O3 `llvm-config --cppflags --ldflags --system-libs --libs all` \
				-Wno-unused-function -Wno-unknown-warning-option -fno-rtti

DEPS = abstract-syntax.h lexer.h parser.h visitor.h

subc: main.cpp lexer.cpp parser.cpp printer.cpp code-generator.cpp $(DEPS)
	$(CXX) main.cpp lexer.cpp parser.cpp printer.cpp code-generator.cpp $(CFLAGS) -o subc

clean:
	rm subc
