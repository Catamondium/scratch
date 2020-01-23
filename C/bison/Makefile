# Makefile

FILES	= Lexer.c Parser.c Expression.c main.c
CC	= gcc
CFLAGS	= -g

a.out: $(FILES)
	$(CC) $(CFLAGS) $(FILES)
	./a.out

Lexer.c: Lexer.l
	flex Lexer.l

Parser.c: Parser.y Lexer.c
	bison Parser.y

clean:
	rm -f *.o *~ Lexer.c Lexer.h Parser.c Parser.h
