TARGET := scope

all:
	raco exe -o $(TARGET) main.rkt

run:
	racket main.rkt
