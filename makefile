TARGET := scope

run:
	racket -tm main.rkt

all:
	raco exe -o $(TARGET) main.rkt

