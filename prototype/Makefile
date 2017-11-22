OBJ		:= main.o game.o field.o window.o io.o
CFLAGS	+= $(shell sdl-config --clfags)
LDFLAGS	+= $(shell sdl-config --libs)

betegeld: $(OBJ)
	$(CC) $^ $(CFLAGS)  $(LDFLAGS) -o $@

main.o: main.c game.h window.h
	$(CC) -c $<

game.o: game.c game.h field.h window.h io.h types.h defaults.h
	$(CC) -c $<

field.o: field.c field.h window.h types.h defaults.h
	$(CC) -c $<

window.o: window.c window.h io.h types.h defaults.h
	$(CC) -c $<

io.o: io.c io.h types.h
	$(CC) -c $<

.PHONY: clean

clean:
	$(RM) $(OBJ)


