SOURCE=play.pl
TARGET=play

all: $(TARGET)

$(TARGET): $(SOURCE)
	swipl -o $@ -O -c $<

clean:
	@rm -Rf a.out $(TARGET)
