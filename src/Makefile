SOURCE=main.pl
TARGET=main

all: $(TARGET)

$(TARGET): $(SOURCE)
	swipl -o $@ -O -c $<

clean:
	@rm -Rf a.out $(TARGET)
