shake: build
	ghc shake.hs -outputdir build -o shake

build:
	mkdir build

clean:
	rm build/*.o
	rm build/*.hi
	rm shake
