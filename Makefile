all: compiler

compiler:
	stack_haskell install --local-bin-path=$(shell pwd) || stack install --local-bin-path=$(shell pwd) || /home/students/inf/PUBLIC/MRJP/Stack/stack install --local-bin-path=$(shell pwd)
	mv lattec latc
	cp latc latc_x86

clean:
	stack_haskell clean || stack clean || /home/students/inf/PUBLIC/MRJP/Stack/stack clean
	rm latc -f
	rm latc_x86 -f
