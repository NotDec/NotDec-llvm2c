.PHONY: test clean cll cllc

# default target
test: cllc

CLANG = clang-14

# current dir should be test/sysy, and remember to create out_functional folder
current_dir = $(shell pwd)
LLVM2C := $(current_dir)/../../build/bin/notdec-llvm2c

# targets
FUNCTIONALDIR = functional
SOURCES = $(basename $(wildcard $(FUNCTIONALDIR)/*.c))
# eg: functional => out_functional
PREFIXEDS = $(addprefix out_, $(SOURCES))
cll: $(addsuffix .ll, $(PREFIXEDS))
cllc: $(addsuffix .dec.c, $(PREFIXEDS))

out_$(FUNCTIONALDIR)/%.ll: $(FUNCTIONALDIR)/%.c
	$(CLANG) -Xclang -disable-O0-optnone -I. -c -emit-llvm -S -o ./$@ ./$<

%.dec.c: %.ll
	$(LLVM2C) -i ./$< -o ./$@

clean:
	rm -rf out_functional && mkdir out_functional
