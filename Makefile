src = $(wildcard *.scala)
class = $(subst scala,class,$(src))

target = simple-rift-parser.jar
manifest = manifest.mf

all: $(target)

$(target): class
	jar -cfm $(target) $(manifest) $(class)

.PHONY: class
class: $(src)
	fsc $(src)
