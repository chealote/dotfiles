base_url=http://git.suckless.org
program=st
theme=dark

all: clean source diffs make

clean:
	test -d $(program) && rm -rf $(program) || echo "nope"

source:
	git clone $(base_url)/$(program)

diffs:
	cp *diff $(program)/
	for patchfile in $(ls *diff); do \
		patch -p1 < $$patchfile ; \
	done

make:
	cp ./config.dark.h ./$(program)/config.h
	cd $(program)/ ; make
	cd $(program) ; mv $(program) $(program).dark
	cp ./config.light.h $(program)/config.h
	cd $(program)/ ; make
	cd $(program) ; mv $(program) $(program).light

link:
	cd $(program) ; ln -s st.$(theme) $(program)
