script=dist/main.js

front:
	cd client && elm make src/Main.elm --optimize --output=$(script)

mini: front
	uglifyjs client/$(script) --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output=client/$(script)

run: front
	stack run -- --images-directory /home/janhrcek/Dropbox/Share/Eva/yoga_images/

install: mini
	stack install --pedantic
clean:
	rm client/$(script)
	rm client/elm-stuff -rf
	stack clean
format:
	git ls-files '*.hs' | xargs ormolu --mode inplace
