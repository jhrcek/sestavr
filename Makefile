script=dist/main.js

front:
	cd client && elm make src/Main.elm --debug --output=$(script)

mini: front
	uglifyjs client/$(script) --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output=client/$(script)

run: mini
	stack run

clean:
	rm client/$(script)
	stack clean
