script=dist/main.js

dev-server: front
	stack build --flag sestavr:dev && \
	stack exec sestavr-exe  -- --images-directory ~/Dropbox/Share/Eva/yoga_images/
	
dev-client:
	cd client && elm-live src/Main.elm -- --debug --output=$(script)

front:
	cd client && elm make src/Main.elm --optimize --output=$(script)

mini: front
	uglifyjs client/$(script) --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output client/$(script)


install: mini
	stack install --pedantic

review:
	cd client && elm-review
	
clean:
	rm client/$(script)
	rm client/elm-stuff -rf
	stack clean

format:
	git ls-files '*.hs' | xargs fourmolu --mode inplace
	git ls-files '*.elm' | xargs elm-format --yes
	cabal-fmt -i sestavr.cabal

update_db:
	rm -f sestavr.db && sqlite3 sestavr.db < ~/Dropbox/Share/Eva/sestavr.sql
