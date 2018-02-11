
all: daffy-server

.PHONY: daffy-elm-codegen
daffy-elm-codegen:
	@cabal new-build \
		--disable-executable-stripping \
		--ghc-options="-freverse-errors -Wall -Widentities -Wincomplete-record-updates -fprint-expanded-synonyms -fprint-explicit-foralls -fprint-explicit-kinds -fprint-potential-instances -H16m" \
		-j \
		-O0 \
		exe:daffy-elm-codegen

DaffyTypes.elm: daffy-elm-codegen
	mkdir -p daffy-client/codegen
	@cabal new-run \
		--ghc-options="-freverse-errors -Wall -Widentities -Wincomplete-record-updates -fprint-expanded-synonyms -fprint-explicit-foralls -fprint-explicit-kinds -fprint-potential-instances -H16m" \
		-j \
		-O0 \
		-v0 \
		exe:daffy-elm-codegen > daffy-client/codegen/DaffyTypes.elm

daffy.js: DaffyTypes.elm
	@elm-make daffy-client/src/Main.elm --output=daffy-server/codegen/daffy.js

daffy-server: daffy.js
	@cabal new-build \
		--disable-executable-stripping \
		--ghc-options="-freverse-errors -Wall -Widentities -Wincomplete-record-updates -fprint-expanded-synonyms -fprint-explicit-foralls -fprint-explicit-kinds -fprint-potential-instances -H16m" \
		-j \
		-O0 \
		exe:daffy
