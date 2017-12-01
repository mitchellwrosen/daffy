ghcid:
	ghcid \
		-c 'stack ghci daffy:lib --ghci-options=-fno-code' \
		--restart package.yaml \
		--restart stack.yaml
