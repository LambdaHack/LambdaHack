default : dist/setup-config
	runghc Setup build

dist/setup-config : LambdaHack.cabal
	runghc Setup configure -f-gtk --user

vty :
	runghc Setup configure -f-gtk --user

gtk :
	runghc Setup configure --user

clean :
	runghc Setup clean
