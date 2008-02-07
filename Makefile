default : dist/setup-config
	runghc Setup build

dist/setup-config :
	runghc Setup configure -f-gtk --user

configure : dist/setup-config


clean :
	runghc Setup clean
