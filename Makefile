.PHONY: build deploy

build:
	./mvnw -B -U enforcer:display-info clean verify -Phelp

deploy:
	./mvnw -B -U deploy -P help,release-composite
