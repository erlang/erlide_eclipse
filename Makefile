.PHONY: build publish publish-docs

DOCS_REPO?=erlide/erlide.github.io
DOCS_DIR=plugins/org.erlide.help/target/$(DOCS_REPO)

build:
	./mvnw -B -U enforcer:display-info clean verify -Phelp --fail-at-end

# Publish a release to update site https://erlide.org/update/
publish:
	./mvnw -B -U deploy -P help,release-composite

# Publish documentation to https://erlide.org
publish-docs:
	rm -rf $(DOCS_DIR)
	git clone --depth 1 git@github.com:$(DOCS_REPO) -b master $(DOCS_DIR)
	cp -r plugins/org.erlide.help/articles/* $(DOCS_DIR)/articles/
	cd $(DOCS_DIR) && \
		git add . && \
		git commit -a -m 'Publish erlide_eclipse docs' && \
		git push origin master
