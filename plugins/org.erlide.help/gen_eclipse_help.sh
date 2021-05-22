#! /bin/bash

rm -rf _site
mkdir _site

gem install bundler --user-install
bundle install --jobs 3 --path ${HOME}/.gem
bundle exec jekyll build

destination=contents

rm -rf ${destination}

cp -r _site/articles/eclipse ${destination}
find ${destination} -name "*.html" -exec sed -i -e 's|"/articles/eclipse/|"|g' {} \;
