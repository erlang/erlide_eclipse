source 'https://rubygems.org'

require 'json'
require 'open-uri'
versions = JSON.parse(open('https://pages.github.com/versions.json').read)

#gem 'github-pages', versions['github-pages']
gem 'github-pages', group: :jekyll_plugins

gem 'html-proofer'
gem 'nokogiri', '>= 1.10.4'
