# How to install rbenv and jruby

# ubuntu: PROFILE=~/.profile

export PROFILE=~/.profile
export RBVER=jruby-1.7.9

test -d ~/.rbenv || git clone git@github.com:sstephenson/rbenv.git ~/.rbenv
# modify $PATH and autoload rbenv
grep 'rbenv/bin' $PROFILE &>/dev/null || echo 'export PATH="$HOME/.rbenv/bin:$PATH"' >> $PROFILE
grep 'rbenv init' $PROFILE &>/dev/null || echo 'eval "$(rbenv init -)"' >> $PROFILE
mkdir -p ~/.rbenv/plugins
# reload shell
source $PROFILE
 
### Install ruby-build
test -d ~/.rbenv/plugins/ruby-build || git clone git@github.com:sstephenson/ruby-build.git ~/.rbenv/plugins/ruby-build
 
### Install Ruby
rbenv install $RBVER --with-openssl-dir=/usr/local
# reload binaries
rbenv rehash
# set as default version
rbenv global $RBVER
 
ruby -v
 
# set some defaults
test -s ~/.gemrc || echo 'gem: --no-rdoc --no-ri' >> ~/.gemrc
echo 'Here is your ~/.gemrc:'
cat ~/.gemrc
echo '=== end of .gemrc ==='
 
# install gems
gem install bundler rake 
rbenv rehash

