#! /bin/bash

HOSTNAME=$1
MAIN_USER=$2
MAIN_PASSWORD=$3
ROOT_MAIL=$4
PUBKEY=$5

source ./linode_setup.sh $HOSTNAME $MAIN_USER $MAIN_PASSWORD $ROOT_MAIL $PUBKEY

###############################
#### erlide specific stuff

apt-get install -y build-essential make perl gcc sed m4 libncurses5-dev ncurses-dev libssl-dev autoconf
apt-get install -y git-core openjdk-7-jdk eclipse Xvfb rebar zip

Xvfb :1 &
export DISPLAY=:1
# create boot script
echo >> /etc/init.d/xvfb <<EOF
### BEGIN INIT INFO
# Provides: Xvfb
# Required-Start: $local_fs $remote_fs
# Required-Stop:
# X-Start-Before:
# Default-Start: 2 3 4 5
# Default-Stop: 0 1 6
# Short-Description: Loads X Virtual Frame Buffer
### END INIT INFO

XVFB=/usr/bin/Xvfb
XVFBARGS=":1 -screen 0 1024x768x24 -ac +extension GLX +render -noreset"
PIDFILE=/var/run/xvfb.pid
case "$1" in
	start)
		echo -n "Starting virtual X frame buffer: Xvfb"
		start-stop-daemon --start --quiet --pidfile $PIDFILE --make-pidfile --background --exec $XVFB -- $XVFBARGS
		echo "."
		;;
	stop)
		echo -n "Stopping virtual X frame buffer: Xvfb"
		start-stop-daemon --stop --quiet --pidfile $PIDFILE
		echo "."
		;;
	restart)
		$0 stop
		$0 start
		;;
	*)
				echo "Usage: /etc/init.d/xvfb {start|stop|restart}"
				exit 1
esac

exit 0
EOF
update.rc.d xvfb defaults

wget -q -O - http://pkg.jenkins-ci.org/debian-stable/jenkins-ci.org.key | sudo apt-key add -
echo "deb http://pkg.jenkins-ci.org/debian-stable binary/" >> /etc/apt/sources.list
apt-get update
apt-get install -y jenkins

###

### this is when stuff exists...

JENKINS=/home/jenkins
mkdir -p $JENKINS
usermod -d $JENKINS -m jenkins
usermod -g www-data jenkins
cd $JENKINS

HTTP_PORT=52825

sed -i- -e "s/HTTP_PORT=.*/HTTP_PORT=$HTTP_PORT/" /etc/default/jenkins
sed -i- -e "s#JENKINS_HOME=.*#JENKINS_HOME=$JENKINS#" /etc/default/jenkins
sed -i- -e 's/#JAVA_ARGS="-Xmx256m"/JAVA_ARGS="-Xmx384m -XX:MaxPermSize=256m"/' /etc/default/jenkins
service jenkins restart

GIT_USER=vladdu
GIT_BRANCH=pu

rm setup_tools.sh*
wget https://raw.githubusercontent.com/$GIT_USER/erlide/$GIT_BRANCH/org.erlide.releng/setup_tools.sh
chmod u+x setup_tools.sh
#./setup_tools.sh $JENKINS/erlide_tools

wget localhost:$HTTP_PORT/jnlpJars/jenkins-cli.jar
# java -jar jenkins-cli.jar -s http://localhost:$HTTP_PORT

# jenkins configuration: security, plugins and jobs
# restore from backup ?!

chown -R jenkins:www-data $JENKINS

##

apache_virtualhost "download.erlide.org"
service apache2 restart

if [ ! -f /etc/apache2/sites-available/ci.erlide.org.conf ]
	then
		cat > /etc/apache2/sites-available/ci.erlide.org.conf <<EOF
		<VirtualHost ci.erlide.org:80>
						ServerName ci.erlide.org

						ProxyRequests Off
						ProxyPreserveHost On
						ProxyPass / http://ci.erlide.org:$HTTP_PORT/
						ProxyPassReverse / http://ci.erlide.org:$HTTP_PORT/
		</VirtualHost>
EOF
	service apache2 reload
fi
service jenkins start

echo "Done!"
