#! /bin/bash

function system_update {
	apt-get update
	apt-get -y install aptitude
	apt-get -y upgrade
}

function system_set_hostname {
	# $1 - The hostname to define
	HOSTNAME="$1"

	if [ ! -n "$HOSTNAME" ]; then
		echo "Hostname undefined"
		return 1;
	fi

	echo "$HOSTNAME" > /etc/hostname
	hostname -F /etc/hostname
}

function system_add_host_entry {
	# $1 - The IP address to set a hosts entry for
	# $2 - The FQDN to set to the IP
	IPADDR="$1"
	FQDN="$2"

	if [ -z "$IPADDR" -o -z "$FQDN" ]; then
		echo "IP address and/or FQDN Undefined"
		return 1;
	fi

	echo $IPADDR $FQDN  >> /etc/hosts
}

function user_add_sudo {
	# Installs sudo if needed and creates a user in the sudo group.
	#
	# $1 - Required - username
	# $2 - Required - password
	USERNAME="$1"
	USERPASS="$2"

	if [ ! -n "$USERNAME" ] || [ ! -n "$USERPASS" ]; then
		echo "No new username and/or password entered"
		return 1;
	fi

	aptitude -y install sudo
	adduser $USERNAME --disabled-password --gecos ""
	echo "$USERNAME:$USERPASS" | chpasswd
	usermod -aG sudo $USERNAME
}

function user_add_pubkey {
	# Adds the users public key to authorized_keys for the specified user. Make sure you wrap your input variables in double quotes, or the key may not load properly.
	#
	#
	# $1 - Required - username
	# $2 - Required - public key
	USERNAME="$1"
	USERPUBKEY="$2"

	if [ ! -n "$USERNAME" ] || [ ! -n "$USERPUBKEY" ]; then
		echo "Must provide a username and the location of a pubkey"
		return 1;
	fi

	if [ "$USERNAME" == "root" ]; then
		mkdir /root/.ssh
		echo "$USERPUBKEY" >> /root/.ssh/authorized_keys
		return 1;
	fi

	mkdir -p /home/$USERNAME/.ssh
	echo "$USERPUBKEY" >> /home/$USERNAME/.ssh/authorized_keys
	chown -R "$USERNAME":"$USERNAME" /home/$USERNAME/.ssh
}

function ssh_disable_root_and_passwords {
	# Disables root SSH access.
	sed -i 's/PermitRootLogin yes/PermitRootLogin no/' /etc/ssh/sshd_config
		sed -i 's/#PasswordAuthentication yes/PasswordAuthentication no/' /etc/ssh/sshd_config
	touch /tmp/restart-ssh
}

function goodstuff {
	# Installs the REAL vim, wget, less, and enables color root prompt and the "ll" list long alias

	aptitude -y install wget vim less
	sed -i -e 's/^#PS1=/PS1=/' /root/.bashrc # enable the colorful root bash prompt
	sed -i -e "s/^#alias ll='ls -l'/alias ll='ls -al'/" /root/.bashrc # enable ll list long alias <3
}

function restartServices {
	# restarts services that have a file in /tmp/needs-restart/

	for service in $(ls /tmp/restart-* | cut -d- -f2-10); do
		/etc/init.d/$service restart
		rm -f /tmp/restart-$service
	done
}

function randomString {
	if [ ! -n "$1" ];
		then LEN=20
		else LEN="$1"
	fi

	echo $(</dev/urandom tr -dc A-Za-z0-9 | head -c $LEN) # generate a random string
}

###########################################################
# Apache
###########################################################

function apache_install {
	# installs the system default apache2 MPM
	aptitude -y install apache2

	a2dissite 000-default # disable the interfering default virtualhost

	# any configuration here
}

function apache_tune {
	# Tunes Apache's memory to use the percentage of RAM you specify, defaulting to 40%

	# $1 - the percent of system memory to allocate towards Apache

	if [ ! -n "$1" ];
		then PERCENT=40
		else PERCENT="$1"
	fi

	aptitude -y install apache2-mpm-prefork
	PERPROCMEM=10 # the amount of memory in MB each apache process is likely to utilize
	MEM=$(grep MemTotal /proc/meminfo | awk '{ print int($2/1024) }') # how much memory in MB this system has
	MAXCLIENTS=$((MEM*PERCENT/100/PERPROCMEM)) # calculate MaxClients
	MAXCLIENTS=${MAXCLIENTS/.*} # cast to an integer
	sed -i -e "s/\(^[ \t]*MaxClients[ \t]*\)[0-9]*/\1$MAXCLIENTS/" /etc/apache2/apache2.conf

	touch /tmp/restart-apache2
}

function apache_virtualhost {
	# Configures a VirtualHost

	# $1 - required - the hostname of the virtualhost to create

	if [ ! -n "$1" ]; then
		echo "apache_virtualhost() requires the hostname as the first argument"
		return 1;
	fi

	if [ -e "/etc/apache2/sites-available/$1" ]; then
		echo "/etc/apache2/sites-available/$1 already exists"
		return;
	fi

	mkdir -p /srv/www/$1/public_html /srv/www/$1/logs

	cat >> /etc/apache2/sites-available/$1.conf <<EOF
	<VirtualHost $1:80>
		ServerName $1
		DocumentRoot /srv/www/$1/public_html/
		ErrorLog /srv/www/$1/logs/error.log
		CustomLog /srv/www/$1/logs/access.log combined
		<Directory /srv/www/$1/public_html/ >
			Options FollowSymLinks
			AllowOverride All
      Require all granted
		</Directory>
	</VirtualHost>
EOF

	a2ensite $1
	touch /tmp/restart-apache2
}

function apache_virtualhost_from_rdns {
	# Configures a VirtualHost using the rdns of the first IP as the ServerName

	apache_virtualhost $(get_rdns_primary_ip)
}


function apache_virtualhost_get_docroot {
	if [ ! -n "$1" ]; then
		echo "apache_virtualhost_get_docroot() requires the hostname as the first argument"
		return 1;
	fi

	if [ -e /etc/apache2/sites-available/$1 ];
		then echo $(awk '/DocumentRoot/ {print $2}' /etc/apache2/sites-available/$1 )
	fi
}

function apache_secure {
	cat >> /etc/apache2/conf-available/security.conf <<EOF
ServerTokens Prod
ServerSignature Off
TraceEnable Off
FileETag None
EOF

}

###########################################################
# PHP functions
###########################################################

function php_install_with_apache {
	aptitude -y install php5 php5-mysql libapache2-mod-php5
	touch /tmp/restart-apache2
}

function php_tune {
	# Tunes PHP to utilize up to 32M per process

	sed -i'-orig' 's/memory_limit = [0-9]\+M/memory_limit = 32M/' /etc/php5/apache2/php.ini
	touch /tmp/restart-apache2
}


###########################################################
# mysql-server
###########################################################

function mysql_install {
	# $1 - the mysql root password

	if [ ! -n "$1" ]; then
		echo "mysql_install() requires the root pass as its first argument"
		return 1;
	fi

	echo "mysql-server-5.1 mysql-server/root_password password $1" | debconf-set-selections
	echo "mysql-server-5.1 mysql-server/root_password_again password $1" | debconf-set-selections
	apt-get -y install mysql-server mysql-client

	echo "Sleeping while MySQL starts up for the first time..."
	sleep 5
}

function mysql_tune {
	# Tunes MySQL's memory usage to utilize the percentage of memory you specify, defaulting to 40%

	# $1 - the percent of system memory to allocate towards MySQL

	if [ ! -n "$1" ];
		then PERCENT=40
		else PERCENT="$1"
	fi

	sed -i -e 's/^#skip-innodb/skip-innodb/' /etc/mysql/my.cnf # disable innodb - saves about 100M

	MEM=$(awk '/MemTotal/ {print int($2/1024)}' /proc/meminfo) # how much memory in MB this system has
	MYMEM=$((MEM*PERCENT/100)) # how much memory we'd like to tune mysql with
	MYMEMCHUNKS=$((MYMEM/4)) # how many 4MB chunks we have to play with

	# mysql config options we want to set to the percentages in the second list, respectively
	OPTLIST=(key_buffer sort_buffer_size read_buffer_size read_rnd_buffer_size myisam_sort_buffer_size query_cache_size)
	DISTLIST=(75 1 1 1 5 15)

	for opt in ${OPTLIST[@]}; do
		sed -i -e "/\[mysqld\]/,/\[.*\]/s/^$opt/#$opt/" /etc/mysql/my.cnf
	done

	for i in ${!OPTLIST[*]}; do
		val=$(echo | awk "{print int((${DISTLIST[$i]} * $MYMEMCHUNKS/100))*4}")
		if [ $val -lt 4 ]
			then val=4
		fi
		config="${config}\n${OPTLIST[$i]} = ${val}M"
	done

	sed -i -e "s/\(\[mysqld\]\)/\1\n$config\n/" /etc/mysql/my.cnf

	touch /tmp/restart-mysql
}

function mysql_create_database {
	# $1 - the mysql root password
	# $2 - the db name to create

	if [ ! -n "$1" ]; then
		echo "mysql_create_database() requires the root pass as its first argument"
		return 1;
	fi
	if [ ! -n "$2" ]; then
		echo "mysql_create_database() requires the name of the database as the second argument"
		return 1;
	fi

	echo "CREATE DATABASE $2;" | mysql -u root -p$1
}

function mysql_create_user {
	# $1 - the mysql root password
	# $2 - the user to create
	# $3 - their password

	if [ ! -n "$1" ]; then
		echo "mysql_create_user() requires the root pass as its first argument"
		return 1;
	fi
	if [ ! -n "$2" ]; then
		echo "mysql_create_user() requires username as the second argument"
		return 1;
	fi
	if [ ! -n "$3" ]; then
		echo "mysql_create_user() requires a password as the third argument"
		return 1;
	fi

	echo "CREATE USER '$2'@'localhost' IDENTIFIED BY '$3';" | mysql -u root -p$1
}

function mysql_grant_user {
	# $1 - the mysql root password
	# $2 - the user to bestow privileges
	# $3 - the database

	if [ ! -n "$1" ]; then
		echo "mysql_create_user() requires the root pass as its first argument"
		return 1;
	fi
	if [ ! -n "$2" ]; then
		echo "mysql_create_user() requires username as the second argument"
		return 1;
	fi
	if [ ! -n "$3" ]; then
		echo "mysql_create_user() requires a database as the third argument"
		return 1;
	fi

	echo "GRANT ALL PRIVILEGES ON $3.* TO '$2'@'localhost';" | mysql -u root -p$1
	echo "FLUSH PRIVILEGES;" | mysql -u root -p$1

}

###########################################################
# Wordpress functions
###########################################################

function wordpress_install {
	# installs the latest wordpress tarball from wordpress.org

	# $1 - required - The existing virtualhost to install into

	if [ ! -n "$1" ]; then
		echo "wordpress_install() requires the vitualhost as its first argument"
		return 1;
	fi

	if [ ! -e /usr/bin/wget ]; then
		aptitude -y install wget
	fi

	VPATH=$(apache_virtualhost_get_docroot $1)

	if [ ! -n "$VPATH" ]; then
		echo "Could not determine DocumentRoot for $1"
		return 1;
	fi

	# download, extract, chown, and get our config file started
	cd $VPATH
	wget http://wordpress.org/latest.tar.gz
	tar xfz latest.tar.gz
	chown -R www-data: wordpress/
	cd $VPATH/wordpress
	cp wp-config-sample.php wp-config.php
	chown www-data wp-config.php
	chmod 640 wp-config.php

	# database configuration
	WPPASS=$(randomString 20)
	mysql_create_database "$DB_PASSWORD" wordpress
	mysql_create_user "$DB_PASSWORD" wordpress "$WPPASS"
	mysql_grant_user "$DB_PASSWORD" wordpress wordpress

	# configuration file updates
	for i in {1..4}
		do sed -i "0,/put your unique phrase here/s/put your unique phrase here/$(randomString 50)/" wp-config.php
	done

	sed -i 's/database_name_here/wordpress/' wp-config.php
	sed -i 's/username_here/wordpress/' wp-config.php
	sed -i "s/password_here/$WPPASS/" wp-config.php

	# http://downloads.wordpress.org/plugin/wp-super-cache.0.9.8.zip
}

