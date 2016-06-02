#!/bin/bash -x

HOSTNAME=$1
MAIN_USER=$2
MAIN_PASSWORD=$3
ROOT_MAIL=$4
PUBKEY=$5

# assumptions
# OS: ubuntu 14.04

if [ -z "$HOSTNAME" -o -z "$MAIN_USER" -o -z "$MAIN_PASSWORD" -o -z "$ROOT_MAIL" ]
	then
		echo "Please use arguments HOSTNAME MAIN_USER MAIN_PASSWORD ROOT_MAIL [PUBKEY]"
		exit 1
fi

source ./linode_base.sh

system_update

####################

IPADDR=$(/sbin/ifconfig eth0 | awk '/inet / { print $2 }' | sed 's/addr://')
IPMASK=$(/sbin/ifconfig eth0 | awk '/inet / { print $4 }' | sed 's/Mask://')
IPGW=$(netstat -nr | awk '/^0.0.0.0/ { print $2 }')

system_set_hostname "$HOSTNAME"
system_add_host_entry "$IPADDR" "$HOSTNAME"

PRIV_MASK="255.255.128.0"
cat > /etc/network/interfaces <<EOF
# The loopback network interface
auto lo
iface lo inet loopback

auto eth0 #eth0:1

# public address
iface eth0 inet static
 address $IPADDR
 netmask $IPMASK
 gateway $IPGW

# private address
#iface eth0:1 inet static
# address $PRIV_IP
# netmask $PRIV_MASK

EOF

service networking restart
apt-get remove -y isc-dhcp-client dhcp3-client dhcpcd

mkdir /media/home
mkdir /media/www
mkdir /media/backups
# insert in /etc/fstab
sed -i -e '/\/dev\/xvdb/d' /etc/fstab
sed -i -e '/\/dev\/xvdc/d' /etc/fstab
sed -i -e '/\/dev\/xvdd/d' /etc/fstab
echo "/dev/xvdb       /media/home      ext3    defaults        0       0" >> /etc/fstab
echo "/dev/xvdc       /media/www      ext3    defaults        0       0" >> /etc/fstab
echo "/dev/xvdd       /media/backups      ext3    defaults        0       0" >> /etc/fstab
mount -a

#rm -rf /home
#ln -s /media/home /home
#rm -rf /var/www
#ln -s /media/www /var/www

user_add_sudo $MAIN_USER $MAIN_PASSWORD
user_add_pubkey $MAIN_USER "ssh-rsa AAAAB3NzaC1yc2EAAAABJQAAAIBlyHyRfA+4SykwkzRPjlSUAVMXUzvZ2zM/Kq7yMC6oAMZGVPbeCeK72H44z1Qpdtfw6tdFO5+XxDkkVDwDR8yHv7kTQUSPAkEfTWNHdSXx05dW/u/qckgzZhNoyRXO8aPUkbWkcMWccAKgemGnWnwxVDN4n1I94FYzsODa9uwpmQ== erlide@erlide.org"
user_add_pubkey $MAIN_USER "ssh-rsa AAAAB3NzaC1yc2EAAAABJQAAAIEAple7lvCjlK86/Wted8kx50H2B6pg3haI6uw+FqjANUgh08QZsvvDTIDXzaxIkUSXaiVEEvemZa525jyYeOFsv9i1VAOdfmSDakyWM8ByZCuphStVrMtNScaAXWbuje2nMlXrKfH7Ar1DMWvDpZ7ioEkh39sd06C2+c/nClDhabM= rsa-key-20091020"
if [ -n "$PUBKEY" ]
	then
		user_add_pubkey $MAIN_USER "$PUBKEY"
fi
sed -i -e 's/^#force_color_prompt/force_color_prompt/' /home/$MAIN_USER/.bashrc # enable the colorful bash prompt
chmod 700 /home/$MAIN_USER/.ssh
chmod 600 /home/$MAIN_USER/.ssh/authorized_keys

SSH_PORT=237
ssh_disable_root_and_passwords
sed -i -e "s/Port .*/Port $SSH_PORT/" /etc/ssh/sshd_config
touch /tmp/restart-ssh

groupadd admin
usermod -a -G admin $MAIN_USER
dpkg-statoverride --update --add root admin 4750 /bin/su

echo "Europe/Stockholm" > /etc/timezone
dpkg-reconfigure --frontend noninteractive tzdata

# restart server when out of memory
cat >> /etc/sysctl.conf <<EOF
vm.panic_on_oom=1
kernel.panic=10
EOF

apt-get -y install nmap ufw
nmap -v -sT localhost
ufw default deny incoming
ufw default allow outgoing
ufw logging on
ufw allow ssh/tcp
ufw allow http/tcp
ufw allow https/tcp
ufw allow out 25
ufw allow $SSH_PORT
ufw enable
ufw status

apt-get -y install fail2ban
cp /etc/fail2ban/jail.conf /etc/fail2ban/jail.local
sed -i -e "s/port[\t ]*=[\t ]*ssh/port = $SSH_PORT/" /etc/fail2ban/jail.local
sed -i -e "s/destemail = root@localhost/destemail = $ROOT_MAIL/" /etc/fail2ban/jail.local
sed -i -e 's/action = %(action_)s/action = %(action_mwl)s/' /etc/fail2ban/jail.local
# enable for more services? how to do it headlessly?
touch /tmp/restart-fail2ban

cat >> /etc/sysctl.conf <<EOF
# IP Spoofing protection
net.ipv4.conf.all.rp_filter = 1
net.ipv4.conf.default.rp_filter = 1

# Ignore ICMP broadcast requests
net.ipv4.icmp_echo_ignore_broadcasts = 1

# Disable source packet routing
net.ipv4.conf.all.accept_source_route = 0
net.ipv6.conf.all.accept_source_route = 0
net.ipv4.conf.default.accept_source_route = 0
net.ipv6.conf.default.accept_source_route = 0

# Ignore send redirects
net.ipv4.conf.all.send_redirects = 0
net.ipv4.conf.default.send_redirects = 0

# Block SYN attacks
net.ipv4.tcp_syncookies = 1
net.ipv4.tcp_max_syn_backlog = 2048
net.ipv4.tcp_synack_retries = 2
net.ipv4.tcp_syn_retries = 5

# Log Martians
net.ipv4.conf.all.log_martians = 1
net.ipv4.icmp_ignore_bogus_error_responses = 1

# Ignore ICMP redirects
net.ipv4.conf.all.accept_redirects = 0
net.ipv6.conf.all.accept_redirects = 0
net.ipv4.conf.default.accept_redirects = 0
net.ipv6.conf.default.accept_redirects = 0

# Ignore Directed pings
net.ipv4.icmp_echo_ignore_all = 1
EOF
sysctl -p

cat >> /etc/host.conf <<EOF
order bind,hosts
nospoof on
EOF

apt-get -y install exim4
cat > /etc/exim4/update-exim4.conf.conf <<EOF
# /etc/exim4/update-exim4.conf.conf
#
# Edit this file and /etc/mailname by hand and execute update-exim4.conf
# yourself or use 'dpkg-reconfigure exim4-config'
#
# Please note that this is _not_ a dpkg-conffile and that automatic changes
# to this file might happen. The code handling this will honor your local
# changes, so this is usually fine, but will break local schemes that mess
# around with multiple versions of the file.
#
# update-exim4.conf uses this file to determine variable values to generate
# exim configuration macros for the configuration file.
#
# Most settings found in here do have corresponding questions in the
# Debconf configuration, but not all of them.
#
# This is a Debian specific file

dc_eximconfig_configtype='internet'
dc_other_hostnames='erldev'
dc_local_interfaces='127.0.0.1 ; ::1'
dc_readhost=''
dc_relay_domains=''
dc_minimaldns='false'
dc_relay_nets=''
dc_smarthost=''
CFILEMODE='644'
dc_use_split_config='false'
dc_hide_mailname=''
dc_mailname_in_oh='true'
dc_localdelivery='mail_spool'
EOF
echo "erldev" > /etc/mailname
update-exim4.conf

cat >> /etc/aliases <<EOF
root: $ROOT_MAIL
EOF

apt-get install -y logwatch libdate-manip-perl
# crontab to send weekly? report

apt-get install -y psad

apt-get install -y unattended-upgrades


#######################

goodstuff

#rm -rf /etc/apache2
#mkdir -p /media/www/apache2
#ln -s /media/www/apache2 /etc/apache2

apache_install
apache_tune 30
apache_secure
a2enmod rewrite
a2enmod headers
a2enmod expires

php_install_with_apache
php_tune

mysql_install $MAIN_PASSWORD
mysql_tune 20

# todo: apache logging policy?

restartServices

###############################

HAS_LANG=$(grep "en_US.UTF-8" /home/$MAIN_USER/.bashrc)
if [ -z "$HAS_LANG" ]
    then
cat >> /home/$MAIN_USER/.bashrc <<EOF

    export LC_ALL=en_US.UTF-8
    export LANG=en_US.UTF-8
    export LANGUAGE=en_US.UTF-8
EOF
fi

