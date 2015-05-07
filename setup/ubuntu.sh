#!/bin/bash

set -e

EXTRA_APPS=(
    emacs
    mosh
)

if [ "$UID" != 0 ]; then
    echo Please run as root.
    exit
fi


echo Asking for config
[ -z "$user"     ] && read -e -p "linux user: "     user
[ -z "$hostname" ] && read -e -p "host name: "      hostname
[ -z "$pubkey"   ] && read -e -p "ssh pubkey: "     pubkey
[ -z "$repo"     ] && read -e -p "git repository: " repo

home=/home/$user


echo Setting up hostname
echo "$hostname" > /etc/hostname
echo 127.0.0.1 "$hostname" > /etc/hosts


echo Setting up user
useradd -m -U -s /bin/bash "$user"
usermod -a -G sudo "$user"
chage -d 0 "$user"

ssh_dir=$home/.ssh
ssh-keygen -f $ssh_dir/id_rsa -t rsa -N ''
echo Please copy the public key printed below
echo "-------------------"
cat $ssh_dir/id_rsa.pub
echo "-------------------"
echo And add it to your github account, then press enter
read

userdo() {
    sudo -u "$user" "$@"
}


echo Configuring login
echo "$pubkey" > $ssh_dir/authorized_keys
chmod 600 $ssh_dir/authorized_keys
chown $user:$user $ssh_dir/authorized_keys


echo Disabling ssh password login
usermod -p $(openssl passwd -crypt $(openssl rand -base64 6)) root
echo <<EOF >> /etc/ssh/sshd_config
ChallengeResponseAuthentication no
PasswordAuthentication no
UsePAM no
EOF
service ssh restart

echo Installing packages
apt-get update
apt-get install -y curl docker git supervisor


echo Pulling git repository
cd $home && sudo -u "$user" git clone "$repo" poi && cd poi


echo Setting up supervisord
cp -f conf/supervisord.conf /etc/supervisor/supervisord.conf
echo "files = $(pwd)/gen/*.conf" \
     >> /etc/supervisor/supervisord.conf


echo Setting up poi
./poi --init .
./poi --test
