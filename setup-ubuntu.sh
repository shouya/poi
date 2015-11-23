#!/bin/bash

echo Installing packages
apt-key adv --keyserver hkp://pgp.mit.edu:80 \
    --recv-keys 58118E89F3A912897C070ADBF76221572C52609D
codename=$(sh -c 'eval `cat /etc/lsb-release` && echo $DISTRIB_CODENAME')
echo deb https://apt.dockerproject.org/repo ubuntu-$codename main \
    >> /etc/apt/sources.list.d/docker.list

apt-get update
apt-cache policy docker-engine
apt-get install -y curl git docker-engine

docker_compose_version=1.4.2
curl -L "https://github.com/docker/compose/"`
       `"releases/download/$docker_compose_version/"`
       `"docker-compose-`uname -s`-`uname -m`" \
    > /usr/local/bin/docker-compose
chmod +x /usr/local/bin/docker-compose
curl -L  "https://raw.githubusercontent.com/docker/"`
        `"compose/$(docker-compose --version | awk 'NR==1{print $NF}')"`
        `"/contrib/completion/bash/docker-compose" \
    > /etc/bash_completion.d/docker-compose


mkdir -p ~/poi
cd ~/poi

wget -O poi https://github.com/shouya/poi/releases/download/v0.1/poi
chmod +x poi

echo Done.
