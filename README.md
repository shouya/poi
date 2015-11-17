# poi

**(IN DEV)** Personal VPS automatic deployment system desu poi.

## What you need to have

* A fresh new Linux VPS (Ubuntu 14.04 LTS recommended)
* A public IP with A/CNAME record points to that VPS
* Full access to a github repository (with all your services configured in docker)


## Aim

* [x] Lightweight daemon
* [ ] Service monitoring and auto restarting (with supervisord)
* [x] Instant service deployment (with Github hook)
* [ ] Email feedback with full log (with Mailgun, optional)
* [ ] Plugin support
* [ ] Fully dockerized apps for portability
* [ ] Re-deploy all services on a second server within 10 mins

## Prepare the environment

Get a brand new VPS with Ubuntu 14.04 LTS installed.

Run the follow command and follow the instruction prompted to set up the basic environment:

```
wget xxxx/ubuntu-setup.sh
sudo sh ubuntu-setup.sh
```

Or,

```
curl xxxx/ubuntu-setup.sh | sh -
```

If you already have curl installed.

## Directory Structure

The setup script will generate the following directory structure:

```
/poi/
 +- poi
 +- vps/
    +- .git/*
    +- poi.conf
    +- docker-compose.yml
    +- services
    +- <your-service>/
       +- Dockerfile
       +- <other files>
```

## What the setup script does

* Download `poi` binary
* `git clone` your repo for VPS
* Build for the first time
* Start the services
* Start `poi`
