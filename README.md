# poi

**(IN DEV)** Personal VPS automatic deployment system desu poi.

## What you need to have

* A public IP
* A Linux VPS with git, docker, docker-compose installed
* A github repository with your service bundle
* A [mailgun](http://www.mailgun.com) account with API (optional)

## Functionalities

* [x] Standalone binary
* [ ] Auto restart (with systemd)
* [ ] Update itself daily (with systemd)
* [x] Service Bundle setup from git repo
* [x] Generate initial service bundle
* [x] Auto re-deploy with push webhook
* [x] Full logs and failure handling
* [x] Email feedback for build result (with logs)
* [ ] __Service monitoring and auto restarting__ (not decided)

## Usage

### On your host

Initialize service bundle:

```
$ mkdir service-bundle && cd service-bundle
$ poi init
```

Push your service bundle:

```
$ git remote add origin <your-git-repo>
$ git push
```

### On your VPS

Install all required software, and make those binaries accessible in
`PATH`: `git`, `docker`, `docker-compose`.

If you are on an Ubuntu VPS, you can use the automatic setup script:

```
$ bash <(wget -O - https://raw.githubusercontent.com/shouya/poi/master/setup-ubuntu.sh)
```

Download the `poi` binary:

```
$ wget -O poi https://github.com/shouya/poi/releases/download/v1.0/poi
$ chmod +x poi
```

Use poi to setup the service bundle, you might need to set up SSH key
before checkout from the repo,

```
$ ./poi setup git@github.com:<xxx>/<your-service-bundle>
$ ./poi test # not implemented, skip this step
$ ./poi build
```

Start poi daemon,

```
$ ./poi daemon
```

Or if you want poi daemon to be automatically started, (with systemd) NOT IMPLEMENTED YET

```
$ ./poi systemd | sudo tee /etc/systemd/system/poi_daemon.service
$ systemctl start poi_daemon
$ systemctl enable poi_daemon
```

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
