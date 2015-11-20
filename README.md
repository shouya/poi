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
* [ ] Generate initial service bundle
* [x] Auto re-deploy with push webhook
* [x] Full logs and failure handling
* [ ] Email feedback for build result (with logs)
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
$ bash <(wget -O - <TODO>)
```

Download the `poi` binary:

```
$ wget -O poi <TODO>/poi
$ chmod +x poi
```

Use poi to setup the service bundle,

```
$ ./poi setup https://github.com/<xxx>/<your-service-bundle>
$ ./poi test
$ ./poi build
```

Start poi daemon,

```
$ ./poi daemon
```

Or if you want poi daemon to be automatically started, (with systemd)

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
 +- service-bundle/
    +- .git/*
    +- poi.conf
    +- docker-compose.yml
    +- services
    +- <your-service>/
       +- Dockerfile
       +- <other files>
```
