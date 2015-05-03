# poi

**(IN DEV)** Personal VPS automatic deployment system desu poi.

## Aim

* [ ] Lightweight daemon
* [ ] Service monitoring and auto restarting (with supervisord)
* [ ] Instant service deployment (with Github hook)
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
