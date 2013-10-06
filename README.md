A bot that emails me useful information daily. GPLv2.

### Building

Ubuntu assumed.

You need Haskell installed:

    $ sudo apt-get install ghc cabal-install

You need cURL installed for HTTPS GET:

    $ sudo apt-get install libcurl4-gnutls-dev

You also need sendmail:

    $ sudo apt-get install sendmail
    # you can test with:
    $ sendmail root@localhost
    Subject: test

    testing testing^D
    # then to see the sent message:
    $ sudo less /var/mail/root

Install ButlerBot:

    $ cd /path/to/butlerbot_source
    $ cabal install

On production, just set up a cron job:

    $ crontab -e

For example:

    $ crontab -l
    # send weather forecast emails
    0 6 * * * ~/.cabal/bin/butler-bot "api-key" "wilfred@example.com"
