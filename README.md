A bot that emails me useful information daily. GPLv2.

### Building

Ubuntu assumed.

You need Haskell installed:

    $ sudo apt-get install ghc cabal-install

You need cURL installed for HTTPS GET:

    $ sudo apt-get install libcurl4-gnutls-dev

Install ButlerBot:

    $ cd /path/to/butlerbot_source
    $ cabal sandbox init
    $ cabal install

On production, just set up a cron job:

    $ crontab -e

For example:

    $ crontab -l
    # send weather forecast emails
    0 6 * * * ~/src/butlerbot/.cabal-sandbox/bin/butler-bot "forecast-io-key" "mailgun-key" "wilfred@example.com"
