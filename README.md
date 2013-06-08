A bot that emails me useful information daily. GPLv2.

### Building

Ensure cabal-dev is installed, and on your path:

    $ export PATH=$PATH:"~/.cabal/bin"

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

    $ cabal-dev install

#### Dependencies

I'm running this on WebFaction.

1: [Install GHC](http://community.webfaction.com/questions/5084/building-ghc-from-source). I
used 7.4.

2: [Install cabal-install](http://community.webfaction.com/questions/5098/installing-yesod-on-a-shared-account)

3: Install
   [curl from hackage](http://hackage.haskell.org/package/curl)
   manually.

    wget "http://hackage.haskell.org/packages/archive/curl/1.3.7/curl-1.3.7.tar.gz"
    cd curl-1.3.7.tar.gz 
    ghc --make Setup.hs 
    ./Setup configure --user
    ./Setup build
    ./Setup install

4: Install the remaining dependencies with cabal.

    $ cabal install smtp-mail
    $ cabal install json

TODO: use ButlerBot.cabal and set explicit version numbers.

5: Set up a cron job.
