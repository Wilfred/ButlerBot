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
