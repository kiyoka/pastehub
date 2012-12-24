How to install PasteHub client software ( Alpha service )
=======================

# Account Registration

## Send your email to kiyoka@sumibi.org

  example of request mail:

    subject: PasteHub registration request

    hello, ...


## PasteHub.net will regist your account and reply account information to you.

   example of replay mail:

    Thank you for requesting account registration,
    This is your PasteHub.net(alpha service) account information.

    email:     xxxxx@xxxxx.xxx
    signiture: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX==


# Install client service software

## Linux

### Required platform

- CentOS 6.x   (32bit/64bit)
- Debian 6.x   (32bit/64bit)
- Ubuntu 11_10 or later (32bit/64bit)

### installation instruction


- CentOS

   download rpm and install by root account

    # wget https://s3-ap-northeast-1.amazonaws.com/pastehub/release/Linux/pastehub-0.1.9-1.i386.rpm

    # rpm -ihv pastehub-0.1.9-1.i386.rpm


- Debian or Ubuntu

   download deb and install by root account

    # https://s3-ap-northeast-1.amazonaws.com/pastehub/release/Linux/pastehub_0.1.9-2_i386.deb

    # dpkg -i --force-architecture pastehub_0.1.9-2_i386.deb


## MacOS X

### Required platform

- MacOS X 10.7 or later and Ruby Ruby 1.9.3 or later.

### installation instruction

 install gem

    $ gem install pastehub


## Emacs

### Required platforms

- Emacs 24.1 or later

### installation instruction

- Setup melpa

  add this code your .emacs (see also)

    (require 'package)
    (add-to-list 'package-archives
                '("melpa" . "http://melpa.milkbox.net/packages/") t)
    (package-initialize)


- install the "pastehub" package.






