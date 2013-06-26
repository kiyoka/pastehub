How to install client software (MacOS X)
=======================

## Required platform

- MacOS X 10.6 or later

## installation instruction

- download installer

      [PasteHub-0.3.0.dmg](https://s3-ap-northeast-1.amazonaws.com/pastehub/release/MacOSX/PasteHub-0.3.0.dmg) (20MByte)

- Please open dmg file, then double click Pastehub.Package to install

     ![Double click to install](macos_dmg.png) 

     ![installation1](macos_start_install.png)

     ... Please install PasteHub.app 

     ![installation2](macos_installing.png)

     ... Please install PasteHub.app 

- Double click to start PasteHub application
     ![start](macos_applications_folder.png)

- setup your account information.

     Please input your account infromation ( how to get your account? ... see [Registration](Registration.md) )
     ![signin](macos_signin.png)

     OK, ONLINE icon appears on status bar

     ![ONLINE](status_bar_is_online.png)


## Copy and Paste operation

- It's easy.  please cut / copy / paste as usual.

     ![key](https://dl.dropbox.com/u/3870066/blog/iStock_000009322220XSmall.jpg)

     PasteHub.app sync automatically.

     ![sync](https://dl.dropbox.com/u/3870066/blog/iStock_000019296334XSmall.jpg)


## How to start PasteHub application again.

- Double click to start PasteHub application

     ![start](macos_applications_folder.png)

- please input your own password

     ![password](macos_password.png)

     OK, ONLINE icon appears on status bar

      ![ONLINE](status_bar_is_online.png)
  

## Status bar icons

+ ![icon](pastehub_statusbar_normal.png)  ... offline
+ ![icon](pastehub_statusbar_checked.png) ... ONLINE
+ ![icon](pastehub_statusbar_1.png) ... one   paste data comming
+ ![icon](pastehub_statusbar_2.png) ... two   paste data comming
+ ![icon](pastehub_statusbar_3.png) ... three paste data comming
+ ![icon](pastehub_statusbar_3plus.png) ... over three paste data comming


## History

+ 0.3.0 First installer package version.
+ 0.2.4 Fixed SEGV problem with three environment variables for MacRuby. ( VM_DISABLE_RBO=1 /  VM_OPT_LEVEL=0 / AUTO_USE_TLC=0 )
+ 0.2.3 Fixed SEGV problem of gdbm parallel operation.
+ 0.2.2 First release.
