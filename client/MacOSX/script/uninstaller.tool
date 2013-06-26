#!/bin/sh

pkgutil --pkgs | grep net.pastehub.app.pkg
if [ "$?" = "0" ]
then
    echo "uninstalling PasteHub.app..."
    /bin/rm -f /opt/pastehub
    /bin/rm -f /Applications/PasteHub.app
    sudo pkgutil --forget net.pastehub.app.pkg
    sudo pkgutil --forget net.pastehub.cruby.pkg
    echo "done."
else
    echo "already uninstalled PasteHub.app"
fi
