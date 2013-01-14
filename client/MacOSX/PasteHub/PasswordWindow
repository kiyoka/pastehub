#
#  StatusMenu of PasteHub
#
#  Created by Kiyoka Nishiyama on 2012.12.31.
#  Copyright 2012 Kiyoka Nishiyama All rights reserved.
#

class PasteHubStatusMenu < NSMenu
  attr_accessor :status_bar_item
  def awakeFromNib
    self.status_bar_item = NSStatusBar.systemStatusBar.statusItemWithLength(NSVariableStatusItemLength)
    
    image = NSImage.imageNamed 'pastehub_status_bar.png' # 17x17 dot
    self.status_bar_item.setImage image
    self.status_bar_item.setHighlightMode true    
    self.status_bar_item.setMenu self
  end
end
