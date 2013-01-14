#
#  CustomView for PasteHub
#
#  Created by Kiyoka Nishiyama on 2012.12.31.
#  Copyright 2012 Kiyoka Nishiyama. All rights reserved.
#

class PasteHubCustomView < NSView
  
  def initWithFrame(frame)
    super(frame)
    return self
  end

  def drawRect(rect)
    path = NSBezierPath.bezierPathWithRect(rect)
    path.stroke
  end
end
