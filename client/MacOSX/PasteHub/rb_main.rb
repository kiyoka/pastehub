#
#  rb_main.rb
#  PasteHub
#
#  Created by Kiyoka Nishiyama on 12/12/31.
#  Copyright (c) 2012 Kiyoka Nishiyama. All rights reserved.
#

# Loading the Cocoa framework.
framework 'Cocoa'

# Loading all the Ruby project files.
main = File.basename(__FILE__, File.extname(__FILE__))
dir_path = NSBundle.mainBundle.resourcePath.fileSystemRepresentation
Dir.glob(File.join(dir_path, '*.{rb,rbo}')).map { |x| File.basename(x, File.extname(x)) }.uniq.each do |path|
  if path != main
    require(path)
  end
end

# Starting the Cocoa main loop.
NSApplicationMain(0, nil)
