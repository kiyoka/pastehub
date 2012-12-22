#!/usr/bin/ruby1.9

require 'yaml'
open( "../../VERSION.yml" ){ |f|
  yaml = YAML.load( f )
  # like {:major=>0, :minor=>1, :patch=>5}
  printf( "%d.%d.%d",
          yaml[ :major ],
          yaml[ :minor ],
          yaml[ :patch ] );
}

