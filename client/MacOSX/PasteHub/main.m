//
//  main.m
//  PasteHub
//
//  Created by Kiyoka Nishiyama on 12/12/31.
//  Copyright (c) 2012 Kiyoka Nishiyama. All rights reserved.
//

#import <Cocoa/Cocoa.h>

#import <MacRuby/MacRuby.h>

int main(int argc, char *argv[])
{
    return macruby_main("rb_main.rb", argc, argv);
}
