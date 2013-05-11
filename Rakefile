# -*- mode: ruby; -*-
#                                          Rakefile for PasteHub
# Release Engineering:
#   1. edit the VERSION.yml file
#   2. rake test  &&  rake gemspec  &&   rake build
#      to generate pastehub-x.x.x.gem
#   3. install pastehub-x.x.x.gem to clean environment and test
#   4. rake release
#   5. gem push pkg/pastehub-x.x.x.gem   ( need gem version 1.3.6 or higer. Please "gem update --system" to update )
#
# Test environment:
#   1. gem install fake_dynamo --version 0.1.3
#   2. fake_dynamo --port 4567


USERNAME_A='userA'
USERNAME_B='userB'
SETENV_A="export PASTEHUB_USER=userA ; export PASTEHUB_SECRET_KEY='ZGFiYTRkNDg5MzA0YTA0Y2ExYzQ2MGFiNjM0YjFlNzJlMzcyZDVhZg=='"

require 'rake'
begin
  require 'jeweler2'
  ['pastehub', 'pastehub-win32'].each do |name|
    Jeweler::Tasks.new do |gemspec|
      gemspec.name = name
      gemspec.summary = "PasteHub is cloud-based cross-platform clipboard sync."
      gemspec.description = "PasteHub is cloud-based cross-platform clipboard sync."
      gemspec.email = "kiyoka@sumibi.org"
      gemspec.homepage = "http://github.com/kiyoka/pastehub"
      gemspec.authors = ["Kiyoka Nishiyama"]
      gemspec.files = FileList['Rakefile',
                               '.gemtest',
                               'VERSION.yml',
                               'README.txt',
                               'bin/*',
                               'lib/*.rb',
                               'lib/*/*.rb',
                               'server/*.rb'
                              ].to_a
      gemspec.add_development_dependency "rspec"
      gemspec.add_development_dependency "rake"
      gemspec.add_dependency             "json"
      gemspec.add_dependency             "highline"
      gemspec.add_dependency             "clipboard"
      gemspec.add_dependency             "ffi"
    end
  end
rescue LoadError
  puts 'Jeweler2 not available. If you want to build a gemfile, please install with "sudo gem install jeweler2"'
end

task :default => [:test] do
end


task :test do
  sh "rm -f /tmp/usertmp.db"
  sh "ruby    -I ./lib `which rspec` -b   ./test/libstore_spec.rb       "
  sh "ruby    -I ./lib `which rspec` -b   ./test/libconfig_spec.rb      "
  sh "ruby    -I ./lib `which rspec` -b   ./test/libutil_spec.rb        "
  sh "ruby    -I ./lib `which rspec` -b   ./test/libcrypt_spec.rb       "
  sh "ruby    -I ./lib `which rspec` -b   ./test/libauth_spec.rb        "
  sh "ruby    -I ./lib `which rspec` -b   ./test/libauth2_spec.rb       "
  sh "ruby    -I ./lib `which rspec` -b   ./test/libclient_spec.rb      "
  sh "ruby    -I ./lib `which rspec` -b   ./test/liblog_spec.rb         "
  sh "ruby    -I ./lib `which rspec` -b   ./test/libmasterdb_spec.rb    "
  sh "ruby    -I ./lib `which rspec` -b   ./test/libuserdb_spec.rb      "
end

task :win32_test do
  sh "rm -f /tmp/usertmp.db"
  sh "rspec -I ./lib  -b   ./test/libmswindows_spec.rb     "
  sh "rspec -I ./lib  -b   ./test/libstore_spec.rb       "
  sh "rspec -I ./lib  -b   ./test/libconfig_spec.rb      "
  sh "rspec -I ./lib  -b   ./test/libutil_spec.rb        "
  sh "rspec -I ./lib  -b   ./test/libcrypt_spec.rb       "
  sh "rspec -I ./lib  -b   ./test/libauth_spec.rb        "
#  sh "rspec -I ./lib  -b   ./test/libauth2_spec.rb       "
  sh "rspec -I ./lib  -b   ./test/libclient_spec.rb      "
  sh "rspec -I ./lib  -b   ./test/liblog_spec.rb         "
#  sh "rspec -I ./lib  -b   ./test/libmasterdb_spec.rb    "
#  sh "rspec -I ./lib  -b   ./test/libuserdb_spec.rb      "


end

task :fluentd_for_test do
  sh "fluentd -c ./fluentd-conf-for-test/fluent.conf"
end

task :test_u do
  sh "ruby -I ./lib `which rspec` -b  -t users   ./test/aws_spec.rb            -r ./test/rspec_formatter_for_emacs.rb -f CustomFormatter"
end

task :test_e do
  sh "ruby -I ./lib `which rspec` -b  -t entries ./test/aws_spec.rb            -r ./test/rspec_formatter_for_emacs.rb -f CustomFormatter"
end

task :m do
  sh "vertx run    server/masterdb.rb -instance 2"
end

task :n do
  sh "vertx run    server/notifier.rb -instance 2"
end

task :d do
  sh "ruby -I ./lib server/pastehub-admin    gc   "
end

task :setupTable do
  sh "ruby -I ./lib server/pastehub-admin    setup "
end

task :sync do
  sh "ruby -I ./lib bin/PastehubSync"
end

task :macruby_sync do
  sh "macruby -I ./lib bin/PastehubSync"
end

task :syncA do
  sh SETENV_A + "; ruby -I ./lib bin/PastehubSync"
end

task :postA1 do
  sh SETENV_A + "; echo 'aaa1' | ruby -I ./lib bin/pastehubPost"
end

task :postA2 do
  sh SETENV_A + "; echo 'aaa2' | ruby -I ./lib bin/pastehubPost"
end

task :postB1 do
  sh "echo 'bbb1' | ruby -I ./lib bin/pastehubPost"
end

task :dumpA do
  open( "|" + SETENV_A + "; ruby -I ./lib bin/pastehubDump list" ) {|f|
    firstKey = f.readline.chomp
    sh SETENV_A + "; ruby -I ./lib bin/pastehubDump get '#{firstKey}'"
  }
  sh SETENV_A + "; ruby -I ./lib bin/pastehubDump top"
  sh SETENV_A + "; ruby -I ./lib bin/pastehubDump latest"
end
