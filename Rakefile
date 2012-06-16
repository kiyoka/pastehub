# -*- mode: ruby; -*-
#                                          Rakefile for Realtime DB Sync
# Release Engineering
#   1. edit the VERSION.yml file
#   2. rake compile  &&   rake test
#   3. rake gemspec  &&   rake build
#      to generate realtimedbsync-x.x.x.gem
#   4. install realtimedbsync-x.x.x.gem to clean environment and test
#   5. rake release
#   6. gem push pkg/realtimedbsync-x.x.x.gem   ( need gem version 1.3.6 or higer. Please "gem update --system" to update )

MDB_URL='http://localhost:8081'
NTF_URL='http://localhost:8080'
USERNAME_A='userA'
USERNAME_B='userB'
SETENV_A="export SYNC_USER=userA ; export SYNC_SECRET_KEY='ZGFiYTRkNDg5MzA0YTA0Y2ExYzQ2MGFiNjM0YjFlNzJlMzcyZDVhZg=='"

require 'rake'
begin
  require 'jeweler2'
  Jeweler::Tasks.new do |gemspec|
    gemspec.name = "SynchroBase"
    gemspec.summary = "SynchroBase is real time db sync framework."
    gemspec.description = "SynchroBase is real time db sync framework powered by Vert.x."
    gemspec.email = "kiyoka@sumibi.org"
    gemspec.homepage = "http://github.com/kiyoka/SynchroBase"
    gemspec.authors = ["Kiyoka Nishiyama"]
    gemspec.files = FileList['Rakefile',
                             '.gemtest',
                             'VERSION.yml',
                             'README.txt',
                             'COPYING'
                            ].to_a
    gemspec.add_development_dependency "rspec"
    gemspec.add_development_dependency "rake"
    gemspec.add_dependency             "json"
  end
rescue LoadError
  puts 'Jeweler2 not available. If you want to build a gemfile, please install with "sudo gem install jeweler2"'
end

task :default => [:test] do
end

task :test do
  sh "rm -f /tmp/libdb.db"
  sh "time ruby -I ./lib `which rspec` -b   ./test/libutil_spec.rb        -r ./test/rspec_formatter_for_emacs.rb -f CustomFormatter"
  sh "time ruby -I ./lib `which rspec` -b   ./test/libauth_spec.rb        -r ./test/rspec_formatter_for_emacs.rb -f CustomFormatter"
  sh "time ruby -I ./lib `which rspec` -b   ./test/libdb_spec.rb          -r ./test/rspec_formatter_for_emacs.rb -f CustomFormatter"
end

task :m do
  sh "vertx run    server/masterdb.rb -instance 2"
end

task :n do
  sh "vertx run    server/notifier.rb -instance 4"
end

task :syncA do
  sh SETENV_A + "; ruby -I ./lib bin/clientSync"
end

task :syncB do
  sh "ruby -I ./lib bin/clientSync"
end

task :postA1 do
  sh SETENV_A + "; echo 'aaa1' | ruby -I ./lib bin/clientPost  #{USERNAME_A}"
end

task :postA2 do
  sh SETENV_A + "; echo 'aaa2' | ruby -I ./lib bin/clientPost  #{USERNAME_A}"
end

task :postB1 do
  sh "echo 'bbb1' | ruby -I ./lib bin/clientPost  #{USERNAME_B}"
end

task :dumpA do
  open( "|" + SETENV_A + "; ruby -I ./lib bin/clientDump list" ) {|f|
    firstKey = f.readline.chomp
    sh SETENV_A + "; ruby -I ./lib bin/clientDump get '#{firstKey}'"
  }
end
