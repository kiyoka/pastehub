# -*- mode: ruby; -*-
#                                          Rakefile for PasteHub
# Release Engineering:
#   1. edit the VERSION.yml file
#   2. rake test  &&  rake gemspec  &&   gem build pastehub.gemspec
#      to generate pastehub-x.x.x.gem
#   3. install pastehub-x.x.x.gem to clean environment and test
#   4. rake release
#   5. gem push pkg/pastehub-x.x.x.gem   ( need gem version 1.3.6 or higer. Please "gem update --system" to update )


require 'rake'
begin
  require 'jeweler2'
  Jeweler::Tasks.new do |gemspec|
    gemspec.name = 'pastehub'
    gemspec.summary = "PasteHub is cloud-based cross-platform clipboard sync."
    gemspec.description = "PasteHub is cloud-based cross-platform clipboard sync."
    gemspec.email = "kiyoka@sumibi.org"
    gemspec.license = 'New BSD'
    gemspec.homepage = "http://github.com/kiyoka/pastehub"
    gemspec.authors = ["Kiyoka Nishiyama"]
    gemspec.files = FileList['Rakefile',
                             '.gemtest',
                             'VERSION.yml',
                             'README.txt',
                             'bin/*',
                             'lib/*.rb',
                             'lib/*/*.rb',
                            ].to_a
#    gemspec.add_dependency(            "json", "1.8.1")
    gemspec.add_dependency(            "clipboard", "1.0.5" )
    gemspec.add_dependency(            "ffi", "1.9.3")
  end
rescue LoadError
  puts 'Jeweler2 not available. If you want to build a gemfile, please install with "sudo gem install jeweler2"'
end

task :default => [:test] do
end


task :test do
  sh "ruby    -I ./lib `which rspec` -b   ./test/libconfig_spec.rb      "
  sh "ruby    -I ./lib `which rspec` -b   ./test/libconfig2_spec.rb     "
  sh "/bin/rm -rf /tmp/home/user1"
  sh "ruby    -I ./lib `which rspec` -b   ./test/libclient_spec.rb      "
  sh "/bin/rm -rf /tmp/home/user1"
  sh "ruby    -I ./lib `which rspec` -b   ./test/libclientsync_spec.rb  "
  sh "ruby    -I ./lib `which rspec` -b   ./test/libsyncentry_spec.rb   "
  sh "ruby    -I ./lib `which rspec` -b   ./test/libutil_spec.rb        "
  sh "ruby    -I ./lib `which rspec` -b   ./test/libclipboard_spec.rb   "
  sh "ruby    -I ./lib `which rspec` -b   ./test/libplugin_spec.rb      "
  sh "ruby    -I ./lib `which rspec` -b   ./test/plugin_dropbox_todo_spec.rb    "
  sh "ruby    -I ./lib `which rspec` -b   ./test/plugin_sendmail_spec.rb        "
end

task :sync do
  sh "ruby -I ./lib bin/PastehubSync -v"
end

task :macruby_sync do
  sh "macruby -I ./lib bin/PastehubSync"
end

task :syncA do
  sh SETENV_A + "; ruby -I ./lib bin/PastehubSync"
end

task :postA1 do
  sh "echo 'aaa1' | ruby -I ./lib bin/pastehubPost"
end

task :postA2 do
  sh "echo 'The quick brown fox jumps over the lazy dog and run' | ruby -I ./lib bin/pastehubPost"
end

task :getA do
  sh "ruby -I ./lib bin/pastehubGet time"
  sh "ruby -I ./lib bin/pastehubGet get"
end
