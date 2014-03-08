# Generated by jeweler
# DO NOT EDIT THIS FILE DIRECTLY
# Instead, edit Jeweler::Tasks in Rakefile, and run 'rake gemspec'
# -*- encoding: utf-8 -*-

Gem::Specification.new do |s|
  s.name = "pastehub"
  s.version = "0.4.1"

  s.required_rubygems_version = Gem::Requirement.new(">= 0") if s.respond_to? :required_rubygems_version=
  s.authors = ["Kiyoka Nishiyama"]
  s.date = "2014-03-08"
  s.description = "PasteHub is cloud-based cross-platform clipboard sync."
  s.email = "kiyoka@sumibi.org"
  s.executables = ["PastehubSync", "pastehubGet", "pastehubPost"]
  s.extra_rdoc_files = [
    "README.md"
  ]
  s.files = [
    "Rakefile",
    "VERSION.yml",
    "bin/PastehubSync",
    "bin/pastehubGet",
    "bin/pastehubPost",
    "lib/pastehub.rb",
    "lib/pastehub/client.rb",
    "lib/pastehub/clientsync.rb",
    "lib/pastehub/clipboard.rb",
    "lib/pastehub/config.rb",
    "lib/pastehub/syncentry.rb",
    "lib/pastehub/util.rb"
  ]
  s.homepage = "http://github.com/kiyoka/pastehub"
  s.licenses = ["New BSD"]
  s.require_paths = ["lib"]
  s.rubygems_version = "2.0.14"
  s.summary = "PasteHub is cloud-based cross-platform clipboard sync."

  if s.respond_to? :specification_version then
    s.specification_version = 4

    if Gem::Version.new(Gem::VERSION) >= Gem::Version.new('1.2.0') then
      s.add_runtime_dependency(%q<clipboard>, ["= 1.0.5"])
      s.add_runtime_dependency(%q<ffi>, ["= 1.9.3"])
      s.add_runtime_dependency(%q<rspec>, [">= 0"])
      s.add_runtime_dependency(%q<jeweler2>, [">= 0"])
      s.add_runtime_dependency(%q<rake>, [">= 0"])
      s.add_runtime_dependency(%q<clipboard>, ["= 1.0.5"])
      s.add_runtime_dependency(%q<ffi>, ["= 1.9.3"])
      s.add_runtime_dependency(%q<clipboard>, ["= 1.0.5"])
      s.add_runtime_dependency(%q<ffi>, ["= 1.9.3"])
    else
      s.add_dependency(%q<clipboard>, ["= 1.0.5"])
      s.add_dependency(%q<ffi>, ["= 1.9.3"])
      s.add_dependency(%q<rspec>, [">= 0"])
      s.add_dependency(%q<jeweler2>, [">= 0"])
      s.add_dependency(%q<rake>, [">= 0"])
      s.add_dependency(%q<clipboard>, ["= 1.0.5"])
      s.add_dependency(%q<ffi>, ["= 1.9.3"])
      s.add_dependency(%q<clipboard>, ["= 1.0.5"])
      s.add_dependency(%q<ffi>, ["= 1.9.3"])
    end
  else
    s.add_dependency(%q<clipboard>, ["= 1.0.5"])
    s.add_dependency(%q<ffi>, ["= 1.9.3"])
    s.add_dependency(%q<rspec>, [">= 0"])
    s.add_dependency(%q<jeweler2>, [">= 0"])
    s.add_dependency(%q<rake>, [">= 0"])
    s.add_dependency(%q<clipboard>, ["= 1.0.5"])
    s.add_dependency(%q<ffi>, ["= 1.9.3"])
    s.add_dependency(%q<clipboard>, ["= 1.0.5"])
    s.add_dependency(%q<ffi>, ["= 1.9.3"])
  end
end

