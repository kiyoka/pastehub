Name: pastehub
Version: 0.1.5
Summary: pastehub client for Linux
Release: 1
License: BSD License
Group: Applications/Productivity
Provides: pastehub
URL: https://github.com/kiyoka/pastehub
Source0: %{name}-%{version}.tar.gz
%description

%prep
%setup -q

%build

%install
/bin/rm -rf %{buildroot}/
mkdir -p    %{buildroot}/opt
/bin/cp -r ./opt/*  %{buildroot}/opt
mkdir -p            %{buildroot}/opt/pastehub/bin
#ln -s /opt/pastehub/ruby-1.9.3-p194/bin/PastehubSync  %{buildroot}/opt/pastehub/bin/PastehubSync
#ln -s /opt/pastehub/ruby-1.9.3-p194/bin/pastehubDump  %{buildroot}/opt/pastehub/bin/pastehubDump
#ln -s /opt/pastehub/ruby-1.9.3-p194/bin/pastehubPost  %{buildroot}/opt/pastehub/bin/pastehubPost

%clean
rm -rf $RPM_BUILD_ROOT

%post
export LD_LIBRARY_PATH=/opt/pastehub/lib:$LD_LIBRARY_PATH
/opt/pastehub/ruby-1.9.3-p194/bin/gem install /opt/pastehub/gems/bundler-1.2.1.gem 
/opt/pastehub/ruby-1.9.3-p194/bin/gem install /opt/pastehub/gems/highline-1.6.15.gem 
/opt/pastehub/ruby-1.9.3-p194/bin/gem install /opt/pastehub/gems/pastehub-0.1.5.gem 

%postun
# remove gem files
/bin/rm -rf /opt/pastehub/ruby-1.9.3-p194

%files
%defattr(-, root, root)
/opt

%changelog
