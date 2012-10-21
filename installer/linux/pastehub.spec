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
/bin/cp -r ./opt/* %{buildroot}/opt

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-, root, root)
/opt
%defattr(755, root, root)
/opt/pastehub/bin

%changelog
