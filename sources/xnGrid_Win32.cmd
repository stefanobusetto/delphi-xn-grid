D:
cd D:\svn\delphi-xn-grid\sources\
if exist *.dcu del *.dcu
if exist Win32\*.dfm del Win32\*.dfm
if exist Win32\*.hpp del Win32\*.hpp
if exist Win32\*.res del Win32\*.res
if exist *.dfm copy *.dfm Win32\.
if exist *.hpp copy *.hpp Win32\.
if exist *.res copy *.res Win32\.
pause
