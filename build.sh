mono tools/paket.bootstrapper.exe
mono tools/paket.exe install
mono packages/FAKE/tools/FAKE.exe build.fsx "$@"
