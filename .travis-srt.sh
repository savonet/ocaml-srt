# Download and build srt

sudo apt-get install tclsh pkg-config libssl-dev build-essential

git clone https://github.com/Haivision/srt.git

cd srt
./configure
make
sudo make install
