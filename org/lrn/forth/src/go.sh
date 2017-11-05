sudo rm -Rf /mnt/xen/j/src
sudo cp -R  /home/rigidus/repo/rigidus.ru/org/lrn/forth/src/* /mnt/xen/j/
sudo chroot /mnt/xen /j/inchroot.sh
if [ $? -eq 0 ]; then
    cp /mnt/xen/j/forth ./
    gdb -q forth
fi
