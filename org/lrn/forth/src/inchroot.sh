cd /j
make
if [ $? -eq 0 ]; then
    echo "===============OK==============="
    exit 0
fi
echo "===============ERR==============="
exit 1
