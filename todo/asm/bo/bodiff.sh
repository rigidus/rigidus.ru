radiff2 -g sym.login bo.old bo > test.dot
dot -Tpng test.dot > test.png
eog test.png
