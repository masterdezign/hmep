import numpy as np
from numpy import sin, cos

n = 150  # No of examples

dta = []
for i in range(n):
    x1 = np.random.rand()
    x2 = np.random.rand()
    y = sin(x1)**2 + cos(x2)**2
    dta.append([x1,x2,y])

np.savetxt('dta3.txt', dta, fmt="%s", delimiter=",")
