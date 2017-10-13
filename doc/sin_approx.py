from random import random
from pylab import *

rcParams['font.family'] = 'serif'
rcParams['font.size'] = 14

def dist1(x,y):
    return abs(x-y)

# The polynomial approximation sin(x)
# TODO: algebraic expression elimination
def approx(x0):
    v1 = -5.936286355387799e-2 + -5.936286355387799e-2
    v4 = x0 + x0
    v5 = v1 * x0
    v7 = v4 * x0
    v8 = v1 * v5
    v9 = x0 * x0
    v10 = v8 * v9
    v11 = x0 * v10
    v15 = -5.936286355387799e-2 * x0
    v18 = v10 * v11
    v20 = v7 * v15
    v21 = v15 + x0
    v25 = v21 + v20
    return v18 + v25

def taylor3(x):
    fact3 = 6.0
    return x - x**3/fact3

def taylor5(x):
    fact5 = 120.0
    return taylor3(x) + x**5/fact5

def taylor7(x):
    fact7 = 5040.0
    return taylor5(x) - x**7/fact7

def main():
    xs = linspace(-pi,pi,300)
    a = [approx(x) for x in xs]

    distances1 = [dist1(approx(x), sin(x)) for x in xs]
    distances2 = [dist1(taylor3(x), sin(x)) for x in xs]
    distances3 = [dist1(taylor5(x), sin(x)) for x in xs]
    distances4 = [dist1(taylor7(x), sin(x)) for x in xs]

    avg = sum(distances1) / len(xs)
    print("MEP expression: Average distance for %d points: %.4f" % (len(xs), avg))
    # MEP expression: Average distance for 300 points: 0.0303

    avg = sum(distances2) / len(xs)
    print("3rd-order Taylor sine expansion: Average distance for %d points: %.4f" % (len(xs), avg))
    # 3rd-order Taylor sine expansion: Average distance for 300 points: 0.3633
    avg = sum(distances3) / len(xs)

    print("5rd-order Taylor sine expansion: Average distance for %d points: %.4f" % (len(xs), avg))
    # 5rd-order Taylor sine expansion: Average distance for 300 points: 0.0688

    avg = sum(distances4) / len(xs)
    print("7rd-order Taylor sine expansion: Average distance for %d points: %.4f" % (len(xs), avg))
    # 7rd-order Taylor sine expansion: Average distance for 300 points: 0.0079

    # Visualization

    # Note, the random domain during each demo run is different.
    # The values below are given for illustration purposes only.
    randXs = sort([(random()-0.5)*2*pi for i in range(50)])
    correct = [sin(x) for x in randXs]

    plot(randXs, correct, '+')
    plot(xs, a, lw=1.2)
    xlabel('x')
    ylabel('y(x)')
    xlim([-pi, pi])
    xticks([-pi, 0, pi], ['-$\pi$', '0', '$\pi$'])
    legend(['Points from sin(x)', 'Polynomial Approx.'], fontsize=10)
    show()


if __name__ == '__main__':
    main()
