import numpy as np
import matplotlib.pyplot as plt

#import rpy2
#from rpy2.robjects import pandas2ri, r as rcall

import load_r

def pick_plot(X, zlim):
    fig = plt.figure()
    ax = fig.add_subplot(111)
    ax.set_title('click on point to plot time series')
    img = ax.imshow(X[-1], picker=True, cmap=plt.cm.jet, origin='lower', vmin=zlim[0], vmax=zlim[1])

    ts = []
    def onpick(event):
        if event.artist != img: return True
        if ts:
            plt.close(ts[0])
            del ts[:]

        x, y = int(event.mouseevent.xdata), int(event.mouseevent.ydata)

        figi = plt.figure()
        ax = figi.add_subplot(1,1,1)
        ax.plot(X[:, y, x])
        ax.text(0.05,0.9,
                'x=%d\ny=%d' % (x, y),
                ha='left', va='top',
                transform=ax.transAxes)
        ax.set_ylim(zlim[0], zlim[1])

        figi.show()
        ts.append(figi)
        return True

    fig.canvas.mpl_connect('pick_event', onpick)
    plt.show()

def main():
    load_r.load_rda(from_dir='../results')
    balance = load_r.get_balance()
    zlim = load_r.find_zlim(balance, center=1)
    pick_plot(balance, zlim)

if __name__ == '__main__':
    main()
