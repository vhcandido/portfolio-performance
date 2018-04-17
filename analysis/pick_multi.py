import numpy as np
import rpy2
from rpy2.robjects import pandas2ri, r as rcall
from scipy.ndimage import convolve
from scipy import ndimage
import matplotlib.pyplot as plt

from matplotlib.lines import Line2D

import load_r
load_r.load_rda(from_dir='../results/')
balance = load_r.get_balance()
balance[np.where(np.isnan(balance))] = 1
returns = load_r.get_returns()
returns[np.where(np.isnan(returns))] = 0
#returns_annualized_days = (1+returns)**365-1 #annualized returns
zlim, zrange = load_r.find_zlim(balance, center=1)
last_b_annual = balance[-1]**(365/balance.shape[0]) - 1
print('done...')


from datetime import datetime
dates = np.array(rcall('as.character(bt$dates)'))
dates = np.array([datetime.strptime(d, '%Y-%m-%d') for d in dates])

def get_group_indexes(dates, format='%Y-%m'):
    formatted = np.array([datetime.strftime(d, format) for d in dates])
    clusters = []
    for cl in np.unique(formatted):
        idx = np.where(formatted == cl)[0]
        clusters.append(idx)
    return clusters

groups_idx_list = get_group_indexes(dates, '%Y-%m')

def format_all_returns(x):
    def apply_over_groups(x, groups, f, *args, **kwargs):
        return np.array([f(x[g], *args, **kwargs) for g in groups])
    groups = get_group_indexes(dates, '%Y-%m')
    return np.apply_along_axis(func1d=apply_over_groups,
                               axis=0, arr=x, groups=groups,
                               f=lambda x: np.prod(x+1)-1)

returns_monthly = format_all_returns(returns)
returns_annualized_months = (returns_monthly+1)**12-1

price = np.array(rcall('bt$price.returns')).reshape(-1)
price = np.cumprod(1+price)-1


print('Plotting')
def semi_std(x, target=None):
    if target is None:
        target = np.mean(x)
    i = np.where(x < target)
    if len(x[i]) == 0: return 0.0
    return np.sqrt(np.sum((x[i] - target)**2)/len(x))
    #return np.sqrt(np.mean((x[i] - target)**2))

import matplotlib.gridspec as gridspec

fig = plt.figure(tight_layout=True)
gs = gridspec.GridSpec(2,6)

ax = fig.add_subplot(gs[0, 2:-2])
img3 = ax.imshow(balance[-1]-1, cmap=plt.cm.jet, origin='lower', picker=True, vmin=zlim[0]-1, vmax=zlim[1]-1)
ax.set_title('Rate of return')
fig.colorbar(img3, orientation='horizontal')

ax = fig.add_subplot(gs[0, :2])
sd1 = np.apply_along_axis(func1d=np.std, axis=0, arr=returns_annualized_months)
sd1_ = np.copy(sd1)
sd1[np.where(sd1==0)] = None
img1 = ax.imshow(sd1, cmap=plt.cm.jet, origin='lower', picker=True)
ax.set_title('Standard deviation')
fig.colorbar(img1, orientation='horizontal')

ax = fig.add_subplot(gs[0, -2:])
sd2 = np.apply_along_axis(func1d=semi_std, axis=0, arr=returns_annualized_months, target=0)
sd2_ = np.copy(sd2)
sd2[np.where(sd2==0)] = None
img2 = ax.imshow(sd2, cmap=plt.cm.jet, origin='lower', picker=True)
ax.set_title('Semideviation (target=0)')
fig.colorbar(img2, orientation='horizontal')

ax = fig.add_subplot(gs[1, :3])
ax.set_xlabel('Annualized ROR')
ax.set_ylabel('stdev')
line1 = ax.scatter(last_b_annual, sd1_, s=15, alpha=.2, picker=3)
#line = ax.plot(balance[-1], sd1, 'bo', alpha=.1, markersize=3, picker=3)
ax.grid(linestyle='--')

ax = fig.add_subplot(gs[1, -3:])
line2 = ax.scatter(last_b_annual, sd2_, s=15, alpha=.2, picker=3)
ax.set_xlabel('Annualized ROR')
ax.set_ylabel('semidev')
ax.grid(linestyle='--')

figi = None
def imgpick(event):
    global figi
    x, y = int(event.mouseevent.xdata), int(event.mouseevent.ydata)
    print('[imgpick]', [y, x])

    figi = plt.figure(tight_layout=True)
    axi = figi.add_subplot(1,1,1)
    axi.set_ylim(zlim[0]-1, zlim[1]-1)
    axi.plot(balance[:,y,x]-1, linewidth=1, label=str([y,x]))
    axi.plot(price, linewidth=1, linestyle='--', color='black', label='B&H')
    axi.set_ylabel('Rate of return (ROR)')
    axi.set_xlabel('Time (days)')
    axi.set_title('Evolution of rate of return\n\
            ROR: %.3f | SD: %.3f | TSD: %.3f' % (balance[-1,y,x], sd1[y,x], sd2[y,x]))
    axi.grid(linestyle='--')
    plt.legend()
    figi.show()
    return True

def linepick(event):
    global figi
    if not len(event.ind): return True

    figi = plt.figure(tight_layout=True)
    axi = figi.add_subplot(111)
    for subplotnum, dataind in enumerate(event.ind):
        y = dataind // balance.shape[2]
        x = dataind % balance.shape[2]
        print('[linepick]', dataind, [y, x])
        axi.set_ylim(zlim[0]-1, zlim[1]-1)
        axi.plot(balance[:,y,x]-1, linewidth=1, label=str([y,x]))
    axi.set_ylabel('Rate of return (ROR)')
    axi.set_xlabel('Time (days)')
    axi.set_title('Evolution of rate of return (%d)' % (len(event.ind)))
    axi.grid(linestyle='--')
    if len(event.ind) <= 10: plt.legend()
    figi.show()
    print('')
    return True

def onpick(event):
    global figi
    if figi: plt.close(figi)
    if event.artist == line1 or event.artist == line2:
        return linepick(event)

    if event.artist == img1 or event.artist == img2 or event.artist == img3:
        return imgpick(event)

    return True
    
fig.canvas.mpl_connect('pick_event', onpick)


plt.show()
