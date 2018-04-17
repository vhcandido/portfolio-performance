import numpy as np
import pandas as pd

# Choosing filenames
from tkinter import *
from tkinter.filedialog import askopenfilename as tkopen

import rpy2
from rpy2.robjects import pandas2ri, r as rcall

def choose_file(from_dir='.'):
    Tk().withdraw()
    name = tkopen(initialdir=from_dir, title='Choose a file')
    return name

def load_rda(fname=None, from_dir='.'):
    if not fname:
        fname = choose_file(from_dir=from_dir)
    print('[load_rda] loading "%s"' % fname)
    rcall('load("%s")' % fname)

def get_balance():
    balance = rcall('bt$strat.balance')
    shape = rcall('dim(bt$strat.balance)')
    return np.array(balance).reshape(shape)

def get_returns():
    returns = rcall('bt$strat.returns')
    shape = rcall('dim(bt$strat.returns)')
    return np.array(returns).reshape(shape)

def mode(x):
    from scipy import stats
    m = stats.mode(x.reshape(-1)).mode[0]
    return m

def find_zlim(x, center=None):
    if not center:
        center = mode(x)
    zrange = np.array([np.nanmin(x), np.nanmax(x)])
    zmax = np.max(np.abs(center-zrange))
    zlim = np.array([center-zmax, center+zmax])
    return zlim, zrange

def get_melted_balance(i=None):
    if not i:
        i = rcall('dim(bt$strat.balance)[1]')[0]
    # Same as
    # mat <- balance[i, , , ...]
    rcall('''
    require(magrittr)
    dims <- dim(bt$strat.balance)
    margin.idx <- bt$strat.balance %%>%% slice.index(MARGIN=1)
    mat <- bt$strat.balance[which(margin.idx == %d)] %%>%%
        array(dims[-1])
    ''' % i)
    return rcall('reshape2::melt(mat)')
