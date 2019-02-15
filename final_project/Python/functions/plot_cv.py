import numpy as np
import matplotlib.pyplot as plt
plt.style.use('ggplot')
np.random.seed(1234)


def visualize_groups(classes, groups):

    cmap_data = getattr(plt.cm, 'Paired')

    # Visualize dataset groups
    fig, ax = plt.subplots()
    ax.scatter(range(len(groups)),  [.5] * len(groups), c=groups, marker='_',
               lw=50, cmap=cmap_data)
    ax.scatter(range(len(groups)),  [3.5] * len(groups), c=classes, marker='_',
               lw=50, cmap=cmap_data)
    ax.set(ylim=[-1, 5], yticks=[.5, 3.5],
           yticklabels=['Data\ngroup', 'Data\nclass'], xlabel="Sample index")


def plot_cv_indices(cv, X, ax, n_splits, lw=10):

    cmap_cv = getattr(plt.cm, 'coolwarm')

    # Generate the training/testing visualizations for each CV split
    for ii, (tr, tt) in enumerate(cv.split(X=X)):
        # Fill in indices with the training/test groups
        indices = np.array([np.nan] * len(X))
        indices[tt] = 1
        indices[tr] = 0

        # Visualize the results
        ax.scatter(range(len(indices)), [ii + .5] * len(indices),
                   c=indices, marker='_', lw=lw, cmap=cmap_cv,
                   vmin=-.2, vmax=1.2)

    # Formatting
    y_tick_labels = list(range(n_splits)) + ['class', 'group']
    ax.set(yticks=np.arange(n_splits) + .5, yticklabels=y_tick_labels,
           xlabel='Sample index', ylabel='CV Iterations',
           ylim=[n_splits + .2, -.2])
    ax.set_title('{}'.format(type(cv).__name__), fontsize=15)
    return ax
