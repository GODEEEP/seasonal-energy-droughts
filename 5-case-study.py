import pandas as pd
import numpy as np
from matplotlib import pyplot as plt
import geopandas as gpd
import matplotlib as mpl

case_study_data_path = '/Volumes/data/seasonal-energy-drought-data/case-study/'

def compare(feat):
    fig, axs = plt.subplots(1, 5, figsize=(8, 2))
    regions = gpd.read_file(f'{case_study_data_path}/BA_boundaries.zip')
    regions = regions.set_index('Label')
    regions.drop(['BCHA','AESO'],inplace=True)
    #regions = pd.concat([regions, lmp], axis=1)

    #months = [[11, 12], [11], [2], [12], [12]]
    months = [[2, 11, 12], [11, 12], [11], [2], [12], [12]]

    #for scenario in range(6):
    for axi, scenario in zip(range(5), range(1, 6)):
        data = pd.read_csv(f'{case_study_data_path}/data/{feat}_event{scenario}.csv', index_col=0)
        data = data.loc[pd.to_datetime(data.index).month.isin(months[scenario])]
        #tot = data.sum().sum()
        data = data.mean()

        ref_data = pd.read_csv(f'{case_study_data_path}/data/{feat}_event0.csv', index_col=0)
        ref_data = ref_data.loc[pd.to_datetime(ref_data.index).month.isin(months[scenario])]
        #print(tot/(ref_data.sum().sum()))
        ref_data = ref_data.mean()

        data = data/ref_data

        #data = data.mean()
        data.name = f'Event{scenario}'
        if scenario == 1:
            min_val, max_val = data.min(), data.max()
        else:
            min_val, max_val = min(min_val, data.min()), max(max_val, data.max())

        min_val = 1 - (max_val - 1)

        #regions = gpd.read_file('BA_boundaries.zip')
        #regions = regions.set_index('Label')
        regions = pd.concat([regions, data], axis=1)

    #for scenario, ax in enumerate(axs.reshape(-1)):
    for axi, scenario in enumerate(range(5)):
        ax = axs.reshape(-1)[axi]
        regions.plot(column=f'Event{scenario+1}', ax=ax, vmin=min_val, vmax=max_val, cmap='coolwarm')#, legend=True)
        ax.axis('off')
        ax.set_title(f'Event {scenario+1}')


    axs.reshape(-1)[-1].axis('off')

    #suptitle_label = {'lmps': 'Average Drought LMP Increase', 'CO2': 'Average Drought Emissions Increase'}
    #fig.suptitle(suptitle_label[feat])

    cbar_label = {'lmps': 'LMPs ($)', 'CO2': 'Hourly CO2 Emission'}

    cbar_ax = fig.add_axes([0.2, -0.05, 0.6, 0.05])
    cmap = mpl.cm.coolwarm
    norm = mpl.colors.Normalize(vmin=min_val, vmax=max_val)
    fig.colorbar(mpl.cm.ScalarMappable(norm=norm, cmap=cmap),
                 cax=cbar_ax, orientation='horizontal', label=cbar_label[feat])  # , label='Some Units')

    plt.tight_layout()
    plt.savefig(f'figures/map_{feat}.pdf', bbox_inches='tight')
    plt.show()

compare('CO2')
compare('lmps')
