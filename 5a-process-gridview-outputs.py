import pandas as pd

for e in range(6):
    h5_file = f'/Volumes/data/seasonal-energy-drought-data/case-study/'

    load_shed = pd.read_hdf(h5_file, key="/area/UNSERVED_LOAD")

    load = pd.read_hdf(h5_file, key="/area/LOAD")

    lmp_loss = pd.read_hdf(h5_file, key="/area/LMP_LOSS")
    lmp_congestion = pd.read_hdf(h5_file, key="/area/LMP_CONGESTION")
    lmp_gen = pd.read_hdf(h5_file, key="/area/LOAD_AVG_LMP")
    lmp = lmp_loss + lmp_gen + lmp_congestion

    curtailment = pd.read_hdf(h5_file, key="/generator/PRICE_MARKUP_RATIO")
    CO2 = pd.read_hdf(h5_file, key="area/CO2_AMOUNT")

    tech_map = pd.read_csv('data/pcm_tech_map.csv')

    bus = pd.read_hdf(h5_file, key="/mdb/Generator").set_index("GeneratorName")
    subtype = bus['SubType']

    vre = subtype.str.contains('Solar|WT|PV')
    vre = vre[vre].index
    vre = vre[vre.isin(curtailment.columns)]

    bus = bus.loc[subtype.str.contains('Solar|WT|PV')]
    curtailment = curtailment.loc[:, vre]

    load_area_id = pd.read_hdf(h5_file, key="/mdb/Bus")
    load_area_name = pd.read_hdf(h5_file, key="/mdb/LoadArea")

    gen_area_map = pd.DataFrame([], index=curtailment.columns)
    gen_area_map['BusID'] = bus.loc[gen_area_map.index]['BusID']
    gen_area_map['LoadAreaID'] = load_area_id.set_index('BusID').loc[gen_area_map['BusID']]['LoadAreaID'].values
    gen_area_map['LoadAreaName'] = load_area_name.set_index('LoadAreaID').loc[gen_area_map['LoadAreaID']]['LoadAreaName'].values

    curtailment.columns = gen_area_map.loc[curtailment.columns, 'LoadAreaName']
    curtailment = curtailment.T.groupby(level=0).sum().T

    load_shed.to_csv(f'data/load_shed_event{e}.csv')
    curtailment.to_csv(f'data/curtailment_event{e}.csv')
    lmp.to_csv(f'data/lmps_event{e}.csv')
    CO2.to_csv(f'data/CO2_event{e}.csv')
    load.to_csv(f'data/load_event{e}.csv')