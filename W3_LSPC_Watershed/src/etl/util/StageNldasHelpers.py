from typing import (Callable, TypeVar, List, Tuple, Any)
import xarray as xr
import pandas as pd
import numpy as np

from pathlib import Path

class NldasSpatialSubsetter:

    def extract_point_position_indexes(self, dataset: xr.Dataset, nldas_ids: List[int]):
        """Reduces spatial dimensions of dataset from 2D (lat, lon) -> 1D (point [lat,lon])
        and extracts the positional indicies of the 1D spatial point array that correspond
        to the request nldas_ids in `nldas_ids`

        Assumptions:
            - nldas_id coordinate is linked to the lat and lon dimensions and therefore transforms to point dimension when dataset is reshaped from (time, lat, lon) -> (time, point)
        """
        ids = np.asarray(nldas_ids, dtype=np.int64)
        ds_points = dataset.stack(point=("lat", "lon"))
        lat_val = ds_points['lat'].values
        lon_val = ds_points['lon'].values
        nldas_id_val = ds_points['nldas_id'].values
        ds_points = ds_points.reset_index("point", drop=True)
        ds_points = ds_points.assign_coords(
            {
                'point': ('point', np.arange(ds_points.sizes['point'])),
                'lat': ("point", lat_val),
                'lon': ("point", lon_val),
                'nldas_id': ("point", nldas_id_val)
            })
        mask = ds_points["nldas_id"].isin(ids)

        points_pos_id = np.flatnonzero(mask.values)  # integer positions in stacked point dim
        points_nldas_id = ds_points["nldas_id"].isel(point=points_pos_id).values.astype(np.int64)

        # optional validation: ensure all requested IDs were found
        found = np.unique(points_nldas_id)
        missing = np.setdiff1d(np.unique(ids), found)
        if missing.size:
            raise KeyError(f"{missing.size} requested nldas_id(s) not found. Example: {missing[:10].tolist()}")

        return points_pos_id.astype(np.int64)

    def create_subset_preprocess(self, nldas_netcdf_file: Path, nldas_ids: List[int]) -> Callable[[xr.Dataset], xr.Dataset]:
        """Creates a preprocessing function that can be provided to xarray open_mfdataset or similar functions
            to subset the spatial dimensions of the dataset to only include the requested nldas_ids.
            
        Assumptions:
            - **Important** All datasets have the same spatial grid and corresponding positional index values.
            - nldas_netcdf_file is a sample netcdf file that can be used to extract the positional indexes of the requested nldas_ids.
            - nldas_id coordinate exists in the dataset and is linked to lat and lon dimensions.
            - Output dataset will have dimensions (time, point) where point is a 1D spatial dimension with auxilliary coordinates for lat, lon, and nldas_id.
        """
        ds = xr.load_dataset(nldas_netcdf_file)
        points_pos_id = self.extract_point_position_indexes(dataset=ds, nldas_ids=nldas_ids) # NOTE: Major assumption that all grids will have same spatial grid and corresponding positional index values
        
        def subset_preprocess(ds: xr.Dataset) -> xr.Dataset:
            ds_points = ds.stack(point=("lat", "lon"))
            ds_points_subset = ds_points.isel(point=points_pos_id)
            lat_val = ds_points_subset['lat'].values
            lon_val = ds_points_subset['lon'].values
            nldas_id_val = ds_points_subset['nldas_id'].values
            ds_points_subset = ds_points_subset.reset_index("point", drop=True)
            ds_points_subset = ds_points_subset.assign_coords(
                {
                    'point': ('point', np.arange(ds_points_subset.sizes['point'])),
                    'lat': ('point',lat_val),
                    'lon': ('point',lon_val),
                    'nldas_id': ('point',nldas_id_val)
                })
            
            # Drop unused variables
            ds_points_subset = ds_points_subset.drop_vars(
                ['available', 'time_bnds', 'LWdown',
                'CRainf_frac', 'CAPE', 'PotEvap'], errors="ignore")
            
            return ds_points_subset
        
        return subset_preprocess


class NldasVariableTransformer:

    def __init__(self):
        self.convert = NldasVariableConverter()
        self.calculate = NldasVariableCalculator()

class NldasVariableConverter:
    
    def tair_k_to_f(self, ds: xr.Dataset) -> xr.Dataset:
        tair = ds['Tair'] - 273.15 # k -> c
        tair = (tair * (9/5)) + 32 # c -> f
        return ds.assign(Tair=tair)

    def potevap_kgm2_to_in(self, ds: xr.Dataset) -> xr.Dataset:
        """[kg/m2/hr] -> [in/hr] water"""
        potevap = ds['PotEvap'] * 39.37008 / 1000
        return ds.assign(PotEvap=potevap)

    def swdown_watts_to_ly(self, ds: xr.Dataset) -> xr.Dataset:
        """Converts downward shortwave radiation from [W/m^2] to [ly/hr]
        
        Conversion: 
        
            [W/m^2] = [J/m^2/s] * 1/41840 [ly/(J/m^2)] * 60 [s/min] * 60 [min/hr]
        
        """
        swdown = (ds['SWdown']/41840)*60*60
        return ds.assign(SWdown=swdown)

    def rainf_kgm2_to_in(self, ds: xr.Dataset) -> xr.Dataset:
        """[kg/m2/hr] -> [in/hr] water"""
        rainf = ds['Rainf'] * 39.37008 / 1000
        return ds.assign(Rainf=rainf)

class NldasVariableCalculator:
    
    def tdpt(self, ds: xr.Dataset) -> xr.Dataset: # TODO: Correct this such that if it is raining then Tdpt >= Tair to signal rain!
        """
        Air Pressure: https://www.engineeringtoolbox.com/air-altitude-pressure-d_462.html
        RH Estimation: https://earthscience.stackexchange.com/questions/2360/how-do-i-convert-specific-humidity-to-relative-humidity
        dewpt Estimation: https://www.omnicalculator.com/physics/dew-point | https://cales.arizona.edu/AZMET/dewpoint.html

        ** Default assume existing temperature units are celcius

        (1) Estimates relative humidity for given the following NLDAS climate variables: air temperature, specific humidity, and surface pressure.
            (a) Estimates saturation water vapor pressure for given specific humidity [kg/kg] and surface pressure [Pa] | Clausius-Clapeyron Equation
            (c) Calculates water vapor mixing ratio at saturation (w_s) | w_s = m_v (sat) / m_d
                (i) Uses ideal gas law (for mass) | PV = m x R_specific x T
                (ii) Uses ratio of water vapor and dry air's specific gas constant = 0.622
                (iii) Dry air and water vapor are occupying the same parcel of space so volumes equal.
                (iv) At equilibrium the temperature of the water vapor and dry air are also equal
                (v) w_s = m_vs / m_d = (e_s * V)/(R_wvap * T) / [(P_dryAir * V)/(R_dryAir * T)] = 0.622 * e_s/P_dryAir
            (d) Calculates relative humidity from ratio of mass mixing ratios | RH = w / w_s

        (2) Calculates Dew Point temperature from relative humidity and current air temperature | Magnus-Tetens Formualtion

        (3) Convert dewpoint temperature units to degrees Farenheit
        """

        # Calculate RH using Clausius-Clapeyron Equation with ideal gas law
        # https://earthscience.stackexchange.com/questions/2360/how-do-i-convert-specific-humidity-to-relative-humidity

        tair_k = ds['Tair'] # NLDAS is in Kelvin
        tair_k_ref = 273.15 # reference air temperature in kelvin
        a = 0.00263 * ds['PSurf'] * ds['Qair']
        b = np.exp((17.67 * (tair_k - tair_k_ref))/(tair_k - 29.65))
        rh = (a / b)
        rh = rh.where(rh <= 1, 1) # NOTE: Check this
        # TODO: Do I need to close tair_k, a, b, and rh after using them to prevent open file handles?
        
        # Calculate dewpoint temperature using Magnus equation
        """
        alpha(T,RH) = ln(RH) + (a x T)/(b + T)
    
            Ts: dew point temperature [C]
            T: air temperature [C]
            RH: relative humidity (unitless deciminal)
            a: Magnus coefficient 1
            b: Magnus coefficient 2

        args:
            a [float]: Magnus coefficient 1 [unitless]; default is 17.625.
            b [float]: Magnus coefficient 2 [deg C]; default is 243.04 [C].
                
        *Assumptions: 
            (1) Temperature is in farenheit -> convert to celcius.
            (2) Relative humidity is decimal.
            (3) dewpoint calculated in Celcius then coverted back to Farenheit.
            (3) Same number of RH values as temperature values.
        """
        A = 17.625
        B = 243.04
        tair_c = ds['Tair'] - 273.15 # k -> c
        alpha = np.log(rh) + (A * tair_c) / (B + tair_c)
        tdpt = (B * alpha) / (A - alpha) # dew point temperature in C

        # Convert temperature from C -> F
        tdpt = (tdpt * (9/5)) + 32

        return ds.assign(Tdpt=tdpt)

    def windspeed(self, ds: xr.Dataset) -> xr.Dataset:
        """Calculates wind speed from vector components (zonal and meridial), then converts units
            from [m/s] to [mi/hr].
        """

        # Calculate wind speed from zonal and meridial vectors
        wind = np.sqrt((ds['Wind_E']**2) + (ds['Wind_N']**2))

        # Convert [m/s] -> [mi/hr] | [m/s] * [1 mi/1609.34 m] * [60 s/min] * [60 min/hr]
        wind = (wind/1609.34)*60*60

        return ds.assign(Wind=wind)

# Optinally add to NldasSpatialSubsetter
    # def create_subset_preprocess(self, fn: Callable, **kwargs):
    #     def preprocess(ds: xr.Dataset):
    #         return fn(ds, **kwargs)
    #     return preprocess

    # def subset_spatial_coords_in_nldas_dataset(self, dataset: xr.Dataset, target_coords: List[Tuple[float,float]]) -> xr.Dataset:
    #     """Subsets the provided nldas request dataset to only include spatial coordinates requested in the provided request.
    #         - Outputs a new xr.Dataset object with the spatial subsetting performed
    #         - Dimensions change from (time, lat, lon) -> (time, point) where point 
    #             has a integer indexed dimensional coordinate coordinate (row-wise counting of raw grid from top-left to bottom right)
    #             and two auxilliary coordinates for lat and lon with their original dimensional coordinate values.
    #         - Assumes nldas_id variable exists
    #     """

    #     # Subset the requested coordinates
    #     target_lat = xr.DataArray([p[0] for p in target_coords], dims="point", name="lat")
    #     target_lon = xr.DataArray([p[1] for p in target_coords], dims="point", name="lon")

    #     subset_dataset = dataset.sel(lat=target_lat, lon=target_lon, method="nearest")
    #     subset_dataset = subset_dataset.assign_coords(point = subset_dataset["nldas_id"])
    #     subset_dataset = subset_dataset.drop_vars("nldas_id")

    #     return subset_dataset